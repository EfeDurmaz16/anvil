package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/agents/architect"
	"github.com/efebarandurmaz/anvil/internal/agents/cartographer"
	"github.com/efebarandurmaz/anvil/internal/agents/judge"
	"github.com/efebarandurmaz/anvil/internal/agents/specular"
	"github.com/efebarandurmaz/anvil/internal/config"
	"github.com/efebarandurmaz/anvil/internal/llm/anthropic"
	"github.com/efebarandurmaz/anvil/internal/llm/openai"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	cobolplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/cobol"
	javaplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/java"
	"github.com/spf13/cobra"

	"github.com/efebarandurmaz/anvil/internal/llm"
)

func main() {
	var (
		sourceLang string
		targetLang string
		inputPath  string
		outputPath string
		configPath string
	)

	rootCmd := &cobra.Command{
		Use:   "anvil",
		Short: "Multi-agent legacy code modernization platform",
	}

	runCmd := &cobra.Command{
		Use:   "run",
		Short: "Run the modernization pipeline",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runPipeline(configPath, sourceLang, targetLang, inputPath, outputPath)
		},
	}

	runCmd.Flags().StringVar(&sourceLang, "source", "cobol", "Source language")
	runCmd.Flags().StringVar(&targetLang, "target", "java", "Target language")
	runCmd.Flags().StringVar(&inputPath, "input", "", "Input path (file or directory)")
	runCmd.Flags().StringVar(&outputPath, "output", "", "Output directory")
	runCmd.Flags().StringVar(&configPath, "config", "configs/anvil.yaml", "Config file path")
	_ = runCmd.MarkFlagRequired("input")
	_ = runCmd.MarkFlagRequired("output")

	rootCmd.AddCommand(runCmd)

	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

func runPipeline(configPath, sourceLang, targetLang, inputPath, outputPath string) error {
	cfg, err := config.Load(configPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Warning: config load failed (%v), using defaults\n", err)
		cfg = &config.Config{}
	}

	registry := plugins.NewRegistry()
	registry.RegisterSource(cobolplugin.New())
	registry.RegisterTarget(javaplugin.New())

	var provider llm.Provider
	switch cfg.LLM.Provider {
	case "openai":
		provider = openai.New(cfg.LLM.APIKey, cfg.LLM.Model, cfg.LLM.BaseURL, "")
	default:
		provider = anthropic.New(cfg.LLM.APIKey, cfg.LLM.Model, cfg.LLM.BaseURL)
	}

	ctx := context.Background()

	// Step 1: Cartographer
	fmt.Println("=== Cartographer: Parsing source ===")
	cart := cartographer.New()
	cartResult, err := cart.Run(ctx, &agents.AgentContext{
		Registry: registry,
		Params:   map[string]string{"source": sourceLang, "input": inputPath},
	})
	if err != nil {
		return fmt.Errorf("cartographer: %w", err)
	}

	graphJSON, _ := json.MarshalIndent(cartResult.Graph, "", "  ")
	fmt.Printf("Parsed %d modules\n", len(cartResult.Graph.Modules))

	// Step 2: Specular
	fmt.Println("=== Specular: Extracting business rules ===")
	spec := specular.New()
	specResult, err := spec.Run(ctx, &agents.AgentContext{
		Graph:    cartResult.Graph,
		LLM:      provider,
		Registry: registry,
	})
	if err != nil {
		return fmt.Errorf("specular: %w", err)
	}
	_ = specResult

	// Step 3 + 4: Architect → Judge with retry
	const maxRetries = 2
	var finalFiles []plugins.GeneratedFile

	for attempt := 0; attempt <= maxRetries; attempt++ {
		fmt.Printf("=== Architect: Generating %s (attempt %d) ===\n", targetLang, attempt+1)
		arch := architect.New()
		archResult, err := arch.Run(ctx, &agents.AgentContext{
			Graph:    cartResult.Graph,
			LLM:      provider,
			Registry: registry,
			Params:   map[string]string{"target": targetLang},
		})
		if err != nil {
			return fmt.Errorf("architect: %w", err)
		}

		fmt.Println("=== Judge: Verifying ===")
		j := judge.New()
		judgeResult, err := j.Run(ctx, &agents.AgentContext{
			Graph: cartResult.Graph,
			LLM:   provider,
			Params: map[string]string{"generated_code": string(graphJSON)},
		})
		if err != nil {
			return fmt.Errorf("judge: %w", err)
		}

		finalFiles = archResult.GeneratedFiles
		fmt.Printf("Score: %.2f\n", judgeResult.Score)

		if judgeResult.Score >= 0.8 {
			break
		}
		if len(judgeResult.Errors) > 0 {
			fmt.Printf("Issues: %v\n", judgeResult.Errors)
		}
	}

	// Write output
	for _, f := range finalFiles {
		outPath := filepath.Join(outputPath, f.Path)
		if err := os.MkdirAll(filepath.Dir(outPath), 0o755); err != nil {
			return err
		}
		if err := os.WriteFile(outPath, f.Content, 0o644); err != nil {
			return err
		}
		fmt.Printf("Wrote: %s\n", outPath)
	}

	fmt.Println("=== Done ===")
	return nil
}
