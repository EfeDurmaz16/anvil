package main

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"time"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/agents/architect"
	"github.com/efebarandurmaz/anvil/internal/agents/cartographer"
	"github.com/efebarandurmaz/anvil/internal/agents/judge"
	"github.com/efebarandurmaz/anvil/internal/agents/specular"
	"github.com/efebarandurmaz/anvil/internal/config"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/llm/anthropic"
	"github.com/efebarandurmaz/anvil/internal/llm/openai"
	"github.com/efebarandurmaz/anvil/internal/metrics"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	cobolplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/cobol"
	fortranplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/fortran"
	perlplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/perl"
	javaplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/java"
	"github.com/spf13/cobra"
)

func main() {
	var (
		sourceLang string
		targetLang string
		inputPath  string
		outputPath string
		configPath string
		jsonReport bool
	)

	rootCmd := &cobra.Command{
		Use:   "anvil",
		Short: "Multi-agent legacy code modernization platform",
	}

	runCmd := &cobra.Command{
		Use:   "run",
		Short: "Run the modernization pipeline",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runPipeline(configPath, sourceLang, targetLang, inputPath, outputPath, jsonReport)
		},
	}

	runCmd.Flags().StringVar(&sourceLang, "source", "cobol", "Source language")
	runCmd.Flags().StringVar(&targetLang, "target", "java", "Target language")
	runCmd.Flags().StringVar(&inputPath, "input", "", "Input path (file or directory)")
	runCmd.Flags().StringVar(&outputPath, "output", "", "Output directory")
	runCmd.Flags().StringVar(&configPath, "config", "configs/anvil.yaml", "Config file path")
	runCmd.Flags().BoolVar(&jsonReport, "json", false, "Output metrics as JSON")
	_ = runCmd.MarkFlagRequired("input")
	_ = runCmd.MarkFlagRequired("output")

	providersCmd := &cobra.Command{
		Use:   "providers",
		Short: "List available LLM providers",
		Run: func(cmd *cobra.Command, args []string) {
			fmt.Println("Available LLM providers:")
			fmt.Println()
			for name, url := range llm.KnownProviders {
				fmt.Printf("  %-14s %s\n", name, url)
			}
			fmt.Println("  custom         (set base_url to any OpenAI-compatible endpoint)")
			fmt.Println("  none           (run without LLM — template-only generation)")
			fmt.Println()
			fmt.Println("Configure in anvil.yaml or via environment:")
			fmt.Println("  ANVIL_LLM_PROVIDER=groq")
			fmt.Println("  ANVIL_LLM_API_KEY=gsk_...")
			fmt.Println("  ANVIL_LLM_MODEL=llama-3.3-70b-versatile")
		},
	}

	rootCmd.AddCommand(runCmd, providersCmd)

	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

func runPipeline(configPath, sourceLang, targetLang, inputPath, outputPath string, jsonReport bool) error {
	m := metrics.New()

	cfg, err := config.Load(configPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Warning: config load failed (%v), using defaults\n", err)
		cfg = &config.Config{}
	}

	registry := plugins.NewRegistry()
	registry.RegisterSource(cobolplugin.New())
	registry.RegisterSource(perlplugin.New())
	registry.RegisterSource(fortranplugin.New())
	registry.RegisterTarget(javaplugin.New())

	// Build LLM provider via factory
	factory := llm.NewFactory()
	factory.Register("anthropic", func(c llm.ProviderConfig) (llm.Provider, error) {
		return anthropic.New(c.APIKey, c.Model, c.BaseURL), nil
	})
	factory.Register("openai", func(c llm.ProviderConfig) (llm.Provider, error) {
		return openai.New(c.APIKey, c.Model, c.BaseURL, c.EmbedModel), nil
	})
	// All OpenAI-compatible providers
	for _, p := range []struct{ name, url string }{
		{"groq", llm.KnownProviders["groq"]},
		{"huggingface", llm.KnownProviders["huggingface"]},
		{"ollama", llm.KnownProviders["ollama"]},
		{"together", llm.KnownProviders["together"]},
		{"deepseek", llm.KnownProviders["deepseek"]},
		{"custom", ""},
	} {
		p := p
		factory.Register(p.name, func(c llm.ProviderConfig) (llm.Provider, error) {
			base := c.BaseURL
			if base == "" {
				base = p.url
			}
			return openai.New(c.APIKey, c.Model, base, c.EmbedModel), nil
		})
	}

	provider, err := factory.Create(llm.ProviderConfig{
		Provider: cfg.LLM.Provider,
		APIKey:   cfg.LLM.APIKey,
		Model:    cfg.LLM.Model,
		BaseURL:  cfg.LLM.BaseURL,
	})
	if err != nil {
		return fmt.Errorf("creating LLM provider: %w", err)
	}

	if provider == nil {
		m.LLMMode = "passthrough"
		fmt.Println("Running without LLM (template-only mode)")
	} else {
		m.LLMMode = "llm:" + provider.Name()
		fmt.Printf("Using LLM provider: %s\n", provider.Name())
	}

	ctx := context.Background()

	// Step 1: Cartographer
	fmt.Println("\n=== Cartographer: Parsing source ===")
	start := time.Now()
	cart := cartographer.New()
	cartResult, err := cart.Run(ctx, &agents.AgentContext{
		Registry: registry,
		Params:   map[string]string{"source": sourceLang, "input": inputPath},
	})
	if err != nil {
		return fmt.Errorf("cartographer: %w", err)
	}
	m.AddAgent("cartographer", time.Since(start), "parse", 0)
	m.CollectSource(sourceLang, countFiles(inputPath), cartResult.Graph)
	fmt.Printf("  Parsed %d modules, %d functions\n", m.Source.ModuleCount, m.Source.FunctionCount)

	// Step 2: Specular
	fmt.Println("\n=== Specular: Extracting business rules ===")
	start = time.Now()
	spec := specular.New()
	specResult, err := spec.Run(ctx, &agents.AgentContext{
		Graph:    cartResult.Graph,
		LLM:      provider,
		Registry: registry,
	})
	if err != nil {
		return fmt.Errorf("specular: %w", err)
	}
	specMode := "llm"
	if specResult.Metadata != nil && specResult.Metadata["mode"] == "passthrough" {
		specMode = "passthrough"
	}
	m.AddAgent("specular", time.Since(start), specMode, len(specResult.Errors))
	m.Source.RuleCount = len(cartResult.Graph.BusinessRules)
	fmt.Printf("  Extracted %d business rules [%s]\n", m.Source.RuleCount, specMode)

	// Step 3 + 4: Architect → Judge with retry
	const maxRetries = 2
	var finalFiles []plugins.GeneratedFile
	var finalScore float64
	var allErrors []string

	for attempt := 0; attempt <= maxRetries; attempt++ {
		fmt.Printf("\n=== Architect: Generating %s (attempt %d/%d) ===\n", targetLang, attempt+1, maxRetries+1)
		start = time.Now()
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
		archMode := "template"
		if provider != nil {
			archMode = "llm"
		}
		m.AddAgent("architect", time.Since(start), archMode, 0)
		fmt.Printf("  Generated %d files\n", len(archResult.GeneratedFiles))

		fmt.Println("\n=== Judge: Verifying ===")
		start = time.Now()
		j := judge.New()
		judgeResult, err := j.Run(ctx, &agents.AgentContext{
			Graph:  cartResult.Graph,
			LLM:    provider,
			Params: map[string]string{"generated_code": ""},
		})
		if err != nil {
			return fmt.Errorf("judge: %w", err)
		}
		judgeMode := "llm"
		if judgeResult.Metadata != nil && judgeResult.Metadata["mode"] == "passthrough" {
			judgeMode = "passthrough"
		}
		m.AddAgent("judge", time.Since(start), judgeMode, len(judgeResult.Errors))

		finalFiles = archResult.GeneratedFiles
		finalScore = judgeResult.Score
		allErrors = judgeResult.Errors
		fmt.Printf("  Score: %.2f [%s]\n", judgeResult.Score, judgeMode)

		if judgeResult.Score >= 0.8 {
			break
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
	}

	// Finalize metrics
	m.CollectTarget(targetLang, finalFiles)
	m.Finish(finalScore, allErrors)

	if jsonReport {
		data, _ := m.JSON()
		fmt.Println(string(data))
	} else {
		m.PrintSummary(os.Stdout)
	}

	return nil
}

func countFiles(path string) int {
	info, err := os.Stat(path)
	if err != nil {
		return 0
	}
	if !info.IsDir() {
		return 1
	}
	count := 0
	filepath.Walk(path, func(_ string, fi os.FileInfo, _ error) error {
		if !fi.IsDir() {
			count++
		}
		return nil
	})
	return count
}
