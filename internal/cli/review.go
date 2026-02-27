package cli

import (
	"context"
	"encoding/json"
	"fmt"
	"log/slog"
	"os"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/agents/architect"
	"github.com/efebarandurmaz/anvil/internal/agents/cartographer"
	"github.com/efebarandurmaz/anvil/internal/agents/judge"
	"github.com/efebarandurmaz/anvil/internal/config"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/llmutil"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	plugindefaults "github.com/efebarandurmaz/anvil/internal/plugins/defaults"
	"github.com/efebarandurmaz/anvil/internal/tui"
	"github.com/spf13/cobra"
)

// ConfigureLoggerFromEnv reconfigures the logger based on config and environment variables.
// Exported so main.go can also use it.
func ConfigureLoggerFromEnv(level, format string) {
	// Environment overrides
	if envFmt := os.Getenv("ANVIL_LOG_FORMAT"); envFmt != "" {
		format = envFmt
	}
	if envLvl := os.Getenv("ANVIL_LOG_LEVEL"); envLvl != "" {
		level = envLvl
	}

	// Parse log level
	var slogLevel slog.Level
	switch strings.ToLower(level) {
	case "debug":
		slogLevel = slog.LevelDebug
	case "warn", "warning":
		slogLevel = slog.LevelWarn
	case "error":
		slogLevel = slog.LevelError
	default:
		slogLevel = slog.LevelInfo
	}

	opts := &slog.HandlerOptions{Level: slogLevel}

	var handler slog.Handler
	if format == "json" {
		handler = slog.NewJSONHandler(os.Stderr, opts)
	} else {
		handler = slog.NewTextHandler(os.Stderr, opts)
	}
	slog.SetDefault(slog.New(handler))
}

// RunReview runs the interactive review process.
func RunReview(sourcePath, targetLang, configPath, outputPath string, scoreThreshold float64) error {
	ctx := context.Background()

	// Load config
	cfg, err := config.Load(configPath)
	if err != nil {
		slog.Warn("config load failed, using defaults", "error", err)
		cfg = &config.Config{}
	}

	// Reconfigure logger
	ConfigureLoggerFromEnv(cfg.Log.Level, cfg.Log.Format)

	// Build default LLM request options from config
	defaultOpts := &llm.RequestOptions{}
	if cfg.LLM.Temperature > 0 {
		temp := cfg.LLM.Temperature
		defaultOpts.Temperature = &temp
	}
	if cfg.LLM.MaxTokens > 0 {
		maxTok := cfg.LLM.MaxTokens
		defaultOpts.MaxTokens = &maxTok
	}

	// Register plugins
	registry := plugins.NewRegistry()
	plugindefaults.RegisterAllDefaults(registry)

	// Create LLM provider
	factory := llm.NewFactory()
	llmutil.RegisterDefaultProviders(factory)

	provider, err := factory.Create(llm.ProviderConfig{
		Provider: cfg.LLM.Provider,
		APIKey:   cfg.LLM.APIKey,
		Model:    cfg.LLM.Model,
		BaseURL:  cfg.LLM.BaseURL,
	})
	if err != nil {
		return fmt.Errorf("creating LLM provider: %w", err)
	}
	if provider != nil {
		provider = llm.WithRateLimit(provider, llm.DefaultRateLimitConfig())
	}

	// Determine source language from file extension or default to cobol
	sourceLang := "cobol"
	if strings.HasSuffix(sourcePath, ".f") || strings.HasSuffix(sourcePath, ".f90") {
		sourceLang = "fortran"
	} else if strings.HasSuffix(sourcePath, ".pl") {
		sourceLang = "perl"
	}

	slog.Info("starting review", "source", sourcePath, "source_lang", sourceLang, "target_lang", targetLang)

	// Run Cartographer
	slog.Info("cartographer: parsing source")
	cart := cartographer.New()
	cartResult, err := cart.Run(ctx, &agents.AgentContext{
		Registry:    registry,
		Params:      map[string]string{"source": sourceLang, "input": sourcePath},
		DefaultOpts: defaultOpts,
	})
	if err != nil {
		return fmt.Errorf("cartographer: %w", err)
	}

	// Run Architect
	slog.Info("architect: generating target", "language", targetLang)
	arch := architect.New()
	archResult, err := arch.Run(ctx, &agents.AgentContext{
		Graph:       cartResult.Graph,
		LLM:         provider,
		Registry:    registry,
		Params:      map[string]string{"target": targetLang},
		DefaultOpts: defaultOpts,
	})
	if err != nil {
		return fmt.Errorf("architect: %w", err)
	}

	// Run Judge
	slog.Info("judge: verifying output")
	j := judge.New()
	genFilesJSON, _ := json.Marshal(archResult.GeneratedFiles)
	judgeResult, err := j.Run(ctx, &agents.AgentContext{
		Graph: cartResult.Graph,
		LLM:   provider,
		Params: map[string]string{
			"source":          sourceLang,
			"target":          targetLang,
			"generated_files": string(genFilesJSON),
		},
		DefaultOpts: defaultOpts,
	})
	if err != nil {
		return fmt.Errorf("judge: %w", err)
	}

	// Create review session
	session := tui.NewReviewSession(cartResult.Graph, archResult.GeneratedFiles, judgeResult)

	// Auto-approve items above threshold
	if scoreThreshold > 0 {
		autoApproved := 0
		for i := range session.Items {
			if session.Items[i].JudgeScore >= scoreThreshold {
				session.Items[i].Status = tui.ReviewApproved
				autoApproved++
			}
		}
		if autoApproved > 0 {
			slog.Info("auto-approved items above threshold", "count", autoApproved, "threshold", scoreThreshold)
		}
	}

	// Run interactive TUI
	finalSession, err := tui.RunReview(session)
	if err != nil {
		return fmt.Errorf("review TUI: %w", err)
	}

	// Save review report
	if err := tui.SaveReviewReport(finalSession, outputPath); err != nil {
		return fmt.Errorf("save review report: %w", err)
	}

	// Print summary
	approved := 0
	rejected := 0
	for _, item := range finalSession.Items {
		if item.Status == tui.ReviewApproved {
			approved++
		} else if item.Status == tui.ReviewRejected {
			rejected++
		}
	}

	fmt.Printf("\n=== Review Summary ===\n")
	fmt.Printf("Total items:    %d\n", len(session.Items))
	fmt.Printf("Approved:       %d\n", approved)
	fmt.Printf("Rejected:       %d\n", rejected)
	fmt.Printf("Average score:  %.2f\n", judgeResult.Score)
	fmt.Printf("Report saved:   %s\n", outputPath)

	return nil
}

// RegisterReviewCommands registers the review subcommand.
func RegisterReviewCommands(parent *cobra.Command) {
	var (
		reviewSource         string
		reviewTargetLang     string
		reviewConfig         string
		reviewOutput         string
		reviewScoreThreshold float64
	)
	reviewCmd := &cobra.Command{
		Use:   "review",
		Short: "Interactive review of migration results",
		RunE: func(cmd *cobra.Command, args []string) error {
			return RunReview(reviewSource, reviewTargetLang, reviewConfig, reviewOutput, reviewScoreThreshold)
		},
	}
	reviewCmd.Flags().StringVarP(&reviewSource, "source", "s", "", "Path to source file(s)")
	reviewCmd.Flags().StringVarP(&reviewTargetLang, "target-lang", "t", "", "Target language (typescript, python, go, java)")
	reviewCmd.Flags().StringVarP(&reviewConfig, "config", "c", "anvil.yaml", "Config file path")
	reviewCmd.Flags().StringVarP(&reviewOutput, "output", "o", "review-report.json", "Output report path")
	reviewCmd.Flags().Float64Var(&reviewScoreThreshold, "score-threshold", 0.0, "Minimum judge score to auto-approve (default 0.0, manual review for all)")
	_ = reviewCmd.MarkFlagRequired("source")
	_ = reviewCmd.MarkFlagRequired("target-lang")

	parent.AddCommand(reviewCmd)
}
