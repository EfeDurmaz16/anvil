package main

import (
	"context"
	"fmt"
	"log/slog"
	"os"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/cli"
	"github.com/efebarandurmaz/anvil/internal/config"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/llmutil"
	"github.com/efebarandurmaz/anvil/internal/pipeline"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	plugindefaults "github.com/efebarandurmaz/anvil/internal/plugins/defaults"
	"github.com/spf13/cobra"
)

func initLogger() {
	configureLoggerFromEnv("", "")
}

func configureLoggerFromEnv(level, format string) {
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

func main() {
	initLogger()

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

	// Register CLI subcommand modules
	cli.RegisterHarnessCommands(rootCmd)
	cli.RegisterSnapshotCommands(rootCmd)
	cli.RegisterGateCommands(rootCmd)
	cli.RegisterDepgraphCommands(rootCmd)
	cli.RegisterIncrementalCommands(rootCmd)
	cli.RegisterDashboardCommands(rootCmd)
	cli.RegisterReviewCommands(rootCmd)

	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

func runPipeline(configPath, sourceLang, targetLang, inputPath, outputPath string, jsonReport bool) error {
	cfg, err := config.Load(configPath)
	if err != nil {
		slog.Warn("config load failed, using defaults", "error", err)
		cfg = &config.Config{}
	}

	// Reconfigure logger with config values
	configureLoggerFromEnv(cfg.Log.Level, cfg.Log.Format)

	// Register plugins
	registry := plugins.NewRegistry()
	plugindefaults.RegisterAllDefaults(registry)

	// Build LLM providers (default + per-agent overrides with rate limiting).
	factory := llm.NewFactory()
	llmutil.RegisterDefaultProviders(factory)

	providers, err := llm.SetupProviders(cfg.LLM, factory)
	if err != nil {
		return fmt.Errorf("setting up LLM providers: %w", err)
	}

	ctx := context.Background()
	_, err = pipeline.Run(ctx, pipeline.PipelineConfig{
		SourceLang: sourceLang,
		TargetLang: targetLang,
		InputPath:  inputPath,
		OutputPath: outputPath,
		Config:     cfg,
		Registry:   registry,
		Providers:  providers,
		JsonReport: jsonReport,
	})
	return err
}
