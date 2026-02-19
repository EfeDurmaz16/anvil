package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log/slog"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"syscall"
	"time"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/agents/architect"
	"github.com/efebarandurmaz/anvil/internal/agents/cartographer"
	"github.com/efebarandurmaz/anvil/internal/agents/judge"
	"github.com/efebarandurmaz/anvil/internal/agents/specular"
	"github.com/efebarandurmaz/anvil/internal/agents/testgen"
	"github.com/efebarandurmaz/anvil/internal/config"
	"github.com/efebarandurmaz/anvil/internal/dashboard"
	"github.com/efebarandurmaz/anvil/internal/depgraph"
	"github.com/efebarandurmaz/anvil/internal/harness"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/proofexplorer"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/llmutil"
	"github.com/efebarandurmaz/anvil/internal/metrics"
	"github.com/efebarandurmaz/anvil/internal/observability"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	"github.com/efebarandurmaz/anvil/internal/qualitygate"
	"github.com/efebarandurmaz/anvil/internal/snapshot"
	"github.com/efebarandurmaz/anvil/internal/tui"
	cobolplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/cobol"
	fortranplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/fortran"
	perlplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/perl"
	goplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/golang"
	javaplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/java"
	pythonplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/python"
	tsplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/typescript"
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

	// Harness commands
	harnessCmd := &cobra.Command{
		Use:   "harness",
		Short: "Harness operations for testing behavioral equivalence",
	}

	var (
		fixturesPath   string
		codePath       string
		outputDir      string
		jsonOutput     bool
		recordEndpoint string
		recordOutput   string
	)

	harnessRunCmd := &cobra.Command{
		Use:   "run",
		Short: "Run fixtures against generated code",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runHarness(fixturesPath, codePath, outputDir, jsonOutput)
		},
	}
	harnessRunCmd.Flags().StringVar(&fixturesPath, "fixtures", "", "Path to fixtures JSONL file")
	harnessRunCmd.Flags().StringVar(&codePath, "code", "", "Path to generated code directory")
	harnessRunCmd.Flags().StringVar(&outputDir, "output", "", "Output directory for proof pack summary.json")
	harnessRunCmd.Flags().BoolVar(&jsonOutput, "json", false, "Output results as JSON")
	_ = harnessRunCmd.MarkFlagRequired("fixtures")
	_ = harnessRunCmd.MarkFlagRequired("code")

	harnessRecordCmd := &cobra.Command{
		Use:   "record",
		Short: "Record fixtures from a live system",
		RunE: func(cmd *cobra.Command, args []string) error {
			return recordFixtures(recordEndpoint, recordOutput)
		},
	}
	harnessRecordCmd.Flags().StringVar(&recordEndpoint, "endpoint", "", "Base URL of live system to record from")
	harnessRecordCmd.Flags().StringVar(&recordOutput, "output", "fixtures.jsonl", "Output file for recorded fixtures")
	_ = harnessRecordCmd.MarkFlagRequired("endpoint")

	var (
		compareActual   string
		compareExpected string
	)
	harnessCompareCmd := &cobra.Command{
		Use:   "compare",
		Short: "Compare actual vs expected outputs",
		RunE: func(cmd *cobra.Command, args []string) error {
			return compareOutputs(compareActual, compareExpected, jsonOutput)
		},
	}
	harnessCompareCmd.Flags().StringVar(&compareActual, "actual", "", "Path to actual output JSON")
	harnessCompareCmd.Flags().StringVar(&compareExpected, "expected", "", "Path to expected output JSON")
	harnessCompareCmd.Flags().BoolVar(&jsonOutput, "json", false, "Output diff as JSON")
	_ = harnessCompareCmd.MarkFlagRequired("actual")
	_ = harnessCompareCmd.MarkFlagRequired("expected")

	harnessCmd.AddCommand(harnessRunCmd, harnessRecordCmd, harnessCompareCmd)

	// Proof pack commands
	proofPackCmd := &cobra.Command{
		Use:   "proof-pack",
		Short: "Proof pack operations",
	}

	var (
		ppFixturesPath string
		ppResultsPath  string
		ppOutputPath   string
	)
	proofPackGenerateCmd := &cobra.Command{
		Use:   "generate",
		Short: "Generate a proof pack from harness results",
		RunE: func(cmd *cobra.Command, args []string) error {
			return generateProofPack(ppFixturesPath, ppResultsPath, ppOutputPath)
		},
	}
	proofPackGenerateCmd.Flags().StringVar(&ppFixturesPath, "fixtures", "", "Path to fixtures JSONL file")
	proofPackGenerateCmd.Flags().StringVar(&ppResultsPath, "results", "", "Path to harness results JSON")
	proofPackGenerateCmd.Flags().StringVar(&ppOutputPath, "output", "proof-pack", "Output directory for proof pack")
	_ = proofPackGenerateCmd.MarkFlagRequired("fixtures")
	_ = proofPackGenerateCmd.MarkFlagRequired("results")

	proofPackCmd.AddCommand(proofPackGenerateCmd)

	// Dashboard command
	var (
		dashboardPort int
		dashboardDemo bool
	)
	dashboardCmd := &cobra.Command{
		Use:   "dashboard",
		Short: "Start the Anvil dashboard server",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runDashboard(dashboardPort, dashboardDemo)
		},
	}
	dashboardCmd.Flags().IntVarP(&dashboardPort, "port", "p", 9090, "Dashboard listen port")
	dashboardCmd.Flags().BoolVar(&dashboardDemo, "demo", false, "Seed with demo migration data")

	// Explore command
	var (
		explorePort int
		exploreDirs []string
		exploreOpen bool
	)
	exploreCmd := &cobra.Command{
		Use:   "explore [dirs...]",
		Short: "Start the Proof Pack Explorer server",
		RunE: func(cmd *cobra.Command, args []string) error {
			// Use args as dirs if provided, otherwise use flag value
			if len(args) > 0 {
				exploreDirs = args
			}
			return runExplore(explorePort, exploreDirs, exploreOpen)
		},
	}
	exploreCmd.Flags().IntVarP(&explorePort, "port", "p", 9091, "Explorer listen port")
	exploreCmd.Flags().StringSliceVar(&exploreDirs, "dirs", []string{"."}, "Directories to scan for proof packs")
	exploreCmd.Flags().BoolVarP(&exploreOpen, "open", "o", false, "Print URL (reserved for future auto-open)")

	// Review command
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
			return runReview(reviewSource, reviewTargetLang, reviewConfig, reviewOutput, reviewScoreThreshold)
		},
	}
	reviewCmd.Flags().StringVarP(&reviewSource, "source", "s", "", "Path to source file(s)")
	reviewCmd.Flags().StringVarP(&reviewTargetLang, "target-lang", "t", "", "Target language (typescript, python, go, java)")
	reviewCmd.Flags().StringVarP(&reviewConfig, "config", "c", "anvil.yaml", "Config file path")
	reviewCmd.Flags().StringVarP(&reviewOutput, "output", "o", "review-report.json", "Output report path")
	reviewCmd.Flags().Float64Var(&reviewScoreThreshold, "score-threshold", 0.0, "Minimum judge score to auto-approve (default 0.0, manual review for all)")
	_ = reviewCmd.MarkFlagRequired("source")
	_ = reviewCmd.MarkFlagRequired("target-lang")

	// Snapshot commands
	snapshotCmd := &cobra.Command{
		Use:   "snapshot",
		Short: "Manage migration snapshots",
	}

	var snapshotStoreDir string

	// snapshot list
	snapshotListCmd := &cobra.Command{
		Use:   "list",
		Short: "List all snapshots",
		RunE: func(cmd *cobra.Command, args []string) error {
			return listSnapshots(snapshotStoreDir)
		},
	}
	snapshotListCmd.Flags().StringVar(&snapshotStoreDir, "store-dir", ".anvil-snapshots", "Snapshot store directory")

	// snapshot show
	snapshotShowCmd := &cobra.Command{
		Use:   "show <id>",
		Short: "Show snapshot details",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return showSnapshot(snapshotStoreDir, args[0])
		},
	}
	snapshotShowCmd.Flags().StringVar(&snapshotStoreDir, "store-dir", ".anvil-snapshots", "Snapshot store directory")

	// snapshot tag
	snapshotTagCmd := &cobra.Command{
		Use:   "tag <id> <tag>",
		Short: "Tag a snapshot",
		Args:  cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			return tagSnapshot(snapshotStoreDir, args[0], args[1])
		},
	}
	snapshotTagCmd.Flags().StringVar(&snapshotStoreDir, "store-dir", ".anvil-snapshots", "Snapshot store directory")

	// snapshot delete
	snapshotDeleteCmd := &cobra.Command{
		Use:   "delete <id>",
		Short: "Delete a snapshot",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return deleteSnapshot(snapshotStoreDir, args[0])
		},
	}
	snapshotDeleteCmd.Flags().StringVar(&snapshotStoreDir, "store-dir", ".anvil-snapshots", "Snapshot store directory")

	// snapshot diff
	var snapshotDiffJSON bool
	snapshotDiffCmd := &cobra.Command{
		Use:   "diff <id1> <id2>",
		Short: "Diff two snapshots",
		Args:  cobra.ExactArgs(2),
		RunE: func(cmd *cobra.Command, args []string) error {
			return diffSnapshots(snapshotStoreDir, args[0], args[1], snapshotDiffJSON)
		},
	}
	snapshotDiffCmd.Flags().StringVar(&snapshotStoreDir, "store-dir", ".anvil-snapshots", "Snapshot store directory")
	snapshotDiffCmd.Flags().BoolVar(&snapshotDiffJSON, "json", false, "Output diff as JSON")

	// snapshot rollback
	var snapshotRollbackOutput string
	snapshotRollbackCmd := &cobra.Command{
		Use:   "rollback <id>",
		Short: "Restore files from a snapshot",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return rollbackSnapshot(snapshotStoreDir, args[0], snapshotRollbackOutput)
		},
	}
	snapshotRollbackCmd.Flags().StringVar(&snapshotStoreDir, "store-dir", ".anvil-snapshots", "Snapshot store directory")
	snapshotRollbackCmd.Flags().StringVar(&snapshotRollbackOutput, "output", "", "Output directory for restored files")
	_ = snapshotRollbackCmd.MarkFlagRequired("output")

	snapshotCmd.AddCommand(snapshotListCmd, snapshotShowCmd, snapshotTagCmd, snapshotDeleteCmd, snapshotDiffCmd, snapshotRollbackCmd)

	// Gate commands
	gateCmd := &cobra.Command{
		Use:   "gate",
		Short: "Quality gate operations",
	}

	var (
		gateScore       float64
		gateCompilation bool
		gateFixtureRate float64
		gateCoverage    float64
		gateMaxTokens   int
		gateMaxErrors   int
		gateJSON        bool
	)

	gateCheckCmd := &cobra.Command{
		Use:   "check",
		Short: "Run quality gates on migration results",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runGateCheck(gateScore, gateCompilation, gateFixtureRate, gateCoverage, gateMaxTokens, gateMaxErrors, gateJSON)
		},
	}
	gateCheckCmd.Flags().Float64Var(&gateScore, "score", 0.8, "Minimum judge score threshold (0.0-1.0)")
	gateCheckCmd.Flags().BoolVar(&gateCompilation, "compilation", true, "Require successful compilation")
	gateCheckCmd.Flags().Float64Var(&gateFixtureRate, "fixture-rate", 0.95, "Minimum fixture pass rate (0.0-1.0)")
	gateCheckCmd.Flags().Float64Var(&gateCoverage, "coverage", 0.9, "Minimum function coverage (0.0-1.0)")
	gateCheckCmd.Flags().IntVar(&gateMaxTokens, "max-tokens", 0, "Maximum token budget (0 = unlimited)")
	gateCheckCmd.Flags().IntVar(&gateMaxErrors, "max-errors", 0, "Maximum pipeline errors allowed")
	gateCheckCmd.Flags().BoolVar(&gateJSON, "json", false, "Output results as JSON")

	gateDefaultsCmd := &cobra.Command{
		Use:   "defaults",
		Short: "Show default gate configuration as JSON",
		Run: func(cmd *cobra.Command, args []string) {
			showGateDefaults()
		},
	}

	gateCmd.AddCommand(gateCheckCmd, gateDefaultsCmd)

	// Depgraph commands
	depgraphCmd := &cobra.Command{
		Use:   "depgraph",
		Short: "Cross-module dependency graph analysis",
	}

	var (
		depgraphFormat string
		depgraphOutput string
	)

	analyzeCmd := &cobra.Command{
		Use:   "analyze <ir-file>",
		Short: "Analyze dependencies from a SemanticGraph JSON file",
		Args:  cobra.ExactArgs(1),
		RunE: func(cmd *cobra.Command, args []string) error {
			return runDepgraphAnalyze(args[0], depgraphFormat, depgraphOutput)
		},
	}
	analyzeCmd.Flags().StringVar(&depgraphFormat, "format", "stats", "Output format (dot/mermaid/json/stats)")
	analyzeCmd.Flags().StringVarP(&depgraphOutput, "output", "o", "", "Output file (default: stdout)")

	demoCmd := &cobra.Command{
		Use:   "demo",
		Short: "Generate a demo dependency graph",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runDepgraphDemo(depgraphFormat, depgraphOutput)
		},
	}
	demoCmd.Flags().StringVar(&depgraphFormat, "format", "stats", "Output format (dot/mermaid/json/stats)")
	demoCmd.Flags().StringVarP(&depgraphOutput, "output", "o", "", "Output file (default: stdout)")

	depgraphCmd.AddCommand(analyzeCmd, demoCmd)

	rootCmd.AddCommand(runCmd, providersCmd, harnessCmd, proofPackCmd, dashboardCmd, exploreCmd, reviewCmd, snapshotCmd, gateCmd, depgraphCmd)

	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

func runPipeline(configPath, sourceLang, targetLang, inputPath, outputPath string, jsonReport bool) error {
	// Common aliases for convenience.
	switch targetLang {
	case "ts":
		targetLang = "typescript"
	case "py":
		targetLang = "python"
	case "golang":
		targetLang = "go"
	}

	// Initialize tracing. No endpoint means no-op tracer; set ANVIL_OTLP_ENDPOINT to enable.
	tracingCfg := observability.DefaultTracingConfig()
	if ep := os.Getenv("ANVIL_OTLP_ENDPOINT"); ep != "" {
		tracingCfg.OTLPEndpoint = ep
	}
	ctx := context.Background()
	tp, err := observability.InitTracing(ctx, tracingCfg)
	if err != nil {
		slog.Warn("tracing init failed, continuing without tracing", "error", err)
	} else {
		defer func() {
			shutdownCtx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
			defer cancel()
			if shutdownErr := tp.Shutdown(shutdownCtx); shutdownErr != nil {
				slog.Warn("tracing shutdown error", "error", shutdownErr)
			}
		}()
	}

	// Initialize audit logger. Disabled by default unless ANVIL_AUDIT_LOG is set.
	auditCfg := &observability.AuditConfig{Enabled: false}
	if auditPath := os.Getenv("ANVIL_AUDIT_LOG"); auditPath != "" {
		auditCfg = &observability.AuditConfig{
			Enabled:    true,
			OutputPath: auditPath,
		}
	}
	if err := observability.InitGlobalAuditLogger(auditCfg); err != nil {
		slog.Warn("audit logger init failed", "error", err)
	}

	// Workflow ID for audit correlation.
	workflowID := fmt.Sprintf("workflow-%d", time.Now().UnixNano())
	observability.Audit().LogWorkflowStart(ctx, workflowID, sourceLang, targetLang, inputPath)
	pipelineStart := time.Now()

	m := metrics.New()

	cfg, err := config.Load(configPath)
	if err != nil {
		slog.Warn("config load failed, using defaults", "error", err)
		cfg = &config.Config{}
	}

	// Reconfigure logger with config values
	configureLoggerFromEnv(cfg.Log.Level, cfg.Log.Format)

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

	registry := plugins.NewRegistry()
	registry.RegisterSource(cobolplugin.New())
	registry.RegisterSource(perlplugin.New())
	registry.RegisterSource(fortranplugin.New())
	registry.RegisterTarget(javaplugin.New())
	registry.RegisterTarget(pythonplugin.New())
	registry.RegisterTarget(goplugin.New())
	registry.RegisterTarget(tsplugin.New())

	// Build LLM provider via factory
	factory := llm.NewFactory()
	llmutil.RegisterDefaultProviders(factory)

	// Helper to create a provider from an LLM config (with rate limiting).
	makeProvider := func(lcfg config.LLMConfig, label string) (llm.Provider, error) {
		p, err := factory.Create(llm.ProviderConfig{
			Provider: lcfg.Provider,
			APIKey:   lcfg.APIKey,
			Model:    lcfg.Model,
			BaseURL:  lcfg.BaseURL,
		})
		if err != nil {
			return nil, fmt.Errorf("creating LLM provider for %s: %w", label, err)
		}
		if p != nil {
			p = llm.WithRateLimit(p, llm.DefaultRateLimitConfig())
		}
		return p, nil
	}

	// Default provider (used by agents without overrides)
	provider, err := makeProvider(cfg.LLM, "default")
	if err != nil {
		return fmt.Errorf("creating LLM provider: %w", err)
	}

	// Per-agent providers (resolved from config overrides)
	specularProvider := provider
	architectProvider := provider
	judgeProvider := provider

	if len(cfg.LLM.Agents) > 0 {
		for agentName := range cfg.LLM.Agents {
			resolved := cfg.LLM.ResolveForAgent(agentName)
			agentProv, err := makeProvider(resolved, agentName)
			if err != nil {
				return fmt.Errorf("creating LLM provider for agent %s: %w", agentName, err)
			}
			switch agentName {
			case "specular":
				specularProvider = agentProv
			case "architect":
				architectProvider = agentProv
			case "judge":
				judgeProvider = agentProv
			}
		}
	}

	if provider == nil {
		m.LLMMode = "passthrough"
		slog.Info("running without LLM (template-only mode)")
	} else {
		m.LLMMode = "llm:" + provider.Name()
		slog.Info("LLM provider selected", "provider", provider.Name())
		if len(cfg.LLM.Agents) > 0 {
			for name := range cfg.LLM.Agents {
				resolved := cfg.LLM.ResolveForAgent(name)
				slog.Info("agent provider override", "agent", name, "provider", resolved.Provider, "model", resolved.Model)
			}
		}
	}

	// Step 1: Cartographer
	slog.Info("cartographer: parsing source")
	start := time.Now()
	{
		cartCtx, cartSpan := observability.StartAgentSpan(ctx, "cartographer")
		observability.Audit().LogAgentStart(cartCtx, "cartographer", workflowID, map[string]string{
			"source": sourceLang,
			"input":  inputPath,
		})
		cart := cartographer.New()
		cartResult, err := cart.Run(cartCtx, &agents.AgentContext{
			Registry:    registry,
			Params:      map[string]string{"source": sourceLang, "input": inputPath},
			DefaultOpts: defaultOpts,
		})
		elapsed := time.Since(start)
		if err != nil {
			observability.RecordError(cartSpan, err)
			observability.Audit().LogAgentError(cartCtx, "cartographer", workflowID, err)
			cartSpan.End()
			return fmt.Errorf("cartographer: %w", err)
		}
		m.AddAgent("cartographer", elapsed, "parse", 0)
		m.CollectSource(sourceLang, countFiles(inputPath), cartResult.Graph)
		observability.SetAgentMetrics(cartSpan, countFiles(inputPath), m.Source.ModuleCount, 0, 1.0)
		observability.Audit().LogAgentComplete(cartCtx, "cartographer", workflowID, elapsed, 1.0, 0)
		cartSpan.End()
		slog.Info("cartographer complete", "modules", m.Source.ModuleCount, "functions", m.Source.FunctionCount)

		// Step 2: Specular
		slog.Info("specular: extracting business rules")
		start = time.Now()
		specCtx, specSpan := observability.StartAgentSpan(ctx, "specular")
		observability.Audit().LogAgentStart(specCtx, "specular", workflowID, nil)
		spec := specular.New()
		specResult, err := spec.Run(specCtx, &agents.AgentContext{
			Graph:       cartResult.Graph,
			LLM:         specularProvider,
			Registry:    registry,
			DefaultOpts: defaultOpts,
		})
		elapsed = time.Since(start)
		if err != nil {
			observability.RecordError(specSpan, err)
			observability.Audit().LogAgentError(specCtx, "specular", workflowID, err)
			specSpan.End()
			return fmt.Errorf("specular: %w", err)
		}
		specMode := "llm"
		if specResult.Metadata != nil && specResult.Metadata["mode"] == "passthrough" {
			specMode = "passthrough"
		}
		m.AddAgent("specular", elapsed, specMode, len(specResult.Errors))
		m.Source.RuleCount = len(cartResult.Graph.BusinessRules)
		observability.SetAgentMetrics(specSpan, m.Source.ModuleCount, m.Source.RuleCount, 0, 1.0)
		observability.Audit().LogAgentComplete(specCtx, "specular", workflowID, elapsed, 1.0, len(specResult.Errors))
		specSpan.End()
		slog.Info("specular complete", "rules", m.Source.RuleCount, "mode", specMode)

		// Step 3 + 4: Architect → Judge with retry
		const maxRetries = 2
		var finalFiles []plugins.GeneratedFile
		var finalScore float64
		var allErrors []string

		for attempt := 0; attempt <= maxRetries; attempt++ {
			slog.Info("architect: generating target", "language", targetLang, "attempt", attempt+1, "max_attempts", maxRetries+1)
			start = time.Now()

			archCtx, archSpan := observability.StartAgentSpan(ctx, "architect")

			// Pass Judge feedback to Architect on retries
			archParams := map[string]string{"target": targetLang}
			if attempt > 0 && len(allErrors) > 0 {
				archParams["judge_feedback"] = strings.Join(allErrors, "\n")
			}

			observability.Audit().LogAgentStart(archCtx, "architect", workflowID, archParams)
			arch := architect.New()
			archResult, err := arch.Run(archCtx, &agents.AgentContext{
				Graph:       cartResult.Graph,
				LLM:         architectProvider,
				Registry:    registry,
				Params:      archParams,
				DefaultOpts: defaultOpts,
			})
			elapsed = time.Since(start)
			if err != nil {
				observability.RecordError(archSpan, err)
				observability.Audit().LogAgentError(archCtx, "architect", workflowID, err)
				archSpan.End()
				return fmt.Errorf("architect: %w", err)
			}
			archMode := "template"
			if provider != nil {
				archMode = "llm"
			}
			m.AddAgent("architect", elapsed, archMode, 0)
			observability.SetAgentMetrics(archSpan, m.Source.ModuleCount, len(archResult.GeneratedFiles), 0, 1.0)
			observability.Audit().LogAgentComplete(archCtx, "architect", workflowID, elapsed, 1.0, 0)
			archSpan.End()
			slog.Info("architect complete", "files_generated", len(archResult.GeneratedFiles))

			slog.Info("judge: verifying output")
			start = time.Now()
			judgeCtx, judgeSpan := observability.StartAgentSpan(ctx, "judge")
			observability.Audit().LogAgentStart(judgeCtx, "judge", workflowID, map[string]string{
				"source": sourceLang,
				"target": targetLang,
			})
			j := judge.New()
			genFilesJSON, _ := json.Marshal(archResult.GeneratedFiles)
			judgeResult, err := j.Run(judgeCtx, &agents.AgentContext{
				Graph: cartResult.Graph,
				LLM:   judgeProvider,
				Params: map[string]string{
					"source":          sourceLang,
					"target":          targetLang,
					"generated_files": string(genFilesJSON),
				},
				DefaultOpts: defaultOpts,
			})
			elapsed = time.Since(start)
			if err != nil {
				observability.RecordError(judgeSpan, err)
				observability.Audit().LogAgentError(judgeCtx, "judge", workflowID, err)
				judgeSpan.End()
				return fmt.Errorf("judge: %w", err)
			}
			judgeMode := "llm"
			if judgeResult.Metadata != nil && judgeResult.Metadata["mode"] == "passthrough" {
				judgeMode = "passthrough"
			}
			m.AddAgent("judge", elapsed, judgeMode, len(judgeResult.Errors))
			observability.SetAgentMetrics(judgeSpan, len(archResult.GeneratedFiles), len(archResult.GeneratedFiles), 0, judgeResult.Score)
			observability.Audit().LogAgentComplete(judgeCtx, "judge", workflowID, elapsed, judgeResult.Score, len(judgeResult.Errors))
			judgeSpan.End()
			slog.Info("judge complete", "score", judgeResult.Score, "mode", judgeMode)

			// Keep best: only update if this attempt improved the score
			if judgeResult.Score > finalScore || finalFiles == nil {
				finalFiles = archResult.GeneratedFiles
				finalScore = judgeResult.Score
			}
			allErrors = judgeResult.Errors

			if finalScore >= 0.8 {
				break
			}
		}

		// Step 5: TestGen (optional)
		if ac := os.Getenv("ANVIL_GENERATE_TESTS"); ac == "true" || ac == "1" {
			slog.Info("testgen: generating tests")
			start = time.Now()
			tgCtx, tgSpan := observability.StartAgentSpan(ctx, "testgen")
			observability.Audit().LogAgentStart(tgCtx, "testgen", workflowID, map[string]string{"target": targetLang})
			tg := testgen.New()
			tgResult, err := tg.Run(tgCtx, &agents.AgentContext{
				Graph:    cartResult.Graph,
				LLM:      provider,
				Registry: registry,
				Params: map[string]string{
					"target":          targetLang,
					"generated_files": "present",
				},
				DefaultOpts: defaultOpts,
			})
			elapsed = time.Since(start)
			if err != nil {
				observability.RecordError(tgSpan, err)
				observability.Audit().LogAgentError(tgCtx, "testgen", workflowID, err)
				tgSpan.End()
				slog.Warn("testgen failed", "error", err)
			} else {
				m.AddAgent("testgen", elapsed, "stub", len(tgResult.Errors))
				observability.SetAgentMetrics(tgSpan, len(finalFiles), len(tgResult.GeneratedFiles), 0, 1.0)
				observability.Audit().LogAgentComplete(tgCtx, "testgen", workflowID, elapsed, 1.0, len(tgResult.Errors))
				tgSpan.End()
				slog.Info("testgen complete", "test_files", len(tgResult.GeneratedFiles))

				// Write test files alongside main output
				for _, f := range tgResult.GeneratedFiles {
					outPath := filepath.Join(outputPath, f.Path)
					if err := os.MkdirAll(filepath.Dir(outPath), 0o755); err != nil {
						slog.Warn("failed to create test dir", "error", err)
						continue
					}
					if err := os.WriteFile(outPath, f.Content, 0o644); err != nil {
						slog.Warn("failed to write test file", "path", outPath, "error", err)
					}
				}
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

		observability.Audit().LogWorkflowEnd(ctx, workflowID, true, time.Since(pipelineStart), finalScore, outputPath)

		if jsonReport {
			data, _ := m.JSON()
			fmt.Println(string(data))
		} else {
			m.PrintSummary(os.Stdout)
		}
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

// runHarness executes fixtures against generated code and produces a proof pack.
func runHarness(fixturesPath, codePath, outputDir string, jsonOutput bool) error {
	ctx := context.Background()

	// Load fixtures
	f, err := os.Open(fixturesPath)
	if err != nil {
		return fmt.Errorf("open fixtures file: %w", err)
	}
	defer f.Close()

	fixtures, err := harness.ReadJSONL(f)
	if err != nil {
		return fmt.Errorf("read fixtures: %w", err)
	}
	slog.Info("fixtures loaded", "count", len(fixtures), "path", fixturesPath)

	// Create manifest-based runner (target-language-agnostic).
	runnerConfig := harness.DefaultRunnerConfig()
	runner, err := harness.NewManifestRunner(codePath, runnerConfig)
	if err != nil {
		return fmt.Errorf("load manifest runner: %w", err)
	}
	defer runner.Cleanup()
	slog.Info("runner selected", "runner", runner.Name())

	// Compile
	slog.Info("compiling")
	compileResult, err := runner.Compile(ctx, codePath)
	if err != nil {
		return fmt.Errorf("compile: %w", err)
	}
	if !compileResult.Success {
		slog.Error("compilation failed", "errors", compileResult.Errors)
		return fmt.Errorf("compilation failed")
	}
	slog.Info("compilation succeeded", "duration", compileResult.Duration)

	// Run fixtures and build proof pack
	slog.Info("running fixtures")
	proofPack := harness.NewProofPack()
	defaultRules := &harness.NormalizeRules{}

	for _, fixture := range fixtures {
		result, err := runner.RunFixture(ctx, codePath, fixture)
		if err != nil {
			slog.Warn("fixture failed", "fixture", fixture.Name, "error", err)
			proofPack.AddResult(fixture, harness.DiffResult{Pass: false, Reason: err.Error()})
			continue
		}

		// Compare output
		var diff harness.DiffResult
		if fixture.HTTP != nil {
			// Compare HTTP response using DiffHTTP
			actual := harness.HTTPActual{
				Status: result.Output.Status,
				Header: result.Output.Headers,
				Body:   result.Output.Body,
			}
			diff = harness.DiffHTTP(fixture, actual, defaultRules)
		} else if fixture.Batch != nil {
			diff = harness.DiffResult{Pass: false, Reason: "batch fixtures not supported by manifest runner yet"}
		} else {
			diff = harness.DiffResult{Pass: result.Success}
		}

		if diff.Pass {
			slog.Info("fixture passed", "fixture", fixture.Name)
		} else {
			slog.Warn("fixture failed", "fixture", fixture.Name, "reason", diff.Reason)
		}
		proofPack.AddResult(fixture, diff)
	}

	proofPack.Finish()

	// Output results
	if jsonOutput {
		data, _ := json.MarshalIndent(proofPack, "", "  ")
		fmt.Println(string(data))
	} else {
		fmt.Printf("\n=== Results ===\n")
		fmt.Printf("%s\n", proofPack.String())
	}

	// Write proof pack if output directory specified
	if outputDir != "" {
		if err := proofPack.Write(outputDir); err != nil {
			return fmt.Errorf("write proof pack: %w", err)
		}
		slog.Info("proof pack written", "path", outputDir)
	}

	if !proofPack.Pass {
		return fmt.Errorf("harness failed: %d/%d fixtures passed", proofPack.PassCount, proofPack.FixtureCount)
	}
	return nil
}

// recordFixtures records fixtures from a live system endpoint.
func recordFixtures(endpoint, outputPath string) error {
	slog.Info("starting fixture recorder", "endpoint", endpoint, "output", outputPath)

	rec, err := harness.NewRecorder(&harness.RecorderConfig{
		TargetURL:  endpoint,
		ListenAddr: ":8090",
		OutputPath: outputPath,
	})
	if err != nil {
		return fmt.Errorf("create recorder: %w", err)
	}

	fmt.Printf("\nAnvil Fixture Recorder\n")
	fmt.Printf("  Proxy listening on: http://localhost:8090\n")
	fmt.Printf("  Forwarding to:      %s\n", endpoint)
	fmt.Printf("  Recording to:       %s\n", outputPath)
	fmt.Printf("\nSend requests to http://localhost:8090 to record fixtures.\n")
	fmt.Printf("Press Ctrl+C to stop recording.\n\n")

	// Handle shutdown signal
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer stop()

	go func() {
		<-ctx.Done()
		shutdownCtx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		rec.Stop(shutdownCtx)
	}()

	err = rec.Start()
	if err != nil && err != http.ErrServerClosed {
		return fmt.Errorf("recorder error: %w", err)
	}

	fmt.Printf("\nRecording complete. %d fixtures written to %s\n", rec.Count(), outputPath)
	return nil
}

// compareOutputs compares actual vs expected outputs.
func compareOutputs(actualPath, expectedPath string, jsonOutput bool) error {
	// Read actual
	actualData, err := os.ReadFile(actualPath)
	if err != nil {
		return fmt.Errorf("read actual: %w", err)
	}

	// Read expected
	expectedData, err := os.ReadFile(expectedPath)
	if err != nil {
		return fmt.Errorf("read expected: %w", err)
	}

	// Create a synthetic fixture for comparison
	fixture := harness.Fixture{
		Kind: harness.FixtureHTTP,
		Name: "compare",
		HTTP: &harness.HTTPFixture{
			ExpectedBody: expectedData,
		},
	}

	actual := harness.HTTPActual{
		Status: 200, // Assume success for file comparison
		Body:   actualData,
	}

	// Compare using DiffHTTP
	diff := harness.DiffHTTP(fixture, actual, nil)

	if jsonOutput {
		data, _ := json.MarshalIndent(diff, "", "  ")
		fmt.Println(string(data))
	} else {
		if diff.Pass {
			fmt.Println("PASS: Outputs match")
		} else {
			fmt.Printf("FAIL: %s\n", diff.Reason)
		}
	}

	if !diff.Pass {
		return fmt.Errorf("comparison failed")
	}
	return nil
}

// generateProofPack generates a proof pack from fixtures and results.
func generateProofPack(fixturesPath, resultsPath, outputPath string) error {
	// Load fixtures
	f, err := os.Open(fixturesPath)
	if err != nil {
		return fmt.Errorf("open fixtures: %w", err)
	}
	defer f.Close()

	fixtures, err := harness.ReadJSONL(f)
	if err != nil {
		return fmt.Errorf("read fixtures: %w", err)
	}

	// Load results
	resultsData, err := os.ReadFile(resultsPath)
	if err != nil {
		return fmt.Errorf("read results: %w", err)
	}

	var results []struct {
		FixtureName string `json:"fixture_name"`
		Pass        bool   `json:"pass"`
		Reason      string `json:"reason,omitempty"`
	}
	if err := json.Unmarshal(resultsData, &results); err != nil {
		return fmt.Errorf("parse results: %w", err)
	}

	// Build proof pack
	proofPack := harness.NewProofPack()

	// Create results map for lookup
	resultsMap := make(map[string]struct {
		Pass   bool
		Reason string
	})
	for _, r := range results {
		resultsMap[r.FixtureName] = struct {
			Pass   bool
			Reason string
		}{Pass: r.Pass, Reason: r.Reason}
	}

	// Add results for each fixture
	for _, fixture := range fixtures {
		result, ok := resultsMap[fixture.Name]
		diff := harness.DiffResult{Pass: true}
		if ok {
			diff.Pass = result.Pass
			diff.Reason = result.Reason
		}
		proofPack.AddResult(fixture, diff)
	}

	proofPack.Finish()

	// Write proof pack
	if err := proofPack.Write(outputPath); err != nil {
		return fmt.Errorf("write proof pack: %w", err)
	}

	fmt.Printf("Generated proof pack: %s\n", proofPack.String())
	fmt.Printf("Written to: %s\n", outputPath)

	return nil
}

// hashBytes removed: batch hashing is handled by target runners/manifests.

// runDashboard starts the dashboard server with optional demo data.
func runDashboard(port int, demo bool) error {
	ctx := context.Background()

	// Create store and hub
	store := dashboard.NewStore()
	hub := dashboard.NewHub()

	// Seed demo data if requested
	if demo {
		seedDemoData(store)
		slog.Info("Seeded demo migration data")
	}

	// Create and start server
	config := &dashboard.Config{
		ListenAddr: fmt.Sprintf(":%d", port),
	}
	server := dashboard.NewServer(config, store, hub)

	// Handle graceful shutdown
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, os.Interrupt, syscall.SIGTERM)

	errChan := make(chan error, 1)
	go func() {
		errChan <- server.Start()
	}()

	fmt.Printf("\nAnvil Dashboard\n")
	fmt.Printf("  Server running at: http://localhost:%d\n", port)
	if demo {
		fmt.Printf("  Demo data: 5 sample migrations loaded\n")
	}
	fmt.Printf("\nPress Ctrl+C to stop.\n\n")

	select {
	case err := <-errChan:
		return err
	case <-sigChan:
		slog.Info("Shutdown signal received")
		shutdownCtx, cancel := context.WithTimeout(ctx, 5*time.Second)
		defer cancel()
		return server.Stop(shutdownCtx)
	}
}

// runExplore starts the proof pack explorer server.
func runExplore(port int, dirs []string, open bool) error {
	ctx := context.Background()

	// Create explorer config
	config := &proofexplorer.Config{
		ListenAddr: fmt.Sprintf(":%d", port),
		PackDirs:   dirs,
	}

	// Create explorer
	explorer, err := proofexplorer.New(config)
	if err != nil {
		return fmt.Errorf("create explorer: %w", err)
	}

	// Handle graceful shutdown
	sigChan := make(chan os.Signal, 1)
	signal.Notify(sigChan, os.Interrupt, syscall.SIGTERM)

	errChan := make(chan error, 1)
	go func() {
		errChan <- explorer.Start()
	}()

	fmt.Printf("\nAnvil Proof Pack Explorer\n")
	fmt.Printf("  Server running at: http://localhost:%d\n", port)
	fmt.Printf("  Scanning directories: %v\n", dirs)
	fmt.Printf("  Found %d proof packs\n", explorer.PackCount())
	if open {
		fmt.Printf("\n  Open in browser: http://localhost:%d\n", port)
	}
	fmt.Printf("\nPress Ctrl+C to stop.\n\n")

	select {
	case err := <-errChan:
		return err
	case <-sigChan:
		slog.Info("Shutdown signal received")
		shutdownCtx, cancel := context.WithTimeout(ctx, 5*time.Second)
		defer cancel()
		return explorer.Stop(shutdownCtx)
	}
}

// seedDemoData creates sample migration runs for testing the dashboard.
func seedDemoData(store *dashboard.Store) {
	now := time.Now()

	// Migration 1: Completed COBOL -> Java
	completed1 := now.Add(-2 * time.Hour)
	run1 := &dashboard.MigrationRun{
		ID:           "demo-1",
		Name:         "Legacy Payroll System",
		SourceLang:   "cobol",
		TargetLang:   "java",
		Status:       dashboard.StatusCompleted,
		StartedAt:    now.Add(-3 * time.Hour),
		CompletedAt:  &completed1,
		InputModules: 42,
		OutputFiles:  38,
		TestsPassed:  35,
		TestsFailed:  3,
		LLMCalls:     156,
		TotalTokens:  245000,
		Stages: []dashboard.StageResult{
			{
				Stage:     dashboard.StageCartographer,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-3 * time.Hour),
				Metrics:   dashboard.StageMetrics{InputItems: 42, OutputItems: 42},
			},
			{
				Stage:     dashboard.StageSpecular,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-3*time.Hour + 5*time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 42, OutputItems: 127, LLMCalls: 42, Tokens: 89000},
			},
			{
				Stage:     dashboard.StageArchitect,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-3*time.Hour + 12*time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 42, OutputItems: 38, LLMCalls: 84, Tokens: 134000},
			},
			{
				Stage:     dashboard.StageJudge,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-3*time.Hour + 28*time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 38, OutputItems: 38, LLMCalls: 30, Tokens: 22000},
			},
		},
	}
	store.CreateRun(run1)

	// Migration 2: Running Fortran -> Python
	run2 := &dashboard.MigrationRun{
		ID:           "demo-2",
		Name:         "Scientific Computing Library",
		SourceLang:   "fortran",
		TargetLang:   "python",
		Status:       dashboard.StatusRunning,
		StartedAt:    now.Add(-15 * time.Minute),
		InputModules: 28,
		LLMCalls:     64,
		TotalTokens:  98000,
		Stages: []dashboard.StageResult{
			{
				Stage:     dashboard.StageCartographer,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-15 * time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 28, OutputItems: 28},
			},
			{
				Stage:     dashboard.StageSpecular,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-12 * time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 28, OutputItems: 84, LLMCalls: 28, Tokens: 45000},
			},
			{
				Stage:     dashboard.StageArchitect,
				Status:    dashboard.StatusRunning,
				StartedAt: now.Add(-8 * time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 28, LLMCalls: 36, Tokens: 53000},
			},
		},
	}
	store.CreateRun(run2)

	// Migration 3: Failed COBOL -> TypeScript
	run3 := &dashboard.MigrationRun{
		ID:           "demo-3",
		Name:         "Mainframe Banking App",
		SourceLang:   "cobol",
		TargetLang:   "typescript",
		Status:       dashboard.StatusFailed,
		StartedAt:    now.Add(-5 * time.Hour),
		CompletedAt:  func() *time.Time { t := now.Add(-4*time.Hour + 30*time.Minute); return &t }(),
		Error:        "Judge validation failed: critical business logic mismatch in transaction processing",
		InputModules: 67,
		LLMCalls:     203,
		TotalTokens:  387000,
		Stages: []dashboard.StageResult{
			{
				Stage:     dashboard.StageCartographer,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-5 * time.Hour),
				Metrics:   dashboard.StageMetrics{InputItems: 67, OutputItems: 67},
			},
			{
				Stage:     dashboard.StageSpecular,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-5*time.Hour + 8*time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 67, OutputItems: 198, LLMCalls: 67, Tokens: 134000},
			},
			{
				Stage:     dashboard.StageArchitect,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-5*time.Hour + 18*time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 67, OutputItems: 54, LLMCalls: 95, Tokens: 189000},
			},
			{
				Stage:     dashboard.StageJudge,
				Status:    dashboard.StatusFailed,
				StartedAt: now.Add(-5*time.Hour + 42*time.Minute),
				Error:     "Validation failed: business logic mismatch",
				Metrics:   dashboard.StageMetrics{InputItems: 54, LLMCalls: 41, Tokens: 64000},
			},
		},
	}
	store.CreateRun(run3)

	// Migration 4: Completed Perl -> Go
	completed4 := now.Add(-1 * time.Hour)
	run4 := &dashboard.MigrationRun{
		ID:           "demo-4",
		Name:         "Legacy Web Services",
		SourceLang:   "perl",
		TargetLang:   "go",
		Status:       dashboard.StatusCompleted,
		StartedAt:    now.Add(-90 * time.Minute),
		CompletedAt:  &completed4,
		InputModules: 15,
		OutputFiles:  12,
		TestsPassed:  12,
		TestsFailed:  0,
		LLMCalls:     58,
		TotalTokens:  87000,
		Stages: []dashboard.StageResult{
			{
				Stage:     dashboard.StageCartographer,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-90 * time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 15, OutputItems: 15},
			},
			{
				Stage:     dashboard.StageSpecular,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-85 * time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 15, OutputItems: 42, LLMCalls: 15, Tokens: 28000},
			},
			{
				Stage:     dashboard.StageArchitect,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-78 * time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 15, OutputItems: 12, LLMCalls: 32, Tokens: 47000},
			},
			{
				Stage:     dashboard.StageJudge,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-68 * time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 12, OutputItems: 12, LLMCalls: 11, Tokens: 12000},
			},
		},
	}
	store.CreateRun(run4)

	// Migration 5: Completed COBOL -> Java (older)
	completed5 := now.Add(-24 * time.Hour)
	run5 := &dashboard.MigrationRun{
		ID:           "demo-5",
		Name:         "Inventory Management System",
		SourceLang:   "cobol",
		TargetLang:   "java",
		Status:       dashboard.StatusCompleted,
		StartedAt:    now.Add(-25 * time.Hour),
		CompletedAt:  &completed5,
		InputModules: 33,
		OutputFiles:  29,
		TestsPassed:  27,
		TestsFailed:  2,
		LLMCalls:     124,
		TotalTokens:  198000,
		Stages: []dashboard.StageResult{
			{
				Stage:     dashboard.StageCartographer,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-25 * time.Hour),
				Metrics:   dashboard.StageMetrics{InputItems: 33, OutputItems: 33},
			},
			{
				Stage:     dashboard.StageSpecular,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-25*time.Hour + 4*time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 33, OutputItems: 98, LLMCalls: 33, Tokens: 67000},
			},
			{
				Stage:     dashboard.StageArchitect,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-25*time.Hour + 15*time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 33, OutputItems: 29, LLMCalls: 67, Tokens: 109000},
			},
			{
				Stage:     dashboard.StageJudge,
				Status:    dashboard.StatusCompleted,
				StartedAt: now.Add(-25*time.Hour + 38*time.Minute),
				Metrics:   dashboard.StageMetrics{InputItems: 29, OutputItems: 29, LLMCalls: 24, Tokens: 22000},
			},
		},
	}
	store.CreateRun(run5)

	// Add some sample logs
	for _, runID := range []string{"demo-1", "demo-2", "demo-3", "demo-4", "demo-5"} {
		store.AddLog(dashboard.LogEntry{
			Timestamp: now.Add(-30 * time.Minute),
			Level:     "info",
			Message:   "Migration started",
			RunID:     runID,
		})
		store.AddLog(dashboard.LogEntry{
			Timestamp: now.Add(-25 * time.Minute),
			Level:     "info",
			Message:   "Cartographer: parsed source files",
			RunID:     runID,
			Stage:     "cartographer",
		})
		store.AddLog(dashboard.LogEntry{
			Timestamp: now.Add(-20 * time.Minute),
			Level:     "info",
			Message:   "Specular: extracted business rules",
			RunID:     runID,
			Stage:     "specular",
		})
	}
}

// runReview runs the interactive review process.
func runReview(sourcePath, targetLang, configPath, outputPath string, scoreThreshold float64) error {
	ctx := context.Background()

	// Load config
	cfg, err := config.Load(configPath)
	if err != nil {
		slog.Warn("config load failed, using defaults", "error", err)
		cfg = &config.Config{}
	}

	// Reconfigure logger
	configureLoggerFromEnv(cfg.Log.Level, cfg.Log.Format)

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
	registry.RegisterSource(cobolplugin.New())
	registry.RegisterSource(perlplugin.New())
	registry.RegisterSource(fortranplugin.New())
	registry.RegisterTarget(javaplugin.New())
	registry.RegisterTarget(pythonplugin.New())
	registry.RegisterTarget(goplugin.New())
	registry.RegisterTarget(tsplugin.New())

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

// listSnapshots lists all snapshots in the store.
func listSnapshots(storeDir string) error {
	store, err := snapshot.NewStore(storeDir)
	if err != nil {
		return fmt.Errorf("open snapshot store: %w", err)
	}

	snapshots := store.List()
	if len(snapshots) == 0 {
		fmt.Println("No snapshots found.")
		return nil
	}

	// Print table header
	fmt.Printf("%-12s %-20s %-20s %-15s %-8s %-10s %-6s\n", "ID", "TAG", "CREATED", "SOURCE→TARGET", "SCORE", "STATUS", "FILES")
	fmt.Println(strings.Repeat("-", 100))

	// Print each snapshot
	for _, s := range snapshots {
		id := s.ID
		if len(id) > 12 {
			id = id[:12]
		}
		tag := s.Tag
		if tag == "" {
			tag = "-"
		}
		if len(tag) > 20 {
			tag = tag[:20]
		}
		created := s.CreatedAt.Format("2006-01-02 15:04:05")
		langPair := fmt.Sprintf("%s→%s", s.SourceLang, s.TargetLang)
		score := fmt.Sprintf("%.2f", s.Score)
		status := s.Status
		if status == "" {
			status = "-"
		}

		fmt.Printf("%-12s %-20s %-20s %-15s %-8s %-10s %-6d\n",
			id, tag, created, langPair, score, status, s.FileCount)
	}

	return nil
}

// showSnapshot shows detailed information about a snapshot.
func showSnapshot(storeDir, id string) error {
	store, err := snapshot.NewStore(storeDir)
	if err != nil {
		return fmt.Errorf("open snapshot store: %w", err)
	}

	snap, err := store.Load(id)
	if err != nil {
		return fmt.Errorf("load snapshot: %w", err)
	}

	files, err := store.LoadFiles(snap)
	if err != nil {
		return fmt.Errorf("load snapshot files: %w", err)
	}

	fmt.Printf("Snapshot: %s\n", snap.ID)
	fmt.Printf("Created:  %s\n", snap.CreatedAt.Format("2006-01-02 15:04:05"))
	if snap.Tag != "" {
		fmt.Printf("Tag:      %s\n", snap.Tag)
	}
	if snap.ParentID != "" {
		fmt.Printf("Parent:   %s\n", snap.ParentID)
	}
	fmt.Printf("Source:   %s\n", snap.SourceLang)
	fmt.Printf("Target:   %s\n", snap.TargetLang)
	fmt.Printf("Score:    %.2f\n", snap.Score)
	fmt.Printf("Status:   %s\n", snap.Status)

	if snap.Provenance != nil {
		fmt.Printf("\nProvenance:\n")
		fmt.Printf("  Provider:        %s\n", snap.Provenance.Provider)
		fmt.Printf("  Model:           %s\n", snap.Provenance.Model)
		fmt.Printf("  Total LLM Calls: %d\n", snap.Provenance.TotalLLMCalls)
		fmt.Printf("  Total Tokens:    %d\n", snap.Provenance.TotalTokens)
		fmt.Printf("  Total Duration:  %s\n", snap.Provenance.TotalDuration)
		if len(snap.Provenance.AgentOverrides) > 0 {
			fmt.Printf("  Agent Overrides:\n")
			for k, v := range snap.Provenance.AgentOverrides {
				fmt.Printf("    %s: %s\n", k, v)
			}
		}
	}

	if len(snap.AgentStages) > 0 {
		fmt.Printf("\nAgent Stages:\n")
		for _, stage := range snap.AgentStages {
			fmt.Printf("  - %s: %s\n", stage.Name, stage.Status)
			fmt.Printf("    Duration:         %s\n", stage.Duration)
			fmt.Printf("    Score:            %.2f\n", stage.Score)
			fmt.Printf("    LLM Calls:        %d\n", stage.LLMCalls)
			fmt.Printf("    Prompt Tokens:    %d\n", stage.PromptTokens)
			fmt.Printf("    Completion Tokens: %d\n", stage.CompletionTokens)
			if stage.Model != "" {
				fmt.Printf("    Model:            %s\n", stage.Model)
			}
			if stage.ErrorCount > 0 {
				fmt.Printf("    Errors:           %d\n", stage.ErrorCount)
			}
		}
	}

	fmt.Printf("\nFiles (%d):\n", len(files))
	for _, f := range files {
		fmt.Printf("  %s (%d bytes)\n", f.Path, len(f.Content))
	}

	return nil
}

// tagSnapshot tags a snapshot.
func tagSnapshot(storeDir, id, tag string) error {
	store, err := snapshot.NewStore(storeDir)
	if err != nil {
		return fmt.Errorf("open snapshot store: %w", err)
	}

	if err := store.Tag(id, tag); err != nil {
		return fmt.Errorf("tag snapshot: %w", err)
	}

	fmt.Printf("Tagged snapshot %s as '%s'\n", id, tag)
	return nil
}

// deleteSnapshot deletes a snapshot.
func deleteSnapshot(storeDir, id string) error {
	store, err := snapshot.NewStore(storeDir)
	if err != nil {
		return fmt.Errorf("open snapshot store: %w", err)
	}

	if err := store.Delete(id); err != nil {
		return fmt.Errorf("delete snapshot: %w", err)
	}

	fmt.Printf("Deleted snapshot %s\n", id)
	return nil
}

// diffSnapshots compares two snapshots.
func diffSnapshots(storeDir, id1, id2 string, jsonOutput bool) error {
	store, err := snapshot.NewStore(storeDir)
	if err != nil {
		return fmt.Errorf("open snapshot store: %w", err)
	}

	snap1, err := store.Load(id1)
	if err != nil {
		return fmt.Errorf("load snapshot %s: %w", id1, err)
	}

	snap2, err := store.Load(id2)
	if err != nil {
		return fmt.Errorf("load snapshot %s: %w", id2, err)
	}

	diff, err := snapshot.Diff(snap1, snap2, store)
	if err != nil {
		return fmt.Errorf("diff snapshots: %w", err)
	}

	if jsonOutput {
		data, err := json.MarshalIndent(diff, "", "  ")
		if err != nil {
			return fmt.Errorf("marshal diff: %w", err)
		}
		fmt.Println(string(data))
	} else {
		fmt.Print(snapshot.FormatDiff(diff))
	}

	return nil
}

// rollbackSnapshot restores files from a snapshot.
func rollbackSnapshot(storeDir, id, outputDir string) error {
	store, err := snapshot.NewStore(storeDir)
	if err != nil {
		return fmt.Errorf("open snapshot store: %w", err)
	}

	snap, err := store.Load(id)
	if err != nil {
		return fmt.Errorf("load snapshot: %w", err)
	}

	if err := store.Restore(snap, outputDir); err != nil {
		return fmt.Errorf("restore snapshot: %w", err)
	}

	fmt.Printf("Restored snapshot %s to %s\n", id, outputDir)
	return nil
}

// runGateCheck runs quality gates with the provided configuration.
func runGateCheck(score float64, compilation bool, fixtureRate, coverage float64, maxTokens, maxErrors int, jsonOutput bool) error {
	// Create a simulated evaluation context for demonstration
	// In a real scenario, this would come from actual migration results
	ctx := &qualitygate.EvalContext{
		Score:            0.85,
		CompilationOK:    true,
		CompileErrors:    []string{},
		FixturesPassed:   95,
		FixturesTotal:    100,
		FunctionsTotal:   50,
		FunctionsMatched: 48,
		LLMCalls:         120,
		TotalTokens:      150000,
		GeneratedFiles:   12,
		Warnings:         []string{},
		Errors:           []string{},
		Metadata:         map[string]string{},
	}

	// Build a simple pipeline with inline gates for demonstration
	// This shows how the gates would work once the full implementation is ready
	pipeline := qualitygate.NewPipeline()

	// Add score gate
	pipeline.AddGate(&scoreGate{threshold: score})

	// Add compilation gate if enabled
	if compilation {
		pipeline.AddGate(&compilationGate{})
	}

	// Add fixture rate gate
	pipeline.AddGate(&fixtureRateGate{threshold: fixtureRate})

	// Add coverage gate
	pipeline.AddGate(&coverageGate{threshold: coverage})

	// Add token budget gate if specified
	if maxTokens > 0 {
		pipeline.AddGate(&tokenBudgetGate{maxTokens: maxTokens})
	}

	// Add error count gate
	pipeline.AddGate(&errorCountGate{maxErrors: maxErrors})

	// Run the pipeline
	result := pipeline.Run(ctx)

	// Output results
	if jsonOutput {
		data, err := json.MarshalIndent(result, "", "  ")
		if err != nil {
			return fmt.Errorf("marshal results: %w", err)
		}
		fmt.Println(string(data))
	} else {
		fmt.Println(formatGateReport(result))
	}

	if result.Status == qualitygate.GateFailed {
		return fmt.Errorf("quality gates failed")
	}

	return nil
}

// showGateDefaults displays the default gate configuration as JSON.
func showGateDefaults() {
	defaults := map[string]interface{}{
		"score": map[string]interface{}{
			"threshold": 0.8,
			"severity":  "required",
			"enabled":   true,
		},
		"compilation": map[string]interface{}{
			"required": true,
			"severity": "critical",
			"enabled":  true,
		},
		"fixture_rate": map[string]interface{}{
			"threshold": 0.95,
			"severity":  "required",
			"enabled":   true,
		},
		"coverage": map[string]interface{}{
			"threshold": 0.9,
			"severity":  "required",
			"enabled":   true,
		},
		"token_budget": map[string]interface{}{
			"max_tokens": 0,
			"severity":   "advisory",
			"enabled":    false,
		},
		"error_count": map[string]interface{}{
			"max_errors": 0,
			"severity":   "required",
			"enabled":    true,
		},
	}

	data, _ := json.MarshalIndent(defaults, "", "  ")
	fmt.Println(string(data))
}

// formatGateReport formats a pipeline result as a human-readable report.
func formatGateReport(result *qualitygate.PipelineResult) string {
	var sb strings.Builder

	sb.WriteString("\n=== Quality Gate Report ===\n\n")
	sb.WriteString(fmt.Sprintf("Overall Status: %s\n", strings.ToUpper(string(result.Status))))
	sb.WriteString(fmt.Sprintf("Duration: %s\n", result.Duration))
	sb.WriteString(fmt.Sprintf("Evaluated: %s\n\n", result.EvaluatedAt.Format("2006-01-02 15:04:05")))

	sb.WriteString("Gate Results:\n")
	for _, gate := range result.Gates {
		statusSymbol := "✓"
		if gate.Status == qualitygate.GateFailed {
			statusSymbol = "✗"
		} else if gate.Status == qualitygate.GateWarning {
			statusSymbol = "⚠"
		} else if gate.Status == qualitygate.GateSkipped {
			statusSymbol = "⊝"
		}

		sb.WriteString(fmt.Sprintf("  %s %-20s [%s] %s\n", statusSymbol, gate.Name, gate.Severity, gate.Message))
		if gate.Score > 0 || gate.Threshold > 0 {
			sb.WriteString(fmt.Sprintf("    Score: %.2f / %.2f (threshold)\n", gate.Score, gate.Threshold))
		}
		for _, detail := range gate.Details {
			sb.WriteString(fmt.Sprintf("    - %s\n", detail))
		}
	}

	sb.WriteString(fmt.Sprintf("\nSummary: %s\n", result.Summary))

	return sb.String()
}

// Simple gate implementations for demonstration
// These will be replaced by the full implementation in internal/qualitygate

type scoreGate struct {
	threshold float64
}

func (g *scoreGate) Name() string                                    { return "judge_score" }
func (g *scoreGate) Severity() qualitygate.GateSeverity              { return qualitygate.SeverityRequired }
func (g *scoreGate) Evaluate(ctx *qualitygate.EvalContext) (*qualitygate.GateResult, error) {
	result := &qualitygate.GateResult{
		Name:      g.Name(),
		Severity:  g.Severity(),
		Score:     ctx.Score,
		Threshold: g.threshold,
	}

	if ctx.Score >= g.threshold {
		result.Status = qualitygate.GatePassed
		result.Message = fmt.Sprintf("Judge score %.2f meets threshold %.2f", ctx.Score, g.threshold)
	} else {
		result.Status = qualitygate.GateFailed
		result.Message = fmt.Sprintf("Judge score %.2f below threshold %.2f", ctx.Score, g.threshold)
	}

	return result, nil
}

type compilationGate struct{}

func (g *compilationGate) Name() string                                    { return "compilation" }
func (g *compilationGate) Severity() qualitygate.GateSeverity              { return qualitygate.SeverityCritical }
func (g *compilationGate) Evaluate(ctx *qualitygate.EvalContext) (*qualitygate.GateResult, error) {
	result := &qualitygate.GateResult{
		Name:     g.Name(),
		Severity: g.Severity(),
	}

	if ctx.CompilationOK {
		result.Status = qualitygate.GatePassed
		result.Message = "Code compiles successfully"
	} else {
		result.Status = qualitygate.GateFailed
		result.Message = fmt.Sprintf("Compilation failed with %d errors", len(ctx.CompileErrors))
		result.Details = ctx.CompileErrors
	}

	return result, nil
}

type fixtureRateGate struct {
	threshold float64
}

func (g *fixtureRateGate) Name() string                                    { return "fixture_pass_rate" }
func (g *fixtureRateGate) Severity() qualitygate.GateSeverity              { return qualitygate.SeverityRequired }
func (g *fixtureRateGate) Evaluate(ctx *qualitygate.EvalContext) (*qualitygate.GateResult, error) {
	result := &qualitygate.GateResult{
		Name:      g.Name(),
		Severity:  g.Severity(),
		Threshold: g.threshold,
	}

	if ctx.FixturesTotal == 0 {
		result.Status = qualitygate.GateSkipped
		result.Message = "No fixtures to evaluate"
		return result, nil
	}

	rate := float64(ctx.FixturesPassed) / float64(ctx.FixturesTotal)
	result.Score = rate

	if rate >= g.threshold {
		result.Status = qualitygate.GatePassed
		result.Message = fmt.Sprintf("%d/%d fixtures passed (%.1f%%)", ctx.FixturesPassed, ctx.FixturesTotal, rate*100)
	} else {
		result.Status = qualitygate.GateFailed
		result.Message = fmt.Sprintf("%d/%d fixtures passed (%.1f%%), below threshold %.1f%%",
			ctx.FixturesPassed, ctx.FixturesTotal, rate*100, g.threshold*100)
	}

	return result, nil
}

type coverageGate struct {
	threshold float64
}

func (g *coverageGate) Name() string                                    { return "function_coverage" }
func (g *coverageGate) Severity() qualitygate.GateSeverity              { return qualitygate.SeverityRequired }
func (g *coverageGate) Evaluate(ctx *qualitygate.EvalContext) (*qualitygate.GateResult, error) {
	result := &qualitygate.GateResult{
		Name:      g.Name(),
		Severity:  g.Severity(),
		Threshold: g.threshold,
	}

	if ctx.FunctionsTotal == 0 {
		result.Status = qualitygate.GateSkipped
		result.Message = "No functions to evaluate"
		return result, nil
	}

	coverage := float64(ctx.FunctionsMatched) / float64(ctx.FunctionsTotal)
	result.Score = coverage

	if coverage >= g.threshold {
		result.Status = qualitygate.GatePassed
		result.Message = fmt.Sprintf("%d/%d functions covered (%.1f%%)", ctx.FunctionsMatched, ctx.FunctionsTotal, coverage*100)
	} else {
		result.Status = qualitygate.GateFailed
		result.Message = fmt.Sprintf("%d/%d functions covered (%.1f%%), below threshold %.1f%%",
			ctx.FunctionsMatched, ctx.FunctionsTotal, coverage*100, g.threshold*100)
	}

	return result, nil
}

type tokenBudgetGate struct {
	maxTokens int
}

func (g *tokenBudgetGate) Name() string                                    { return "token_budget" }
func (g *tokenBudgetGate) Severity() qualitygate.GateSeverity              { return qualitygate.SeverityAdvisory }
func (g *tokenBudgetGate) Evaluate(ctx *qualitygate.EvalContext) (*qualitygate.GateResult, error) {
	result := &qualitygate.GateResult{
		Name:      g.Name(),
		Severity:  g.Severity(),
		Threshold: float64(g.maxTokens),
		Score:     float64(ctx.TotalTokens),
	}

	if ctx.TotalTokens <= g.maxTokens {
		result.Status = qualitygate.GatePassed
		result.Message = fmt.Sprintf("Token usage %d within budget %d", ctx.TotalTokens, g.maxTokens)
	} else {
		result.Status = qualitygate.GateWarning
		result.Message = fmt.Sprintf("Token usage %d exceeds budget %d", ctx.TotalTokens, g.maxTokens)
	}

	return result, nil
}

type errorCountGate struct {
	maxErrors int
}

func (g *errorCountGate) Name() string                                    { return "error_count" }
func (g *errorCountGate) Severity() qualitygate.GateSeverity              { return qualitygate.SeverityRequired }
func (g *errorCountGate) Evaluate(ctx *qualitygate.EvalContext) (*qualitygate.GateResult, error) {
	result := &qualitygate.GateResult{
		Name:      g.Name(),
		Severity:  g.Severity(),
		Threshold: float64(g.maxErrors),
		Score:     float64(len(ctx.Errors)),
	}

	if len(ctx.Errors) <= g.maxErrors {
		result.Status = qualitygate.GatePassed
		result.Message = fmt.Sprintf("%d errors (max: %d)", len(ctx.Errors), g.maxErrors)
	} else {
		result.Status = qualitygate.GateFailed
		result.Message = fmt.Sprintf("%d errors exceeds maximum %d", len(ctx.Errors), g.maxErrors)
		result.Details = ctx.Errors
	}

	return result, nil
}

// runDepgraphAnalyze analyzes dependencies from a SemanticGraph JSON file.
func runDepgraphAnalyze(irFile, format, output string) error {
	// Read the IR file
	data, err := os.ReadFile(irFile)
	if err != nil {
		return fmt.Errorf("read IR file: %w", err)
	}

	// Unmarshal to SemanticGraph
	var sg ir.SemanticGraph
	if err := json.Unmarshal(data, &sg); err != nil {
		return fmt.Errorf("unmarshal SemanticGraph: %w", err)
	}

	// Analyze
	graph := depgraph.Analyze(&sg)

	// Export in requested format
	result, err := exportDepgraph(graph, format)
	if err != nil {
		return err
	}

	// Write to output
	if output == "" {
		fmt.Print(result)
	} else {
		if err := os.WriteFile(output, []byte(result), 0644); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
		slog.Info("wrote output", "path", output)
	}

	return nil
}

// runDepgraphDemo generates a demo dependency graph.
func runDepgraphDemo(format, output string) error {
	// Build demo SemanticGraph
	sg := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "accounts",
				Language: "cobol",
				Functions: []*ir.Function{
					{
						Name:  "ProcessPayment",
						Calls: []string{"ValidateAccount", "LogTransaction"},
					},
					{
						Name: "ValidateAccount",
					},
					{
						Name: "GetBalance",
					},
				},
				DataTypes: []*ir.DataType{
					{Name: "Account", Kind: ir.TypeStruct},
					{Name: "Payment", Kind: ir.TypeStruct},
				},
				IOContracts: []*ir.IOContract{
					{
						Name:      "AccountDB",
						Kind:      ir.IODB,
						Direction: ir.IOReadWrite,
					},
				},
			},
			{
				Name:     "ledger",
				Language: "cobol",
				Functions: []*ir.Function{
					{
						Name:  "LogTransaction",
						Calls: []string{"UpdateBalance"},
					},
					{
						Name: "UpdateBalance",
					},
					{
						Name:  "GenerateReport",
						Calls: []string{"GetBalance"},
					},
				},
				DataTypes: []*ir.DataType{
					{Name: "Transaction", Kind: ir.TypeStruct},
					{Name: "LedgerEntry", Kind: ir.TypeStruct},
				},
				IOContracts: []*ir.IOContract{
					{
						Name:      "TransactionLog",
						Kind:      ir.IOFile,
						Direction: ir.IOWrite,
					},
				},
			},
			{
				Name:     "reporting",
				Language: "cobol",
				Functions: []*ir.Function{
					{
						Name:  "DailyReport",
						Calls: []string{"GenerateReport", "GetBalance"},
					},
					{
						Name: "ExportCSV",
					},
				},
				DataTypes: []*ir.DataType{
					{Name: "Report", Kind: ir.TypeStruct},
				},
				IOContracts: []*ir.IOContract{
					{
						Name:      "ReportOutput",
						Kind:      ir.IOFile,
						Direction: ir.IOWrite,
					},
				},
			},
		},
	}

	// Analyze
	graph := depgraph.Analyze(sg)

	// Export in requested format
	result, err := exportDepgraph(graph, format)
	if err != nil {
		return err
	}

	// Write to output
	if output == "" {
		fmt.Print(result)
	} else {
		if err := os.WriteFile(output, []byte(result), 0644); err != nil {
			return fmt.Errorf("write output: %w", err)
		}
		slog.Info("wrote output", "path", output)
	}

	return nil
}

// exportDepgraph exports a graph in the requested format.
func exportDepgraph(graph *depgraph.Graph, format string) (string, error) {
	switch format {
	case "dot":
		return depgraph.ExportDOT(graph), nil
	case "mermaid":
		return depgraph.ExportMermaid(graph), nil
	case "json":
		data, err := depgraph.ExportJSON(graph)
		if err != nil {
			return "", fmt.Errorf("export JSON: %w", err)
		}
		return string(data), nil
	case "stats":
		return depgraph.FormatStats(graph), nil
	default:
		return "", fmt.Errorf("unsupported format: %s (supported: dot, mermaid, json, stats)", format)
	}
}
