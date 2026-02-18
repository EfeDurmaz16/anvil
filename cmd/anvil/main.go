package main

import (
	"context"
	"encoding/json"
	"fmt"
	"log/slog"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/agents/architect"
	"github.com/efebarandurmaz/anvil/internal/agents/cartographer"
	"github.com/efebarandurmaz/anvil/internal/agents/judge"
	"github.com/efebarandurmaz/anvil/internal/agents/specular"
	"github.com/efebarandurmaz/anvil/internal/agents/testgen"
	"github.com/efebarandurmaz/anvil/internal/config"
	"github.com/efebarandurmaz/anvil/internal/harness"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/llmutil"
	"github.com/efebarandurmaz/anvil/internal/metrics"
	"github.com/efebarandurmaz/anvil/internal/observability"
	"github.com/efebarandurmaz/anvil/internal/plugins"
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
	var handler slog.Handler
	if os.Getenv("ANVIL_LOG_FORMAT") == "json" {
		handler = slog.NewJSONHandler(os.Stderr, nil)
	} else {
		handler = slog.NewTextHandler(os.Stderr, nil)
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

	rootCmd.AddCommand(runCmd, providersCmd, harnessCmd, proofPackCmd)

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
			Registry: registry,
			Params:   map[string]string{"source": sourceLang, "input": inputPath},
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
			Graph:    cartResult.Graph,
			LLM:      specularProvider,
			Registry: registry,
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
				Graph:    cartResult.Graph,
				LLM:      architectProvider,
				Registry: registry,
				Params:   archParams,
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
	slog.Info("recording fixtures", "endpoint", endpoint, "output", outputPath)

	// Create output file
	f, err := os.Create(outputPath)
	if err != nil {
		return fmt.Errorf("create output file: %w", err)
	}
	defer f.Close()

	// Note: Full implementation would proxy requests to the endpoint
	// and record request/response pairs as fixtures.
	// For now, we provide a placeholder that explains the workflow.

	fmt.Println("\nRecording mode:")
	fmt.Println("  1. Configure your client to proxy through Anvil")
	fmt.Println("  2. Make requests to the legacy system")
	fmt.Println("  3. Anvil captures request/response pairs as fixtures")
	fmt.Println("  4. Use 'anvil harness run' to replay against modernized code")
	fmt.Println("\nNote: For production recording, configure your API gateway")
	fmt.Println("to export logs in Anvil's JSONL fixture format.")

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
