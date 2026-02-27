// Package pipeline orchestrates the Anvil migration pipeline:
// Cartographer -> Specular -> Architect+Judge (with retry) -> TestGen.
package pipeline

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
	"github.com/efebarandurmaz/anvil/internal/fileutil"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/metrics"
	"github.com/efebarandurmaz/anvil/internal/observability"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// maxRetries is the maximum number of Architect->Judge retry iterations.
const maxRetries = 2

// Run executes the full migration pipeline with the given configuration.
// It returns a PipelineResult containing generated files, score, and errors.
func Run(ctx context.Context, cfg PipelineConfig) (*PipelineResult, error) {
	// Resolve target language aliases.
	targetLang := cfg.TargetLang
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
	observability.Audit().LogWorkflowStart(ctx, workflowID, cfg.SourceLang, targetLang, cfg.InputPath)
	pipelineStart := time.Now()

	m := metrics.New()

	// Build default LLM request options from config.
	defaultOpts := &llm.RequestOptions{}
	if cfg.Config.LLM.Temperature > 0 {
		temp := cfg.Config.LLM.Temperature
		defaultOpts.Temperature = &temp
	}
	if cfg.Config.LLM.MaxTokens > 0 {
		maxTok := cfg.Config.LLM.MaxTokens
		defaultOpts.MaxTokens = &maxTok
	}

	// Resolve per-agent providers.
	defaultProvider := cfg.Providers["default"]
	specularProvider := resolveProvider(cfg.Providers, "specular", defaultProvider)
	architectProvider := resolveProvider(cfg.Providers, "architect", defaultProvider)
	judgeProvider := resolveProvider(cfg.Providers, "judge", defaultProvider)

	if defaultProvider == nil {
		m.LLMMode = "passthrough"
		slog.Info("running without LLM (template-only mode)")
	} else {
		m.LLMMode = "llm:" + defaultProvider.Name()
		slog.Info("LLM provider selected", "provider", defaultProvider.Name())
		for name := range cfg.Providers {
			if name != "default" {
				slog.Info("agent provider override", "agent", name, "provider", cfg.Providers[name].Name())
			}
		}
	}

	// Step 1: Cartographer
	slog.Info("cartographer: parsing source")
	start := time.Now()

	cartCtx, cartSpan := observability.StartAgentSpan(ctx, "cartographer")
	observability.Audit().LogAgentStart(cartCtx, "cartographer", workflowID, map[string]string{
		"source": cfg.SourceLang,
		"input":  cfg.InputPath,
	})
	cart := cartographer.New()
	cartResult, err := cart.Run(cartCtx, &agents.AgentContext{
		Registry:    cfg.Registry,
		Params:      map[string]string{"source": cfg.SourceLang, "input": cfg.InputPath},
		DefaultOpts: defaultOpts,
	})
	elapsed := time.Since(start)
	if err != nil {
		observability.RecordError(cartSpan, err)
		observability.Audit().LogAgentError(cartCtx, "cartographer", workflowID, err)
		cartSpan.End()
		return nil, fmt.Errorf("cartographer: %w", err)
	}
	m.AddAgent("cartographer", elapsed, "parse", 0)
	m.CollectSource(cfg.SourceLang, countFiles(cfg.InputPath), cartResult.Graph)
	observability.SetAgentMetrics(cartSpan, countFiles(cfg.InputPath), m.Source.ModuleCount, 0, 1.0)
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
		Registry:    cfg.Registry,
		DefaultOpts: defaultOpts,
	})
	elapsed = time.Since(start)
	if err != nil {
		observability.RecordError(specSpan, err)
		observability.Audit().LogAgentError(specCtx, "specular", workflowID, err)
		specSpan.End()
		return nil, fmt.Errorf("specular: %w", err)
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

	// Step 3 + 4: Architect -> Judge with retry
	var finalFiles []plugins.GeneratedFile
	var finalScore float64
	var allErrors []string
	iterations := 0

	for attempt := 0; attempt <= maxRetries; attempt++ {
		iterations++
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
			Registry:    cfg.Registry,
			Params:      archParams,
			DefaultOpts: defaultOpts,
		})
		elapsed = time.Since(start)
		if err != nil {
			observability.RecordError(archSpan, err)
			observability.Audit().LogAgentError(archCtx, "architect", workflowID, err)
			archSpan.End()
			return nil, fmt.Errorf("architect: %w", err)
		}
		archMode := "template"
		if defaultProvider != nil {
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
			"source": cfg.SourceLang,
			"target": targetLang,
		})
		j := judge.New()
		genFilesJSON, _ := json.Marshal(archResult.GeneratedFiles)
		judgeResult, err := j.Run(judgeCtx, &agents.AgentContext{
			Graph: cartResult.Graph,
			LLM:   judgeProvider,
			Params: map[string]string{
				"source":          cfg.SourceLang,
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
			return nil, fmt.Errorf("judge: %w", err)
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
			LLM:      defaultProvider,
			Registry: cfg.Registry,
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
				outPath, pathErr := fileutil.SafeJoin(cfg.OutputPath, f.Path)
				if pathErr != nil {
					slog.Warn("skipping file with unsafe path", "path", f.Path, "error", pathErr)
					continue
				}
				if mkErr := os.MkdirAll(filepath.Dir(outPath), 0o755); mkErr != nil {
					slog.Warn("failed to create test dir", "error", mkErr)
					continue
				}
				if wErr := os.WriteFile(outPath, f.Content, 0o644); wErr != nil {
					slog.Warn("failed to write test file", "path", outPath, "error", wErr)
				}
			}
		}
	}

	// Write output files
	for _, f := range finalFiles {
		outPath, pathErr := fileutil.SafeJoin(cfg.OutputPath, f.Path)
		if pathErr != nil {
			return nil, fmt.Errorf("unsafe file path %q: %w", f.Path, pathErr)
		}
		if err := os.MkdirAll(filepath.Dir(outPath), 0o755); err != nil {
			return nil, err
		}
		if err := os.WriteFile(outPath, f.Content, 0o644); err != nil {
			return nil, err
		}
	}

	// Finalize metrics
	m.CollectTarget(targetLang, finalFiles)
	m.Finish(finalScore, allErrors)

	observability.Audit().LogWorkflowEnd(ctx, workflowID, true, time.Since(pipelineStart), finalScore, cfg.OutputPath)

	if cfg.JsonReport {
		data, _ := m.JSON()
		fmt.Println(string(data))
	} else {
		m.PrintSummary(os.Stdout)
	}

	return &PipelineResult{
		Files:      finalFiles,
		Score:      finalScore,
		Errors:     allErrors,
		Iterations: iterations,
	}, nil
}

// resolveProvider returns the agent-specific provider if present, otherwise the fallback.
func resolveProvider(providers map[string]llm.Provider, agentName string, fallback llm.Provider) llm.Provider {
	if p, ok := providers[agentName]; ok && p != nil {
		return p
	}
	return fallback
}

// countFiles counts the number of files at the given path.
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
