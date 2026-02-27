package cli

import (
	"encoding/json"
	"fmt"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/qualitygate"
	"github.com/spf13/cobra"
)

// Gate implementations

type scoreGate struct {
	threshold float64
}

func (g *scoreGate) Name() string                       { return "judge_score" }
func (g *scoreGate) Severity() qualitygate.GateSeverity { return qualitygate.SeverityRequired }
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

func (g *compilationGate) Name() string                       { return "compilation" }
func (g *compilationGate) Severity() qualitygate.GateSeverity { return qualitygate.SeverityCritical }
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

func (g *fixtureRateGate) Name() string                       { return "fixture_pass_rate" }
func (g *fixtureRateGate) Severity() qualitygate.GateSeverity { return qualitygate.SeverityRequired }
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

func (g *coverageGate) Name() string                       { return "function_coverage" }
func (g *coverageGate) Severity() qualitygate.GateSeverity { return qualitygate.SeverityRequired }
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

func (g *tokenBudgetGate) Name() string                       { return "token_budget" }
func (g *tokenBudgetGate) Severity() qualitygate.GateSeverity { return qualitygate.SeverityAdvisory }
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

func (g *errorCountGate) Name() string                       { return "error_count" }
func (g *errorCountGate) Severity() qualitygate.GateSeverity { return qualitygate.SeverityRequired }
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

// RunGateCheck runs quality gates with the provided configuration.
func RunGateCheck(score float64, compilation bool, fixtureRate, coverage float64, maxTokens, maxErrors int, jsonOutput bool) error {
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
		fmt.Println(FormatGateReport(result))
	}

	if result.Status == qualitygate.GateFailed {
		return fmt.Errorf("quality gates failed")
	}

	return nil
}

// ShowGateDefaults displays the default gate configuration as JSON.
func ShowGateDefaults() {
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

// FormatGateReport formats a pipeline result as a human-readable report.
func FormatGateReport(result *qualitygate.PipelineResult) string {
	var sb strings.Builder

	sb.WriteString("\n=== Quality Gate Report ===\n\n")
	sb.WriteString(fmt.Sprintf("Overall Status: %s\n", strings.ToUpper(string(result.Status))))
	sb.WriteString(fmt.Sprintf("Duration: %s\n", result.Duration))
	sb.WriteString(fmt.Sprintf("Evaluated: %s\n\n", result.EvaluatedAt.Format("2006-01-02 15:04:05")))

	sb.WriteString("Gate Results:\n")
	for _, gate := range result.Gates {
		statusSymbol := "+"
		if gate.Status == qualitygate.GateFailed {
			statusSymbol = "X"
		} else if gate.Status == qualitygate.GateWarning {
			statusSymbol = "!"
		} else if gate.Status == qualitygate.GateSkipped {
			statusSymbol = "-"
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

// RegisterGateCommands registers all gate subcommands.
func RegisterGateCommands(parent *cobra.Command) {
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
			return RunGateCheck(gateScore, gateCompilation, gateFixtureRate, gateCoverage, gateMaxTokens, gateMaxErrors, gateJSON)
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
			ShowGateDefaults()
		},
	}

	gateCmd.AddCommand(gateCheckCmd, gateDefaultsCmd)

	parent.AddCommand(gateCmd)
}
