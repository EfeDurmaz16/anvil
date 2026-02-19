package qualitygate

import "fmt"

// ScoreGate checks that the overall migration score meets a threshold.
type ScoreGate struct {
	MinScore float64
	severity GateSeverity
}

func NewScoreGate(minScore float64, severity GateSeverity) *ScoreGate {
	return &ScoreGate{MinScore: minScore, severity: severity}
}

func (g *ScoreGate) Name() string          { return "score" }
func (g *ScoreGate) Severity() GateSeverity { return g.severity }
func (g *ScoreGate) Evaluate(ctx *EvalContext) (*GateResult, error) {
	r := &GateResult{
		Name:      g.Name(),
		Severity:  g.severity,
		Score:     ctx.Score,
		Threshold: g.MinScore,
	}
	if ctx.Score >= g.MinScore {
		r.Status = GatePassed
		r.Message = fmt.Sprintf("Score %.2f meets threshold %.2f", ctx.Score, g.MinScore)
	} else {
		r.Status = GateFailed
		r.Message = fmt.Sprintf("Score %.2f below threshold %.2f", ctx.Score, g.MinScore)
	}
	return r, nil
}

// CompilationGate checks that generated code compiles successfully.
type CompilationGate struct {
	severity GateSeverity
}

func NewCompilationGate(severity GateSeverity) *CompilationGate {
	return &CompilationGate{severity: severity}
}

func (g *CompilationGate) Name() string          { return "compilation" }
func (g *CompilationGate) Severity() GateSeverity { return g.severity }
func (g *CompilationGate) Evaluate(ctx *EvalContext) (*GateResult, error) {
	r := &GateResult{
		Name:     g.Name(),
		Severity: g.severity,
	}
	if ctx.CompilationOK {
		r.Status = GatePassed
		r.Score = 1.0
		r.Message = "Code compiles successfully"
	} else {
		r.Status = GateFailed
		r.Score = 0.0
		r.Message = fmt.Sprintf("Compilation failed with %d errors", len(ctx.CompileErrors))
		r.Details = ctx.CompileErrors
	}
	return r, nil
}

// FixtureGate checks that enough test fixtures pass.
type FixtureGate struct {
	MinPassRate float64
	severity    GateSeverity
}

func NewFixtureGate(minPassRate float64, severity GateSeverity) *FixtureGate {
	return &FixtureGate{MinPassRate: minPassRate, severity: severity}
}

func (g *FixtureGate) Name() string          { return "fixtures" }
func (g *FixtureGate) Severity() GateSeverity { return g.severity }
func (g *FixtureGate) Evaluate(ctx *EvalContext) (*GateResult, error) {
	r := &GateResult{
		Name:      g.Name(),
		Severity:  g.severity,
		Threshold: g.MinPassRate,
	}

	if ctx.FixturesTotal == 0 {
		r.Status = GateSkipped
		r.Message = "No fixtures to evaluate"
		return r, nil
	}

	passRate := float64(ctx.FixturesPassed) / float64(ctx.FixturesTotal)
	r.Score = passRate

	if passRate >= g.MinPassRate {
		r.Status = GatePassed
		r.Message = fmt.Sprintf("Fixture pass rate %.1f%% meets threshold %.1f%%",
			passRate*100, g.MinPassRate*100)
	} else {
		r.Status = GateFailed
		r.Message = fmt.Sprintf("Fixture pass rate %.1f%% below threshold %.1f%% (%d/%d passed)",
			passRate*100, g.MinPassRate*100, ctx.FixturesPassed, ctx.FixturesTotal)
	}
	return r, nil
}

// CoverageGate checks that enough source functions are covered by generated code.
type CoverageGate struct {
	MinCoverage float64
	severity    GateSeverity
}

func NewCoverageGate(minCoverage float64, severity GateSeverity) *CoverageGate {
	return &CoverageGate{MinCoverage: minCoverage, severity: severity}
}

func (g *CoverageGate) Name() string          { return "coverage" }
func (g *CoverageGate) Severity() GateSeverity { return g.severity }
func (g *CoverageGate) Evaluate(ctx *EvalContext) (*GateResult, error) {
	r := &GateResult{
		Name:      g.Name(),
		Severity:  g.severity,
		Threshold: g.MinCoverage,
	}

	if ctx.FunctionsTotal == 0 {
		r.Status = GateSkipped
		r.Message = "No functions to evaluate"
		return r, nil
	}

	coverage := float64(ctx.FunctionsMatched) / float64(ctx.FunctionsTotal)
	r.Score = coverage

	if coverage >= g.MinCoverage {
		r.Status = GatePassed
		r.Message = fmt.Sprintf("Function coverage %.1f%% meets threshold %.1f%%",
			coverage*100, g.MinCoverage*100)
	} else {
		r.Status = GateFailed
		r.Message = fmt.Sprintf("Function coverage %.1f%% below threshold %.1f%% (%d/%d matched)",
			coverage*100, g.MinCoverage*100, ctx.FunctionsMatched, ctx.FunctionsTotal)
	}
	return r, nil
}

// TokenBudgetGate checks that LLM token usage stays within budget.
type TokenBudgetGate struct {
	MaxTokens int
	severity  GateSeverity
}

func NewTokenBudgetGate(maxTokens int, severity GateSeverity) *TokenBudgetGate {
	return &TokenBudgetGate{MaxTokens: maxTokens, severity: severity}
}

func (g *TokenBudgetGate) Name() string          { return "token_budget" }
func (g *TokenBudgetGate) Severity() GateSeverity { return g.severity }
func (g *TokenBudgetGate) Evaluate(ctx *EvalContext) (*GateResult, error) {
	r := &GateResult{
		Name:     g.Name(),
		Severity: g.severity,
	}

	if g.MaxTokens <= 0 {
		r.Status = GateSkipped
		r.Message = "No token budget configured"
		return r, nil
	}

	usage := float64(ctx.TotalTokens) / float64(g.MaxTokens)
	r.Score = 1.0 - usage
	if r.Score < 0 {
		r.Score = 0
	}
	r.Threshold = 0 // Must be under budget

	if ctx.TotalTokens <= g.MaxTokens {
		r.Status = GatePassed
		r.Message = fmt.Sprintf("Token usage %d within budget %d (%.0f%% used)",
			ctx.TotalTokens, g.MaxTokens, usage*100)
	} else {
		r.Status = GateFailed
		r.Message = fmt.Sprintf("Token usage %d exceeds budget %d (%.0f%% over)",
			ctx.TotalTokens, g.MaxTokens, (usage-1)*100)
	}
	return r, nil
}

// ErrorGate checks that the pipeline has no critical errors.
type ErrorGate struct {
	MaxErrors int
	severity  GateSeverity
}

func NewErrorGate(maxErrors int, severity GateSeverity) *ErrorGate {
	return &ErrorGate{MaxErrors: maxErrors, severity: severity}
}

func (g *ErrorGate) Name() string          { return "errors" }
func (g *ErrorGate) Severity() GateSeverity { return g.severity }
func (g *ErrorGate) Evaluate(ctx *EvalContext) (*GateResult, error) {
	r := &GateResult{
		Name:     g.Name(),
		Severity: g.severity,
	}

	errorCount := len(ctx.Errors)
	if errorCount <= g.MaxErrors {
		r.Status = GatePassed
		r.Score = 1.0
		r.Message = fmt.Sprintf("Error count %d within limit %d", errorCount, g.MaxErrors)
	} else {
		r.Status = GateFailed
		r.Score = 0.0
		r.Message = fmt.Sprintf("Error count %d exceeds limit %d", errorCount, g.MaxErrors)
		r.Details = ctx.Errors
	}
	return r, nil
}
