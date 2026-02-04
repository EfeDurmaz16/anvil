package judge

// GateScore represents the gate-based scoring model for Judge 2.0.
// The overall score depends on passing all gates - LLM verdict is supplementary only.
type GateScore struct {
	Overall         float64 `json:"overall"`          // 0.0 - 1.0
	CompilationGate bool    `json:"compilation_gate"` // Must pass - code compiles
	FunctionalGate  bool    `json:"functional_gate"`  // Must pass - >= 95% fixtures pass
	DBDiffGate      bool    `json:"db_diff_gate"`     // Must pass - 0 violations (whitelist excluded)
	LLMConfidence   float64 `json:"llm_confidence"`   // Optional signal (0.0 - 1.0)

	Details GateScoreDetails `json:"details"`
}

// GateScoreDetails contains detailed breakdown of the scoring.
type GateScoreDetails struct {
	// Compilation details
	CompileErrors   []string `json:"compile_errors,omitempty"`
	CompileWarnings int      `json:"compile_warnings,omitempty"`

	// Fixture details
	FixturesPassed int `json:"fixtures_passed"`
	FixturesFailed int `json:"fixtures_failed"`
	FixturesTotal  int `json:"fixtures_total"`

	// DB diff details
	DBDiffViolations     int      `json:"db_diff_violations"`
	DBDiffWhitelisted    int      `json:"db_diff_whitelisted,omitempty"`
	DBDiffViolationsList []string `json:"db_diff_violations_list,omitempty"`

	// Performance details
	PerfWithinBudget bool    `json:"perf_within_budget"`
	PerfRatio        float64 `json:"perf_ratio,omitempty"` // target_latency / source_latency
}

// VerificationResult contains raw verification data used to compute gate scores.
type VerificationResult struct {
	// Compilation
	Compilation CompilationResult `json:"compilation"`

	// Fixtures
	FixturesPassed int `json:"fixtures_passed"`
	FixturesFailed int `json:"fixtures_failed"`
	FixturesTotal  int `json:"fixtures_total"`

	// DB Diff
	DBDiffViolations  int `json:"db_diff_violations"`
	DBDiffWhitelisted int `json:"db_diff_whitelisted"`

	// Performance
	SourceLatencyMs float64 `json:"source_latency_ms,omitempty"`
	TargetLatencyMs float64 `json:"target_latency_ms,omitempty"`

	// LLM confidence from semantic verification
	LLMConfidence float64 `json:"llm_confidence"`
}

// CompilationResult contains compilation outcome.
type CompilationResult struct {
	Success      bool     `json:"success"`
	ErrorCount   int      `json:"error_count"`
	WarningCount int      `json:"warning_count"`
	Errors       []string `json:"errors,omitempty"`
}

// GateThresholds defines the pass/fail thresholds for gates.
type GateThresholds struct {
	// MinFixturePassRate is the minimum fixture pass rate (default: 0.95 = 95%)
	MinFixturePassRate float64

	// MaxDBDiffViolations is the maximum allowed DB diff violations (default: 0)
	MaxDBDiffViolations int

	// MaxPerfRatio is the maximum allowed performance ratio (default: 1.1 = 110%)
	MaxPerfRatio float64

	// LLMWeight is the weight of LLM confidence in final score (default: 0.05 = 5%)
	LLMWeight float64
}

// DefaultGateThresholds returns the default gate thresholds.
func DefaultGateThresholds() *GateThresholds {
	return &GateThresholds{
		MinFixturePassRate:  0.95,
		MaxDBDiffViolations: 0,
		MaxPerfRatio:        1.1,
		LLMWeight:           0.05,
	}
}

// ComputeGateScore computes the gate-based score from verification results.
// The score follows these rules:
// 1. Compilation gate MUST pass - if not, overall = 0
// 2. Functional gate requires >= 95% fixtures pass
// 3. DB diff gate requires 0 violations (whitelist excluded)
// 4. LLM verdict adds max 5% to score (supplementary only)
func ComputeGateScore(result *VerificationResult, thresholds *GateThresholds) *GateScore {
	if thresholds == nil {
		thresholds = DefaultGateThresholds()
	}

	score := &GateScore{
		LLMConfidence: result.LLMConfidence,
		Details: GateScoreDetails{
			CompileErrors:    result.Compilation.Errors,
			CompileWarnings:  result.Compilation.WarningCount,
			FixturesPassed:   result.FixturesPassed,
			FixturesFailed:   result.FixturesFailed,
			FixturesTotal:    result.FixturesTotal,
			DBDiffViolations: result.DBDiffViolations,
			DBDiffWhitelisted: result.DBDiffWhitelisted,
		},
	}

	// Gate 1: Compilation (must pass)
	score.CompilationGate = result.Compilation.Success
	if !score.CompilationGate {
		score.Overall = 0.0
		return score
	}

	// Gate 2: Functional tests (must pass >= threshold%)
	var passRate float64
	if result.FixturesTotal > 0 {
		passRate = float64(result.FixturesPassed) / float64(result.FixturesTotal)
	} else {
		// No fixtures = assume pass (nothing to test)
		passRate = 1.0
	}
	score.FunctionalGate = passRate >= thresholds.MinFixturePassRate

	// Gate 3: DB diff (must have 0 violations outside whitelist)
	score.DBDiffGate = result.DBDiffViolations <= thresholds.MaxDBDiffViolations

	// Gate 4: Performance budget (informational, doesn't fail gate)
	if result.SourceLatencyMs > 0 && result.TargetLatencyMs > 0 {
		perfRatio := result.TargetLatencyMs / result.SourceLatencyMs
		score.Details.PerfRatio = perfRatio
		score.Details.PerfWithinBudget = perfRatio <= thresholds.MaxPerfRatio
	} else {
		score.Details.PerfWithinBudget = true // No perf data = assume ok
	}

	// Compute overall score
	if score.CompilationGate && score.FunctionalGate && score.DBDiffGate {
		// All gates pass: base score 0.95, LLM adds up to 0.05
		baseScore := 0.95
		llmBonus := result.LLMConfidence * thresholds.LLMWeight
		score.Overall = baseScore + llmBonus
		if score.Overall > 1.0 {
			score.Overall = 1.0
		}
	} else {
		// Not all gates pass: partial credit based on fixture pass rate
		score.Overall = passRate * 0.5
	}

	return score
}

// AllGatesPass returns true if all required gates passed.
func (gs *GateScore) AllGatesPass() bool {
	return gs.CompilationGate && gs.FunctionalGate && gs.DBDiffGate
}

// Summary returns a human-readable summary of the gate score.
func (gs *GateScore) Summary() string {
	status := "PASS"
	if !gs.AllGatesPass() {
		status = "FAIL"
	}

	return formatSummary(status, gs)
}

func formatSummary(status string, gs *GateScore) string {
	return status + ": " +
		"Compilation=" + boolToPassFail(gs.CompilationGate) + ", " +
		"Functional=" + boolToPassFail(gs.FunctionalGate) + ", " +
		"DBDiff=" + boolToPassFail(gs.DBDiffGate) + ", " +
		formatScore(gs.Overall)
}

func boolToPassFail(b bool) string {
	if b {
		return "PASS"
	}
	return "FAIL"
}

func formatScore(score float64) string {
	return "Score=" + formatFloat(score)
}

func formatFloat(v float64) string {
	// Simple float formatting without fmt
	intPart := int(v * 100)
	if intPart >= 100 {
		return "1.00"
	}
	if intPart < 10 {
		return "0.0" + itoa(intPart)
	}
	return "0." + itoa(intPart)
}

func itoa(i int) string {
	if i == 0 {
		return "0"
	}
	var digits [10]byte
	n := len(digits)
	for i > 0 {
		n--
		digits[n] = byte('0' + i%10)
		i /= 10
	}
	return string(digits[n:])
}
