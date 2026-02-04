package judge

import (
	"testing"
)

func TestDefaultGateThresholds(t *testing.T) {
	th := DefaultGateThresholds()
	if th.MinFixturePassRate != 0.95 {
		t.Errorf("expected MinFixturePassRate 0.95, got %f", th.MinFixturePassRate)
	}
	if th.MaxDBDiffViolations != 0 {
		t.Errorf("expected MaxDBDiffViolations 0, got %d", th.MaxDBDiffViolations)
	}
	if th.MaxPerfRatio != 1.1 {
		t.Errorf("expected MaxPerfRatio 1.1, got %f", th.MaxPerfRatio)
	}
	if th.LLMWeight != 0.05 {
		t.Errorf("expected LLMWeight 0.05, got %f", th.LLMWeight)
	}
}

func TestComputeGateScore_AllGatesPass(t *testing.T) {
	result := &VerificationResult{
		Compilation:    CompilationResult{Success: true},
		FixturesPassed: 100,
		FixturesFailed: 2,
		FixturesTotal:  102,
		DBDiffViolations: 0,
		LLMConfidence:  0.9,
	}

	score := ComputeGateScore(result, nil)

	if !score.CompilationGate {
		t.Error("expected compilation gate to pass")
	}
	if !score.FunctionalGate {
		t.Error("expected functional gate to pass")
	}
	if !score.DBDiffGate {
		t.Error("expected db diff gate to pass")
	}
	if !score.AllGatesPass() {
		t.Error("expected all gates to pass")
	}
	// 0.95 + (0.9 * 0.05) = 0.95 + 0.045 = 0.995
	if score.Overall < 0.99 || score.Overall > 1.0 {
		t.Errorf("expected overall ~0.995, got %f", score.Overall)
	}
}

func TestComputeGateScore_CompilationFails(t *testing.T) {
	result := &VerificationResult{
		Compilation: CompilationResult{
			Success:    false,
			ErrorCount: 5,
			Errors:     []string{"error1", "error2"},
		},
		FixturesPassed:   100,
		FixturesTotal:    100,
		DBDiffViolations: 0,
		LLMConfidence:    1.0,
	}

	score := ComputeGateScore(result, nil)

	if score.CompilationGate {
		t.Error("expected compilation gate to fail")
	}
	if score.Overall != 0.0 {
		t.Errorf("expected overall 0.0 when compilation fails, got %f", score.Overall)
	}
	if score.AllGatesPass() {
		t.Error("expected all gates to NOT pass")
	}
}

func TestComputeGateScore_FunctionalGateFails(t *testing.T) {
	result := &VerificationResult{
		Compilation:      CompilationResult{Success: true},
		FixturesPassed:   90,
		FixturesFailed:   10,
		FixturesTotal:    100, // 90% pass rate < 95%
		DBDiffViolations: 0,
		LLMConfidence:    1.0,
	}

	score := ComputeGateScore(result, nil)

	if !score.CompilationGate {
		t.Error("expected compilation gate to pass")
	}
	if score.FunctionalGate {
		t.Error("expected functional gate to fail (90% < 95%)")
	}
	if score.AllGatesPass() {
		t.Error("expected all gates to NOT pass")
	}
	// Partial credit: 0.90 * 0.5 = 0.45
	if score.Overall < 0.44 || score.Overall > 0.46 {
		t.Errorf("expected overall ~0.45, got %f", score.Overall)
	}
}

func TestComputeGateScore_DBDiffGateFails(t *testing.T) {
	result := &VerificationResult{
		Compilation:      CompilationResult{Success: true},
		FixturesPassed:   100,
		FixturesTotal:    100,
		DBDiffViolations: 3,
		LLMConfidence:    1.0,
	}

	score := ComputeGateScore(result, nil)

	if !score.CompilationGate {
		t.Error("expected compilation gate to pass")
	}
	if !score.FunctionalGate {
		t.Error("expected functional gate to pass")
	}
	if score.DBDiffGate {
		t.Error("expected db diff gate to fail")
	}
	if score.AllGatesPass() {
		t.Error("expected all gates to NOT pass")
	}
}

func TestComputeGateScore_NoFixtures(t *testing.T) {
	result := &VerificationResult{
		Compilation:      CompilationResult{Success: true},
		FixturesPassed:   0,
		FixturesTotal:    0, // No fixtures
		DBDiffViolations: 0,
		LLMConfidence:    0.8,
	}

	score := ComputeGateScore(result, nil)

	// No fixtures = assume pass
	if !score.FunctionalGate {
		t.Error("expected functional gate to pass when no fixtures")
	}
	if !score.AllGatesPass() {
		t.Error("expected all gates to pass")
	}
}

func TestComputeGateScore_ExactThreshold(t *testing.T) {
	result := &VerificationResult{
		Compilation:      CompilationResult{Success: true},
		FixturesPassed:   95,
		FixturesTotal:    100, // Exactly 95%
		DBDiffViolations: 0,
		LLMConfidence:    0.5,
	}

	score := ComputeGateScore(result, nil)

	if !score.FunctionalGate {
		t.Error("expected functional gate to pass at exactly 95%")
	}
	if !score.AllGatesPass() {
		t.Error("expected all gates to pass")
	}
}

func TestComputeGateScore_CustomThresholds(t *testing.T) {
	result := &VerificationResult{
		Compilation:      CompilationResult{Success: true},
		FixturesPassed:   85,
		FixturesTotal:    100, // 85% pass rate
		DBDiffViolations: 2,
		LLMConfidence:    0.9,
	}

	// Relaxed thresholds
	thresholds := &GateThresholds{
		MinFixturePassRate:  0.80, // 80% instead of 95%
		MaxDBDiffViolations: 5,    // Allow up to 5 violations
		MaxPerfRatio:        1.5,
		LLMWeight:           0.10,
	}

	score := ComputeGateScore(result, thresholds)

	if !score.FunctionalGate {
		t.Error("expected functional gate to pass with relaxed threshold")
	}
	if !score.DBDiffGate {
		t.Error("expected db diff gate to pass with relaxed threshold")
	}
	if !score.AllGatesPass() {
		t.Error("expected all gates to pass with relaxed thresholds")
	}
}

func TestComputeGateScore_PerfBudget(t *testing.T) {
	result := &VerificationResult{
		Compilation:     CompilationResult{Success: true},
		FixturesPassed:  100,
		FixturesTotal:   100,
		SourceLatencyMs: 100,
		TargetLatencyMs: 105, // 105% of source
		LLMConfidence:   1.0,
	}

	score := ComputeGateScore(result, nil)

	if !score.Details.PerfWithinBudget {
		t.Error("expected perf to be within budget (105% <= 110%)")
	}

	// Test exceeding budget
	result.TargetLatencyMs = 120 // 120% of source
	score = ComputeGateScore(result, nil)

	if score.Details.PerfWithinBudget {
		t.Error("expected perf to exceed budget (120% > 110%)")
	}
	// Perf budget is informational, shouldn't fail gates
	if !score.AllGatesPass() {
		t.Error("perf budget shouldn't fail the gates")
	}
}

func TestComputeGateScore_LLMWeightCapped(t *testing.T) {
	result := &VerificationResult{
		Compilation:    CompilationResult{Success: true},
		FixturesPassed: 100,
		FixturesTotal:  100,
		LLMConfidence:  1.0, // Max confidence
	}

	score := ComputeGateScore(result, nil)

	// 0.95 + (1.0 * 0.05) = 1.0, should not exceed 1.0
	if score.Overall > 1.0 {
		t.Errorf("overall should not exceed 1.0, got %f", score.Overall)
	}
}

func TestGateScore_AllGatesPass(t *testing.T) {
	tests := []struct {
		name     string
		score    GateScore
		expected bool
	}{
		{
			name: "all pass",
			score: GateScore{
				CompilationGate: true,
				FunctionalGate:  true,
				DBDiffGate:      true,
			},
			expected: true,
		},
		{
			name: "compilation fails",
			score: GateScore{
				CompilationGate: false,
				FunctionalGate:  true,
				DBDiffGate:      true,
			},
			expected: false,
		},
		{
			name: "functional fails",
			score: GateScore{
				CompilationGate: true,
				FunctionalGate:  false,
				DBDiffGate:      true,
			},
			expected: false,
		},
		{
			name: "db diff fails",
			score: GateScore{
				CompilationGate: true,
				FunctionalGate:  true,
				DBDiffGate:      false,
			},
			expected: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if got := tt.score.AllGatesPass(); got != tt.expected {
				t.Errorf("AllGatesPass() = %v, want %v", got, tt.expected)
			}
		})
	}
}

func TestGateScore_Summary(t *testing.T) {
	score := &GateScore{
		CompilationGate: true,
		FunctionalGate:  true,
		DBDiffGate:      true,
		Overall:         0.98,
	}

	summary := score.Summary()
	if summary == "" {
		t.Error("expected non-empty summary")
	}
	if !contains(summary, "PASS") {
		t.Error("expected summary to contain PASS")
	}

	// Test failing summary
	score.FunctionalGate = false
	summary = score.Summary()
	if !contains(summary, "FAIL") {
		t.Error("expected summary to contain FAIL")
	}
}

func TestFormatFloat(t *testing.T) {
	tests := []struct {
		input    float64
		expected string
	}{
		{0.0, "0.00"},
		{0.5, "0.50"},
		{0.95, "0.95"},
		{0.99, "0.99"},
		{1.0, "1.00"},
	}

	for _, tt := range tests {
		got := formatFloat(tt.input)
		if got != tt.expected {
			t.Errorf("formatFloat(%f) = %s, want %s", tt.input, got, tt.expected)
		}
	}
}

func TestItoa(t *testing.T) {
	tests := []struct {
		input    int
		expected string
	}{
		{0, "0"},
		{1, "1"},
		{42, "42"},
		{100, "100"},
	}

	for _, tt := range tests {
		got := itoa(tt.input)
		if got != tt.expected {
			t.Errorf("itoa(%d) = %s, want %s", tt.input, got, tt.expected)
		}
	}
}

func contains(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
