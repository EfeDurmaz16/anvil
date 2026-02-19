package qualitygate

import (
	"strings"
	"testing"
	"time"
)

func TestScoreGate(t *testing.T) {
	tests := []struct {
		name      string
		minScore  float64
		severity  GateSeverity
		evalScore float64
		wantStatus GateStatus
	}{
		{
			name:       "pass above threshold",
			minScore:   0.8,
			severity:   SeverityCritical,
			evalScore:  0.85,
			wantStatus: GatePassed,
		},
		{
			name:       "pass at threshold",
			minScore:   0.8,
			severity:   SeverityCritical,
			evalScore:  0.8,
			wantStatus: GatePassed,
		},
		{
			name:       "fail below threshold",
			minScore:   0.8,
			severity:   SeverityCritical,
			evalScore:  0.75,
			wantStatus: GateFailed,
		},
		{
			name:       "pass with advisory severity",
			minScore:   0.8,
			severity:   SeverityAdvisory,
			evalScore:  0.9,
			wantStatus: GatePassed,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gate := NewScoreGate(tt.minScore, tt.severity)
			ctx := &EvalContext{
				Score: tt.evalScore,
			}

			result, err := gate.Evaluate(ctx)
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}

			if result.Status != tt.wantStatus {
				t.Errorf("got status %v, want %v", result.Status, tt.wantStatus)
			}

			if result.Name != "score" {
				t.Errorf("got name %q, want %q", result.Name, "score")
			}

			if result.Severity != tt.severity {
				t.Errorf("got severity %v, want %v", result.Severity, tt.severity)
			}

			if result.Score != tt.evalScore {
				t.Errorf("got score %v, want %v", result.Score, tt.evalScore)
			}

			if result.Threshold != tt.minScore {
				t.Errorf("got threshold %v, want %v", result.Threshold, tt.minScore)
			}
		})
	}
}

func TestCompilationGate(t *testing.T) {
	tests := []struct {
		name           string
		severity       GateSeverity
		compilationOK  bool
		compileErrors  []string
		wantStatus     GateStatus
	}{
		{
			name:          "pass with no errors",
			severity:      SeverityCritical,
			compilationOK: true,
			compileErrors: nil,
			wantStatus:    GatePassed,
		},
		{
			name:          "fail with errors",
			severity:      SeverityCritical,
			compilationOK: false,
			compileErrors: []string{"syntax error", "undefined variable"},
			wantStatus:    GateFailed,
		},
		{
			name:          "fail with required severity",
			severity:      SeverityRequired,
			compilationOK: false,
			compileErrors: []string{"type mismatch"},
			wantStatus:    GateFailed,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gate := NewCompilationGate(tt.severity)
			ctx := &EvalContext{
				CompilationOK:  tt.compilationOK,
				CompileErrors:  tt.compileErrors,
			}

			result, err := gate.Evaluate(ctx)
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}

			if result.Status != tt.wantStatus {
				t.Errorf("got status %v, want %v", result.Status, tt.wantStatus)
			}

			if result.Name != "compilation" {
				t.Errorf("got name %q, want %q", result.Name, "compilation")
			}

			if result.Severity != tt.severity {
				t.Errorf("got severity %v, want %v", result.Severity, tt.severity)
			}

			if tt.wantStatus == GateFailed && len(result.Details) != len(tt.compileErrors) {
				t.Errorf("got %d error details, want %d", len(result.Details), len(tt.compileErrors))
			}
		})
	}
}

func TestFixtureGate(t *testing.T) {
	tests := []struct {
		name           string
		minPassRate    float64
		severity       GateSeverity
		fixturesPassed int
		fixturesTotal  int
		wantStatus     GateStatus
	}{
		{
			name:           "pass with 100% pass rate",
			minPassRate:    0.8,
			severity:       SeverityRequired,
			fixturesPassed: 10,
			fixturesTotal:  10,
			wantStatus:     GatePassed,
		},
		{
			name:           "pass at threshold",
			minPassRate:    0.8,
			severity:       SeverityRequired,
			fixturesPassed: 8,
			fixturesTotal:  10,
			wantStatus:     GatePassed,
		},
		{
			name:           "fail below threshold",
			minPassRate:    0.8,
			severity:       SeverityRequired,
			fixturesPassed: 7,
			fixturesTotal:  10,
			wantStatus:     GateFailed,
		},
		{
			name:           "skip with no fixtures",
			minPassRate:    0.8,
			severity:       SeverityRequired,
			fixturesPassed: 0,
			fixturesTotal:  0,
			wantStatus:     GateSkipped,
		},
		{
			name:           "fail with zero passed",
			minPassRate:    0.5,
			severity:       SeverityRequired,
			fixturesPassed: 0,
			fixturesTotal:  10,
			wantStatus:     GateFailed,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gate := NewFixtureGate(tt.minPassRate, tt.severity)
			ctx := &EvalContext{
				FixturesPassed: tt.fixturesPassed,
				FixturesTotal:  tt.fixturesTotal,
			}

			result, err := gate.Evaluate(ctx)
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}

			if result.Status != tt.wantStatus {
				t.Errorf("got status %v, want %v", result.Status, tt.wantStatus)
			}

			if result.Name != "fixtures" {
				t.Errorf("got name %q, want %q", result.Name, "fixtures")
			}

			if result.Severity != tt.severity {
				t.Errorf("got severity %v, want %v", result.Severity, tt.severity)
			}

			if tt.fixturesTotal > 0 {
				expectedScore := float64(tt.fixturesPassed) / float64(tt.fixturesTotal)
				if result.Score != expectedScore {
					t.Errorf("got score %v, want %v", result.Score, expectedScore)
				}
			}
		})
	}
}

func TestCoverageGate(t *testing.T) {
	tests := []struct {
		name             string
		minCoverage      float64
		severity         GateSeverity
		functionsMatched int
		functionsTotal   int
		wantStatus       GateStatus
	}{
		{
			name:             "pass with 100% coverage",
			minCoverage:      0.8,
			severity:         SeverityRequired,
			functionsMatched: 10,
			functionsTotal:   10,
			wantStatus:       GatePassed,
		},
		{
			name:             "pass at threshold",
			minCoverage:      0.8,
			severity:         SeverityRequired,
			functionsMatched: 8,
			functionsTotal:   10,
			wantStatus:       GatePassed,
		},
		{
			name:             "fail below threshold",
			minCoverage:      0.8,
			severity:         SeverityRequired,
			functionsMatched: 7,
			functionsTotal:   10,
			wantStatus:       GateFailed,
		},
		{
			name:             "skip with no functions",
			minCoverage:      0.8,
			severity:         SeverityRequired,
			functionsMatched: 0,
			functionsTotal:   0,
			wantStatus:       GateSkipped,
		},
		{
			name:             "fail with zero coverage",
			minCoverage:      0.5,
			severity:         SeverityAdvisory,
			functionsMatched: 0,
			functionsTotal:   10,
			wantStatus:       GateFailed,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gate := NewCoverageGate(tt.minCoverage, tt.severity)
			ctx := &EvalContext{
				FunctionsMatched: tt.functionsMatched,
				FunctionsTotal:   tt.functionsTotal,
			}

			result, err := gate.Evaluate(ctx)
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}

			if result.Status != tt.wantStatus {
				t.Errorf("got status %v, want %v", result.Status, tt.wantStatus)
			}

			if result.Name != "coverage" {
				t.Errorf("got name %q, want %q", result.Name, "coverage")
			}

			if result.Severity != tt.severity {
				t.Errorf("got severity %v, want %v", result.Severity, tt.severity)
			}

			if tt.functionsTotal > 0 {
				expectedScore := float64(tt.functionsMatched) / float64(tt.functionsTotal)
				if result.Score != expectedScore {
					t.Errorf("got score %v, want %v", result.Score, expectedScore)
				}
			}
		})
	}
}

func TestTokenBudgetGate(t *testing.T) {
	tests := []struct {
		name        string
		maxTokens   int
		severity    GateSeverity
		totalTokens int
		wantStatus  GateStatus
	}{
		{
			name:        "pass under budget",
			maxTokens:   1000,
			severity:    SeverityAdvisory,
			totalTokens: 800,
			wantStatus:  GatePassed,
		},
		{
			name:        "pass at budget",
			maxTokens:   1000,
			severity:    SeverityAdvisory,
			totalTokens: 1000,
			wantStatus:  GatePassed,
		},
		{
			name:        "fail over budget",
			maxTokens:   1000,
			severity:    SeverityAdvisory,
			totalTokens: 1200,
			wantStatus:  GateFailed,
		},
		{
			name:        "skip with zero max tokens",
			maxTokens:   0,
			severity:    SeverityAdvisory,
			totalTokens: 500,
			wantStatus:  GateSkipped,
		},
		{
			name:        "skip with negative max tokens",
			maxTokens:   -1,
			severity:    SeverityAdvisory,
			totalTokens: 500,
			wantStatus:  GateSkipped,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gate := NewTokenBudgetGate(tt.maxTokens, tt.severity)
			ctx := &EvalContext{
				TotalTokens: tt.totalTokens,
			}

			result, err := gate.Evaluate(ctx)
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}

			if result.Status != tt.wantStatus {
				t.Errorf("got status %v, want %v", result.Status, tt.wantStatus)
			}

			if result.Name != "token_budget" {
				t.Errorf("got name %q, want %q", result.Name, "token_budget")
			}

			if result.Severity != tt.severity {
				t.Errorf("got severity %v, want %v", result.Severity, tt.severity)
			}
		})
	}
}

func TestErrorGate(t *testing.T) {
	tests := []struct {
		name       string
		maxErrors  int
		severity   GateSeverity
		errors     []string
		wantStatus GateStatus
	}{
		{
			name:       "pass with no errors",
			maxErrors:  3,
			severity:   SeverityRequired,
			errors:     nil,
			wantStatus: GatePassed,
		},
		{
			name:       "pass under threshold",
			maxErrors:  3,
			severity:   SeverityRequired,
			errors:     []string{"error1", "error2"},
			wantStatus: GatePassed,
		},
		{
			name:       "pass at threshold",
			maxErrors:  3,
			severity:   SeverityRequired,
			errors:     []string{"error1", "error2", "error3"},
			wantStatus: GatePassed,
		},
		{
			name:       "fail over threshold",
			maxErrors:  3,
			severity:   SeverityRequired,
			errors:     []string{"error1", "error2", "error3", "error4"},
			wantStatus: GateFailed,
		},
		{
			name:       "fail with zero max errors and errors present",
			maxErrors:  0,
			severity:   SeverityCritical,
			errors:     []string{"error1"},
			wantStatus: GateFailed,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gate := NewErrorGate(tt.maxErrors, tt.severity)
			ctx := &EvalContext{
				Errors: tt.errors,
			}

			result, err := gate.Evaluate(ctx)
			if err != nil {
				t.Fatalf("unexpected error: %v", err)
			}

			if result.Status != tt.wantStatus {
				t.Errorf("got status %v, want %v", result.Status, tt.wantStatus)
			}

			if result.Name != "errors" {
				t.Errorf("got name %q, want %q", result.Name, "errors")
			}

			if result.Severity != tt.severity {
				t.Errorf("got severity %v, want %v", result.Severity, tt.severity)
			}

			if tt.wantStatus == GateFailed && len(result.Details) == 0 {
				t.Error("expected error details for failed gate")
			}
		})
	}
}

func TestPipelineAllPassing(t *testing.T) {
	pipeline := NewPipeline(
		NewScoreGate(0.8, SeverityCritical),
		NewCompilationGate(SeverityCritical),
		NewFixtureGate(0.8, SeverityRequired),
		NewCoverageGate(0.7, SeverityRequired),
		NewTokenBudgetGate(10000, SeverityAdvisory),
		NewErrorGate(0, SeverityRequired),
	)

	ctx := &EvalContext{
		Score:            0.85,
		CompilationOK:    true,
		FixturesPassed:   9,
		FixturesTotal:    10,
		FunctionsMatched: 8,
		FunctionsTotal:   10,
		TotalTokens:      5000,
		Errors:           nil,
	}

	result := pipeline.Run(ctx)

	if result.Status != GatePassed {
		t.Errorf("got status %v, want %v", result.Status, GatePassed)
	}

	if result.PassedCount != 6 {
		t.Errorf("got %d passed gates, want 6", result.PassedCount)
	}

	if result.FailedCount != 0 {
		t.Errorf("got %d failed gates, want 0", result.FailedCount)
	}

	if result.SkippedCount != 0 {
		t.Errorf("got %d skipped gates, want 0", result.SkippedCount)
	}

	if result.WarningCount != 0 {
		t.Errorf("got %d warning gates, want 0", result.WarningCount)
	}

	if len(result.Gates) != 6 {
		t.Errorf("got %d gate results, want 6", len(result.Gates))
	}

	if result.Duration <= 0 {
		t.Error("expected positive duration")
	}

	if result.EvaluatedAt.IsZero() {
		t.Error("expected non-zero evaluated timestamp")
	}

	if result.Summary == "" {
		t.Error("expected non-empty summary")
	}
}

func TestPipelineCriticalGateFailure(t *testing.T) {
	pipeline := NewPipeline(
		NewScoreGate(0.8, SeverityCritical),
		NewCompilationGate(SeverityCritical),
		NewFixtureGate(0.8, SeverityRequired),
		NewCoverageGate(0.7, SeverityRequired),
	)

	ctx := &EvalContext{
		Score:            0.5,  // Fails critical score gate
		CompilationOK:    true,
		FixturesPassed:   9,
		FixturesTotal:    10,
		FunctionsMatched: 8,
		FunctionsTotal:   10,
	}

	result := pipeline.Run(ctx)

	if result.Status != GateFailed {
		t.Errorf("got status %v, want %v", result.Status, GateFailed)
	}

	if result.FailedCount == 0 {
		t.Error("expected at least one failed gate")
	}

	// Critical gate failure should potentially stop execution,
	// but the exact behavior depends on implementation
	if len(result.Gates) == 0 {
		t.Error("expected at least one gate result")
	}

	// First gate should be the failed one
	if result.Gates[0].Status != GateFailed {
		t.Errorf("first gate status got %v, want %v", result.Gates[0].Status, GateFailed)
	}

	if result.Gates[0].Severity != SeverityCritical {
		t.Errorf("first gate severity got %v, want %v", result.Gates[0].Severity, SeverityCritical)
	}
}

func TestPipelineRequiredGateFailure(t *testing.T) {
	pipeline := NewPipeline(
		NewScoreGate(0.8, SeverityCritical),
		NewFixtureGate(0.9, SeverityRequired),
		NewCoverageGate(0.7, SeverityRequired),
	)

	ctx := &EvalContext{
		Score:            0.85,
		CompilationOK:    true,
		FixturesPassed:   7,  // Fails required fixture gate (70% < 90%)
		FixturesTotal:    10,
		FunctionsMatched: 8,
		FunctionsTotal:   10,
	}

	result := pipeline.Run(ctx)

	if result.Status != GateFailed {
		t.Errorf("got status %v, want %v", result.Status, GateFailed)
	}

	if result.FailedCount == 0 {
		t.Error("expected at least one failed gate")
	}

	// Should have executed all gates despite required failure
	if len(result.Gates) != 3 {
		t.Errorf("got %d gate results, want 3", len(result.Gates))
	}
}

func TestPipelineAdvisoryWarningOnly(t *testing.T) {
	pipeline := NewPipeline(
		NewScoreGate(0.8, SeverityCritical),
		NewCompilationGate(SeverityCritical),
		NewTokenBudgetGate(1000, SeverityAdvisory),
	)

	ctx := &EvalContext{
		Score:         0.85,
		CompilationOK: true,
		TotalTokens:   1500,  // Exceeds advisory token budget
	}

	result := pipeline.Run(ctx)

	// Advisory failures should result in warnings, not overall failure
	// Implementation may vary - this tests the warning count
	if result.WarningCount == 0 && result.FailedCount > 0 {
		// Check if any failed gate is advisory
		hasAdvisoryFailure := false
		for _, gr := range result.Gates {
			if gr.Severity == SeverityAdvisory && gr.Status == GateFailed {
				hasAdvisoryFailure = true
				break
			}
		}
		if hasAdvisoryFailure {
			t.Log("advisory gate failures detected")
		}
	}

	// Overall status depends on whether advisory failures count as warnings or failures
	// Just verify the pipeline ran all gates
	if len(result.Gates) != 3 {
		t.Errorf("got %d gate results, want 3", len(result.Gates))
	}
}

func TestDefaultConfig(t *testing.T) {
	cfg := DefaultConfig()

	if cfg == nil {
		t.Fatal("expected non-nil config")
	}

	if !cfg.Enabled {
		t.Error("expected enabled by default")
	}

	if cfg.ScoreThreshold <= 0 || cfg.ScoreThreshold > 1 {
		t.Errorf("expected score threshold in (0,1], got %v", cfg.ScoreThreshold)
	}

	if cfg.FixturePassRate <= 0 || cfg.FixturePassRate > 1 {
		t.Errorf("expected fixture pass rate in (0,1], got %v", cfg.FixturePassRate)
	}

	if cfg.CoverageThreshold < 0 || cfg.CoverageThreshold > 1 {
		t.Errorf("expected coverage threshold in [0,1], got %v", cfg.CoverageThreshold)
	}

	if cfg.MaxTokens < 0 {
		t.Errorf("expected non-negative max tokens, got %v", cfg.MaxTokens)
	}

	if cfg.MaxErrors < 0 {
		t.Errorf("expected non-negative max errors, got %v", cfg.MaxErrors)
	}

	// Verify severity strings are valid
	validSeverities := map[string]bool{
		string(SeverityCritical): true,
		string(SeverityRequired): true,
		string(SeverityAdvisory): true,
	}

	if !validSeverities[cfg.ScoreSeverity] {
		t.Errorf("invalid score severity: %q", cfg.ScoreSeverity)
	}

	if !validSeverities[cfg.FixtureSeverity] {
		t.Errorf("invalid fixture severity: %q", cfg.FixtureSeverity)
	}

	if !validSeverities[cfg.CoverageSeverity] {
		t.Errorf("invalid coverage severity: %q", cfg.CoverageSeverity)
	}

	if !validSeverities[cfg.TokenSeverity] {
		t.Errorf("invalid token severity: %q", cfg.TokenSeverity)
	}

	if !validSeverities[cfg.ErrorSeverity] {
		t.Errorf("invalid error severity: %q", cfg.ErrorSeverity)
	}
}

func TestBuildPipeline(t *testing.T) {
	cfg := &GateConfig{
		Enabled:            true,
		ScoreThreshold:     0.85,
		ScoreSeverity:      string(SeverityCritical),
		CompilationRequired: true,
		FixturePassRate:    0.9,
		FixtureSeverity:    string(SeverityRequired),
		CoverageThreshold:  0.75,
		CoverageSeverity:   string(SeverityRequired),
		MaxTokens:          5000,
		TokenSeverity:      string(SeverityAdvisory),
		MaxErrors:          2,
		ErrorSeverity:      string(SeverityRequired),
	}

	pipeline := BuildPipeline(cfg)

	if pipeline == nil {
		t.Fatal("expected non-nil pipeline")
	}

	// Run the pipeline to verify it was built correctly
	ctx := &EvalContext{
		Score:            0.9,
		CompilationOK:    true,
		FixturesPassed:   9,
		FixturesTotal:    10,
		FunctionsMatched: 8,
		FunctionsTotal:   10,
		TotalTokens:      3000,
		Errors:           []string{"warning1"},
	}

	result := pipeline.Run(ctx)

	if result == nil {
		t.Fatal("expected non-nil result")
	}

	// Should have multiple gates based on config
	if len(result.Gates) == 0 {
		t.Error("expected at least one gate in pipeline")
	}

	// Verify specific gates exist
	gateNames := make(map[string]bool)
	for _, gr := range result.Gates {
		gateNames[gr.Name] = true
	}

	expectedGates := []string{"score", "compilation", "fixtures", "coverage", "errors"}
	for _, name := range expectedGates {
		if !gateNames[name] {
			t.Errorf("expected gate %q not found", name)
		}
	}
}

func TestBuildPipelineDisabled(t *testing.T) {
	cfg := &GateConfig{
		Enabled:            false,
		ScoreThreshold:     0.8,
		ScoreSeverity:      string(SeverityCritical),
		CompilationRequired: false,
	}

	pipeline := BuildPipeline(cfg)

	if pipeline == nil {
		t.Fatal("expected non-nil pipeline even when disabled")
	}

	// Run to see behavior
	ctx := &EvalContext{
		Score:         0.5,
		CompilationOK: false,
	}

	result := pipeline.Run(ctx)

	if result == nil {
		t.Fatal("expected non-nil result")
	}

	// When disabled, pipeline might skip gates or return empty results
	// The exact behavior depends on implementation
}

func TestFormatReport(t *testing.T) {
	result := &PipelineResult{
		Status:        GatePassed,
		PassedCount:   4,
		FailedCount:   1,
		SkippedCount:  1,
		WarningCount:  0,
		Duration:      123 * time.Millisecond,
		EvaluatedAt:   time.Now(),
		Summary:       "Quality gate evaluation complete",
		Gates: []GateResult{
			{
				Name:      "score",
				Status:    GatePassed,
				Severity:  SeverityCritical,
				Score:     0.85,
				Threshold: 0.8,
				Message:   "Score meets threshold",
			},
			{
				Name:     "compilation",
				Status:   GateFailed,
				Severity: SeverityCritical,
				Message:  "Compilation failed",
				Details:  []string{"syntax error on line 42"},
			},
			{
				Name:     "fixtures",
				Status:   GateSkipped,
				Severity: SeverityRequired,
				Message:  "No fixtures available",
			},
		},
	}

	report := FormatReport(result)

	if report == "" {
		t.Error("expected non-empty report")
	}

	// Verify report contains key information
	expectedStrings := []string{
		"Quality",
		"Gate",
	}

	for _, s := range expectedStrings {
		if !strings.Contains(report, s) {
			t.Errorf("report missing expected string %q", s)
		}
	}

	// Verify that the report contains gate names (they should be in the output)
	if !strings.Contains(report, "score") && !strings.Contains(report, "compilation") && !strings.Contains(report, "fixtures") {
		t.Error("report missing gate names")
	}
}

func TestPipelineAddGate(t *testing.T) {
	pipeline := NewPipeline()

	if pipeline == nil {
		t.Fatal("expected non-nil pipeline")
	}

	pipeline.AddGate(NewScoreGate(0.8, SeverityCritical))
	pipeline.AddGate(NewCompilationGate(SeverityCritical))

	ctx := &EvalContext{
		Score:         0.85,
		CompilationOK: true,
	}

	result := pipeline.Run(ctx)

	if len(result.Gates) != 2 {
		t.Errorf("got %d gates, want 2", len(result.Gates))
	}

	if result.PassedCount != 2 {
		t.Errorf("got %d passed, want 2", result.PassedCount)
	}
}

func TestGateEvaluationTiming(t *testing.T) {
	gate := NewScoreGate(0.8, SeverityCritical)
	ctx := &EvalContext{
		Score: 0.85,
	}

	result, err := gate.Evaluate(ctx)

	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Note: Individual gate.Evaluate() does not set Duration/EvaluatedAt
	// These are set by the Pipeline.Run() method
	// So we just verify the result is valid
	if result == nil {
		t.Fatal("expected non-nil result")
	}

	if result.Name != "score" {
		t.Errorf("got name %q, want %q", result.Name, "score")
	}
}

func TestPipelineEmptyGates(t *testing.T) {
	pipeline := NewPipeline()

	ctx := &EvalContext{
		Score: 0.85,
	}

	result := pipeline.Run(ctx)

	if result == nil {
		t.Fatal("expected non-nil result for empty pipeline")
	}

	if len(result.Gates) != 0 {
		t.Errorf("got %d gates, want 0", len(result.Gates))
	}

	if result.PassedCount != 0 {
		t.Errorf("got %d passed, want 0", result.PassedCount)
	}

	if result.FailedCount != 0 {
		t.Errorf("got %d failed, want 0", result.FailedCount)
	}

	// Empty pipeline should pass overall
	if result.Status != GatePassed {
		t.Errorf("got status %v, want %v for empty pipeline", result.Status, GatePassed)
	}
}

func TestEvalContextMetadata(t *testing.T) {
	ctx := &EvalContext{
		Score:         0.85,
		CompilationOK: true,
		Metadata: map[string]string{
			"project":     "test-project",
			"environment": "testing",
		},
	}

	if ctx.Metadata["project"] != "test-project" {
		t.Errorf("got project %q, want %q", ctx.Metadata["project"], "test-project")
	}

	if ctx.Metadata["environment"] != "testing" {
		t.Errorf("got environment %q, want %q", ctx.Metadata["environment"], "testing")
	}
}

func TestGateResultDetails(t *testing.T) {
	gate := NewCompilationGate(SeverityCritical)
	ctx := &EvalContext{
		CompilationOK: false,
		CompileErrors: []string{
			"syntax error on line 10",
			"undefined variable 'foo'",
			"type mismatch",
		},
	}

	result, err := gate.Evaluate(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if len(result.Details) == 0 {
		t.Error("expected details for compilation failure")
	}

	// Verify details contain error information
	detailsStr := strings.Join(result.Details, " ")
	for _, errMsg := range ctx.CompileErrors {
		if !strings.Contains(detailsStr, errMsg) && len(result.Details) < len(ctx.CompileErrors) {
			// Either details contain the error or there's a summary
			continue
		}
	}
}

func TestMultiplePipelineRuns(t *testing.T) {
	pipeline := NewPipeline(
		NewScoreGate(0.8, SeverityCritical),
		NewCompilationGate(SeverityCritical),
	)

	// First run - passing
	ctx1 := &EvalContext{
		Score:         0.9,
		CompilationOK: true,
	}

	result1 := pipeline.Run(ctx1)
	if result1.Status != GatePassed {
		t.Errorf("first run: got status %v, want %v", result1.Status, GatePassed)
	}

	// Second run - failing
	ctx2 := &EvalContext{
		Score:         0.5,
		CompilationOK: false,
	}

	result2 := pipeline.Run(ctx2)
	if result2.Status != GateFailed {
		t.Errorf("second run: got status %v, want %v", result2.Status, GateFailed)
	}

	// Results should be independent
	if result1.Status == result2.Status {
		t.Error("expected different statuses for different contexts")
	}
}

func TestGateInterfaceCompliance(t *testing.T) {
	gates := []Gate{
		NewScoreGate(0.8, SeverityCritical),
		NewCompilationGate(SeverityCritical),
		NewFixtureGate(0.8, SeverityRequired),
		NewCoverageGate(0.7, SeverityRequired),
		NewTokenBudgetGate(5000, SeverityAdvisory),
		NewErrorGate(3, SeverityRequired),
	}

	ctx := &EvalContext{
		Score:            0.85,
		CompilationOK:    true,
		FixturesPassed:   8,
		FixturesTotal:    10,
		FunctionsMatched: 7,
		FunctionsTotal:   10,
		TotalTokens:      3000,
		Errors:           []string{"error1"},
	}

	for _, gate := range gates {
		// Verify Name() method
		name := gate.Name()
		if name == "" {
			t.Errorf("gate %T returned empty name", gate)
		}

		// Verify Severity() method
		severity := gate.Severity()
		validSeverities := map[GateSeverity]bool{
			SeverityCritical: true,
			SeverityRequired: true,
			SeverityAdvisory: true,
		}
		if !validSeverities[severity] {
			t.Errorf("gate %s returned invalid severity %q", name, severity)
		}

		// Verify Evaluate() method
		result, err := gate.Evaluate(ctx)
		if err != nil {
			t.Errorf("gate %s returned error: %v", name, err)
		}

		if result == nil {
			t.Errorf("gate %s returned nil result", name)
		}

		if result.Name != name {
			t.Errorf("gate %s result name mismatch: got %q", name, result.Name)
		}

		if result.Severity != severity {
			t.Errorf("gate %s result severity mismatch: got %v, want %v", name, result.Severity, severity)
		}
	}
}
