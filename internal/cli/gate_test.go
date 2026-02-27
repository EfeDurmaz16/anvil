package cli

import (
	"testing"

	"github.com/efebarandurmaz/anvil/internal/qualitygate"
)

func TestScoreGate_Pass(t *testing.T) {
	g := &scoreGate{threshold: 0.7}
	ctx := &qualitygate.EvalContext{Score: 0.85}
	result, err := g.Evaluate(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Status != qualitygate.GatePassed {
		t.Errorf("expected GatePassed with score 0.85 > 0.7, got %s", result.Status)
	}
}

func TestScoreGate_Fail(t *testing.T) {
	g := &scoreGate{threshold: 0.7}
	ctx := &qualitygate.EvalContext{Score: 0.5}
	result, err := g.Evaluate(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Status != qualitygate.GateFailed {
		t.Errorf("expected GateFailed with score 0.5 < 0.7, got %s", result.Status)
	}
}

func TestCompilationGate_Pass(t *testing.T) {
	g := &compilationGate{}
	ctx := &qualitygate.EvalContext{CompilationOK: true}
	result, err := g.Evaluate(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Status != qualitygate.GatePassed {
		t.Errorf("expected compilation gate to pass, got %s", result.Status)
	}
}

func TestCompilationGate_Fail(t *testing.T) {
	g := &compilationGate{}
	ctx := &qualitygate.EvalContext{CompilationOK: false, CompileErrors: []string{"undefined: foo"}}
	result, err := g.Evaluate(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Status != qualitygate.GateFailed {
		t.Errorf("expected compilation gate to fail, got %s", result.Status)
	}
}

func TestFixtureRateGate_Pass(t *testing.T) {
	g := &fixtureRateGate{threshold: 0.9}
	ctx := &qualitygate.EvalContext{FixturesPassed: 95, FixturesTotal: 100}
	result, err := g.Evaluate(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Status != qualitygate.GatePassed {
		t.Errorf("expected fixture gate to pass, got %s", result.Status)
	}
}

func TestFixtureRateGate_Fail(t *testing.T) {
	g := &fixtureRateGate{threshold: 0.9}
	ctx := &qualitygate.EvalContext{FixturesPassed: 50, FixturesTotal: 100}
	result, err := g.Evaluate(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Status != qualitygate.GateFailed {
		t.Errorf("expected fixture gate to fail, got %s", result.Status)
	}
}

func TestFixtureRateGate_NoFixtures(t *testing.T) {
	g := &fixtureRateGate{threshold: 0.9}
	ctx := &qualitygate.EvalContext{FixturesTotal: 0}
	result, err := g.Evaluate(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Status != qualitygate.GateSkipped {
		t.Errorf("expected GateSkipped when no fixtures, got %s", result.Status)
	}
}

func TestCoverageGate_Pass(t *testing.T) {
	g := &coverageGate{threshold: 0.8}
	ctx := &qualitygate.EvalContext{FunctionsMatched: 9, FunctionsTotal: 10}
	result, err := g.Evaluate(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Status != qualitygate.GatePassed {
		t.Errorf("expected coverage gate to pass, got %s", result.Status)
	}
}

func TestCoverageGate_Fail(t *testing.T) {
	g := &coverageGate{threshold: 0.8}
	ctx := &qualitygate.EvalContext{FunctionsMatched: 5, FunctionsTotal: 10}
	result, err := g.Evaluate(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Status != qualitygate.GateFailed {
		t.Errorf("expected coverage gate to fail, got %s", result.Status)
	}
}

func TestTokenBudgetGate_Pass(t *testing.T) {
	g := &tokenBudgetGate{maxTokens: 200000}
	ctx := &qualitygate.EvalContext{TotalTokens: 150000}
	result, err := g.Evaluate(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Status != qualitygate.GatePassed {
		t.Errorf("expected token budget gate to pass, got %s", result.Status)
	}
}

func TestTokenBudgetGate_Exceed(t *testing.T) {
	g := &tokenBudgetGate{maxTokens: 100000}
	ctx := &qualitygate.EvalContext{TotalTokens: 200000}
	result, err := g.Evaluate(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Status != qualitygate.GateWarning {
		t.Errorf("expected token budget gate to warn, got %s", result.Status)
	}
}

func TestErrorCountGate_Pass(t *testing.T) {
	g := &errorCountGate{maxErrors: 5}
	ctx := &qualitygate.EvalContext{Errors: []string{"e1", "e2"}}
	result, err := g.Evaluate(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Status != qualitygate.GatePassed {
		t.Errorf("expected error count gate to pass, got %s", result.Status)
	}
}

func TestErrorCountGate_Fail(t *testing.T) {
	g := &errorCountGate{maxErrors: 1}
	ctx := &qualitygate.EvalContext{Errors: []string{"e1", "e2", "e3"}}
	result, err := g.Evaluate(ctx)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if result.Status != qualitygate.GateFailed {
		t.Errorf("expected error count gate to fail, got %s", result.Status)
	}
}
