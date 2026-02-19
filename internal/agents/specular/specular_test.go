package specular

import (
	"context"
	"errors"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/llm"
)

// mockProvider is a mock LLM provider for testing.
type mockProvider struct {
	name    string
	content string
	err     error
}

func (m *mockProvider) Complete(_ context.Context, _ *llm.Prompt, _ *llm.RequestOptions) (*llm.Response, error) {
	if m.err != nil {
		return nil, m.err
	}
	return &llm.Response{Content: m.content}, nil
}

func (m *mockProvider) Embed(_ context.Context, _ []string) ([][]float32, error) {
	return nil, nil
}

func (m *mockProvider) Name() string {
	return m.name
}

func TestSpecular_Name(t *testing.T) {
	s := New()
	if got := s.Name(); got != "specular" {
		t.Errorf("Name() = %q, want %q", got, "specular")
	}
}

func TestSpecular_NilGraph(t *testing.T) {
	s := New()
	ac := &agents.AgentContext{
		Graph: nil,
		LLM:   &mockProvider{name: "mock", content: "[]"},
	}

	res, err := s.Run(context.Background(), ac)
	if err == nil {
		t.Fatal("expected error for nil graph")
	}
	if res.Status != agents.StatusFailed {
		t.Errorf("Status = %v, want %v", res.Status, agents.StatusFailed)
	}
	if len(res.Errors) == 0 {
		t.Error("expected errors in result")
	}
}

func TestSpecular_NilLLMPassthrough(t *testing.T) {
	s := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Functions: []*ir.Function{
					{Name: "F1", Body: "test body"},
				},
			},
		},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   nil, // No LLM provider
	}

	res, err := s.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if res.Status != agents.StatusPassthrough {
		t.Errorf("Status = %v, want %v", res.Status, agents.StatusPassthrough)
	}
	if res.Metadata["mode"] != "passthrough" {
		t.Errorf("mode = %q, want %q", res.Metadata["mode"], "passthrough")
	}
	if res.Metrics.OutputItems != 0 {
		t.Errorf("OutputItems = %d, want 0", res.Metrics.OutputItems)
	}
	if res.Metrics.InputItems != 1 {
		t.Errorf("InputItems = %d, want 1", res.Metrics.InputItems)
	}
}

func TestSpecular_ValidJSONRulesExtracted(t *testing.T) {
	s := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "TestModule",
				Language: "cobol",
				Functions: []*ir.Function{
					{Name: "ProcessPayment", Body: "IF BALANCE > 0 THEN APPROVE."},
				},
			},
		},
	}

	jsonResponse := `[
		{
			"id": "rule1",
			"description": "Approve payment if balance is positive",
			"confidence": 0.95,
			"tags": ["payment", "validation"]
		},
		{
			"id": "rule2",
			"description": "Check account status",
			"confidence": 0.85,
			"tags": ["account"]
		}
	]`

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   &mockProvider{name: "mock", content: jsonResponse},
	}

	res, err := s.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if res.Status != agents.StatusSuccess {
		t.Errorf("Status = %v, want %v", res.Status, agents.StatusSuccess)
	}
	if res.Metrics.InputItems != 1 {
		t.Errorf("InputItems = %d, want 1", res.Metrics.InputItems)
	}
	if res.Metrics.OutputItems != 2 {
		t.Errorf("OutputItems = %d, want 2", res.Metrics.OutputItems)
	}
	if len(graph.BusinessRules) != 2 {
		t.Fatalf("BusinessRules count = %d, want 2", len(graph.BusinessRules))
	}

	// Verify SourceRef was populated
	if graph.BusinessRules[0].SourceRef != "TestModule.ProcessPayment" {
		t.Errorf("SourceRef = %q, want %q", graph.BusinessRules[0].SourceRef, "TestModule.ProcessPayment")
	}
}

func TestSpecular_InvalidJSONCreatesFallbackRule(t *testing.T) {
	s := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "TestModule",
				Language: "cobol",
				Functions: []*ir.Function{
					{Name: "ValidateInput", Body: "VALIDATE FIELD-A."},
				},
			},
		},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   &mockProvider{name: "mock", content: "This is not valid JSON"},
	}

	res, err := s.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if res.Status != agents.StatusSuccess {
		t.Errorf("Status = %v, want %v", res.Status, agents.StatusSuccess)
	}
	if len(graph.BusinessRules) != 1 {
		t.Fatalf("BusinessRules count = %d, want 1", len(graph.BusinessRules))
	}

	rule := graph.BusinessRules[0]
	if rule.Confidence != 0.5 {
		t.Errorf("Confidence = %f, want 0.5", rule.Confidence)
	}
	if rule.Description != "This is not valid JSON" {
		t.Errorf("Description = %q, want raw content", rule.Description)
	}
	if rule.ID != "TestModule.ValidateInput.rule1" {
		t.Errorf("ID = %q, want %q", rule.ID, "TestModule.ValidateInput.rule1")
	}
}

func TestSpecular_LLMErrorSetsPartialOrFailed(t *testing.T) {
	s := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "TestModule",
				Language: "cobol",
				Functions: []*ir.Function{
					{Name: "F1", Body: "body"},
				},
			},
		},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   &mockProvider{name: "mock", err: errors.New("LLM unavailable")},
	}

	res, err := s.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Since all functions failed, status should be StatusFailed
	if res.Status != agents.StatusFailed {
		t.Errorf("Status = %v, want %v", res.Status, agents.StatusFailed)
	}
	if len(res.Errors) == 0 {
		t.Error("expected errors in result")
	}
	if res.Metrics.SkippedItems != 1 {
		t.Errorf("SkippedItems = %d, want 1", res.Metrics.SkippedItems)
	}
}

func TestSpecular_ConcurrentProcessing(t *testing.T) {
	s := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "Module1",
				Language: "cobol",
				Functions: []*ir.Function{
					{Name: "F1", Body: "body1"},
					{Name: "F2", Body: "body2"},
					{Name: "F3", Body: "body3"},
				},
			},
		},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   &mockProvider{name: "mock", content: `[{"id":"r1","description":"test","confidence":0.9}]`},
		Params: map[string]string{
			"max_concurrent": "3",
		},
	}

	res, err := s.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if res.Status != agents.StatusSuccess {
		t.Errorf("Status = %v, want %v", res.Status, agents.StatusSuccess)
	}
	// Each function should extract rules
	if res.Metrics.OutputItems != 3 {
		t.Errorf("OutputItems = %d, want 3", res.Metrics.OutputItems)
	}
	if res.Metrics.LLMCalls != 3 {
		t.Errorf("LLMCalls = %d, want 3", res.Metrics.LLMCalls)
	}
}

func TestSpecular_AllFunctionsFail(t *testing.T) {
	s := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "TestModule",
				Language: "cobol",
				Functions: []*ir.Function{
					{Name: "F1", Body: "body1"},
					{Name: "F2", Body: "body2"},
				},
			},
		},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   &mockProvider{name: "mock", err: errors.New("all fail")},
	}

	res, err := s.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if res.Status != agents.StatusFailed {
		t.Errorf("Status = %v, want %v", res.Status, agents.StatusFailed)
	}
	if res.Metrics.OutputItems != 0 {
		t.Errorf("OutputItems = %d, want 0", res.Metrics.OutputItems)
	}
	if res.Metrics.SkippedItems != 2 {
		t.Errorf("SkippedItems = %d, want 2", res.Metrics.SkippedItems)
	}
}

func TestSpecular_SomeFunctionsFail(t *testing.T) {
	s := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "TestModule",
				Language: "cobol",
				Functions: []*ir.Function{
					{Name: "F1", Body: "body1"},
					{Name: "F2", Body: "body2"},
					{Name: "F3", Body: "body3"},
				},
			},
		},
	}

	// Mock provider that fails on certain calls
	provider := &conditionalMockProvider{callCount: 0}

	ac := &agents.AgentContext{
		Graph:  graph,
		LLM:    provider,
		Params: map[string]string{"max_concurrent": "1"}, // Sequential to control order
	}

	res, err := s.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Some succeeded, some failed -> StatusPartial
	if res.Status != agents.StatusPartial {
		t.Errorf("Status = %v, want %v", res.Status, agents.StatusPartial)
	}
	if res.Metrics.OutputItems != 2 {
		t.Errorf("OutputItems = %d, want 2", res.Metrics.OutputItems)
	}
	if res.Metrics.SkippedItems != 1 {
		t.Errorf("SkippedItems = %d, want 1", res.Metrics.SkippedItems)
	}
}

// conditionalMockProvider fails on the second call.
type conditionalMockProvider struct {
	callCount int
}

func (c *conditionalMockProvider) Complete(ctx context.Context, p *llm.Prompt, opts *llm.RequestOptions) (*llm.Response, error) {
	c.callCount++
	if c.callCount == 2 {
		return nil, errors.New("fail second call")
	}
	return &llm.Response{Content: `[{"id":"r1","description":"test","confidence":0.9}]`}, nil
}

func (c *conditionalMockProvider) Embed(ctx context.Context, texts []string) ([][]float32, error) {
	return nil, nil
}

func (c *conditionalMockProvider) Name() string {
	return "conditional-mock"
}

func TestSpecular_EmptyGraph(t *testing.T) {
	s := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   &mockProvider{name: "mock", content: "[]"},
	}

	res, err := s.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if res.Status != agents.StatusSuccess {
		t.Errorf("Status = %v, want %v", res.Status, agents.StatusSuccess)
	}
	if res.Metrics.InputItems != 0 {
		t.Errorf("InputItems = %d, want 0", res.Metrics.InputItems)
	}
	if res.Metrics.OutputItems != 0 {
		t.Errorf("OutputItems = %d, want 0", res.Metrics.OutputItems)
	}
}

func TestSpecular_MetadataPopulated(t *testing.T) {
	s := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "TestModule",
				Language: "cobol",
				Functions: []*ir.Function{
					{Name: "F1", Body: "body"},
				},
			},
		},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   &mockProvider{name: "mock", content: `[{"id":"r1","description":"test","confidence":0.9},{"id":"r2","description":"test2","confidence":0.8}]`},
	}

	res, err := s.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if res.Metadata["mode"] != "llm" {
		t.Errorf("mode = %q, want %q", res.Metadata["mode"], "llm")
	}
	if res.Metadata["rules_extracted"] != "2" {
		t.Errorf("rules_extracted = %q, want %q", res.Metadata["rules_extracted"], "2")
	}
}

func TestSpecular_MultipleFunctionsMultipleModules(t *testing.T) {
	s := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "Module1",
				Language: "cobol",
				Functions: []*ir.Function{
					{Name: "F1", Body: "body1"},
					{Name: "F2", Body: "body2"},
				},
			},
			{
				Name:     "Module2",
				Language: "cobol",
				Functions: []*ir.Function{
					{Name: "F3", Body: "body3"},
				},
			},
		},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   &mockProvider{name: "mock", content: `[{"id":"r1","description":"test","confidence":0.9}]`},
	}

	res, err := s.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Should process all functions across all modules
	if res.Metrics.InputItems != 3 {
		t.Errorf("InputItems = %d, want 3", res.Metrics.InputItems)
	}
	if res.Metrics.OutputItems != 3 {
		t.Errorf("OutputItems = %d, want 3", res.Metrics.OutputItems)
	}
	if len(graph.BusinessRules) != 3 {
		t.Errorf("BusinessRules count = %d, want 3", len(graph.BusinessRules))
	}
}

func TestSpecular_InvalidMaxConcurrentDefaultsToOne(t *testing.T) {
	s := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Functions: []*ir.Function{{Name: "F1", Body: "body"}},
			},
		},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   &mockProvider{name: "mock", content: `[{"id":"r1","description":"test","confidence":0.9}]`},
		Params: map[string]string{
			"max_concurrent": "invalid",
		},
	}

	// Should not crash, just default to 1
	res, err := s.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if res.Status != agents.StatusSuccess {
		t.Errorf("Status = %v, want %v", res.Status, agents.StatusSuccess)
	}
}
