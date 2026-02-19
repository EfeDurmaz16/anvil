package architect

import (
	"context"
	"errors"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// newMockRegistry creates a registry with a mock target plugin.
func newMockRegistry(target plugins.TargetPlugin) *plugins.Registry {
	reg := plugins.NewRegistry()
	if target != nil {
		reg.RegisterTarget(target)
	}
	return reg
}

// mockTargetPlugin is a mock TargetPlugin for testing.
type mockTargetPlugin struct {
	language      string
	scaffoldFiles []plugins.GeneratedFile
	scaffoldError error
	genFiles      []plugins.GeneratedFile
	genError      error
	lastContext   context.Context
}

func (m *mockTargetPlugin) Language() string {
	return m.language
}

func (m *mockTargetPlugin) Scaffold(ctx context.Context, graph *ir.SemanticGraph) ([]plugins.GeneratedFile, error) {
	if m.scaffoldError != nil {
		return nil, m.scaffoldError
	}
	return m.scaffoldFiles, nil
}

func (m *mockTargetPlugin) Generate(ctx context.Context, graph *ir.SemanticGraph, provider llm.Provider) ([]plugins.GeneratedFile, error) {
	m.lastContext = ctx
	if m.genError != nil {
		return nil, m.genError
	}
	return m.genFiles, nil
}

// mockLLMProvider is a simple mock LLM provider.
type mockLLMProvider struct{}

func (m *mockLLMProvider) Complete(ctx context.Context, prompt *llm.Prompt, opts *llm.RequestOptions) (*llm.Response, error) {
	return &llm.Response{Content: "test"}, nil
}

func (m *mockLLMProvider) Embed(ctx context.Context, texts []string) ([][]float32, error) {
	return nil, nil
}

func (m *mockLLMProvider) Name() string {
	return "mock"
}

func TestArchitect_Name(t *testing.T) {
	a := New()
	if got := a.Name(); got != "architect" {
		t.Errorf("Name() = %q, want %q", got, "architect")
	}
}

func TestArchitect_NilGraph(t *testing.T) {
	a := New()
	ac := &agents.AgentContext{
		Graph:    nil,
		Registry: plugins.NewRegistry(),
		Params:   map[string]string{"target": "java"},
	}

	res, err := a.Run(context.Background(), ac)
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

func TestArchitect_SuccessfulRun(t *testing.T) {
	a := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "TestModule",
				Language: "cobol",
				Functions: []*ir.Function{
					{Name: "Func1", Body: "test body 1"},
					{Name: "Func2", Body: "test body 2"},
				},
			},
		},
	}

	mockTarget := &mockTargetPlugin{
		language: "java",
		scaffoldFiles: []plugins.GeneratedFile{
			{Path: "pom.xml", Content: []byte("pom content")},
		},
		genFiles: []plugins.GeneratedFile{
			{Path: "Main.java", Content: []byte("java code")},
			{Path: "Test.java", Content: []byte("test code")},
		},
	}

	ac := &agents.AgentContext{
		Graph:    graph,
		LLM:      &mockLLMProvider{},
		Registry: newMockRegistry(mockTarget),
		Params:   map[string]string{"target": "java"},
	}

	res, err := a.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if res.Status != agents.StatusSuccess {
		t.Errorf("Status = %v, want %v", res.Status, agents.StatusSuccess)
	}

	// Check metrics
	if res.Metrics.InputItems != 2 {
		t.Errorf("InputItems = %d, want 2", res.Metrics.InputItems)
	}
	if res.Metrics.OutputItems != 3 {
		t.Errorf("OutputItems = %d, want 3", res.Metrics.OutputItems)
	}

	// Check combined files
	if len(res.GeneratedFiles) != 3 {
		t.Errorf("GeneratedFiles count = %d, want 3", len(res.GeneratedFiles))
	}
}

func TestArchitect_ScaffoldAndGenerateFilesCombined(t *testing.T) {
	a := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{Functions: []*ir.Function{{Name: "F1"}}},
		},
	}

	mockTarget := &mockTargetPlugin{
		language: "python",
		scaffoldFiles: []plugins.GeneratedFile{
			{Path: "setup.py", Content: []byte("setup")},
			{Path: "requirements.txt", Content: []byte("reqs")},
		},
		genFiles: []plugins.GeneratedFile{
			{Path: "main.py", Content: []byte("code")},
		},
	}

	ac := &agents.AgentContext{
		Graph:    graph,
		Registry: newMockRegistry(mockTarget),
		Params:   map[string]string{"target": "python"},
	}

	res, err := a.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Verify scaffold files come first, then generated files
	if len(res.GeneratedFiles) != 3 {
		t.Fatalf("GeneratedFiles count = %d, want 3", len(res.GeneratedFiles))
	}
	if res.GeneratedFiles[0].Path != "setup.py" {
		t.Errorf("First file = %q, want %q", res.GeneratedFiles[0].Path, "setup.py")
	}
	if res.GeneratedFiles[2].Path != "main.py" {
		t.Errorf("Last file = %q, want %q", res.GeneratedFiles[2].Path, "main.py")
	}
}

func TestArchitect_RegistryLookupError(t *testing.T) {
	a := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{Functions: []*ir.Function{{Name: "F1"}}},
		},
	}

	// Empty registry - no plugins registered, so lookup will fail
	ac := &agents.AgentContext{
		Graph:    graph,
		Registry: plugins.NewRegistry(),
		Params:   map[string]string{"target": "unknown"},
	}

	res, err := a.Run(context.Background(), ac)
	if err == nil {
		t.Fatal("expected error for registry lookup failure")
	}
	if res.Status != agents.StatusFailed {
		t.Errorf("Status = %v, want %v", res.Status, agents.StatusFailed)
	}
	if len(res.Errors) == 0 {
		t.Error("expected errors in result")
	}
}

func TestArchitect_ScaffoldError(t *testing.T) {
	a := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{Functions: []*ir.Function{{Name: "F1"}}},
		},
	}

	mockTarget := &mockTargetPlugin{
		language:      "java",
		scaffoldError: errors.New("scaffolding failed"),
	}

	ac := &agents.AgentContext{
		Graph:    graph,
		Registry: newMockRegistry(mockTarget),
		Params:   map[string]string{"target": "java"},
	}

	res, err := a.Run(context.Background(), ac)
	if err == nil {
		t.Fatal("expected error for scaffold failure")
	}
	if res.Status != agents.StatusFailed {
		t.Errorf("Status = %v, want %v", res.Status, agents.StatusFailed)
	}
	if len(res.Errors) == 0 {
		t.Error("expected errors in result")
	}
}

func TestArchitect_GenerateError(t *testing.T) {
	a := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{Functions: []*ir.Function{{Name: "F1"}}},
		},
	}

	mockTarget := &mockTargetPlugin{
		language: "java",
		scaffoldFiles: []plugins.GeneratedFile{
			{Path: "pom.xml", Content: []byte("pom")},
		},
		genError: errors.New("generation failed"),
	}

	ac := &agents.AgentContext{
		Graph:    graph,
		Registry: newMockRegistry(mockTarget),
		Params:   map[string]string{"target": "java"},
	}

	res, err := a.Run(context.Background(), ac)
	if err == nil {
		t.Fatal("expected error for generate failure")
	}
	if res.Status != agents.StatusFailed {
		t.Errorf("Status = %v, want %v", res.Status, agents.StatusFailed)
	}
	if len(res.Errors) == 0 {
		t.Error("expected errors in result")
	}
}

func TestArchitect_JudgeFeedbackPassedViaContext(t *testing.T) {
	a := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{Functions: []*ir.Function{{Name: "F1"}}},
		},
	}

	mockTarget := &mockTargetPlugin{
		language:      "java",
		scaffoldFiles: []plugins.GeneratedFile{},
		genFiles:      []plugins.GeneratedFile{},
	}

	ac := &agents.AgentContext{
		Graph:    graph,
		Registry: newMockRegistry(mockTarget),
		Params: map[string]string{
			"target":         "java",
			"judge_feedback": "Fix the null pointer bug",
		},
	}

	_, err := a.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Verify judge feedback was passed to Generate via context
	feedback := JudgeFeedbackFromContext(mockTarget.lastContext)
	if feedback != "Fix the null pointer bug" {
		t.Errorf("JudgeFeedback in context = %q, want %q", feedback, "Fix the null pointer bug")
	}
}

func TestArchitect_MetadataPopulated(t *testing.T) {
	a := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{Functions: []*ir.Function{{Name: "F1"}}},
		},
	}

	mockTarget := &mockTargetPlugin{
		language: "go",
		scaffoldFiles: []plugins.GeneratedFile{
			{Path: "go.mod", Content: []byte("module")},
			{Path: "go.sum", Content: []byte("sum")},
		},
		genFiles: []plugins.GeneratedFile{
			{Path: "main.go", Content: []byte("code")},
		},
	}

	ac := &agents.AgentContext{
		Graph:    graph,
		Registry: newMockRegistry(mockTarget),
		Params:   map[string]string{"target": "go"},
	}

	res, err := a.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Check metadata
	if res.Metadata["target_language"] != "go" {
		t.Errorf("target_language = %q, want %q", res.Metadata["target_language"], "go")
	}
	if res.Metadata["scaffold_files"] != "2" {
		t.Errorf("scaffold_files = %q, want %q", res.Metadata["scaffold_files"], "2")
	}
	if res.Metadata["generated_files"] != "1" {
		t.Errorf("generated_files = %q, want %q", res.Metadata["generated_files"], "1")
	}
}

func TestArchitect_EmptyGraph(t *testing.T) {
	a := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{},
	}

	mockTarget := &mockTargetPlugin{
		language:      "java",
		scaffoldFiles: []plugins.GeneratedFile{{Path: "pom.xml", Content: []byte("pom")}},
		genFiles:      []plugins.GeneratedFile{},
	}

	ac := &agents.AgentContext{
		Graph:    graph,
		Registry: newMockRegistry(mockTarget),
		Params:   map[string]string{"target": "java"},
	}

	res, err := a.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if res.Metrics.InputItems != 0 {
		t.Errorf("InputItems = %d, want 0", res.Metrics.InputItems)
	}
	if res.Status != agents.StatusSuccess {
		t.Errorf("Status = %v, want %v", res.Status, agents.StatusSuccess)
	}
}

func TestArchitect_MultipleFunctionsMetrics(t *testing.T) {
	a := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Functions: []*ir.Function{
					{Name: "F1"},
					{Name: "F2"},
					{Name: "F3"},
				},
			},
			{
				Functions: []*ir.Function{
					{Name: "F4"},
					{Name: "F5"},
				},
			},
		},
	}

	mockTarget := &mockTargetPlugin{
		language:      "java",
		scaffoldFiles: []plugins.GeneratedFile{},
		genFiles: []plugins.GeneratedFile{
			{Path: "F1.java", Content: []byte("code1")},
			{Path: "F2.java", Content: []byte("code2")},
			{Path: "F3.java", Content: []byte("code3")},
			{Path: "F4.java", Content: []byte("code4")},
			{Path: "F5.java", Content: []byte("code5")},
		},
	}

	ac := &agents.AgentContext{
		Graph:    graph,
		Registry: newMockRegistry(mockTarget),
		Params:   map[string]string{"target": "java"},
	}

	res, err := a.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Should count all functions across all modules
	if res.Metrics.InputItems != 5 {
		t.Errorf("InputItems = %d, want 5", res.Metrics.InputItems)
	}
	if res.Metrics.OutputItems != 5 {
		t.Errorf("OutputItems = %d, want 5", res.Metrics.OutputItems)
	}
}

func TestArchitect_NoJudgeFeedback(t *testing.T) {
	a := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{Functions: []*ir.Function{{Name: "F1"}}},
		},
	}

	mockTarget := &mockTargetPlugin{
		language:      "java",
		scaffoldFiles: []plugins.GeneratedFile{},
		genFiles:      []plugins.GeneratedFile{},
	}

	ac := &agents.AgentContext{
		Graph:    graph,
		Registry: newMockRegistry(mockTarget),
		Params:   map[string]string{"target": "java"},
	}

	_, err := a.Run(context.Background(), ac)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Verify no feedback in context when not provided
	feedback := JudgeFeedbackFromContext(mockTarget.lastContext)
	if feedback != "" {
		t.Errorf("JudgeFeedback in context = %q, want empty", feedback)
	}
}
