package testgen

import (
	"context"
	"strings"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// mockLLMProvider for testing LLM-based test generation
type mockLLMProvider struct {
	content string
	err     error
}

func (m *mockLLMProvider) Complete(_ context.Context, _ *llm.Prompt, _ *llm.RequestOptions) (*llm.Response, error) {
	if m.err != nil {
		return nil, m.err
	}
	return &llm.Response{Content: m.content}, nil
}

func (m *mockLLMProvider) Embed(_ context.Context, _ []string) ([][]float32, error) {
	return nil, nil
}

func (m *mockLLMProvider) Name() string {
	return "mock"
}

// TestName verifies the agent name
func TestName(t *testing.T) {
	tg := New()
	if got := tg.Name(); got != "testgen" {
		t.Errorf("Name() = %q, want %q", got, "testgen")
	}
}

// TestRunNilGraph verifies error handling with nil graph
func TestRunNilGraph(t *testing.T) {
	tg := New()
	ctx := context.Background()
	ac := &agents.AgentContext{
		Graph: nil,
		Params: map[string]string{
			"target":          "typescript",
			"generated_files": "[]",
		},
	}

	result, err := tg.Run(ctx, ac)
	if err == nil {
		t.Error("Run() with nil graph should return error")
	}
	if result.Status != agents.StatusFailed {
		t.Errorf("Status = %v, want %v", result.Status, agents.StatusFailed)
	}
}

// TestRunNoGeneratedFilesParam verifies error handling with missing generated_files param
func TestRunNoGeneratedFilesParam(t *testing.T) {
	tg := New()
	ctx := context.Background()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name:     "TEST",
			Language: "cobol",
			Functions: []*ir.Function{
				{Name: "TEST-FN"},
			},
		}},
		CallGraph: &ir.CallGraph{},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		Params: map[string]string{
			"target": "typescript",
			// missing "generated_files"
		},
	}

	result, err := tg.Run(ctx, ac)
	if err == nil {
		t.Error("Run() without generated_files param should return error")
	}
	if result.Status != agents.StatusFailed {
		t.Errorf("Status = %v, want %v", result.Status, agents.StatusFailed)
	}
}

// TestStubTestContentTypeScript verifies TypeScript stub content
func TestStubTestContentTypeScript(t *testing.T) {
	tg := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name:     "PAYMENT",
			Path:     "payment.cbl",
			Language: "cobol",
			Functions: []*ir.Function{
				{Name: "PROCESS-PAYMENT"},
			},
		}},
		CallGraph: &ir.CallGraph{},
	}

	result, err := tg.Run(context.Background(), &agents.AgentContext{
		Graph:    graph,
		Registry: plugins.NewRegistry(),
		Params: map[string]string{
			"target":          "typescript",
			"generated_files": "[]",
		},
	})

	if err != nil {
		t.Fatal(err)
	}

	if len(result.GeneratedFiles) == 0 {
		t.Fatal("expected test files to be generated")
	}

	content := string(result.GeneratedFiles[0].Content)

	// Check required TypeScript test structures
	requiredParts := []string{
		"describe(",
		"it(",
		"beforeEach(",
		"expect(",
	}

	for _, part := range requiredParts {
		if !strings.Contains(content, part) {
			t.Errorf("TypeScript stub missing %q", part)
		}
	}
}

// TestStubTestContentPython verifies Python stub content
func TestStubTestContentPython(t *testing.T) {
	tg := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name:     "INVENTORY",
			Language: "cobol",
			Functions: []*ir.Function{
				{Name: "CHECK-STOCK"},
			},
		}},
		CallGraph: &ir.CallGraph{},
	}

	result, err := tg.Run(context.Background(), &agents.AgentContext{
		Graph:    graph,
		Registry: plugins.NewRegistry(),
		Params: map[string]string{
			"target":          "python",
			"generated_files": "[]",
		},
	})

	if err != nil {
		t.Fatal(err)
	}

	content := string(result.GeneratedFiles[0].Content)

	// Check required Python test structures
	requiredParts := []string{
		"import pytest",
		"class Test",
		"def setup_method",
		"def test_",
	}

	for _, part := range requiredParts {
		if !strings.Contains(content, part) {
			t.Errorf("Python stub missing %q", part)
		}
	}
}

// TestStubTestContentJava verifies Java stub content
func TestStubTestContentJava(t *testing.T) {
	tg := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name:     "ORDER",
			Language: "cobol",
			Functions: []*ir.Function{
				{Name: "VALIDATE-ORDER"},
			},
		}},
		CallGraph: &ir.CallGraph{},
	}

	result, err := tg.Run(context.Background(), &agents.AgentContext{
		Graph:    graph,
		Registry: plugins.NewRegistry(),
		Params: map[string]string{
			"target":          "java",
			"generated_files": "[]",
		},
	})

	if err != nil {
		t.Fatal(err)
	}

	content := string(result.GeneratedFiles[0].Content)

	// Check required Java test structures
	requiredParts := []string{
		"@Test",
		"@BeforeEach",
		"import org.junit.jupiter",
	}

	for _, part := range requiredParts {
		if !strings.Contains(content, part) {
			t.Errorf("Java stub missing %q", part)
		}
	}
}

// TestStubTestContentGo verifies Go stub content
func TestStubTestContentGo(t *testing.T) {
	tg := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name:     "ACCOUNT",
			Language: "cobol",
			Functions: []*ir.Function{
				{Name: "CREATE-ACCOUNT"},
			},
		}},
		CallGraph: &ir.CallGraph{},
	}

	result, err := tg.Run(context.Background(), &agents.AgentContext{
		Graph:    graph,
		Registry: plugins.NewRegistry(),
		Params: map[string]string{
			"target":          "go",
			"generated_files": "[]",
		},
	})

	if err != nil {
		t.Fatal(err)
	}

	content := string(result.GeneratedFiles[0].Content)

	// Check required Go test structures
	requiredParts := []string{
		"import \"testing\"",
		"func Test",
		"*testing.T",
	}

	for _, part := range requiredParts {
		if !strings.Contains(content, part) {
			t.Errorf("Go stub missing %q", part)
		}
	}
}

// TestTestFilePathTypeScript verifies TypeScript path generation
func TestTestFilePathTypeScript(t *testing.T) {
	path := testFilePath("USER-SERVICE", "", "typescript")
	expected := "src/tests/user-service.test.ts"
	if path != expected {
		t.Errorf("testFilePath() = %q, want %q", path, expected)
	}
}

// TestTestFilePathPython verifies Python path generation
func TestTestFilePathPython(t *testing.T) {
	path := testFilePath("DATA-PROCESSOR", "", "python")
	expected := "tests/test_data_processor.py"
	if path != expected {
		t.Errorf("testFilePath() = %q, want %q", path, expected)
	}
}

// TestTestFilePathJava verifies Java path generation
func TestTestFilePathJava(t *testing.T) {
	path := testFilePath("REPORT-GEN", "", "java")
	expected := "src/test/java/generated/ReportGenServiceTest.java"
	if path != expected {
		t.Errorf("testFilePath() = %q, want %q", path, expected)
	}
}

// TestTestFilePathGo verifies Go path generation
func TestTestFilePathGo(t *testing.T) {
	path := testFilePath("AUTH-MODULE", "", "go")
	expected := "generated/auth_module_test.go"
	if path != expected {
		t.Errorf("testFilePath() = %q, want %q", path, expected)
	}
}

// TestToPascalCaseEdgeCases verifies edge cases in toPascalCase
func TestToPascalCaseEdgeCases(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"", "Generated"},
		{"simple", "Simple"},
		{"WITH-DASHES", "WithDashes"},
		{"with_underscores", "WithUnderscores"},
		{"with spaces", "WithSpaces"},
		{"with.dots", "WithDots"},
		{"mixed-case_STYLES.here", "MixedCaseStylesHere"},
		{"---multiple---dashes---", "MultipleDashes"},
	}

	for _, tt := range tests {
		got := toPascalCase(tt.input)
		if got != tt.expected {
			t.Errorf("toPascalCase(%q) = %q, want %q", tt.input, got, tt.expected)
		}
	}
}

// TestToCamelCaseEdgeCases verifies edge cases in toCamelCase
func TestToCamelCaseEdgeCases(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"", "generated"},
		{"SIMPLE", "simple"},
		{"PROCESS-PAYMENT", "processPayment"},
		{"GET_USER_DATA", "getUserData"},
		{"handle.request", "handleRequest"},
	}

	for _, tt := range tests {
		got := toCamelCase(tt.input)
		if got != tt.expected {
			t.Errorf("toCamelCase(%q) = %q, want %q", tt.input, got, tt.expected)
		}
	}
}

// TestToKebabCaseEdgeCases verifies edge cases in toKebabCase
func TestToKebabCaseEdgeCases(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"", "generated"},
		{"SIMPLE", "simple"},
		{"USER_SERVICE", "user-service"},
		{"Data.Processor", "data-processor"},
		{"mixed CASE_styles.here", "mixed-case-styles-here"},
		{"---leading-trailing---", "leading-trailing"},
		{"double--dash", "double-dash"},
		{"   spaces   ", "spaces"},
	}

	for _, tt := range tests {
		got := toKebabCase(tt.input)
		if got != tt.expected {
			t.Errorf("toKebabCase(%q) = %q, want %q", tt.input, got, tt.expected)
		}
	}
}

// TestToSnakeCaseEdgeCases verifies edge cases in toSnakeCase
func TestToSnakeCaseEdgeCases(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"", "generated"},
		{"SIMPLE", "simple"},
		{"USER-SERVICE", "user_service"},
		{"Data.Processor", "data_processor"},
		{"___leading_trailing___", "leading_trailing"},
		{"double__underscore", "double_underscore"},
		{"   spaces   ", "spaces"},
	}

	for _, tt := range tests {
		got := toSnakeCase(tt.input)
		if got != tt.expected {
			t.Errorf("toSnakeCase(%q) = %q, want %q", tt.input, got, tt.expected)
		}
	}
}

// TestGenerateTestFileWithMockLLM verifies LLM-based test generation
func TestGenerateTestFileWithMockLLM(t *testing.T) {
	mod := &ir.Module{
		Name:     "CALCULATOR",
		Language: "cobol",
	}
	fn := &ir.Function{
		Name: "ADD-NUMBERS",
		Body: "COMPUTE RESULT = A + B.",
	}

	mockLLM := &mockLLMProvider{
		content: "import { test } from 'framework';\n\ntest('adds numbers', () => {\n  expect(true).toBe(true);\n});",
	}

	testFile := generateTestFile(context.Background(), mod, fn, "typescript", mockLLM)

	if testFile == nil {
		t.Fatal("generateTestFile() returned nil")
	}

	if !strings.Contains(string(testFile.Content), "test(") {
		t.Error("generated test content missing test framework code")
	}
}

// TestGenerateTestFileWithNilLLM verifies nil LLM returns nil
func TestGenerateTestFileWithNilLLM(t *testing.T) {
	mod := &ir.Module{
		Name:     "CALCULATOR",
		Language: "cobol",
	}
	fn := &ir.Function{
		Name: "ADD-NUMBERS",
	}

	testFile := generateTestFile(context.Background(), mod, fn, "typescript", nil)

	if testFile != nil {
		t.Error("generateTestFile() with nil LLM should return nil")
	}
}

// TestMetricsPopulated verifies InputItems and OutputItems are set
func TestMetricsPopulated(t *testing.T) {
	tg := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "MODULE_A",
				Language: "cobol",
				Functions: []*ir.Function{
					{Name: "FN-1"},
					{Name: "FN-2"},
				},
			},
			{
				Name:     "MODULE_B",
				Language: "cobol",
				Functions: []*ir.Function{
					{Name: "FN-3"},
				},
			},
		},
		CallGraph: &ir.CallGraph{},
	}

	result, err := tg.Run(context.Background(), &agents.AgentContext{
		Graph:    graph,
		Registry: plugins.NewRegistry(),
		Params: map[string]string{
			"target":          "go",
			"generated_files": "[]",
		},
	})

	if err != nil {
		t.Fatal(err)
	}

	// Should have 3 functions as input
	if result.Metrics.InputItems != 3 {
		t.Errorf("InputItems = %d, want 3", result.Metrics.InputItems)
	}

	// Should have 2 test files as output (one per module)
	if result.Metrics.OutputItems != 2 {
		t.Errorf("OutputItems = %d, want 2", result.Metrics.OutputItems)
	}
}
