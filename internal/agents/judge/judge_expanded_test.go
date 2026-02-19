package judge

import (
	"context"
	"encoding/json"
	"strings"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// TestName verifies the agent name
func TestName(t *testing.T) {
	j := New()
	if got := j.Name(); got != "judge" {
		t.Errorf("Name() = %q, want %q", got, "judge")
	}
}

// TestRunNilGraph verifies error handling with nil graph
func TestRunNilGraph(t *testing.T) {
	j := New()
	ctx := context.Background()
	ac := &agents.AgentContext{
		Graph: nil,
		Params: map[string]string{
			"generated_code": "some code",
		},
	}

	result, err := j.Run(ctx, ac)
	if err == nil {
		t.Error("Run() with nil graph should return error")
	}
	if result.Status != agents.StatusFailed {
		t.Errorf("Status = %v, want %v", result.Status, agents.StatusFailed)
	}
}

// TestRunNilLLMPassthrough verifies passthrough mode when no LLM
func TestRunNilLLMPassthrough(t *testing.T) {
	j := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name: "TEST",
			Functions: []*ir.Function{{
				Name: "TEST-FN",
				Body: "MOVE A TO B.",
			}},
		}},
		CallGraph: &ir.CallGraph{},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   nil, // No LLM
		Params: map[string]string{
			"generated_code": "public class Test {}",
		},
	}

	result, err := j.Run(context.Background(), ac)
	if err != nil {
		t.Fatal(err)
	}

	if result.Score != 1.0 {
		t.Errorf("Score = %f, want 1.0 in passthrough mode", result.Score)
	}

	if result.Status != agents.StatusPassthrough {
		t.Errorf("Status = %v, want %v", result.Status, agents.StatusPassthrough)
	}
}

// TestRunAllFunctionsVerified verifies Score=1.0 when all pass
func TestRunAllFunctionsVerified(t *testing.T) {
	j := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name: "M1",
			Functions: []*ir.Function{
				{Name: "F1", Body: "MOVE A TO B."},
				{Name: "F2", Body: "ADD 1 TO X."},
			},
		}},
		CallGraph: &ir.CallGraph{},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   &mockProvider{name: "mock", content: `{"equivalent": true, "reason": "all good"}`},
		Params: map[string]string{
			"generated_code": "class M1 { f1() {} f2() {} }",
		},
	}

	result, err := j.Run(context.Background(), ac)
	if err != nil {
		t.Fatal(err)
	}

	if result.Score != 1.0 {
		t.Errorf("Score = %f, want 1.0", result.Score)
	}

	if result.Status != agents.StatusSuccess {
		t.Errorf("Status = %v, want %v", result.Status, agents.StatusSuccess)
	}
}

// TestRunMixedResults verifies partial score with mixed results
func TestRunMixedResults(t *testing.T) {
	j := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name: "M",
			Functions: []*ir.Function{
				{Name: "GOOD", Body: "MOVE A TO B."},
				{Name: "BAD", Body: "COMPUTE X."},
			},
		}},
		CallGraph: &ir.CallGraph{},
	}

	callCount := 0
	mockLLM := &mockProviderMulti{
		responses: []string{
			`{"equivalent": true, "reason": "good"}`,
			`{"equivalent": false, "reason": "bad"}`,
		},
		callCount: &callCount,
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   mockLLM,
		Params: map[string]string{
			"generated_code": "class M {}",
		},
	}

	result, err := j.Run(context.Background(), ac)
	if err != nil {
		t.Fatal(err)
	}

	// 1 out of 2 functions verified = 0.5 score
	if result.Score != 0.5 {
		t.Errorf("Score = %f, want 0.5", result.Score)
	}

	if result.Status != agents.StatusPartial {
		t.Errorf("Status = %v, want %v", result.Status, agents.StatusPartial)
	}
}

// mockProviderMulti provides different responses for each call
type mockProviderMulti struct {
	responses []string
	callCount *int
}

func (m *mockProviderMulti) Complete(_ context.Context, _ *llm.Prompt, _ *llm.RequestOptions) (*llm.Response, error) {
	idx := *m.callCount
	*m.callCount++
	if idx < len(m.responses) {
		return &llm.Response{Content: m.responses[idx]}, nil
	}
	return &llm.Response{Content: m.responses[len(m.responses)-1]}, nil
}

func (m *mockProviderMulti) Embed(_ context.Context, _ []string) ([][]float32, error) {
	return nil, nil
}

func (m *mockProviderMulti) Name() string {
	return "mockMulti"
}

// TestRunAllFailures verifies Score=0.0 when all fail
func TestRunAllFailures(t *testing.T) {
	j := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name: "M",
			Functions: []*ir.Function{
				{Name: "F1", Body: "MOVE A TO B."},
				{Name: "F2", Body: "ADD 1 TO X."},
			},
		}},
		CallGraph: &ir.CallGraph{},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   &mockProvider{name: "mock", content: `{"equivalent": false, "reason": "wrong"}`},
		Params: map[string]string{
			"generated_code": "class M {}",
		},
	}

	result, err := j.Run(context.Background(), ac)
	if err != nil {
		t.Fatal(err)
	}

	if result.Score != 0.0 {
		t.Errorf("Score = %f, want 0.0", result.Score)
	}

	if result.Status != agents.StatusFailed {
		t.Errorf("Status = %v, want %v", result.Status, agents.StatusFailed)
	}
}

// TestRunEmptyGeneratedCode verifies handling of empty generated_code
func TestRunEmptyGeneratedCode(t *testing.T) {
	j := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name: "M",
			Functions: []*ir.Function{{
				Name: "F",
				Body: "MOVE A TO B.",
			}},
		}},
		CallGraph: &ir.CallGraph{},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   &mockProvider{name: "mock", content: `{"equivalent": true, "reason": "ok"}`},
		Params: map[string]string{
			"generated_code": "",
		},
	}

	result, err := j.Run(context.Background(), ac)
	if err != nil {
		t.Fatal(err)
	}

	// Should still run but add warning
	if len(result.Warnings) == 0 {
		t.Error("expected warning about empty generated code")
	}
}

// TestRunEmptyGeneratedFiles verifies handling of empty generated_files
func TestRunEmptyGeneratedFiles(t *testing.T) {
	j := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name: "M",
			Functions: []*ir.Function{{
				Name: "F",
				Body: "MOVE A TO B.",
			}},
		}},
		CallGraph: &ir.CallGraph{},
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   &mockProvider{name: "mock", content: `{"equivalent": true, "reason": "ok"}`},
		Params: map[string]string{
			"generated_files": "",
		},
	}

	result, err := j.Run(context.Background(), ac)
	if err != nil {
		t.Fatal(err)
	}

	// Should still run but add warning
	if len(result.Warnings) == 0 {
		t.Error("expected warning about empty generated code")
	}
}

// TestVerifyFunctionCleanJSON verifies parsing of clean JSON response
func TestVerifyFunctionCleanJSON(t *testing.T) {
	mockLLM := &mockProvider{
		name:    "mock",
		content: `{"equivalent": true, "reason": "Logic preserved"}`,
	}

	ok, reason, err := verifyFunction(context.Background(), mockLLM, nil, "cobol", "java", "M", "F", "MOVE A TO B.", "a = b;")

	if err != nil {
		t.Fatal(err)
	}

	if !ok {
		t.Error("expected ok=true")
	}

	if reason != "Logic preserved" {
		t.Errorf("reason = %q, want %q", reason, "Logic preserved")
	}
}

// TestParseVerdictValidJSON verifies parsing of valid JSON
func TestParseVerdictValidJSON(t *testing.T) {
	text := `{"equivalent": true, "reason": "All good"}`

	v, err := parseVerdict(text)
	if err != nil {
		t.Fatal(err)
	}

	if !v.Equivalent {
		t.Error("expected Equivalent=true")
	}

	if v.Reason != "All good" {
		t.Errorf("Reason = %q, want %q", v.Reason, "All good")
	}
}

// TestParseVerdictJSONInText verifies extraction of JSON from surrounding text
func TestParseVerdictJSONInText(t *testing.T) {
	text := `Here is my analysis:
{"equivalent": false, "reason": "Missing error handling"}
Hope this helps!`

	v, err := parseVerdict(text)
	if err != nil {
		t.Fatal(err)
	}

	if v.Equivalent {
		t.Error("expected Equivalent=false")
	}

	if v.Reason != "Missing error handling" {
		t.Errorf("Reason = %q, want %q", v.Reason, "Missing error handling")
	}
}

// TestParseVerdictHeuristicPositive verifies heuristic fallback with positive signals
func TestParseVerdictHeuristicPositive(t *testing.T) {
	text := "The code is equivalent and semantically correct. Logic is preserved."

	v, err := parseVerdict(text)
	if err != nil {
		t.Fatal(err)
	}

	if !v.Equivalent {
		t.Error("expected Equivalent=true from positive signals")
	}
}

// TestParseVerdictHeuristicNegative verifies heuristic fallback with negative signals
func TestParseVerdictHeuristicNegative(t *testing.T) {
	text := "The code is not equivalent. Missing critical logic and incorrect implementation."

	v, err := parseVerdict(text)
	if err != nil {
		t.Fatal(err)
	}

	if v.Equivalent {
		t.Error("expected Equivalent=false from negative signals")
	}
}

// TestParseVerdictUnparseable verifies handling of completely unparseable text
func TestParseVerdictUnparseable(t *testing.T) {
	text := "I don't understand the question."

	v, err := parseVerdict(text)
	if err != nil {
		t.Fatal(err)
	}

	// Should return a verdict even if unparseable
	if v.Equivalent {
		t.Error("expected Equivalent=false for unparseable text")
	}

	if !strings.Contains(v.Reason, "could not parse verdict") {
		t.Errorf("expected 'could not parse verdict' in reason, got %q", v.Reason)
	}
}

// TestExtractFunctionSnippet verifies function extraction by name
func TestExtractFunctionSnippet(t *testing.T) {
	generatedText := `
class Calculator {
  /**
   * Adds two numbers
   */
  addNumbers(a: number, b: number): number {
    return a + b;
  }

  subtractNumbers(a: number, b: number): number {
    return a - b;
  }
}
`

	snippet := extractFunctionSnippet(generatedText, "ADD-NUMBERS")

	if !strings.Contains(snippet, "addNumbers") {
		t.Error("snippet should contain addNumbers")
	}

	if strings.Contains(snippet, "subtractNumbers") {
		t.Error("snippet should not contain subtractNumbers")
	}

	if !strings.Contains(snippet, "return a + b") {
		t.Error("snippet should contain function body")
	}
}

// TestExtractFunctionSnippetAsync verifies extraction of async functions
func TestExtractFunctionSnippetAsync(t *testing.T) {
	generatedText := `
class Service {
  async processData(data: any): Promise<void> {
    await this.validate(data);
    return;
  }
}
`

	snippet := extractFunctionSnippet(generatedText, "PROCESS-DATA")

	if !strings.Contains(snippet, "async processData") {
		t.Error("snippet should contain async processData")
	}

	if !strings.Contains(snippet, "await this.validate") {
		t.Error("snippet should contain function body")
	}
}

// TestExtractFunctionSnippetFallback verifies fallback to truncated full text
func TestExtractFunctionSnippetFallback(t *testing.T) {
	generatedText := strings.Repeat("x", 3000)

	snippet := extractFunctionSnippet(generatedText, "NONEXISTENT")

	// Should return truncated text
	if len(snippet) > 2100 {
		t.Errorf("snippet length = %d, expected <= 2100", len(snippet))
	}

	if !strings.Contains(snippet, "truncated") {
		t.Error("snippet should contain truncation marker")
	}
}

// TestToCamelCaseConversion verifies toCamelCase conversion
func TestToCamelCaseConversion(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"PROCESS-PAYMENT", "processPayment"},
		{"GET_USER_DATA", "getUserData"},
		{"simple", "simple"},
		{"MULTI-WORD_mixed.INPUT", "multiWordMixedInput"},
	}

	for _, tt := range tests {
		got := toCamelCase(tt.input)
		if got != tt.expected {
			t.Errorf("toCamelCase(%q) = %q, want %q", tt.input, got, tt.expected)
		}
	}
}

// TestTruncateFunction verifies truncate function
func TestTruncateFunction(t *testing.T) {
	tests := []struct {
		input    string
		n        int
		expected string
	}{
		{"hello world", 5, "hello…"},
		{"short", 10, "short"},
		{"exact", 5, "exact"},
		{"", 5, ""},
		{"test", 0, "test"},
		{"negative", -1, "negative"},
	}

	for _, tt := range tests {
		got := truncate(tt.input, tt.n)
		if got != tt.expected {
			t.Errorf("truncate(%q, %d) = %q, want %q", tt.input, tt.n, got, tt.expected)
		}
	}
}

// TestFormatGeneratedInputJSONArray verifies JSON array parsing
func TestFormatGeneratedInputJSONArray(t *testing.T) {
	files := []plugins.GeneratedFile{
		{Path: "test1.ts", Content: []byte("content1")},
		{Path: "test2.ts", Content: []byte("content2")},
	}

	jsonBytes, _ := json.Marshal(files)
	result := formatGeneratedInput(string(jsonBytes), 0)

	if !strings.Contains(result, "test1.ts") {
		t.Error("result should contain test1.ts")
	}

	if !strings.Contains(result, "content1") {
		t.Error("result should contain content1")
	}

	if !strings.Contains(result, "test2.ts") {
		t.Error("result should contain test2.ts")
	}
}

// TestFormatGeneratedInputPlainText verifies plain text handling
func TestFormatGeneratedInputPlainText(t *testing.T) {
	input := "public class Test { }"

	result := formatGeneratedInput(input, 0)

	if result != input {
		t.Errorf("formatGeneratedInput() = %q, want %q", result, input)
	}
}

// TestFormatGeneratedInputTruncation verifies maxBytes truncation
func TestFormatGeneratedInputTruncation(t *testing.T) {
	input := strings.Repeat("x", 1000)

	result := formatGeneratedInput(input, 100)

	if len(result) > 120 {
		t.Errorf("result length = %d, expected <= 120", len(result))
	}

	if !strings.Contains(result, "truncated") {
		t.Error("result should contain truncation marker")
	}
}

// TestConcurrentExecution verifies concurrent function verification
func TestConcurrentExecution(t *testing.T) {
	j := New()

	// Create graph with multiple functions
	var functions []*ir.Function
	for i := 0; i < 5; i++ {
		functions = append(functions, &ir.Function{
			Name: "FN" + string(rune('A'+i)),
			Body: "MOVE A TO B.",
		})
	}

	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name:      "M",
			Functions: functions,
		}},
		CallGraph: &ir.CallGraph{},
	}

	mockLLM := &mockProvider{
		name:    "mock",
		content: `{"equivalent": true, "reason": "ok"}`,
	}

	ac := &agents.AgentContext{
		Graph: graph,
		LLM:   mockLLM,
		Params: map[string]string{
			"generated_code": "class M {}",
			"max_concurrent": "3", // Allow 3 concurrent verifications
		},
	}

	result, err := j.Run(context.Background(), ac)
	if err != nil {
		t.Fatal(err)
	}

	// All 5 functions should be verified
	if result.Metrics.OutputItems != 5 {
		t.Errorf("OutputItems = %d, want 5", result.Metrics.OutputItems)
	}

	// Should have made 5 LLM calls
	if result.Metrics.LLMCalls != 5 {
		t.Errorf("LLMCalls = %d, want 5", result.Metrics.LLMCalls)
	}

	if result.Score != 1.0 {
		t.Errorf("Score = %f, want 1.0", result.Score)
	}
}
