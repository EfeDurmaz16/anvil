package metrics

import (
	"bytes"
	"encoding/json"
	"strings"
	"testing"
	"time"

	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

func TestNew_InitializesStartedAt(t *testing.T) {
	before := time.Now()
	m := New()
	after := time.Now()

	if m.StartedAt.Before(before) || m.StartedAt.After(after) {
		t.Errorf("StartedAt %v not between %v and %v", m.StartedAt, before, after)
	}
}

func TestNew_InitialState(t *testing.T) {
	m := New()

	if m.Score != 0 {
		t.Errorf("expected initial Score 0, got %f", m.Score)
	}
	if len(m.Agents) != 0 {
		t.Errorf("expected empty Agents, got %d", len(m.Agents))
	}
	if len(m.Errors) != 0 {
		t.Errorf("expected empty Errors, got %d", len(m.Errors))
	}
	if !m.FinishedAt.IsZero() {
		t.Error("expected zero FinishedAt before Finish()")
	}
}

func TestAddAgent_RecordsMetrics(t *testing.T) {
	m := New()
	m.AddAgent("cartographer", 2*time.Second, "llm", 0)

	if len(m.Agents) != 1 {
		t.Fatalf("expected 1 agent, got %d", len(m.Agents))
	}
	a := m.Agents[0]
	if a.Name != "cartographer" {
		t.Errorf("expected name 'cartographer', got %q", a.Name)
	}
	if a.Duration != 2*time.Second {
		t.Errorf("expected duration 2s, got %v", a.Duration)
	}
	if a.Mode != "llm" {
		t.Errorf("expected mode 'llm', got %q", a.Mode)
	}
	if a.Errors != 0 {
		t.Errorf("expected 0 errors, got %d", a.Errors)
	}
}

func TestAddAgent_MultipleAgents(t *testing.T) {
	m := New()
	m.AddAgent("cartographer", 1*time.Second, "llm", 0)
	m.AddAgent("specular", 2*time.Second, "passthrough", 1)
	m.AddAgent("architect", 3*time.Second, "llm", 0)

	if len(m.Agents) != 3 {
		t.Fatalf("expected 3 agents, got %d", len(m.Agents))
	}
	if m.Agents[1].Name != "specular" {
		t.Errorf("expected second agent 'specular', got %q", m.Agents[1].Name)
	}
	if m.Agents[1].Errors != 1 {
		t.Errorf("expected 1 error for specular, got %d", m.Agents[1].Errors)
	}
}

func TestFinish_SetsFields(t *testing.T) {
	m := New()
	time.Sleep(1 * time.Millisecond) // ensure some duration

	errs := []string{"something went wrong"}
	m.Finish(0.85, errs)

	if m.Score != 0.85 {
		t.Errorf("expected score 0.85, got %f", m.Score)
	}
	if m.FinishedAt.IsZero() {
		t.Error("expected non-zero FinishedAt after Finish()")
	}
	if m.Duration <= 0 {
		t.Error("expected positive Duration after Finish()")
	}
	if len(m.Errors) != 1 || m.Errors[0] != "something went wrong" {
		t.Errorf("expected errors ['something went wrong'], got %v", m.Errors)
	}
}

func TestFinish_DurationIsPositive(t *testing.T) {
	m := New()
	time.Sleep(1 * time.Millisecond)
	m.Finish(1.0, nil)

	if m.Duration <= 0 {
		t.Errorf("expected positive duration, got %v", m.Duration)
	}
	if m.FinishedAt.Before(m.StartedAt) {
		t.Error("FinishedAt should not be before StartedAt")
	}
}

func TestCollectSource_SetsFields(t *testing.T) {
	m := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Functions:   []*ir.Function{{}, {}},
				DataTypes:   []*ir.DataType{{}},
				IOContracts: []*ir.IOContract{{}},
			},
		},
		DataTypes:     []*ir.DataType{{}, {}},
		BusinessRules: []*ir.BusinessRule{{}, {}, {}},
	}

	m.CollectSource("COBOL", 5, graph)

	if m.Source.Language != "COBOL" {
		t.Errorf("expected language 'COBOL', got %q", m.Source.Language)
	}
	if m.Source.FileCount != 5 {
		t.Errorf("expected file count 5, got %d", m.Source.FileCount)
	}
	if m.Source.ModuleCount != 1 {
		t.Errorf("expected module count 1, got %d", m.Source.ModuleCount)
	}
	if m.Source.FunctionCount != 2 {
		t.Errorf("expected function count 2, got %d", m.Source.FunctionCount)
	}
	if m.Source.RuleCount != 3 {
		t.Errorf("expected rule count 3, got %d", m.Source.RuleCount)
	}
	if m.Source.IOContractCount != 1 {
		t.Errorf("expected io contract count 1, got %d", m.Source.IOContractCount)
	}
}

func TestCollectTarget_SetsFields(t *testing.T) {
	m := New()
	files := []plugins.GeneratedFile{
		{Path: "foo.ts", Content: []byte("hello world")},
		{Path: "bar.ts", Content: []byte("goodbye")},
	}

	m.CollectTarget("TypeScript", files)

	if m.Target.Language != "TypeScript" {
		t.Errorf("expected language 'TypeScript', got %q", m.Target.Language)
	}
	if m.Target.FilesGenerated != 2 {
		t.Errorf("expected 2 files generated, got %d", m.Target.FilesGenerated)
	}
	expectedBytes := len("hello world") + len("goodbye")
	if m.Target.TotalBytes != expectedBytes {
		t.Errorf("expected total bytes %d, got %d", expectedBytes, m.Target.TotalBytes)
	}
}

func TestCollectTarget_EmptyFiles(t *testing.T) {
	m := New()
	m.CollectTarget("Go", []plugins.GeneratedFile{})

	if m.Target.FilesGenerated != 0 {
		t.Errorf("expected 0 files, got %d", m.Target.FilesGenerated)
	}
	if m.Target.TotalBytes != 0 {
		t.Errorf("expected 0 bytes, got %d", m.Target.TotalBytes)
	}
}

func TestJSON_ReturnsValidJSON(t *testing.T) {
	m := New()
	m.LLMMode = "llm"
	m.AddAgent("cartographer", 1*time.Second, "llm", 0)
	m.Finish(0.9, nil)

	data, err := m.JSON()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	var decoded PipelineMetrics
	if err := json.Unmarshal(data, &decoded); err != nil {
		t.Fatalf("failed to unmarshal JSON: %v", err)
	}
	if decoded.Score != 0.9 {
		t.Errorf("expected score 0.9, got %f", decoded.Score)
	}
	if decoded.LLMMode != "llm" {
		t.Errorf("expected llm_mode 'llm', got %q", decoded.LLMMode)
	}
}

func TestJSON_IncludesAgents(t *testing.T) {
	m := New()
	m.AddAgent("cartographer", 500*time.Millisecond, "passthrough", 0)
	m.AddAgent("architect", 1500*time.Millisecond, "llm", 2)
	m.Finish(0.5, []string{"err1"})

	data, err := m.JSON()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	var raw map[string]any
	if err := json.Unmarshal(data, &raw); err != nil {
		t.Fatalf("failed to unmarshal JSON: %v", err)
	}

	agents, ok := raw["agents"].([]interface{})
	if !ok {
		t.Fatal("expected 'agents' array in JSON")
	}
	if len(agents) != 2 {
		t.Errorf("expected 2 agents in JSON, got %d", len(agents))
	}

	errors, ok := raw["errors"].([]interface{})
	if !ok {
		t.Fatal("expected 'errors' array in JSON")
	}
	if len(errors) != 1 || errors[0] != "err1" {
		t.Errorf("expected errors ['err1'], got %v", errors)
	}
}

func TestPrintSummary_ContainsKeyFields(t *testing.T) {
	m := New()
	m.LLMMode = "llm"
	m.AddAgent("cartographer", 1*time.Second, "llm", 0)
	m.AddAgent("specular", 2*time.Second, "llm", 1)
	m.Finish(0.75, []string{"some error"})
	m.Source.Language = "COBOL"
	m.Source.FileCount = 3
	m.Target.Language = "TypeScript"
	m.Target.FilesGenerated = 5

	var buf bytes.Buffer
	m.PrintSummary(&buf)
	output := buf.String()

	checks := []string{
		"llm",
		"COBOL",
		"TypeScript",
		"cartographer",
		"specular",
		"some error",
	}
	for _, check := range checks {
		if !strings.Contains(output, check) {
			t.Errorf("expected output to contain %q, but it did not.\nOutput:\n%s", check, output)
		}
	}
}

func TestPrintSummary_ShowsErrorCount(t *testing.T) {
	m := New()
	m.AddAgent("judge", 500*time.Millisecond, "llm", 3)
	m.Finish(0.0, nil)

	var buf bytes.Buffer
	m.PrintSummary(&buf)
	output := buf.String()

	if !strings.Contains(output, "3 errors") {
		t.Errorf("expected '3 errors' in output, got:\n%s", output)
	}
}

func TestFormatBytes(t *testing.T) {
	tests := []struct {
		input    int
		expected string
	}{
		{500, "500 B"},
		{1024, "1.0 KB"},
		{1536, "1.5 KB"},
		{1048576, "1.0 MB"},
		{2097152, "2.0 MB"},
	}

	for _, tc := range tests {
		got := formatBytes(tc.input)
		if got != tc.expected {
			t.Errorf("formatBytes(%d): expected %q, got %q", tc.input, tc.expected, got)
		}
	}
}
