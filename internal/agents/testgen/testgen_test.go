package testgen

import (
	"context"
	"strings"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

func TestTestGenName(t *testing.T) {
	tg := New()
	if tg.Name() != "testgen" {
		t.Errorf("expected testgen, got %s", tg.Name())
	}
}

func TestTestGenStubTypeScript(t *testing.T) {
	tg := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name:     "CALCULATOR",
			Path:     "calculator.cbl",
			Language: "cobol",
			Functions: []*ir.Function{
				{Name: "ADD-NUMBERS"},
				{Name: "SUBTRACT-NUMBERS"},
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

	if result.Status != agents.StatusSuccess {
		t.Errorf("expected success, got %s", result.Status)
	}

	if len(result.GeneratedFiles) == 0 {
		t.Fatal("expected test files to be generated")
	}

	// Check the test file content
	testFile := result.GeneratedFiles[0]
	content := string(testFile.Content)

	if !strings.Contains(content, "CalculatorService") {
		t.Error("expected CalculatorService in test content")
	}
	if !strings.Contains(content, "addNumbers") {
		t.Error("expected addNumbers test in content")
	}
	if !strings.Contains(content, "subtractNumbers") {
		t.Error("expected subtractNumbers test in content")
	}
	if !strings.HasSuffix(testFile.Path, ".test.ts") {
		t.Errorf("expected .test.ts path, got %s", testFile.Path)
	}
}

func TestTestGenStubPython(t *testing.T) {
	tg := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name:     "CALCULATOR",
			Path:     "calculator.cbl",
			Language: "cobol",
			Functions: []*ir.Function{
				{Name: "MAIN-PARAGRAPH"},
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

	if len(result.GeneratedFiles) == 0 {
		t.Fatal("expected test files")
	}

	content := string(result.GeneratedFiles[0].Content)
	if !strings.Contains(content, "import pytest") {
		t.Error("expected pytest import")
	}
	if !strings.Contains(content, "def test_main_paragraph") {
		t.Error("expected test_main_paragraph")
	}
}

func TestTestGenStubJava(t *testing.T) {
	tg := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name:     "HELLO",
			Language: "cobol",
			Functions: []*ir.Function{
				{Name: "MAIN"},
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
	if !strings.Contains(content, "import org.junit.jupiter") {
		t.Error("expected JUnit import")
	}
	if !strings.Contains(content, "HelloServiceTest") {
		t.Error("expected HelloServiceTest class")
	}
}

func TestTestGenStubGo(t *testing.T) {
	tg := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name:     "REPORT-GEN",
			Language: "cobol",
			Functions: []*ir.Function{
				{Name: "GENERATE-REPORT"},
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
	if !strings.Contains(content, "import \"testing\"") {
		t.Error("expected testing import")
	}
	if !strings.Contains(content, "TestGenerateReport") {
		t.Error("expected TestGenerateReport")
	}
}

func TestTestGenNoGraph(t *testing.T) {
	tg := New()
	_, err := tg.Run(context.Background(), &agents.AgentContext{
		Params: map[string]string{"target": "typescript", "generated_files": "[]"},
	})
	if err == nil {
		t.Error("expected error with nil graph")
	}
}
