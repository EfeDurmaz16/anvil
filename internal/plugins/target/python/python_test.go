package python

import (
	"context"
	"strings"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/ir"
)

func TestScaffold(t *testing.T) {
	p := New()
	files, err := p.Scaffold(context.Background(), &ir.SemanticGraph{})
	if err != nil {
		t.Fatal(err)
	}
	if len(files) != 5 {
		t.Fatalf("expected 5 scaffold files, got %d", len(files))
	}
	foundManifest := false
	for _, f := range files {
		if f.Path == "anvil.manifest.json" {
			foundManifest = true
			if !strings.Contains(string(f.Content), "\"run_fixture\"") {
				t.Fatal("expected run_fixture in manifest")
			}
		}
	}
	if !foundManifest {
		t.Fatal("expected anvil.manifest.json in scaffold")
	}
}

func TestGenerateWithoutLLM(t *testing.T) {
	p := New()
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{{
			Name:     "CALCULATOR",
			Path:     "calculator.cbl",
			Language: "cobol",
			Functions: []*ir.Function{
				{Name: "ADD-NUMBERS"},
			},
		}},
	}

	files, err := p.Generate(context.Background(), graph, nil)
	if err != nil {
		t.Fatal(err)
	}
	if len(files) == 0 {
		t.Fatal("expected generated files")
	}

	var mod string
	for _, f := range files {
		if strings.Contains(f.Path, "calculator.py") {
			mod = string(f.Content)
			break
		}
	}
	if mod == "" {
		t.Fatal("expected calculator module file")
	}
	if !strings.Contains(mod, "class CalculatorService") {
		t.Error("expected CalculatorService")
	}
	if !strings.Contains(mod, "def add_numbers") {
		t.Error("expected add_numbers method")
	}
}

func TestTypeMapper(t *testing.T) {
	tests := []struct {
		dt   *ir.DataType
		want string
	}{
		{&ir.DataType{Kind: ir.TypeString}, "str"},
		{&ir.DataType{Kind: ir.TypeInteger}, "int"},
		{&ir.DataType{Kind: ir.TypeDecimal}, "Decimal"},
		{&ir.DataType{Kind: ir.TypeBoolean}, "bool"},
	}
	for _, tt := range tests {
		got := mapType(tt.dt)
		if got != tt.want {
			t.Errorf("mapType(%v) = %s, want %s", tt.dt.Kind, got, tt.want)
		}
	}
}
