package golang

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
	if len(files) != 3 {
		t.Fatalf("expected 3 scaffold files, got %d", len(files))
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
		if strings.Contains(f.Path, "calculator.go") {
			mod = string(f.Content)
			break
		}
	}
	if mod == "" {
		t.Fatal("expected calculator go file")
	}
	if !strings.Contains(mod, "type CalculatorService struct") {
		t.Error("expected CalculatorService struct")
	}
	if !strings.Contains(mod, "func (s *CalculatorService) AddNumbers()") {
		t.Error("expected AddNumbers method")
	}
}

func TestTypeMapper(t *testing.T) {
	tests := []struct {
		dt   *ir.DataType
		want string
	}{
		{&ir.DataType{Kind: ir.TypeString}, "string"},
		{&ir.DataType{Kind: ir.TypeInteger, Size: 5}, "int"},
		{&ir.DataType{Kind: ir.TypeInteger, Size: 15}, "int64"},
		{&ir.DataType{Kind: ir.TypeDecimal}, "decimal.Decimal"},
		{&ir.DataType{Kind: ir.TypeBoolean}, "bool"},
	}
	for _, tt := range tests {
		got := mapType(tt.dt)
		if got != tt.want {
			t.Errorf("mapType(%v) = %s, want %s", tt.dt.Kind, got, tt.want)
		}
	}
}
