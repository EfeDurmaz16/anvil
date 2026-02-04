package typescript

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
		DataTypes: []*ir.DataType{{
			Name: "WS-INPUT",
			Kind: ir.TypeStruct,
			Fields: []*ir.DataType{
				{Name: "WS-NUM1", Kind: ir.TypeInteger},
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

	var svc string
	for _, f := range files {
		if strings.HasPrefix(f.Path, "src/generated/") && strings.HasSuffix(f.Path, ".ts") && strings.Contains(f.Path, "calculator") {
			svc = string(f.Content)
			break
		}
	}
	if svc == "" {
		t.Fatal("expected calculator service file")
	}
	if !strings.Contains(svc, "export class CalculatorService") {
		t.Error("expected CalculatorService")
	}
	if !strings.Contains(svc, "addNumbers") {
		t.Error("expected addNumbers method")
	}
}

func TestTypeMapper(t *testing.T) {
	tests := []struct {
		dt   *ir.DataType
		want string
	}{
		{&ir.DataType{Kind: ir.TypeString}, "string"},
		{&ir.DataType{Kind: ir.TypeInteger}, "number"},
		{&ir.DataType{Kind: ir.TypeDecimal}, "number"},
		{&ir.DataType{Kind: ir.TypeBoolean}, "boolean"},
	}
	for _, tt := range tests {
		got := mapType(tt.dt)
		if got != tt.want {
			t.Errorf("mapType(%v) = %s, want %s", tt.dt.Kind, got, tt.want)
		}
	}
}
