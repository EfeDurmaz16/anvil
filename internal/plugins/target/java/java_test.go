package java

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
	if len(files) != 4 {
		t.Fatalf("expected 4 scaffold files, got %d", len(files))
	}
}

func TestScaffold_IncludesManifest(t *testing.T) {
	p := New()
	files, err := p.Scaffold(context.Background(), &ir.SemanticGraph{})
	if err != nil {
		t.Fatal(err)
	}

	var hasManifest, hasRunner bool
	for _, f := range files {
		if f.Path == "anvil.manifest.json" {
			hasManifest = true
			content := string(f.Content)
			if !strings.Contains(content, `"language": "java"`) {
				t.Error("manifest missing language field")
			}
			if !strings.Contains(content, "mvn") {
				t.Error("manifest missing mvn compile command")
			}
			if !strings.Contains(content, "run_fixture") {
				t.Error("manifest missing run_fixture")
			}
		}
		if f.Path == "src/main/java/com/anvil/generated/AnvilRunner.java" {
			hasRunner = true
			content := string(f.Content)
			if !strings.Contains(content, "class AnvilRunner") {
				t.Error("AnvilRunner missing class definition")
			}
			if !strings.Contains(content, "public static void main") {
				t.Error("AnvilRunner missing main method")
			}
		}
	}

	if !hasManifest {
		t.Error("scaffold missing anvil.manifest.json")
	}
	if !hasRunner {
		t.Error("scaffold missing AnvilRunner.java")
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
				{Name: "SUBTRACT-NUMBERS"},
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

	content := string(files[0].Content)
	if !strings.Contains(content, "class Calculator") {
		t.Error("expected class Calculator in output")
	}
	if !strings.Contains(content, "addNumbers") {
		t.Error("expected addNumbers method")
	}
}

func TestTypeMapper(t *testing.T) {
	tests := []struct {
		dt   *ir.DataType
		want string
	}{
		{&ir.DataType{Kind: ir.TypeString}, "String"},
		{&ir.DataType{Kind: ir.TypeInteger, Size: 5}, "int"},
		{&ir.DataType{Kind: ir.TypeInteger, Size: 15}, "long"},
		{&ir.DataType{Kind: ir.TypeDecimal}, "java.math.BigDecimal"},
		{&ir.DataType{Kind: ir.TypeBoolean}, "boolean"},
	}
	for _, tt := range tests {
		got := mapType(tt.dt)
		if got != tt.want {
			t.Errorf("mapType(%v) = %s, want %s", tt.dt.Kind, got, tt.want)
		}
	}
}
