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
	if len(files) != 2 {
		t.Fatalf("expected 2 scaffold files, got %d", len(files))
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
