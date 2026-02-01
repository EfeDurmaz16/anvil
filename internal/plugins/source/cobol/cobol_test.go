package cobol

import (
	"context"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/plugins"
)

func TestParseHello(t *testing.T) {
	src := []byte(`       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "HELLO, WORLD!".
           STOP RUN.
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "hello.cbl", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(graph.Modules) != 1 {
		t.Fatalf("expected 1 module, got %d", len(graph.Modules))
	}
	if graph.Modules[0].Name != "HELLO" {
		t.Errorf("expected program name HELLO, got %s", graph.Modules[0].Name)
	}
	if len(graph.Modules[0].Functions) == 0 {
		t.Fatal("expected at least one function")
	}
}

func TestParseCalculator(t *testing.T) {
	src := []byte(`       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM1 PIC 9(5) VALUE ZEROS.
       01 WS-NUM2 PIC 9(5) VALUE ZEROS.
       01 WS-RESULT PIC 9(10) VALUE ZEROS.
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           PERFORM ADD-NUMBERS.
           STOP RUN.
       ADD-NUMBERS.
           ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULT.
           DISPLAY "SUM: " WS-RESULT.
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "calculator.cbl", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}
	mod := graph.Modules[0]
	if mod.Name != "CALCULATOR" {
		t.Errorf("expected CALCULATOR, got %s", mod.Name)
	}
	if len(mod.DataTypes) < 3 {
		t.Errorf("expected at least 3 data types, got %d", len(mod.DataTypes))
	}
	// Should have call graph edge from MAIN-PARAGRAPH → ADD-NUMBERS
	if len(graph.CallGraph.Edges) == 0 {
		t.Error("expected call graph edges")
	}
}

func TestParseCopybook(t *testing.T) {
	src := []byte(`       01 CUSTOMER-RECORD.
           05 CUST-ID PIC 9(8).
           05 CUST-NAME PIC X(30).
`)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "customer.cpy", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(graph.DataTypes) == 0 {
		t.Fatal("expected data types from copybook")
	}
	if graph.DataTypes[0].Name != "CUSTOMER" {
		t.Errorf("expected CUSTOMER, got %s", graph.DataTypes[0].Name)
	}
}
