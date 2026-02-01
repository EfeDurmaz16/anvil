package cobol

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/plugins"
)

func TestParseHello(t *testing.T) {
	src := []byte("       IDENTIFICATION DIVISION.\n       PROGRAM-ID. HELLO.\n       PROCEDURE DIVISION.\n       MAIN-PARAGRAPH.\n           DISPLAY \"HELLO, WORLD!\".\n           STOP RUN.\n")
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
	src := []byte("       IDENTIFICATION DIVISION.\n       PROGRAM-ID. CALCULATOR.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01 WS-NUM1 PIC 9(5) VALUE ZEROS.\n       01 WS-NUM2 PIC 9(5) VALUE ZEROS.\n       01 WS-RESULT PIC 9(10) VALUE ZEROS.\n       PROCEDURE DIVISION.\n       MAIN-PARAGRAPH.\n           PERFORM ADD-NUMBERS.\n           STOP RUN.\n       ADD-NUMBERS.\n           ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULT.\n           DISPLAY \"SUM: \" WS-RESULT.\n")
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
	if len(graph.CallGraph.Edges) == 0 {
		t.Error("expected call graph edges")
	}
	// Should find both MAIN-PARAGRAPH and ADD-NUMBERS
	if len(mod.Functions) < 2 {
		t.Errorf("expected at least 2 functions, got %d", len(mod.Functions))
	}
}

func TestParseCopybook(t *testing.T) {
	src := []byte("       01 CUSTOMER-RECORD.\n           05 CUST-ID PIC 9(8).\n           05 CUST-NAME PIC X(30).\n           05 CUST-BALANCE PIC 9(7)V99.\n")
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

func TestParsePerformThru(t *testing.T) {
	src := []byte("       IDENTIFICATION DIVISION.\n       PROGRAM-ID. THRUTEST.\n       PROCEDURE DIVISION.\n       0000-MAIN.\n           PERFORM 1000-PROCESS\n              THRU 1000-PROCESS-EXIT.\n           STOP RUN.\n       1000-PROCESS.\n           DISPLAY \"PROCESSING\".\n       1000-PROCESS-EXIT.\n           EXIT.\n")
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "thru.cbl", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(graph.Modules[0].Functions) < 3 {
		t.Errorf("expected 3 paragraphs (0000-MAIN, 1000-PROCESS, 1000-PROCESS-EXIT), got %d", len(graph.Modules[0].Functions))
	}
	// Check 0000-MAIN calls 1000-PROCESS
	mainFn := graph.Modules[0].Functions[0]
	if mainFn.Name != "0000-MAIN" {
		t.Errorf("expected 0000-MAIN, got %s", mainFn.Name)
	}
	found := false
	for _, c := range mainFn.Calls {
		if c == "1000-PROCESS" {
			found = true
		}
	}
	if !found {
		t.Errorf("expected call to 1000-PROCESS, got %v", mainFn.Calls)
	}
}

func TestParse88Level(t *testing.T) {
	types := parseDataDivision([]string{
		"       DATA DIVISION.",
		"       WORKING-STORAGE SECTION.",
		"       01 WS-FLAGS.",
		"          05 WS-STATUS PIC X(1).",
		"             88 STATUS-ACTIVE VALUE 'A'.",
		"             88 STATUS-INACTIVE VALUE 'I'.",
		"          05 WS-COUNT PIC S9(4) COMP.",
	})

	boolCount := 0
	compFound := false
	for _, dt := range types {
		if dt.Kind == "boolean" {
			boolCount++
		}
		if dt.Name == "WS-COUNT" && dt.Metadata["usage"] == "COMP" {
			compFound = true
		}
	}
	if boolCount != 2 {
		t.Errorf("expected 2 boolean (88-level) types, got %d", boolCount)
	}
	if !compFound {
		t.Error("expected WS-COUNT with COMP usage")
	}
}

func TestParseCardDemo(t *testing.T) {
	carddemoPath := filepath.Join("../../../../testdata/carddemo/app")
	if _, err := os.Stat(carddemoPath); os.IsNotExist(err) {
		t.Skip("CardDemo test data not available")
	}

	p := New()
	var files []plugins.SourceFile

	filepath.Walk(carddemoPath, func(path string, fi os.FileInfo, err error) error {
		if err != nil || fi.IsDir() {
			return err
		}
		ext := filepath.Ext(path)
		if ext == ".cbl" || ext == ".CBL" || ext == ".cpy" || ext == ".CPY" {
			data, _ := os.ReadFile(path)
			files = append(files, plugins.SourceFile{Path: path, Content: data})
		}
		return nil
	})

	if len(files) == 0 {
		t.Skip("No COBOL files found")
	}

	graph, err := p.Parse(context.Background(), files)
	if err != nil {
		t.Fatal(err)
	}

	t.Logf("Files: %d", len(files))
	t.Logf("Modules: %d", len(graph.Modules))

	totalFns := 0
	totalDT := len(graph.DataTypes)
	for _, mod := range graph.Modules {
		totalFns += len(mod.Functions)
		totalDT += len(mod.DataTypes)
	}
	t.Logf("Functions: %d", totalFns)
	t.Logf("Data Types: %d", totalDT)
	t.Logf("Call Edges: %d", len(graph.CallGraph.Edges))

	// CardDemo should have significantly more than 29 functions now
	if totalFns < 100 {
		t.Errorf("expected at least 100 functions from CardDemo, got %d", totalFns)
	}
}
