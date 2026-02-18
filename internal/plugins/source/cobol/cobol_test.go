package cobol

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/ir"
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

// findDataType recursively searches a slice of DataTypes for one matching the predicate.
func findDataType(types []*ir.DataType, pred func(*ir.DataType) bool) *ir.DataType {
	for _, dt := range types {
		if pred(dt) {
			return dt
		}
		if found := findDataType(dt.Fields, pred); found != nil {
			return found
		}
	}
	return nil
}

// countDataTypes recursively counts all DataType nodes matching the predicate.
func countDataTypes(types []*ir.DataType, pred func(*ir.DataType) bool) int {
	count := 0
	for _, dt := range types {
		if pred(dt) {
			count++
		}
		count += countDataTypes(dt.Fields, pred)
	}
	return count
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

	// With hierarchy, only WS-FLAGS is top-level; children are nested.
	if len(types) != 1 {
		t.Fatalf("expected 1 top-level data type (WS-FLAGS), got %d", len(types))
	}
	if types[0].Name != "WS-FLAGS" {
		t.Errorf("expected top-level WS-FLAGS, got %s", types[0].Name)
	}

	boolCount := countDataTypes(types, func(dt *ir.DataType) bool {
		return dt.Kind == "boolean"
	})
	compFound := findDataType(types, func(dt *ir.DataType) bool {
		return dt.Name == "WS-COUNT" && dt.Metadata["usage"] == "COMP"
	})

	if boolCount != 2 {
		t.Errorf("expected 2 boolean (88-level) types, got %d", boolCount)
	}
	if compFound == nil {
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

func TestCopybookExpansion(t *testing.T) {
	cpySrc := []byte("       01 CUSTOMER-RECORD.\n           05 CUST-ID PIC 9(8).\n           05 CUST-NAME PIC X(30).\n")
	mainSrc := []byte("       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TESTCOPY.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       COPY CUSTCPY.\n       PROCEDURE DIVISION.\n       MAIN-PARAGRAPH.\n           DISPLAY CUST-ID.\n           STOP RUN.\n")

	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "CUSTCPY.cpy", Content: cpySrc},
		{Path: "testcopy.cbl", Content: mainSrc},
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(graph.Modules) != 1 {
		t.Fatalf("expected 1 module, got %d", len(graph.Modules))
	}
	mod := graph.Modules[0]
	if mod.Name != "TESTCOPY" {
		t.Errorf("expected TESTCOPY, got %s", mod.Name)
	}
	// After expansion, the module should have the copybook's data types.
	// CUST-ID is a child of CUSTOMER-RECORD (05 level), so search recursively.
	found := findDataType(mod.DataTypes, func(dt *ir.DataType) bool {
		return dt.Name == "CUST-ID"
	})
	if found == nil {
		t.Error("expected CUST-ID from expanded copybook in module data types (recursive)")
	}
}

func TestCopybookReplacingLeading(t *testing.T) {
	cpySrc := []byte("       05 PREFIX-X PIC 9(5).\n       05 PREFIX-Y PIC 9(5).\n")
	mainSrc := []byte("       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TESTREPL.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01 MY-RECORD.\n       COPY COORDS REPLACING LEADING ==PREFIX== BY ==PLAYER==.\n       PROCEDURE DIVISION.\n       MAIN-PARAGRAPH.\n           STOP RUN.\n")

	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "COORDS.cpy", Content: cpySrc},
		{Path: "testrepl.cbl", Content: mainSrc},
	})
	if err != nil {
		t.Fatal(err)
	}
	mod := graph.Modules[0]
	// PLAYER-X and PLAYER-Y are 05-level children of MY-RECORD (01-level); search recursively.
	foundPlayerX := findDataType(mod.DataTypes, func(dt *ir.DataType) bool {
		return dt.Name == "PLAYER-X"
	})
	foundPlayerY := findDataType(mod.DataTypes, func(dt *ir.DataType) bool {
		return dt.Name == "PLAYER-Y"
	})
	if foundPlayerX == nil {
		t.Error("expected PLAYER-X after LEADING REPLACING")
	}
	if foundPlayerY == nil {
		t.Error("expected PLAYER-Y after LEADING REPLACING")
	}
}

func TestContinuationLines(t *testing.T) {
	// In fixed-format COBOL, a '-' in column 7 (index 6) marks a continuation.
	// The content from column 12 (index 11) is appended to the previous line.
	lines := []string{
		"       01 LONG-NAME-",
		"      -    RECORD PIC X(10).",
	}
	joined := joinContinuationLines(lines)
	if len(joined) != 1 {
		t.Fatalf("expected 1 joined line, got %d: %v", len(joined), joined)
	}
	if !strings.Contains(joined[0], "RECORD") {
		t.Errorf("expected continuation content 'RECORD' in joined line, got: %q", joined[0])
	}

	// Verify it parses correctly through parseDataDivision
	src := []string{
		"       DATA DIVISION.",
		"       WORKING-STORAGE SECTION.",
		"       01 WS-CONTINUED-",
		"      -    VALUE PIC X(20).",
	}
	preprocessed := joinContinuationLines(src)
	types := parseDataDivision(preprocessed)
	if len(types) != 1 {
		t.Fatalf("expected 1 data type after continuation join, got %d", len(types))
	}
	if types[0].Name != "WS-CONTINUED-VALUE" {
		t.Errorf("expected WS-CONTINUED-VALUE, got %s", types[0].Name)
	}
}

// TestPreprocessLinesCommentRemoval verifies that fixed-format comment lines
// (column 7 == '*' or '/') are dropped by preprocessLines.
func TestPreprocessLinesCommentRemoval(t *testing.T) {
	lines := []string{
		"       IDENTIFICATION DIVISION.",
		"      * This is a comment line",
		"       PROGRAM-ID. COMMENTS.",
		"      / Page-eject comment",
		"       PROCEDURE DIVISION.",
		"       MAIN.",
		"           STOP RUN.",
	}
	result := preprocessLines(lines)
	for _, l := range result {
		if len(l) >= 7 && (l[6] == '*' || l[6] == '/') {
			t.Errorf("comment line was not removed: %q", l)
		}
	}
	// Should have 5 lines (2 comments removed)
	if len(result) != 5 {
		t.Errorf("expected 5 lines after comment removal, got %d: %v", len(result), result)
	}
}

// TestPreprocessLinesStringLiteralContinuation verifies that a string literal
// split across lines with a '-' continuation indicator is rejoined correctly.
func TestPreprocessLinesStringLiteralContinuation(t *testing.T) {
	// Typical pattern: string value split across lines
	lines := []string{
		`       MOVE "HELLO, W` + strings.Repeat(" ", 52) + `"`,
		`      -    ORLD" TO WS-MESSAGE.`,
	}
	result := preprocessLines(lines)
	if len(result) != 1 {
		t.Fatalf("expected 1 joined line, got %d: %v", len(result), result)
	}
	if !strings.Contains(result[0], "ORLD") {
		t.Errorf("expected continuation content 'ORLD' in joined line, got: %q", result[0])
	}
}

// TestPreprocessLinesMixed verifies correct handling of a mix of regular,
// comment, and continuation lines together.
func TestPreprocessLinesMixed(t *testing.T) {
	lines := []string{
		"       DATA DIVISION.",
		"      * Top-of-section comment",
		"       WORKING-STORAGE SECTION.",
		"       01 WS-LONG-",
		"      -    NAME PIC X(10).",
		"      * Another comment",
		"       01 WS-SHORT PIC 9(4).",
	}
	result := preprocessLines(lines)
	// 2 comments dropped, continuation merged: expect 4 lines
	if len(result) != 4 {
		t.Fatalf("expected 4 lines, got %d: %v", len(result), result)
	}
	// The third result line should be the merged data item
	found := false
	for _, l := range result {
		if strings.Contains(l, "WS-LONG-") && strings.Contains(l, "NAME") {
			found = true
		}
	}
	if !found {
		t.Errorf("expected merged 'WS-LONG-NAME' line in result: %v", result)
	}
}

// TestPreprocessLinesShortLines verifies that lines shorter than 7 characters
// (free-format or truncated) are passed through without column-based processing.
func TestPreprocessLinesShortLines(t *testing.T) {
	// All lines < 7 chars — not fixed-format, no column rules apply
	lines := []string{
		"ID.",
		"FOO.",
		"BAR.",
	}
	result := preprocessLines(lines)
	if len(result) != 3 {
		t.Errorf("expected 3 lines unchanged, got %d: %v", len(result), result)
	}
}

// TestPreprocessLinesIntegration verifies that a MOVE statement split with a
// continuation line is parsed correctly end-to-end via Parse.
func TestPreprocessLinesIntegration(t *testing.T) {
	// Build source where a COMPUTE is split across a continuation line.
	// Column positions: 1-6 sequence area, 7 indicator, 8-72 content area.
	src := []byte(
		"       IDENTIFICATION DIVISION.\n" +
			"       PROGRAM-ID. CONTTEST.\n" +
			"       DATA DIVISION.\n" +
			"       WORKING-STORAGE SECTION.\n" +
			"       01 WS-RESULT PIC 9(10).\n" +
			"       PROCEDURE DIVISION.\n" +
			"       MAIN-PARAGRAPH.\n" +
			"           COMPUTE WS-RESULT =\n" +
			"      -        100 + 200.\n" +
			"           STOP RUN.\n",
	)
	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "conttest.cbl", Content: src},
	})
	if err != nil {
		t.Fatal(err)
	}
	if len(graph.Modules) != 1 {
		t.Fatalf("expected 1 module, got %d", len(graph.Modules))
	}
	if graph.Modules[0].Name != "CONTTEST" {
		t.Errorf("expected CONTTEST, got %s", graph.Modules[0].Name)
	}
}

// TestPreprocessLinesFreeFormat verifies that free-format COBOL (short lines,
// no fixed-column structure) skips column-based indicator processing.
func TestPreprocessLinesFreeFormat(t *testing.T) {
	// Free-format: lines well under 72 chars; *> is the free-format comment marker
	lines := []string{
		"IDENTIFICATION DIVISION.",
		"PROGRAM-ID. FREE.",
		"PROCEDURE DIVISION. *> inline comment",
		"MAIN.",
		"    STOP RUN.",
	}
	result := preprocessLines(lines)
	// Inline *> comment should be stripped from line 3
	for _, l := range result {
		if strings.Contains(l, "*>") {
			t.Errorf("free-format inline comment was not stripped: %q", l)
		}
	}
	if len(result) != 5 {
		t.Errorf("expected 5 lines, got %d", len(result))
	}
}

func TestCopybookMissingWarns(t *testing.T) {
	mainSrc := []byte("       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TESTMISS.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       COPY DFHAID.\n       COPY NONEXIST.\n       PROCEDURE DIVISION.\n       MAIN-PARAGRAPH.\n           STOP RUN.\n")

	p := New()
	graph, err := p.Parse(context.Background(), []plugins.SourceFile{
		{Path: "testmiss.cbl", Content: mainSrc},
	})
	if err != nil {
		t.Fatal(err)
	}
	// Should have warnings for missing copybooks but no error
	warnings := graph.Metadata["copybook_warnings"]
	if !strings.Contains(warnings, "DFHAID") {
		t.Error("expected warning about system copybook DFHAID")
	}
	if !strings.Contains(warnings, "NONEXIST") {
		t.Error("expected warning about missing copybook NONEXIST")
	}
}
