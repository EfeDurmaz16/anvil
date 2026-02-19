package temporal

import (
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	"github.com/efebarandurmaz/anvil/internal/plugins/source/cobol"
	"github.com/efebarandurmaz/anvil/internal/plugins/target/golang"
	"github.com/efebarandurmaz/anvil/internal/plugins/target/python"
	"github.com/efebarandurmaz/anvil/internal/plugins/target/typescript"
)

// setupTestRegistry creates a registry with COBOL source and multiple target plugins.
func setupTestRegistry() *plugins.Registry {
	reg := plugins.NewRegistry()
	reg.RegisterSource(cobol.New())
	reg.RegisterTarget(typescript.New())
	reg.RegisterTarget(python.New())
	reg.RegisterTarget(golang.New())
	return reg
}

func TestSetDependencies(t *testing.T) {
	reg := setupTestRegistry()
	testDeps := &Dependencies{
		Registry: reg,
		LLM:      agents.AgentContext{},
	}

	SetDependencies(testDeps)

	if deps == nil {
		t.Fatal("SetDependencies failed: deps is nil")
	}
	if deps.Registry != reg {
		t.Error("SetDependencies did not set registry correctly")
	}
}

func TestCartographerActivity_HelloWorld(t *testing.T) {
	reg := setupTestRegistry()
	SetDependencies(&Dependencies{
		Registry: reg,
		LLM:      agents.AgentContext{},
	})

	// Create a temp directory with a simple COBOL file
	tmpDir := t.TempDir()
	helloPath := filepath.Join(tmpDir, "hello.cbl")
	helloSrc := []byte(`       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "HELLO, WORLD!".
           STOP RUN.
`)
	if err := os.WriteFile(helloPath, helloSrc, 0644); err != nil {
		t.Fatal(err)
	}

	input := ModernizationInput{
		SourceLang: "cobol",
		TargetLang: "typescript",
		InputPath:  tmpDir,
		OutputPath: filepath.Join(tmpDir, "output"),
	}

	ctx := context.Background()
	result, err := CartographerActivity(ctx, input)
	if err != nil {
		t.Fatalf("CartographerActivity failed: %v", err)
	}

	if result.GraphJSON == "" {
		t.Fatal("expected non-empty GraphJSON")
	}

	// Unmarshal to verify it's valid JSON
	var graph ir.SemanticGraph
	if err := json.Unmarshal([]byte(result.GraphJSON), &graph); err != nil {
		t.Fatalf("GraphJSON is not valid JSON: %v", err)
	}

	if len(graph.Modules) == 0 {
		t.Error("expected at least one module")
	}
	if graph.Modules[0].Name != "HELLO" {
		t.Errorf("expected module name HELLO, got %s", graph.Modules[0].Name)
	}
}

func TestCartographerActivity_NoSourcePlugin(t *testing.T) {
	// Create registry without COBOL plugin
	reg := plugins.NewRegistry()
	SetDependencies(&Dependencies{
		Registry: reg,
		LLM:      agents.AgentContext{},
	})

	input := ModernizationInput{
		SourceLang: "cobol",
		TargetLang: "typescript",
		InputPath:  "/nonexistent",
	}

	ctx := context.Background()
	_, err := CartographerActivity(ctx, input)
	if err == nil {
		t.Fatal("expected error when source plugin is missing")
	}
}

func TestSpecularActivity_ValidGraph(t *testing.T) {
	reg := setupTestRegistry()
	SetDependencies(&Dependencies{
		Registry: reg,
		LLM:      agents.AgentContext{},
	})

	// Create a minimal graph
	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "TESTPROG",
				Language: "cobol",
				Functions: []*ir.Function{
					{Name: "MAIN-PARA"},
				},
			},
		},
		CallGraph: &ir.CallGraph{Edges: []ir.CallEdge{}},
	}

	graphJSON, _ := json.Marshal(graph)

	input := ModernizationInput{
		SourceLang: "cobol",
		TargetLang: "typescript",
	}

	ctx := context.Background()
	result, err := SpecularActivity(ctx, input, string(graphJSON))
	if err != nil {
		t.Fatalf("SpecularActivity failed: %v", err)
	}

	if result.GraphJSON == "" {
		t.Fatal("expected non-empty GraphJSON")
	}

	// Verify the enriched graph is valid
	var enriched ir.SemanticGraph
	if err := json.Unmarshal([]byte(result.GraphJSON), &enriched); err != nil {
		t.Fatalf("enriched GraphJSON is not valid: %v", err)
	}
}

func TestSpecularActivity_InvalidJSON(t *testing.T) {
	reg := setupTestRegistry()
	SetDependencies(&Dependencies{
		Registry: reg,
		LLM:      agents.AgentContext{},
	})

	input := ModernizationInput{
		SourceLang: "cobol",
		TargetLang: "typescript",
	}

	ctx := context.Background()
	_, err := SpecularActivity(ctx, input, "invalid json")
	if err == nil {
		t.Fatal("expected error with invalid JSON")
	}
}

func TestArchitectActivity_TypeScript(t *testing.T) {
	reg := setupTestRegistry()
	SetDependencies(&Dependencies{
		Registry: reg,
		LLM:      agents.AgentContext{},
	})

	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "CALCULATOR",
				Language: "cobol",
				Functions: []*ir.Function{
					{Name: "ADD-NUMBERS", Body: "ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULT."},
				},
				DataTypes: []*ir.DataType{
					{Name: "WS-NUM1", Kind: ir.TypeInteger},
					{Name: "WS-NUM2", Kind: ir.TypeInteger},
				},
			},
		},
		CallGraph: &ir.CallGraph{Edges: []ir.CallEdge{}},
	}

	graphJSON, _ := json.Marshal(graph)

	input := ModernizationInput{
		SourceLang: "cobol",
		TargetLang: "typescript",
	}

	ctx := context.Background()
	result, err := ArchitectActivity(ctx, input, string(graphJSON))
	if err != nil {
		t.Fatalf("ArchitectActivity failed: %v", err)
	}

	if result.FilesJSON == "" {
		t.Fatal("expected non-empty FilesJSON")
	}

	// Verify FilesJSON is valid
	var files []plugins.GeneratedFile
	if err := json.Unmarshal([]byte(result.FilesJSON), &files); err != nil {
		t.Fatalf("FilesJSON is not valid: %v", err)
	}

	if len(files) == 0 {
		t.Error("expected at least one generated file")
	}

	// Check for expected TypeScript files
	foundService := false
	for _, f := range files {
		if filepath.Ext(f.Path) == ".ts" {
			foundService = true
		}
	}
	if !foundService {
		t.Error("expected at least one .ts file")
	}
}

func TestArchitectActivity_Python(t *testing.T) {
	reg := setupTestRegistry()
	SetDependencies(&Dependencies{
		Registry: reg,
		LLM:      agents.AgentContext{},
	})

	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "HELLO",
				Language: "cobol",
				Functions: []*ir.Function{
					{Name: "MAIN-PARAGRAPH"},
				},
			},
		},
		CallGraph: &ir.CallGraph{Edges: []ir.CallEdge{}},
	}

	graphJSON, _ := json.Marshal(graph)

	input := ModernizationInput{
		SourceLang: "cobol",
		TargetLang: "python",
	}

	ctx := context.Background()
	result, err := ArchitectActivity(ctx, input, string(graphJSON))
	if err != nil {
		t.Fatalf("ArchitectActivity failed: %v", err)
	}

	var files []plugins.GeneratedFile
	if err := json.Unmarshal([]byte(result.FilesJSON), &files); err != nil {
		t.Fatal(err)
	}

	foundPython := false
	for _, f := range files {
		if filepath.Ext(f.Path) == ".py" {
			foundPython = true
		}
	}
	if !foundPython {
		t.Error("expected at least one .py file")
	}
}

func TestJudgeActivity_ValidFiles(t *testing.T) {
	reg := setupTestRegistry()
	SetDependencies(&Dependencies{
		Registry: reg,
		LLM:      agents.AgentContext{},
	})

	graph := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "TESTPROG",
				Language: "cobol",
			},
		},
		CallGraph: &ir.CallGraph{Edges: []ir.CallEdge{}},
	}
	graphJSON, _ := json.Marshal(graph)

	files := []plugins.GeneratedFile{
		{Path: "test.ts", Content: []byte("export class TestService {}")},
	}
	filesJSON, _ := json.Marshal(files)

	input := ModernizationInput{
		SourceLang: "cobol",
		TargetLang: "typescript",
	}

	ctx := context.Background()
	result, err := JudgeActivity(ctx, input, string(graphJSON), string(filesJSON))
	if err != nil {
		t.Fatalf("JudgeActivity failed: %v", err)
	}

	// Score should be between 0 and 1
	if result.Score < 0 || result.Score > 1 {
		t.Errorf("expected score in [0,1], got %f", result.Score)
	}

	// Should have GraphJSON with judge metadata
	var judgedGraph ir.SemanticGraph
	if err := json.Unmarshal([]byte(result.GraphJSON), &judgedGraph); err != nil {
		t.Fatalf("judged GraphJSON is not valid: %v", err)
	}
}

func TestHarnessActivity_NoFixtures(t *testing.T) {
	// Test harness with no fixtures configured
	tmpDir := t.TempDir()

	// Generate minimal valid TypeScript project
	files := []plugins.GeneratedFile{
		{
			Path:    "package.json",
			Content: []byte(`{"name":"test","type":"module","scripts":{"build":"tsc"},"devDependencies":{"typescript":"^5.0.0"}}`),
		},
		{
			Path:    "tsconfig.json",
			Content: []byte(`{"compilerOptions":{"target":"ES2022","module":"ES2022","outDir":"dist"}}`),
		},
		{
			Path:    "src/test.ts",
			Content: []byte(`export class TestService { hello(): void { console.log("hello"); } }`),
		},
		{
			Path: "anvil.manifest.json",
			Content: []byte(`{
				"version": "1",
				"language": "typescript",
				"compile": [{"cmd": "tsc", "args": ["-p", "tsconfig.json"]}]
			}`),
		},
	}
	filesJSON, _ := json.Marshal(files)

	input := HarnessInput{
		TargetLang:   "typescript",
		FilesJSON:    string(filesJSON),
		OutputPath:   tmpDir,
		FixturesPath: "", // No fixtures
	}

	ctx := context.Background()
	result, err := HarnessActivity(ctx, input)
	if err != nil {
		t.Fatalf("HarnessActivity failed: %v", err)
	}

	// DBDiffScore should be 1.0 (no DB diff configured)
	if result.DBDiffScore != 1.0 {
		t.Errorf("expected DBDiffScore=1.0 with no DB diff, got %f", result.DBDiffScore)
	}

	// Harness should complete without fatal errors
	// Compilation may fail if tsc is not installed, but structure should be correct
	t.Logf("Harness result: Compile=%f, Functional=%f, DBDiff=%f, Overall=%f",
		result.CompileScore, result.FunctionalScore, result.DBDiffScore, result.OverallScore)

	// If manifest loaded successfully, FunctionalScore should be 1.0 when no fixtures
	// If manifest failed to load, we expect errors
	manifestLoaded := true
	for _, e := range result.Errors {
		if strings.Contains(e, "load manifest runner") {
			manifestLoaded = false
			break
		}
	}

	if manifestLoaded && result.FunctionalScore != 1.0 {
		t.Errorf("when manifest loads and no fixtures configured, FunctionalScore should be 1.0, got %f", result.FunctionalScore)
	}
}

func TestHarnessActivity_InvalidFilesJSON(t *testing.T) {
	tmpDir := t.TempDir()

	input := HarnessInput{
		TargetLang: "typescript",
		FilesJSON:  "invalid json",
		OutputPath: tmpDir,
	}

	ctx := context.Background()
	result, err := HarnessActivity(ctx, input)
	// Should not error, but should have errors in result
	if err != nil {
		t.Fatalf("expected no error, got %v", err)
	}

	if len(result.Errors) == 0 {
		t.Error("expected errors in result for invalid JSON")
	}
}

func TestArchitectActivity_NoTargetPlugin(t *testing.T) {
	// Create registry without target plugins
	reg := plugins.NewRegistry()
	reg.RegisterSource(cobol.New())
	SetDependencies(&Dependencies{
		Registry: reg,
		LLM:      agents.AgentContext{},
	})

	graph := &ir.SemanticGraph{
		Modules:   []*ir.Module{{Name: "TEST"}},
		CallGraph: &ir.CallGraph{Edges: []ir.CallEdge{}},
	}
	graphJSON, _ := json.Marshal(graph)

	input := ModernizationInput{
		SourceLang: "cobol",
		TargetLang: "nonexistent",
	}

	ctx := context.Background()
	_, err := ArchitectActivity(ctx, input, string(graphJSON))
	if err == nil {
		t.Fatal("expected error when target plugin is missing")
	}
}
