package e2e

import (
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/agents/architect"
	"github.com/efebarandurmaz/anvil/internal/agents/cartographer"
	"github.com/efebarandurmaz/anvil/internal/agents/judge"
	"github.com/efebarandurmaz/anvil/internal/agents/specular"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	cobolplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/cobol"
	golangplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/golang"
	pythonplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/python"
	tsplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/typescript"
)

func TestE2E_COBOLToTypeScript_TemplateMode(t *testing.T) {
	ctx := context.Background()

	// 1. Setup: write COBOL source to temp dir
	tmpDir := t.TempDir()
	cobolSource := `       IDENTIFICATION DIVISION.
       PROGRAM-ID. CALCULATOR.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 NUM1 PIC 9(5).
       01 NUM2 PIC 9(5).
       01 RESULT PIC 9(6).
       PROCEDURE DIVISION.
           ADD NUM1 TO NUM2 GIVING RESULT.
           DISPLAY RESULT.
           STOP RUN.
`
	cobolPath := filepath.Join(tmpDir, "calculator.cbl")
	if err := os.WriteFile(cobolPath, []byte(cobolSource), 0o644); err != nil {
		t.Fatal(err)
	}

	// 2. Register plugins
	reg := plugins.NewRegistry()
	reg.RegisterSource(cobolplugin.New())
	reg.RegisterTarget(tsplugin.New())

	// 3. Run Cartographer
	carto := cartographer.New()
	cartoCtx := &agents.AgentContext{
		Registry: reg,
		Params: map[string]string{
			"source": "cobol",
			"input":  tmpDir,
		},
	}
	cartoResult, err := carto.Run(ctx, cartoCtx)
	if err != nil {
		t.Fatalf("cartographer failed: %v", err)
	}
	if cartoResult.Graph == nil {
		t.Fatal("cartographer returned nil graph")
	}
	if len(cartoResult.Graph.Modules) == 0 {
		t.Fatal("cartographer returned empty modules")
	}

	// 4. Run Specular in passthrough mode (nil LLM)
	spec := specular.New()
	specCtx := &agents.AgentContext{
		Graph:    cartoResult.Graph,
		LLM:      nil, // passthrough mode
		Registry: reg,
	}
	specResult, err := spec.Run(ctx, specCtx)
	if err != nil {
		t.Fatalf("specular failed: %v", err)
	}
	if specResult.Status != agents.StatusPassthrough {
		t.Errorf("expected passthrough status, got %s", specResult.Status)
	}
	if specResult.Graph != cartoResult.Graph {
		t.Error("specular should pass through graph unchanged")
	}

	// 5. Run Architect (template mode, nil LLM)
	arch := architect.New()
	archCtx := &agents.AgentContext{
		Graph:    specResult.Graph,
		LLM:      nil, // template mode
		Registry: reg,
		Params: map[string]string{
			"target": "typescript",
		},
	}
	archResult, err := arch.Run(ctx, archCtx)
	if err != nil {
		t.Fatalf("architect failed: %v", err)
	}
	if archResult.Status != agents.StatusSuccess {
		t.Errorf("expected success status, got %s", archResult.Status)
	}
	if len(archResult.GeneratedFiles) == 0 {
		t.Fatal("architect generated no files")
	}

	// 6. Verify output includes anvil.manifest.json
	var hasManifest, hasPackageJSON, hasTSConfig, hasRunner bool
	var hasServiceFile bool
	for _, f := range archResult.GeneratedFiles {
		switch f.Path {
		case "anvil.manifest.json":
			hasManifest = true
			content := string(f.Content)
			if !strings.Contains(content, `"language": "typescript"`) {
				t.Error("manifest missing language field")
			}
			if !strings.Contains(content, "tsc") {
				t.Error("manifest missing tsc compile command")
			}
			if !strings.Contains(content, "run_fixture") {
				t.Error("manifest missing run_fixture")
			}
		case "package.json":
			hasPackageJSON = true
		case "tsconfig.json":
			hasTSConfig = true
		case "src/anvil_runner.ts":
			hasRunner = true
		}
		// Check for generated service file
		if strings.HasPrefix(f.Path, "src/generated/") && strings.HasSuffix(f.Path, ".ts") && f.Path != "src/generated/index.ts" && f.Path != "src/generated/model.ts" {
			hasServiceFile = true
			content := string(f.Content)
			if !strings.Contains(content, "class") {
				t.Errorf("service file %s missing class definition", f.Path)
			}
		}
	}

	if !hasManifest {
		t.Error("generated files missing anvil.manifest.json")
	}
	if !hasPackageJSON {
		t.Error("generated files missing package.json")
	}
	if !hasTSConfig {
		t.Error("generated files missing tsconfig.json")
	}
	if !hasRunner {
		t.Error("generated files missing src/anvil_runner.ts")
	}
	if !hasServiceFile {
		t.Error("generated files missing service TypeScript file")
	}

	// 7. Run Judge (passthrough mode, nil LLM)
	judgeAgent := judge.New()
	filesJSON, _ := json.Marshal(archResult.GeneratedFiles)
	judgeCtx := &agents.AgentContext{
		Graph: archResult.Graph,
		LLM:   nil, // passthrough mode
		Params: map[string]string{
			"source":          "cobol",
			"target":          "typescript",
			"generated_files": string(filesJSON),
		},
	}
	judgeResult, err := judgeAgent.Run(ctx, judgeCtx)
	if err != nil {
		t.Fatalf("judge failed: %v", err)
	}
	if judgeResult.Score < 0 || judgeResult.Score > 1 {
		t.Errorf("expected score in [0,1], got %f", judgeResult.Score)
	}
	t.Logf("Judge score: %f", judgeResult.Score)
}

func TestE2E_COBOLToPython_TemplateMode(t *testing.T) {
	ctx := context.Background()

	tmpDir := t.TempDir()
	cobolSource := `       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           DISPLAY "HELLO, WORLD!".
           STOP RUN.
`
	cobolPath := filepath.Join(tmpDir, "hello.cbl")
	if err := os.WriteFile(cobolPath, []byte(cobolSource), 0o644); err != nil {
		t.Fatal(err)
	}

	reg := plugins.NewRegistry()
	reg.RegisterSource(cobolplugin.New())
	reg.RegisterTarget(pythonplugin.New())

	// Cartographer
	carto := cartographer.New()
	cartoCtx := &agents.AgentContext{
		Registry: reg,
		Params:   map[string]string{"source": "cobol", "input": tmpDir},
	}
	cartoResult, err := carto.Run(ctx, cartoCtx)
	if err != nil {
		t.Fatalf("cartographer failed: %v", err)
	}

	// Specular (passthrough)
	spec := specular.New()
	specCtx := &agents.AgentContext{
		Graph:    cartoResult.Graph,
		LLM:      nil,
		Registry: reg,
	}
	specResult, err := spec.Run(ctx, specCtx)
	if err != nil {
		t.Fatalf("specular failed: %v", err)
	}

	// Architect (template mode)
	arch := architect.New()
	archCtx := &agents.AgentContext{
		Graph:    specResult.Graph,
		LLM:      nil,
		Registry: reg,
		Params:   map[string]string{"target": "python"},
	}
	archResult, err := arch.Run(ctx, archCtx)
	if err != nil {
		t.Fatalf("architect failed: %v", err)
	}

	if len(archResult.GeneratedFiles) == 0 {
		t.Fatal("architect generated no files")
	}

	// Verify Python files
	var hasManifest, hasPyproject, hasRunner bool
	var hasPythonService bool
	for _, f := range archResult.GeneratedFiles {
		switch f.Path {
		case "anvil.manifest.json":
			hasManifest = true
			content := string(f.Content)
			if !strings.Contains(content, `"language": "python"`) {
				t.Error("manifest missing python language field")
			}
		case "pyproject.toml":
			hasPyproject = true
		case "src/anvil_generated/runner.py":
			hasRunner = true
		}
		if strings.HasPrefix(f.Path, "src/anvil_generated/") && strings.HasSuffix(f.Path, ".py") && !strings.Contains(f.Path, "runner.py") && !strings.Contains(f.Path, "__init__.py") && !strings.Contains(f.Path, "model.py") {
			hasPythonService = true
			content := string(f.Content)
			if !strings.Contains(content, "class") {
				t.Errorf("service file %s missing class definition", f.Path)
			}
		}
	}

	if !hasManifest {
		t.Error("missing anvil.manifest.json")
	}
	if !hasPyproject {
		t.Error("missing pyproject.toml")
	}
	if !hasRunner {
		t.Error("missing runner.py")
	}
	if !hasPythonService {
		t.Error("missing Python service file")
	}

	// Judge (passthrough)
	judgeAgent := judge.New()
	filesJSON, _ := json.Marshal(archResult.GeneratedFiles)
	judgeCtx := &agents.AgentContext{
		Graph: archResult.Graph,
		LLM:   nil,
		Params: map[string]string{
			"source":          "cobol",
			"target":          "python",
			"generated_files": string(filesJSON),
		},
	}
	judgeResult, err := judgeAgent.Run(ctx, judgeCtx)
	if err != nil {
		t.Fatalf("judge failed: %v", err)
	}
	if judgeResult.Score < 0 || judgeResult.Score > 1 {
		t.Errorf("expected score in [0,1], got %f", judgeResult.Score)
	}
	t.Logf("Judge score: %f", judgeResult.Score)
}

func TestE2E_COBOLToGo_TemplateMode(t *testing.T) {
	ctx := context.Background()

	tmpDir := t.TempDir()
	cobolSource := `       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADDER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-A PIC 9(3).
       01 WS-B PIC 9(3).
       01 WS-SUM PIC 9(4).
       PROCEDURE DIVISION.
           COMPUTE WS-SUM = WS-A + WS-B.
           DISPLAY "SUM: " WS-SUM.
           STOP RUN.
`
	cobolPath := filepath.Join(tmpDir, "adder.cbl")
	if err := os.WriteFile(cobolPath, []byte(cobolSource), 0o644); err != nil {
		t.Fatal(err)
	}

	reg := plugins.NewRegistry()
	reg.RegisterSource(cobolplugin.New())
	reg.RegisterTarget(golangplugin.New())

	// Cartographer
	carto := cartographer.New()
	cartoCtx := &agents.AgentContext{
		Registry: reg,
		Params:   map[string]string{"source": "cobol", "input": tmpDir},
	}
	cartoResult, err := carto.Run(ctx, cartoCtx)
	if err != nil {
		t.Fatalf("cartographer failed: %v", err)
	}

	// Specular (passthrough)
	spec := specular.New()
	specCtx := &agents.AgentContext{
		Graph:    cartoResult.Graph,
		LLM:      nil,
		Registry: reg,
	}
	specResult, err := spec.Run(ctx, specCtx)
	if err != nil {
		t.Fatalf("specular failed: %v", err)
	}

	// Architect (template mode)
	arch := architect.New()
	archCtx := &agents.AgentContext{
		Graph:    specResult.Graph,
		LLM:      nil,
		Registry: reg,
		Params:   map[string]string{"target": "go"},
	}
	archResult, err := arch.Run(ctx, archCtx)
	if err != nil {
		t.Fatalf("architect failed: %v", err)
	}

	if len(archResult.GeneratedFiles) == 0 {
		t.Fatal("architect generated no files")
	}

	// Verify Go files
	var hasManifest, hasGoMod, hasRunner bool
	var hasGoService bool
	for _, f := range archResult.GeneratedFiles {
		switch f.Path {
		case "anvil.manifest.json":
			hasManifest = true
			content := string(f.Content)
			if !strings.Contains(content, `"language": "go"`) {
				t.Error("manifest missing go language field")
			}
		case "go.mod":
			hasGoMod = true
		case "cmd/anvil_runner/main.go":
			hasRunner = true
		}
		if strings.HasPrefix(f.Path, "generated/") && strings.HasSuffix(f.Path, ".go") && f.Path != "generated/model.go" {
			hasGoService = true
			content := string(f.Content)
			if !strings.Contains(content, "type") || !strings.Contains(content, "Service") {
				t.Errorf("service file %s missing type Service definition", f.Path)
			}
		}
	}

	if !hasManifest {
		t.Error("missing anvil.manifest.json")
	}
	if !hasGoMod {
		t.Error("missing go.mod")
	}
	if !hasRunner {
		t.Error("missing anvil_runner main.go")
	}
	if !hasGoService {
		t.Error("missing Go service file")
	}

	// Judge (passthrough)
	judgeAgent := judge.New()
	filesJSON, _ := json.Marshal(archResult.GeneratedFiles)
	judgeCtx := &agents.AgentContext{
		Graph: archResult.Graph,
		LLM:   nil,
		Params: map[string]string{
			"source":          "cobol",
			"target":          "go",
			"generated_files": string(filesJSON),
		},
	}
	judgeResult, err := judgeAgent.Run(ctx, judgeCtx)
	if err != nil {
		t.Fatalf("judge failed: %v", err)
	}
	if judgeResult.Score < 0 || judgeResult.Score > 1 {
		t.Errorf("expected score in [0,1], got %f", judgeResult.Score)
	}
	t.Logf("Judge score: %f", judgeResult.Score)
}

func TestE2E_Pipeline_NilGraph(t *testing.T) {
	ctx := context.Background()

	reg := plugins.NewRegistry()
	reg.RegisterSource(cobolplugin.New())
	reg.RegisterTarget(tsplugin.New())

	// Try to run Specular with nil graph
	spec := specular.New()
	specCtx := &agents.AgentContext{
		Graph:    nil, // nil graph
		LLM:      nil,
		Registry: reg,
	}
	_, err := spec.Run(ctx, specCtx)
	if err == nil {
		t.Error("expected error when running specular with nil graph")
	}

	// Try to run Architect with nil graph
	arch := architect.New()
	archCtx := &agents.AgentContext{
		Graph:    nil, // nil graph
		LLM:      nil,
		Registry: reg,
		Params:   map[string]string{"target": "typescript"},
	}
	_, err = arch.Run(ctx, archCtx)
	if err == nil {
		t.Error("expected error when running architect with nil graph")
	}
}

func TestE2E_Pipeline_EmptyGraph(t *testing.T) {
	ctx := context.Background()

	reg := plugins.NewRegistry()
	reg.RegisterSource(cobolplugin.New())
	reg.RegisterTarget(tsplugin.New())

	// Create empty graph (no modules)
	emptyGraph := &ir.SemanticGraph{
		Modules:   []*ir.Module{}, // empty modules
		CallGraph: &ir.CallGraph{Edges: []ir.CallEdge{}},
		Metadata:  map[string]string{},
	}

	// Specular should handle empty graph gracefully
	spec := specular.New()
	specCtx := &agents.AgentContext{
		Graph:    emptyGraph,
		LLM:      nil,
		Registry: reg,
	}
	specResult, err := spec.Run(ctx, specCtx)
	if err != nil {
		t.Fatalf("specular failed with empty graph: %v", err)
	}
	if specResult.Status != agents.StatusPassthrough {
		t.Errorf("expected passthrough status, got %s", specResult.Status)
	}

	// Architect should handle empty graph (generate only scaffold files)
	arch := architect.New()
	archCtx := &agents.AgentContext{
		Graph:    emptyGraph,
		LLM:      nil,
		Registry: reg,
		Params:   map[string]string{"target": "typescript"},
	}
	archResult, err := arch.Run(ctx, archCtx)
	if err != nil {
		t.Fatalf("architect failed with empty graph: %v", err)
	}

	// Should have scaffold files even with empty graph
	var hasManifest bool
	for _, f := range archResult.GeneratedFiles {
		if f.Path == "anvil.manifest.json" {
			hasManifest = true
		}
	}
	if !hasManifest {
		t.Error("architect should generate manifest even with empty graph")
	}

	// Judge should handle empty graph
	filesJSON, _ := json.Marshal(archResult.GeneratedFiles)
	judgeAgent := judge.New()
	judgeCtx := &agents.AgentContext{
		Graph: emptyGraph,
		LLM:   nil,
		Params: map[string]string{
			"source":          "cobol",
			"target":          "typescript",
			"generated_files": string(filesJSON),
		},
	}
	judgeResult, err := judgeAgent.Run(ctx, judgeCtx)
	if err != nil {
		t.Fatalf("judge failed with empty graph: %v", err)
	}
	if judgeResult.Score < 0 || judgeResult.Score > 1 {
		t.Errorf("expected score in [0,1], got %f", judgeResult.Score)
	}
	t.Logf("Judge score for empty graph: %f", judgeResult.Score)
}
