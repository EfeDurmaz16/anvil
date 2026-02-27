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
	fortranplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/fortran"
	perlplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/perl"
	golangplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/golang"
	javaplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/java"
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

// runFullPipeline is a helper that runs Cartographer → Specular → Architect → Judge
// in template mode (nil LLM) and returns the architect result and judge score.
func runFullPipeline(t *testing.T, reg *plugins.Registry, source, target, inputDir string) (*agents.AgentResult, float64) {
	t.Helper()
	ctx := context.Background()

	// Cartographer
	carto := cartographer.New()
	cartoResult, err := carto.Run(ctx, &agents.AgentContext{
		Registry: reg,
		Params:   map[string]string{"source": source, "input": inputDir},
	})
	if err != nil {
		t.Fatalf("cartographer failed: %v", err)
	}
	if cartoResult.Graph == nil {
		t.Fatal("cartographer returned nil graph")
	}

	// Specular (passthrough)
	spec := specular.New()
	specResult, err := spec.Run(ctx, &agents.AgentContext{
		Graph:    cartoResult.Graph,
		LLM:     nil,
		Registry: reg,
	})
	if err != nil {
		t.Fatalf("specular failed: %v", err)
	}

	// Architect (template mode)
	arch := architect.New()
	archResult, err := arch.Run(ctx, &agents.AgentContext{
		Graph:    specResult.Graph,
		LLM:     nil,
		Registry: reg,
		Params:   map[string]string{"target": target},
	})
	if err != nil {
		t.Fatalf("architect failed: %v", err)
	}
	if len(archResult.GeneratedFiles) == 0 {
		t.Fatal("architect generated no files")
	}

	// Judge (passthrough)
	filesJSON, _ := json.Marshal(archResult.GeneratedFiles)
	judgeAgent := judge.New()
	judgeResult, err := judgeAgent.Run(ctx, &agents.AgentContext{
		Graph: archResult.Graph,
		LLM:   nil,
		Params: map[string]string{
			"source":          source,
			"target":          target,
			"generated_files": string(filesJSON),
		},
	})
	if err != nil {
		t.Fatalf("judge failed: %v", err)
	}
	if judgeResult.Score < 0 || judgeResult.Score > 1 {
		t.Errorf("expected score in [0,1], got %f", judgeResult.Score)
	}

	return archResult, judgeResult.Score
}

func TestE2E_PerlToPython_TemplateMode(t *testing.T) {
	tmpDir := t.TempDir()
	perlSource := `package Calculator;
use strict;
use warnings;

sub new {
    my ($class, %args) = @_;
    return bless { result => 0 }, $class;
}

sub add {
    my ($self, $a, $b) = @_;
    $self->{result} = $a + $b;
    return $self->{result};
}

sub subtract {
    my ($self, $a, $b) = @_;
    $self->{result} = $a - $b;
    return $self->{result};
}

1;
`
	if err := os.WriteFile(filepath.Join(tmpDir, "Calculator.pm"), []byte(perlSource), 0o644); err != nil {
		t.Fatal(err)
	}

	reg := plugins.NewRegistry()
	reg.RegisterSource(perlplugin.New())
	reg.RegisterTarget(pythonplugin.New())

	archResult, score := runFullPipeline(t, reg, "perl", "python", tmpDir)

	var hasManifest, hasPyproject bool
	for _, f := range archResult.GeneratedFiles {
		switch f.Path {
		case "anvil.manifest.json":
			hasManifest = true
			if !strings.Contains(string(f.Content), `"language": "python"`) {
				t.Error("manifest missing python language")
			}
		case "pyproject.toml":
			hasPyproject = true
		}
	}
	if !hasManifest {
		t.Error("missing anvil.manifest.json")
	}
	if !hasPyproject {
		t.Error("missing pyproject.toml")
	}
	t.Logf("Perl→Python judge score: %f (%d files generated)", score, len(archResult.GeneratedFiles))
}

func TestE2E_FortranToGo_TemplateMode(t *testing.T) {
	tmpDir := t.TempDir()
	fortranSource := `program adder
    implicit none
    integer :: a, b, total

    a = 10
    b = 20
    call compute_sum(a, b, total)
    print *, "Sum:", total

contains

    subroutine compute_sum(x, y, result)
        integer, intent(in) :: x, y
        integer, intent(out) :: result
        result = x + y
    end subroutine compute_sum

end program adder
`
	if err := os.WriteFile(filepath.Join(tmpDir, "adder.f90"), []byte(fortranSource), 0o644); err != nil {
		t.Fatal(err)
	}

	reg := plugins.NewRegistry()
	reg.RegisterSource(fortranplugin.New())
	reg.RegisterTarget(golangplugin.New())

	archResult, score := runFullPipeline(t, reg, "fortran", "go", tmpDir)

	var hasManifest, hasGoMod bool
	for _, f := range archResult.GeneratedFiles {
		switch f.Path {
		case "anvil.manifest.json":
			hasManifest = true
			if !strings.Contains(string(f.Content), `"language": "go"`) {
				t.Error("manifest missing go language")
			}
		case "go.mod":
			hasGoMod = true
		}
	}
	if !hasManifest {
		t.Error("missing anvil.manifest.json")
	}
	if !hasGoMod {
		t.Error("missing go.mod")
	}
	t.Logf("Fortran→Go judge score: %f (%d files generated)", score, len(archResult.GeneratedFiles))
}

func TestE2E_COBOLToJava_TemplateMode(t *testing.T) {
	tmpDir := t.TempDir()
	cobolSource := `       IDENTIFICATION DIVISION.
       PROGRAM-ID. INVENTORY.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 ITEM-COUNT PIC 9(4).
       01 ITEM-PRICE PIC 9(5)V99.
       01 TOTAL-VALUE PIC 9(7)V99.
       PROCEDURE DIVISION.
       MAIN-PARAGRAPH.
           COMPUTE TOTAL-VALUE = ITEM-COUNT * ITEM-PRICE.
           DISPLAY "TOTAL: " TOTAL-VALUE.
           STOP RUN.
`
	if err := os.WriteFile(filepath.Join(tmpDir, "inventory.cbl"), []byte(cobolSource), 0o644); err != nil {
		t.Fatal(err)
	}

	reg := plugins.NewRegistry()
	reg.RegisterSource(cobolplugin.New())
	reg.RegisterTarget(javaplugin.New())

	archResult, score := runFullPipeline(t, reg, "cobol", "java", tmpDir)

	var hasManifest, hasPom, hasApplication, hasRunner, hasMvnw, hasMvnwCmd bool
	for _, f := range archResult.GeneratedFiles {
		switch f.Path {
		case "anvil.manifest.json":
			hasManifest = true
			content := string(f.Content)
			if !strings.Contains(content, `"language": "java"`) {
				t.Error("manifest missing java language")
			}
			if !strings.Contains(content, "./mvnw") {
				t.Error("manifest missing mvnw compile command")
			}
			if !strings.Contains(content, `"test"`) {
				t.Error("manifest missing test command")
			}
		case "pom.xml":
			hasPom = true
			if !strings.Contains(string(f.Content), "spring-boot-starter") {
				t.Error("pom.xml missing Spring Boot dependency")
			}
		case "mvnw":
			hasMvnw = true
			if !strings.Contains(string(f.Content), "#!/bin/sh") {
				t.Error("mvnw missing shebang")
			}
		case "mvnw.cmd":
			hasMvnwCmd = true
		case "src/main/java/com/anvil/generated/Application.java":
			hasApplication = true
		case "src/main/java/com/anvil/generated/AnvilRunner.java":
			hasRunner = true
		}
	}

	if !hasManifest {
		t.Error("missing anvil.manifest.json")
	}
	if !hasPom {
		t.Error("missing pom.xml")
	}
	if !hasMvnw {
		t.Error("missing mvnw (Maven wrapper)")
	}
	if !hasMvnwCmd {
		t.Error("missing mvnw.cmd (Maven wrapper for Windows)")
	}
	if !hasApplication {
		t.Error("missing Application.java")
	}
	if !hasRunner {
		t.Error("missing AnvilRunner.java")
	}
	t.Logf("COBOL→Java judge score: %f (%d files generated)", score, len(archResult.GeneratedFiles))
}

func TestE2E_MultiFileCOBOL_TemplateMode(t *testing.T) {
	tmpDir := t.TempDir()

	mainCbl := `       IDENTIFICATION DIVISION.
       PROGRAM-ID. MAIN-PROGRAM.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NAME PIC X(20).
       01 WS-GREETING PIC X(50).
       PROCEDURE DIVISION.
           MOVE "World" TO WS-NAME.
           STRING "Hello, " WS-NAME INTO WS-GREETING.
           DISPLAY WS-GREETING.
           STOP RUN.
`
	helperCbl := `       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELPER.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-RESULT PIC 9(5).
       PROCEDURE DIVISION.
       COMPUTE-AREA.
           COMPUTE WS-RESULT = 10 * 20.
           DISPLAY "AREA: " WS-RESULT.
           STOP RUN.
`
	os.WriteFile(filepath.Join(tmpDir, "main.cbl"), []byte(mainCbl), 0o644)
	os.WriteFile(filepath.Join(tmpDir, "helper.cbl"), []byte(helperCbl), 0o644)

	reg := plugins.NewRegistry()
	reg.RegisterSource(cobolplugin.New())
	reg.RegisterTarget(tsplugin.New())

	archResult, score := runFullPipeline(t, reg, "cobol", "typescript", tmpDir)

	// Should have generated service files for both modules
	serviceCount := 0
	for _, f := range archResult.GeneratedFiles {
		if strings.HasPrefix(f.Path, "src/generated/") && strings.HasSuffix(f.Path, ".ts") &&
			f.Path != "src/generated/index.ts" && f.Path != "src/generated/model.ts" {
			serviceCount++
		}
	}
	if serviceCount < 2 {
		t.Errorf("expected at least 2 service files for 2 COBOL programs, got %d", serviceCount)
	}
	t.Logf("Multi-file COBOL→TS judge score: %f (%d files, %d services)", score, len(archResult.GeneratedFiles), serviceCount)
}
