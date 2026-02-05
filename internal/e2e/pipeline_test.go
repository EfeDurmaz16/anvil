package e2e

import (
	"context"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/agents/architect"
	"github.com/efebarandurmaz/anvil/internal/agents/cartographer"
	"github.com/efebarandurmaz/anvil/internal/agents/specular"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	cobolplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/cobol"
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
}
