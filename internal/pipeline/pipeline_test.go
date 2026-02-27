package pipeline

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/config"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	plugindefaults "github.com/efebarandurmaz/anvil/internal/plugins/defaults"
)

func defaultConfig() *config.Config {
	return &config.Config{}
}

func TestRun_TemplateMode(t *testing.T) {
	inputDir := t.TempDir()
	outputDir := t.TempDir()

	cobolSrc := `       IDENTIFICATION DIVISION.
       PROGRAM-ID. TEST-PROG.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01 WS-NUM PIC 9(3).
       PROCEDURE DIVISION.
           DISPLAY "HELLO".
           STOP RUN.`

	if err := os.WriteFile(filepath.Join(inputDir, "test.cbl"), []byte(cobolSrc), 0644); err != nil {
		t.Fatalf("write test COBOL file: %v", err)
	}

	registry := plugins.NewRegistry()
	plugindefaults.RegisterAllDefaults(registry)

	cfg := PipelineConfig{
		SourceLang: "cobol",
		TargetLang: "typescript",
		InputPath:  inputDir,
		OutputPath: outputDir,
		Config:     defaultConfig(),
		Registry:   registry,
		Providers:  map[string]llm.Provider{}, // no "default" key → template mode
	}

	result, err := Run(context.Background(), cfg)
	if err != nil {
		t.Fatalf("Run() error: %v", err)
	}
	if result == nil {
		t.Fatal("expected non-nil result")
	}
	if len(result.Files) == 0 {
		t.Error("expected at least one generated file in template mode")
	}
}

func TestRun_MissingInput(t *testing.T) {
	registry := plugins.NewRegistry()
	plugindefaults.RegisterAllDefaults(registry)

	cfg := PipelineConfig{
		SourceLang: "cobol",
		TargetLang: "typescript",
		InputPath:  "/nonexistent/path/anvil-test-xyz",
		OutputPath: t.TempDir(),
		Config:     defaultConfig(),
		Registry:   registry,
		Providers:  map[string]llm.Provider{},
	}

	_, err := Run(context.Background(), cfg)
	if err == nil {
		t.Error("expected error for missing input path")
	}
}

func TestRun_InvalidLanguage(t *testing.T) {
	inputDir := t.TempDir()
	// Write a dummy file so cartographer doesn't bail on empty dir.
	if err := os.WriteFile(filepath.Join(inputDir, "dummy.xyz"), []byte("data"), 0644); err != nil {
		t.Fatalf("write dummy file: %v", err)
	}

	registry := plugins.NewRegistry()
	plugindefaults.RegisterAllDefaults(registry)

	cfg := PipelineConfig{
		SourceLang: "invalidlang",
		TargetLang: "typescript",
		InputPath:  inputDir,
		OutputPath: t.TempDir(),
		Config:     defaultConfig(),
		Registry:   registry,
		Providers:  map[string]llm.Provider{},
	}

	_, err := Run(context.Background(), cfg)
	if err == nil {
		t.Error("expected error for invalid source language")
	}
}

func TestRun_TargetLangAlias(t *testing.T) {
	// Verify alias resolution does not panic; use missing input to get an early error.
	registry := plugins.NewRegistry()
	plugindefaults.RegisterAllDefaults(registry)

	aliases := []string{"ts", "py", "golang"}
	for _, alias := range aliases {
		cfg := PipelineConfig{
			SourceLang: "cobol",
			TargetLang: alias,
			InputPath:  "/nonexistent/path/anvil-alias-test",
			OutputPath: t.TempDir(),
			Config:     defaultConfig(),
			Registry:   registry,
			Providers:  map[string]llm.Provider{},
		}
		// We only care that it doesn't panic; error is expected due to bad input.
		_, _ = Run(context.Background(), cfg)
	}
}
