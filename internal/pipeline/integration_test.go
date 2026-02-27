//go:build integration

package pipeline

import (
	"context"
	"os"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/config"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/llmutil"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	plugindefaults "github.com/efebarandurmaz/anvil/internal/plugins/defaults"
)

// TestIntegration_COBOLToTypeScript_WithLLM runs the full pipeline with a real
// LLM (Ollama local or configured provider). Skip if no LLM is available.
//
// Run with: go test -tags integration -run TestIntegration_COBOLToTypeScript_WithLLM ./internal/pipeline/...
func TestIntegration_COBOLToTypeScript_WithLLM(t *testing.T) {
	provider := os.Getenv("ANVIL_LLM_PROVIDER")
	if provider == "" || provider == "none" {
		t.Skip("ANVIL_LLM_PROVIDER not set or set to 'none'; skipping LLM integration test")
	}

	// Build LLM providers from env
	cfg := config.LLMConfig{
		Provider: provider,
		APIKey:   os.Getenv("ANVIL_LLM_API_KEY"),
		Model:    os.Getenv("ANVIL_LLM_MODEL"),
		BaseURL:  os.Getenv("ANVIL_LLM_BASE_URL"),
	}

	factory := llm.NewFactory()
	llmutil.RegisterDefaultProviders(factory)

	providers, err := llm.SetupProviders(cfg, factory)
	if err != nil {
		t.Fatalf("failed to setup LLM providers: %v", err)
	}

	// Verify the LLM is reachable
	if providers["default"] == nil {
		t.Skip("default LLM provider is nil; skipping")
	}

	// Setup
	registry := plugins.NewRegistry()
	plugindefaults.RegisterAllDefaults(registry)

	inputDir := "../../testdata/cobol"
	if _, err := os.Stat(inputDir); os.IsNotExist(err) {
		t.Skipf("testdata/cobol not found at %s; skipping", inputDir)
	}

	outputDir := t.TempDir()

	ctx := context.Background()
	result, err := Run(ctx, PipelineConfig{
		SourceLang: "cobol",
		TargetLang: "typescript",
		InputPath:  inputDir,
		OutputPath: outputDir,
		Config:     &config.Config{LLM: cfg},
		Registry:   registry,
		Providers:  providers,
	})
	if err != nil {
		t.Fatalf("pipeline failed: %v", err)
	}

	if len(result.Files) == 0 {
		t.Fatal("pipeline produced no files")
	}

	t.Logf("Pipeline produced %d files with score %.2f in %d iterations",
		len(result.Files), result.Score, result.Iterations)

	if result.Score < 0.6 {
		t.Errorf("expected judge score >= 0.6, got %.2f", result.Score)
	}

	// Verify output directory has files
	entries, err := os.ReadDir(outputDir)
	if err != nil {
		t.Fatalf("failed to read output dir: %v", err)
	}
	if len(entries) == 0 {
		t.Error("output directory is empty")
	}
}

// TestIntegration_COBOLToJava_WithLLM tests Java target with real LLM.
func TestIntegration_COBOLToJava_WithLLM(t *testing.T) {
	provider := os.Getenv("ANVIL_LLM_PROVIDER")
	if provider == "" || provider == "none" {
		t.Skip("ANVIL_LLM_PROVIDER not set; skipping")
	}

	cfg := config.LLMConfig{
		Provider: provider,
		APIKey:   os.Getenv("ANVIL_LLM_API_KEY"),
		Model:    os.Getenv("ANVIL_LLM_MODEL"),
		BaseURL:  os.Getenv("ANVIL_LLM_BASE_URL"),
	}

	factory := llm.NewFactory()
	llmutil.RegisterDefaultProviders(factory)

	providers, err := llm.SetupProviders(cfg, factory)
	if err != nil {
		t.Fatalf("failed to setup LLM providers: %v", err)
	}

	if providers["default"] == nil {
		t.Skip("default LLM provider is nil; skipping")
	}

	registry := plugins.NewRegistry()
	plugindefaults.RegisterAllDefaults(registry)

	inputDir := "../../testdata/cobol"
	if _, err := os.Stat(inputDir); os.IsNotExist(err) {
		t.Skipf("testdata/cobol not found; skipping")
	}

	outputDir := t.TempDir()

	ctx := context.Background()
	result, err := Run(ctx, PipelineConfig{
		SourceLang: "cobol",
		TargetLang: "java",
		InputPath:  inputDir,
		OutputPath: outputDir,
		Config:     &config.Config{LLM: cfg},
		Registry:   registry,
		Providers:  providers,
	})
	if err != nil {
		t.Fatalf("pipeline failed: %v", err)
	}

	t.Logf("COBOL→Java: %d files, score %.2f, %d iterations",
		len(result.Files), result.Score, result.Iterations)

	if result.Score < 0.5 {
		t.Errorf("expected judge score >= 0.5, got %.2f", result.Score)
	}
}
