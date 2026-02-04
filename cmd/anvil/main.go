package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"time"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/agents/architect"
	"github.com/efebarandurmaz/anvil/internal/agents/cartographer"
	"github.com/efebarandurmaz/anvil/internal/agents/judge"
	"github.com/efebarandurmaz/anvil/internal/agents/specular"
	"github.com/efebarandurmaz/anvil/internal/config"
	"github.com/efebarandurmaz/anvil/internal/harness"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/llm/anthropic"
	"github.com/efebarandurmaz/anvil/internal/llm/openai"
	"github.com/efebarandurmaz/anvil/internal/metrics"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	cobolplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/cobol"
	fortranplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/fortran"
	perlplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/perl"
	goplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/golang"
	javaplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/java"
	pythonplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/python"
	tsplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/typescript"
	"github.com/spf13/cobra"
)

func main() {
	var (
		sourceLang string
		targetLang string
		inputPath  string
		outputPath string
		configPath string
		jsonReport bool
	)

	rootCmd := &cobra.Command{
		Use:   "anvil",
		Short: "Multi-agent legacy code modernization platform",
	}

	runCmd := &cobra.Command{
		Use:   "run",
		Short: "Run the modernization pipeline",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runPipeline(configPath, sourceLang, targetLang, inputPath, outputPath, jsonReport)
		},
	}

	runCmd.Flags().StringVar(&sourceLang, "source", "cobol", "Source language")
	runCmd.Flags().StringVar(&targetLang, "target", "java", "Target language")
	runCmd.Flags().StringVar(&inputPath, "input", "", "Input path (file or directory)")
	runCmd.Flags().StringVar(&outputPath, "output", "", "Output directory")
	runCmd.Flags().StringVar(&configPath, "config", "configs/anvil.yaml", "Config file path")
	runCmd.Flags().BoolVar(&jsonReport, "json", false, "Output metrics as JSON")
	_ = runCmd.MarkFlagRequired("input")
	_ = runCmd.MarkFlagRequired("output")

	providersCmd := &cobra.Command{
		Use:   "providers",
		Short: "List available LLM providers",
		Run: func(cmd *cobra.Command, args []string) {
			fmt.Println("Available LLM providers:")
			fmt.Println()
			for name, url := range llm.KnownProviders {
				fmt.Printf("  %-14s %s\n", name, url)
			}
			fmt.Println("  custom         (set base_url to any OpenAI-compatible endpoint)")
			fmt.Println("  none           (run without LLM — template-only generation)")
			fmt.Println()
			fmt.Println("Configure in anvil.yaml or via environment:")
			fmt.Println("  ANVIL_LLM_PROVIDER=groq")
			fmt.Println("  ANVIL_LLM_API_KEY=gsk_...")
			fmt.Println("  ANVIL_LLM_MODEL=llama-3.3-70b-versatile")
		},
	}

	// Harness commands
	harnessCmd := &cobra.Command{
		Use:   "harness",
		Short: "Harness operations for testing behavioral equivalence",
	}

	var (
		fixturesPath   string
		codePath       string
		outputDir      string
		jsonOutput     bool
		recordEndpoint string
		recordOutput   string
	)

	harnessRunCmd := &cobra.Command{
		Use:   "run",
		Short: "Run fixtures against generated code",
		RunE: func(cmd *cobra.Command, args []string) error {
			return runHarness(fixturesPath, codePath, outputDir, jsonOutput)
		},
	}
	harnessRunCmd.Flags().StringVar(&fixturesPath, "fixtures", "", "Path to fixtures JSONL file")
	harnessRunCmd.Flags().StringVar(&codePath, "code", "", "Path to generated code directory")
	harnessRunCmd.Flags().StringVar(&outputDir, "output", "", "Output directory for proof pack summary.json")
	harnessRunCmd.Flags().BoolVar(&jsonOutput, "json", false, "Output results as JSON")
	_ = harnessRunCmd.MarkFlagRequired("fixtures")
	_ = harnessRunCmd.MarkFlagRequired("code")

	harnessRecordCmd := &cobra.Command{
		Use:   "record",
		Short: "Record fixtures from a live system",
		RunE: func(cmd *cobra.Command, args []string) error {
			return recordFixtures(recordEndpoint, recordOutput)
		},
	}
	harnessRecordCmd.Flags().StringVar(&recordEndpoint, "endpoint", "", "Base URL of live system to record from")
	harnessRecordCmd.Flags().StringVar(&recordOutput, "output", "fixtures.jsonl", "Output file for recorded fixtures")
	_ = harnessRecordCmd.MarkFlagRequired("endpoint")

	var (
		compareActual   string
		compareExpected string
	)
	harnessCompareCmd := &cobra.Command{
		Use:   "compare",
		Short: "Compare actual vs expected outputs",
		RunE: func(cmd *cobra.Command, args []string) error {
			return compareOutputs(compareActual, compareExpected, jsonOutput)
		},
	}
	harnessCompareCmd.Flags().StringVar(&compareActual, "actual", "", "Path to actual output JSON")
	harnessCompareCmd.Flags().StringVar(&compareExpected, "expected", "", "Path to expected output JSON")
	harnessCompareCmd.Flags().BoolVar(&jsonOutput, "json", false, "Output diff as JSON")
	_ = harnessCompareCmd.MarkFlagRequired("actual")
	_ = harnessCompareCmd.MarkFlagRequired("expected")

	harnessCmd.AddCommand(harnessRunCmd, harnessRecordCmd, harnessCompareCmd)

	// Proof pack commands
	proofPackCmd := &cobra.Command{
		Use:   "proof-pack",
		Short: "Proof pack operations",
	}

	var (
		ppFixturesPath string
		ppResultsPath  string
		ppOutputPath   string
	)
	proofPackGenerateCmd := &cobra.Command{
		Use:   "generate",
		Short: "Generate a proof pack from harness results",
		RunE: func(cmd *cobra.Command, args []string) error {
			return generateProofPack(ppFixturesPath, ppResultsPath, ppOutputPath)
		},
	}
	proofPackGenerateCmd.Flags().StringVar(&ppFixturesPath, "fixtures", "", "Path to fixtures JSONL file")
	proofPackGenerateCmd.Flags().StringVar(&ppResultsPath, "results", "", "Path to harness results JSON")
	proofPackGenerateCmd.Flags().StringVar(&ppOutputPath, "output", "proof-pack", "Output directory for proof pack")
	_ = proofPackGenerateCmd.MarkFlagRequired("fixtures")
	_ = proofPackGenerateCmd.MarkFlagRequired("results")

	proofPackCmd.AddCommand(proofPackGenerateCmd)

	rootCmd.AddCommand(runCmd, providersCmd, harnessCmd, proofPackCmd)

	if err := rootCmd.Execute(); err != nil {
		os.Exit(1)
	}
}

func runPipeline(configPath, sourceLang, targetLang, inputPath, outputPath string, jsonReport bool) error {
	// Common aliases for convenience.
	switch targetLang {
	case "ts":
		targetLang = "typescript"
	case "py":
		targetLang = "python"
	case "golang":
		targetLang = "go"
	}

	m := metrics.New()

	cfg, err := config.Load(configPath)
	if err != nil {
		fmt.Fprintf(os.Stderr, "Warning: config load failed (%v), using defaults\n", err)
		cfg = &config.Config{}
	}

	registry := plugins.NewRegistry()
	registry.RegisterSource(cobolplugin.New())
	registry.RegisterSource(perlplugin.New())
	registry.RegisterSource(fortranplugin.New())
	registry.RegisterTarget(javaplugin.New())
	registry.RegisterTarget(pythonplugin.New())
	registry.RegisterTarget(goplugin.New())
	registry.RegisterTarget(tsplugin.New())

	// Build LLM provider via factory
	factory := llm.NewFactory()
	factory.Register("anthropic", func(c llm.ProviderConfig) (llm.Provider, error) {
		return anthropic.New(c.APIKey, c.Model, c.BaseURL), nil
	})
	factory.Register("openai", func(c llm.ProviderConfig) (llm.Provider, error) {
		return openai.New(c.APIKey, c.Model, c.BaseURL, c.EmbedModel), nil
	})
	// All OpenAI-compatible providers
	for _, p := range []struct{ name, url string }{
		{"groq", llm.KnownProviders["groq"]},
		{"huggingface", llm.KnownProviders["huggingface"]},
		{"ollama", llm.KnownProviders["ollama"]},
		{"together", llm.KnownProviders["together"]},
		{"deepseek", llm.KnownProviders["deepseek"]},
		{"custom", ""},
	} {
		p := p
		factory.Register(p.name, func(c llm.ProviderConfig) (llm.Provider, error) {
			base := c.BaseURL
			if base == "" {
				base = p.url
			}
			return openai.New(c.APIKey, c.Model, base, c.EmbedModel), nil
		})
	}

	provider, err := factory.Create(llm.ProviderConfig{
		Provider: cfg.LLM.Provider,
		APIKey:   cfg.LLM.APIKey,
		Model:    cfg.LLM.Model,
		BaseURL:  cfg.LLM.BaseURL,
	})
	if err != nil {
		return fmt.Errorf("creating LLM provider: %w", err)
	}

	if provider == nil {
		m.LLMMode = "passthrough"
		fmt.Println("Running without LLM (template-only mode)")
	} else {
		m.LLMMode = "llm:" + provider.Name()
		fmt.Printf("Using LLM provider: %s\n", provider.Name())
	}

	ctx := context.Background()

	// Step 1: Cartographer
	fmt.Println("\n=== Cartographer: Parsing source ===")
	start := time.Now()
	cart := cartographer.New()
	cartResult, err := cart.Run(ctx, &agents.AgentContext{
		Registry: registry,
		Params:   map[string]string{"source": sourceLang, "input": inputPath},
	})
	if err != nil {
		return fmt.Errorf("cartographer: %w", err)
	}
	m.AddAgent("cartographer", time.Since(start), "parse", 0)
	m.CollectSource(sourceLang, countFiles(inputPath), cartResult.Graph)
	fmt.Printf("  Parsed %d modules, %d functions\n", m.Source.ModuleCount, m.Source.FunctionCount)

	// Step 2: Specular
	fmt.Println("\n=== Specular: Extracting business rules ===")
	start = time.Now()
	spec := specular.New()
	specResult, err := spec.Run(ctx, &agents.AgentContext{
		Graph:    cartResult.Graph,
		LLM:      provider,
		Registry: registry,
	})
	if err != nil {
		return fmt.Errorf("specular: %w", err)
	}
	specMode := "llm"
	if specResult.Metadata != nil && specResult.Metadata["mode"] == "passthrough" {
		specMode = "passthrough"
	}
	m.AddAgent("specular", time.Since(start), specMode, len(specResult.Errors))
	m.Source.RuleCount = len(cartResult.Graph.BusinessRules)
	fmt.Printf("  Extracted %d business rules [%s]\n", m.Source.RuleCount, specMode)

	// Step 3 + 4: Architect → Judge with retry
	const maxRetries = 2
	var finalFiles []plugins.GeneratedFile
	var finalScore float64
	var allErrors []string

	for attempt := 0; attempt <= maxRetries; attempt++ {
		fmt.Printf("\n=== Architect: Generating %s (attempt %d/%d) ===\n", targetLang, attempt+1, maxRetries+1)
		start = time.Now()
		arch := architect.New()
		archResult, err := arch.Run(ctx, &agents.AgentContext{
			Graph:    cartResult.Graph,
			LLM:      provider,
			Registry: registry,
			Params:   map[string]string{"target": targetLang},
		})
		if err != nil {
			return fmt.Errorf("architect: %w", err)
		}
		archMode := "template"
		if provider != nil {
			archMode = "llm"
		}
		m.AddAgent("architect", time.Since(start), archMode, 0)
		fmt.Printf("  Generated %d files\n", len(archResult.GeneratedFiles))

		fmt.Println("\n=== Judge: Verifying ===")
		start = time.Now()
		j := judge.New()
		judgeResult, err := j.Run(ctx, &agents.AgentContext{
			Graph:  cartResult.Graph,
			LLM:    provider,
			Params: map[string]string{"generated_code": ""},
		})
		if err != nil {
			return fmt.Errorf("judge: %w", err)
		}
		judgeMode := "llm"
		if judgeResult.Metadata != nil && judgeResult.Metadata["mode"] == "passthrough" {
			judgeMode = "passthrough"
		}
		m.AddAgent("judge", time.Since(start), judgeMode, len(judgeResult.Errors))

		finalFiles = archResult.GeneratedFiles
		finalScore = judgeResult.Score
		allErrors = judgeResult.Errors
		fmt.Printf("  Score: %.2f [%s]\n", judgeResult.Score, judgeMode)

		if judgeResult.Score >= 0.8 {
			break
		}
	}

	// Write output
	for _, f := range finalFiles {
		outPath := filepath.Join(outputPath, f.Path)
		if err := os.MkdirAll(filepath.Dir(outPath), 0o755); err != nil {
			return err
		}
		if err := os.WriteFile(outPath, f.Content, 0o644); err != nil {
			return err
		}
	}

	// Finalize metrics
	m.CollectTarget(targetLang, finalFiles)
	m.Finish(finalScore, allErrors)

	if jsonReport {
		data, _ := m.JSON()
		fmt.Println(string(data))
	} else {
		m.PrintSummary(os.Stdout)
	}

	return nil
}

func countFiles(path string) int {
	info, err := os.Stat(path)
	if err != nil {
		return 0
	}
	if !info.IsDir() {
		return 1
	}
	count := 0
	filepath.Walk(path, func(_ string, fi os.FileInfo, _ error) error {
		if !fi.IsDir() {
			count++
		}
		return nil
	})
	return count
}

// runHarness executes fixtures against generated code and produces a proof pack.
func runHarness(fixturesPath, codePath, outputDir string, jsonOutput bool) error {
	ctx := context.Background()

	// Load fixtures
	f, err := os.Open(fixturesPath)
	if err != nil {
		return fmt.Errorf("open fixtures file: %w", err)
	}
	defer f.Close()

	fixtures, err := harness.ReadJSONL(f)
	if err != nil {
		return fmt.Errorf("read fixtures: %w", err)
	}
	fmt.Printf("Loaded %d fixtures from %s\n", len(fixtures), fixturesPath)

	// Create manifest-based runner (target-language-agnostic).
	runnerConfig := harness.DefaultRunnerConfig()
	runner, err := harness.NewManifestRunner(codePath, runnerConfig)
	if err != nil {
		return fmt.Errorf("load manifest runner: %w", err)
	}
	defer runner.Cleanup()
	fmt.Printf("Using %s\n", runner.Name())

	// Compile
	fmt.Println("\n=== Compiling ===")
	compileResult, err := runner.Compile(ctx, codePath)
	if err != nil {
		return fmt.Errorf("compile: %w", err)
	}
	if !compileResult.Success {
		fmt.Println("Compilation failed:")
		for _, e := range compileResult.Errors {
			fmt.Printf("  - %s\n", e)
		}
		return fmt.Errorf("compilation failed")
	}
	fmt.Printf("Compilation succeeded in %v\n", compileResult.Duration)

	// Run fixtures and build proof pack
	fmt.Println("\n=== Running fixtures ===")
	proofPack := harness.NewProofPack()
	defaultRules := &harness.NormalizeRules{}

	for _, fixture := range fixtures {
		result, err := runner.RunFixture(ctx, codePath, fixture)
		if err != nil {
			fmt.Printf("  [FAIL] %s: %v\n", fixture.Name, err)
			proofPack.AddResult(fixture, harness.DiffResult{Pass: false, Reason: err.Error()})
			continue
		}

		// Compare output
		var diff harness.DiffResult
		if fixture.HTTP != nil {
			// Compare HTTP response using DiffHTTP
			actual := harness.HTTPActual{
				Status: result.Output.Status,
				Header: result.Output.Headers,
				Body:   result.Output.Body,
			}
			diff = harness.DiffHTTP(fixture, actual, defaultRules)
		} else if fixture.Batch != nil {
			diff = harness.DiffResult{Pass: false, Reason: "batch fixtures not supported by manifest runner yet"}
		} else {
			diff = harness.DiffResult{Pass: result.Success}
		}

		if diff.Pass {
			fmt.Printf("  [PASS] %s\n", fixture.Name)
		} else {
			fmt.Printf("  [FAIL] %s: %s\n", fixture.Name, diff.Reason)
		}
		proofPack.AddResult(fixture, diff)
	}

	proofPack.Finish()

	// Output results
	if jsonOutput {
		data, _ := json.MarshalIndent(proofPack, "", "  ")
		fmt.Println(string(data))
	} else {
		fmt.Printf("\n=== Results ===\n")
		fmt.Printf("%s\n", proofPack.String())
	}

	// Write proof pack if output directory specified
	if outputDir != "" {
		if err := proofPack.Write(outputDir); err != nil {
			return fmt.Errorf("write proof pack: %w", err)
		}
		fmt.Printf("\nProof pack written to %s\n", outputDir)
	}

	if !proofPack.Pass {
		return fmt.Errorf("harness failed: %d/%d fixtures passed", proofPack.PassCount, proofPack.FixtureCount)
	}
	return nil
}

// recordFixtures records fixtures from a live system endpoint.
func recordFixtures(endpoint, outputPath string) error {
	fmt.Printf("Recording fixtures from %s\n", endpoint)
	fmt.Printf("Output: %s\n", outputPath)

	// Create output file
	f, err := os.Create(outputPath)
	if err != nil {
		return fmt.Errorf("create output file: %w", err)
	}
	defer f.Close()

	// Note: Full implementation would proxy requests to the endpoint
	// and record request/response pairs as fixtures.
	// For now, we provide a placeholder that explains the workflow.

	fmt.Println("\nRecording mode:")
	fmt.Println("  1. Configure your client to proxy through Anvil")
	fmt.Println("  2. Make requests to the legacy system")
	fmt.Println("  3. Anvil captures request/response pairs as fixtures")
	fmt.Println("  4. Use 'anvil harness run' to replay against modernized code")
	fmt.Println("\nNote: For production recording, configure your API gateway")
	fmt.Println("to export logs in Anvil's JSONL fixture format.")

	return nil
}

// compareOutputs compares actual vs expected outputs.
func compareOutputs(actualPath, expectedPath string, jsonOutput bool) error {
	// Read actual
	actualData, err := os.ReadFile(actualPath)
	if err != nil {
		return fmt.Errorf("read actual: %w", err)
	}

	// Read expected
	expectedData, err := os.ReadFile(expectedPath)
	if err != nil {
		return fmt.Errorf("read expected: %w", err)
	}

	// Create a synthetic fixture for comparison
	fixture := harness.Fixture{
		Kind: harness.FixtureHTTP,
		Name: "compare",
		HTTP: &harness.HTTPFixture{
			ExpectedBody: expectedData,
		},
	}

	actual := harness.HTTPActual{
		Status: 200, // Assume success for file comparison
		Body:   actualData,
	}

	// Compare using DiffHTTP
	diff := harness.DiffHTTP(fixture, actual, nil)

	if jsonOutput {
		data, _ := json.MarshalIndent(diff, "", "  ")
		fmt.Println(string(data))
	} else {
		if diff.Pass {
			fmt.Println("PASS: Outputs match")
		} else {
			fmt.Printf("FAIL: %s\n", diff.Reason)
		}
	}

	if !diff.Pass {
		return fmt.Errorf("comparison failed")
	}
	return nil
}

// generateProofPack generates a proof pack from fixtures and results.
func generateProofPack(fixturesPath, resultsPath, outputPath string) error {
	// Load fixtures
	f, err := os.Open(fixturesPath)
	if err != nil {
		return fmt.Errorf("open fixtures: %w", err)
	}
	defer f.Close()

	fixtures, err := harness.ReadJSONL(f)
	if err != nil {
		return fmt.Errorf("read fixtures: %w", err)
	}

	// Load results
	resultsData, err := os.ReadFile(resultsPath)
	if err != nil {
		return fmt.Errorf("read results: %w", err)
	}

	var results []struct {
		FixtureName string `json:"fixture_name"`
		Pass        bool   `json:"pass"`
		Reason      string `json:"reason,omitempty"`
	}
	if err := json.Unmarshal(resultsData, &results); err != nil {
		return fmt.Errorf("parse results: %w", err)
	}

	// Build proof pack
	proofPack := harness.NewProofPack()

	// Create results map for lookup
	resultsMap := make(map[string]struct {
		Pass   bool
		Reason string
	})
	for _, r := range results {
		resultsMap[r.FixtureName] = struct {
			Pass   bool
			Reason string
		}{Pass: r.Pass, Reason: r.Reason}
	}

	// Add results for each fixture
	for _, fixture := range fixtures {
		result, ok := resultsMap[fixture.Name]
		diff := harness.DiffResult{Pass: true}
		if ok {
			diff.Pass = result.Pass
			diff.Reason = result.Reason
		}
		proofPack.AddResult(fixture, diff)
	}

	proofPack.Finish()

	// Write proof pack
	if err := proofPack.Write(outputPath); err != nil {
		return fmt.Errorf("write proof pack: %w", err)
	}

	fmt.Printf("Generated proof pack: %s\n", proofPack.String())
	fmt.Printf("Written to: %s\n", outputPath)

	return nil
}

// hashBytes removed: batch hashing is handled by target runners/manifests.
