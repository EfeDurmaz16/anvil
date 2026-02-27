package cli

import (
	"context"
	"encoding/json"
	"fmt"
	"log/slog"
	"net/http"
	"os"
	"os/signal"
	"syscall"
	"time"

	"github.com/efebarandurmaz/anvil/internal/harness"
	"github.com/spf13/cobra"
)

// RunHarness executes fixtures against generated code and produces a proof pack.
func RunHarness(fixturesPath, codePath, outputDir string, jsonOutput bool) error {
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
	slog.Info("fixtures loaded", "count", len(fixtures), "path", fixturesPath)

	// Create manifest-based runner (target-language-agnostic).
	runnerConfig := harness.DefaultRunnerConfig()
	runner, err := harness.NewManifestRunner(codePath, runnerConfig)
	if err != nil {
		return fmt.Errorf("load manifest runner: %w", err)
	}
	defer runner.Cleanup()
	slog.Info("runner selected", "runner", runner.Name())

	// Compile
	slog.Info("compiling")
	compileResult, err := runner.Compile(ctx, codePath)
	if err != nil {
		return fmt.Errorf("compile: %w", err)
	}
	if !compileResult.Success {
		slog.Error("compilation failed", "errors", compileResult.Errors)
		return fmt.Errorf("compilation failed")
	}
	slog.Info("compilation succeeded", "duration", compileResult.Duration)

	// Run fixtures and build proof pack
	slog.Info("running fixtures")
	proofPack := harness.NewProofPack()
	defaultRules := &harness.NormalizeRules{}

	for _, fixture := range fixtures {
		result, err := runner.RunFixture(ctx, codePath, fixture)
		if err != nil {
			slog.Warn("fixture failed", "fixture", fixture.Name, "error", err)
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
			slog.Info("fixture passed", "fixture", fixture.Name)
		} else {
			slog.Warn("fixture failed", "fixture", fixture.Name, "reason", diff.Reason)
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
		slog.Info("proof pack written", "path", outputDir)
	}

	if !proofPack.Pass {
		return fmt.Errorf("harness failed: %d/%d fixtures passed", proofPack.PassCount, proofPack.FixtureCount)
	}
	return nil
}

// RecordFixtures records fixtures from a live system endpoint.
func RecordFixtures(endpoint, outputPath string) error {
	slog.Info("starting fixture recorder", "endpoint", endpoint, "output", outputPath)

	rec, err := harness.NewRecorder(&harness.RecorderConfig{
		TargetURL:  endpoint,
		ListenAddr: ":8090",
		OutputPath: outputPath,
	})
	if err != nil {
		return fmt.Errorf("create recorder: %w", err)
	}

	fmt.Printf("\nAnvil Fixture Recorder\n")
	fmt.Printf("  Proxy listening on: http://localhost:8090\n")
	fmt.Printf("  Forwarding to:      %s\n", endpoint)
	fmt.Printf("  Recording to:       %s\n", outputPath)
	fmt.Printf("\nSend requests to http://localhost:8090 to record fixtures.\n")
	fmt.Printf("Press Ctrl+C to stop recording.\n\n")

	// Handle shutdown signal
	ctx, stop := signal.NotifyContext(context.Background(), os.Interrupt, syscall.SIGTERM)
	defer stop()

	go func() {
		<-ctx.Done()
		shutdownCtx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		rec.Stop(shutdownCtx)
	}()

	err = rec.Start()
	if err != nil && err != http.ErrServerClosed {
		return fmt.Errorf("recorder error: %w", err)
	}

	fmt.Printf("\nRecording complete. %d fixtures written to %s\n", rec.Count(), outputPath)
	return nil
}

// CompareOutputs compares actual vs expected outputs.
func CompareOutputs(actualPath, expectedPath string, jsonOutput bool) error {
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

// GenerateProofPack generates a proof pack from fixtures and results.
func GenerateProofPack(fixturesPath, resultsPath, outputPath string) error {
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

// RegisterHarnessCommands registers all harness and proof-pack subcommands.
func RegisterHarnessCommands(parent *cobra.Command) {
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
			return RunHarness(fixturesPath, codePath, outputDir, jsonOutput)
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
			return RecordFixtures(recordEndpoint, recordOutput)
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
			return CompareOutputs(compareActual, compareExpected, jsonOutput)
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
			return GenerateProofPack(ppFixturesPath, ppResultsPath, ppOutputPath)
		},
	}
	proofPackGenerateCmd.Flags().StringVar(&ppFixturesPath, "fixtures", "", "Path to fixtures JSONL file")
	proofPackGenerateCmd.Flags().StringVar(&ppResultsPath, "results", "", "Path to harness results JSON")
	proofPackGenerateCmd.Flags().StringVar(&ppOutputPath, "output", "proof-pack", "Output directory for proof pack")
	_ = proofPackGenerateCmd.MarkFlagRequired("fixtures")
	_ = proofPackGenerateCmd.MarkFlagRequired("results")

	proofPackCmd.AddCommand(proofPackGenerateCmd)

	parent.AddCommand(harnessCmd, proofPackCmd)
}
