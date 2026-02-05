package temporal

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
	"github.com/efebarandurmaz/anvil/internal/harness"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// ActivityResult is the serializable result passed between activities.
type ActivityResult struct {
	GraphJSON string
	FilesJSON string
	Score     float64
	Errors    []string
}

// HarnessInput holds input for the harness testing activity.
type HarnessInput struct {
	TargetLang        string
	FilesJSON         string
	OutputPath        string
	FixturesPath      string
	CriticalTables    []string
	DBConnectionStr   string
	GenerateProofPack bool
}

// HarnessResult holds the result of harness testing.
type HarnessResult struct {
	OverallScore    float64  // Combined gate score (0-1)
	CompileScore    float64  // Compilation gate (0 or 1)
	FunctionalScore float64  // Functional test score (0-1)
	DBDiffScore     float64  // DB diff score (0-1, 1 = no violations)
	ProofPackPath   string   // Path to generated proof pack
	Errors          []string // All errors from harness testing
}

// Dependencies holds shared resources injected into activities.
type Dependencies struct {
	LLM      agents.AgentContext // Template for building per-activity contexts
	Registry *plugins.Registry
}

var deps *Dependencies

// SetDependencies injects shared resources (called during worker setup).
func SetDependencies(d *Dependencies) {
	deps = d
}

func CartographerActivity(ctx context.Context, input ModernizationInput) (ActivityResult, error) {
	agent := cartographer.New()
	ac := &agents.AgentContext{
		Registry: deps.Registry,
		Params: map[string]string{
			"source": input.SourceLang,
			"input":  input.InputPath,
		},
	}

	result, err := agent.Run(ctx, ac)
	if err != nil {
		return ActivityResult{}, err
	}

	graphJSON, err := json.Marshal(result.Graph)
	if err != nil {
		return ActivityResult{}, fmt.Errorf("marshal graph: %w", err)
	}

	return ActivityResult{GraphJSON: string(graphJSON)}, nil
}

func SpecularActivity(ctx context.Context, input ModernizationInput, graphJSON string) (ActivityResult, error) {
	var graph ir.SemanticGraph
	if err := json.Unmarshal([]byte(graphJSON), &graph); err != nil {
		return ActivityResult{}, err
	}

	agent := specular.New()
	ac := &agents.AgentContext{
		Graph:    &graph,
		LLM:      deps.LLM.LLM,
		Registry: deps.Registry,
		Params:   map[string]string{},
	}

	result, err := agent.Run(ctx, ac)
	if err != nil {
		return ActivityResult{}, err
	}

	out, err := json.Marshal(result.Graph)
	if err != nil {
		return ActivityResult{}, err
	}
	return ActivityResult{GraphJSON: string(out), Errors: result.Errors}, nil
}

func ArchitectActivity(ctx context.Context, input ModernizationInput, graphJSON string) (ActivityResult, error) {
	var graph ir.SemanticGraph
	if err := json.Unmarshal([]byte(graphJSON), &graph); err != nil {
		return ActivityResult{}, err
	}

	agent := architect.New()
	ac := &agents.AgentContext{
		Graph:    &graph,
		LLM:      deps.LLM.LLM,
		Registry: deps.Registry,
		Params: map[string]string{
			"target": input.TargetLang,
		},
	}

	result, err := agent.Run(ctx, ac)
	if err != nil {
		return ActivityResult{}, err
	}

	filesJSON, err := json.Marshal(result.GeneratedFiles)
	if err != nil {
		return ActivityResult{}, err
	}

	gOut, err := json.Marshal(result.Graph)
	if err != nil {
		return ActivityResult{}, fmt.Errorf("marshal graph: %w", err)
	}
	return ActivityResult{GraphJSON: string(gOut), FilesJSON: string(filesJSON)}, nil
}

func JudgeActivity(ctx context.Context, input ModernizationInput, graphJSON, filesJSON string) (ActivityResult, error) {
	var graph ir.SemanticGraph
	if err := json.Unmarshal([]byte(graphJSON), &graph); err != nil {
		return ActivityResult{}, err
	}

	agent := judge.New()
	ac := &agents.AgentContext{
		Graph: &graph,
		LLM:   deps.LLM.LLM,
		Params: map[string]string{
			"source":          input.SourceLang,
			"target":          input.TargetLang,
			"generated_files": filesJSON,
		},
	}

	result, err := agent.Run(ctx, ac)
	if err != nil {
		return ActivityResult{}, err
	}

	gOut, err := json.Marshal(result.Graph)
	if err != nil {
		return ActivityResult{}, fmt.Errorf("marshal graph: %w", err)
	}
	return ActivityResult{
		GraphJSON: string(gOut),
		Score:     result.Score,
		Errors:    result.Errors,
	}, nil
}

// HarnessActivity runs the test harness against generated code.
// It executes: Compile → Run Fixtures → DB Diff → Generate Proof Pack
func HarnessActivity(ctx context.Context, input HarnessInput) (HarnessResult, error) {
	result := HarnessResult{
		OverallScore:    0,
		CompileScore:    0,
		FunctionalScore: 0,
		DBDiffScore:     1.0, // Default to pass if DB diff not configured
	}

	// Deserialize generated files
	var files []plugins.GeneratedFile
	if err := json.Unmarshal([]byte(input.FilesJSON), &files); err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("unmarshal files: %v", err))
		return result, nil
	}

	// Write generated files to output directory
	workDir := filepath.Join(input.OutputPath, "harness-work")
	if err := os.MkdirAll(workDir, 0755); err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("create work dir: %v", err))
		return result, nil
	}

	for _, f := range files {
		outPath := filepath.Join(workDir, f.Path)
		if err := os.MkdirAll(filepath.Dir(outPath), 0755); err != nil {
			result.Errors = append(result.Errors, fmt.Sprintf("create dir for %s: %v", f.Path, err))
			continue
		}
		if err := os.WriteFile(outPath, f.Content, 0644); err != nil {
			result.Errors = append(result.Errors, fmt.Sprintf("write %s: %v", f.Path, err))
			continue
		}
	}

	// Use manifest-based runner (target-language-agnostic).
	runnerConfig := harness.DefaultRunnerConfig()
	runnerConfig.Timeout = 5 * time.Minute

	runner, err := harness.NewManifestRunner(workDir, runnerConfig)
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("load manifest runner: %v", err))
		return result, nil
	}
	defer runner.Cleanup()

	// Gate 1: Compilation
	compileResult, err := runner.Compile(ctx, workDir)
	if err != nil {
		result.Errors = append(result.Errors, fmt.Sprintf("compile error: %v", err))
		return result, nil
	}

	if !compileResult.Success {
		result.CompileScore = 0
		result.Errors = append(result.Errors, compileResult.Errors...)
		// Early exit: no point running fixtures if compilation fails
		return result, nil
	}
	result.CompileScore = 1.0

	// Gate 2: Functional Tests (run fixtures)
	if input.FixturesPath != "" {
		fixturesFile, err := os.Open(input.FixturesPath)
		if err != nil {
			result.Errors = append(result.Errors, fmt.Sprintf("open fixtures: %v", err))
		} else {
			fixtures, err := harness.ReadJSONL(fixturesFile)
			fixturesFile.Close()
			if err != nil {
				result.Errors = append(result.Errors, fmt.Sprintf("load fixtures: %v", err))
			} else {
				passed := 0
				total := len(fixtures)

				for _, fixture := range fixtures {
					runResult, err := runner.RunFixture(ctx, workDir, fixture)
					if err != nil {
						result.Errors = append(result.Errors, fmt.Sprintf("fixture %s: %v", fixture.Name, err))
						continue
					}
					if runResult.Success {
						passed++
					} else {
						result.Errors = append(result.Errors, fmt.Sprintf("fixture %s failed: %s", fixture.Name, runResult.Error))
					}
				}

				if total > 0 {
					result.FunctionalScore = float64(passed) / float64(total)
				} else {
					result.FunctionalScore = 1.0 // No fixtures = pass
				}
			}
		}
	} else {
		result.FunctionalScore = 1.0 // No fixtures configured = pass
	}

	// Gate 3: DB Diff (if configured)
	if len(input.CriticalTables) > 0 && input.DBConnectionStr != "" {
		dbConfig := &harness.DBDiffConfig{
			CriticalTables:   input.CriticalTables,
			ConnectionString: input.DBConnectionStr,
		}
		differ := harness.NewDBDiffer(dbConfig)

		if err := differ.Connect(ctx, input.DBConnectionStr); err != nil {
			result.Errors = append(result.Errors, fmt.Sprintf("db connect: %v", err))
			result.DBDiffScore = 0
		} else {
			defer differ.Close()

			// Take before snapshot
			beforeSnap, err := differ.TakeSnapshot(ctx)
			if err != nil {
				result.Errors = append(result.Errors, fmt.Sprintf("db snapshot before: %v", err))
				result.DBDiffScore = 0
			} else {
				// Run a representative fixture (first one) to see DB effects
				if input.FixturesPath != "" {
					if f, err := os.Open(input.FixturesPath); err == nil {
						fixtures, _ := harness.ReadJSONL(f)
						f.Close()
						if len(fixtures) > 0 {
							runner.RunFixture(ctx, workDir, fixtures[0])
						}
					}
				}

				// Take after snapshot
				afterSnap, err := differ.TakeSnapshot(ctx)
				if err != nil {
					result.Errors = append(result.Errors, fmt.Sprintf("db snapshot after: %v", err))
					result.DBDiffScore = 0
				} else {
					// Compare snapshots
					diffReport := differ.Compare(beforeSnap, afterSnap)
					if diffReport.TotalViolations == 0 {
						result.DBDiffScore = 1.0
					} else {
						// Deduct based on violations
						result.DBDiffScore = 1.0 - (float64(diffReport.TotalViolations) * 0.1)
						if result.DBDiffScore < 0 {
							result.DBDiffScore = 0
						}
						for _, table := range diffReport.Tables {
							for _, v := range table.Violations {
								result.Errors = append(result.Errors, fmt.Sprintf("db violation: %s", v))
							}
						}
					}
				}
			}
		}
	}

	// Calculate overall score (weighted average of gates)
	// Compilation is required (gate), others are weighted
	if result.CompileScore == 0 {
		result.OverallScore = 0
	} else {
		// 40% functional, 30% db diff, 30% compile (already passed)
		result.OverallScore = (result.CompileScore*0.3 + result.FunctionalScore*0.4 + result.DBDiffScore*0.3)
	}

	// Generate proof pack if requested
	if input.GenerateProofPack && result.OverallScore > 0 {
		proofDir := filepath.Join(input.OutputPath, "proof-pack")
		proofPack := harness.NewProofPack()

		// Load fixtures to record results
		if input.FixturesPath != "" {
			if f, err := os.Open(input.FixturesPath); err == nil {
				fixtures, _ := harness.ReadJSONL(f)
				f.Close()
				for _, fixture := range fixtures {
					// Create a diff result based on functional score
					diffResult := harness.DiffResult{
						Pass: result.FunctionalScore >= 0.8,
					}
					proofPack.AddResult(fixture, diffResult)
				}
			}
		}

		proofPack.Finish()
		if err := proofPack.Write(proofDir); err != nil {
			result.Errors = append(result.Errors, fmt.Sprintf("proof pack: %v", err))
		} else {
			result.ProofPackPath = proofDir
		}
	}

	return result, nil
}
