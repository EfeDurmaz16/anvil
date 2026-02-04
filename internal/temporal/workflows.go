package temporal

import (
	"fmt"
	"time"

	"go.temporal.io/sdk/workflow"
)

const maxRetries = 2

// ModernizationInput holds the workflow parameters.
type ModernizationInput struct {
	SourceLang string
	TargetLang string
	InputPath  string
	OutputPath string

	// Harness configuration (optional)
	FixturesPath      string   // Path to JSONL fixtures file
	CriticalTables    []string // Tables to compare for DB diff
	DBConnectionStr   string   // Database connection string (if DB diff enabled)
	GenerateProofPack bool     // Whether to generate proof pack
}

// ModernizationOutput holds the workflow result.
type ModernizationOutput struct {
	OutputPath string
	Score      float64
	Errors     []string

	// Harness results
	HarnessScore    float64 // Combined harness score (0-1)
	CompileScore    float64 // Compilation gate score
	FunctionalScore float64 // Functional test score
	DBDiffScore     float64 // Database diff score
	ProofPackPath   string  // Path to generated proof pack
}

// ModernizationWorkflow orchestrates the 4-agent pipeline with harness testing.
func ModernizationWorkflow(ctx workflow.Context, input ModernizationInput) (*ModernizationOutput, error) {
	ao := workflow.ActivityOptions{
		StartToCloseTimeout: 10 * time.Minute,
	}
	ctx = workflow.WithActivityOptions(ctx, ao)

	// Step 1: Cartographer
	var cartResult ActivityResult
	if err := workflow.ExecuteActivity(ctx, CartographerActivity, input).Get(ctx, &cartResult); err != nil {
		return nil, fmt.Errorf("cartographer: %w", err)
	}

	// Step 2: Specular
	var specResult ActivityResult
	if err := workflow.ExecuteActivity(ctx, SpecularActivity, input, cartResult.GraphJSON).Get(ctx, &specResult); err != nil {
		return nil, fmt.Errorf("specular: %w", err)
	}

	// Step 3 + 4 + 5: Architect → Judge → Harness with retry loop
	var finalResult ActivityResult
	var harnessResult HarnessResult
	graphJSON := specResult.GraphJSON

	for attempt := 0; attempt <= maxRetries; attempt++ {
		// Generate code
		var archResult ActivityResult
		if err := workflow.ExecuteActivity(ctx, ArchitectActivity, input, graphJSON).Get(ctx, &archResult); err != nil {
			return nil, fmt.Errorf("architect attempt %d: %w", attempt, err)
		}

		// Semantic verification via LLM
		var judgeResult ActivityResult
		if err := workflow.ExecuteActivity(ctx, JudgeActivity, input, graphJSON, archResult.FilesJSON).Get(ctx, &judgeResult); err != nil {
			return nil, fmt.Errorf("judge attempt %d: %w", attempt, err)
		}

		finalResult = judgeResult

		// Run harness testing if fixtures are configured
		if input.FixturesPath != "" {
			harnessInput := HarnessInput{
				TargetLang:        input.TargetLang,
				FilesJSON:         archResult.FilesJSON,
				OutputPath:        input.OutputPath,
				FixturesPath:      input.FixturesPath,
				CriticalTables:    input.CriticalTables,
				DBConnectionStr:   input.DBConnectionStr,
				GenerateProofPack: input.GenerateProofPack,
			}

			if err := workflow.ExecuteActivity(ctx, HarnessActivity, harnessInput).Get(ctx, &harnessResult); err != nil {
				return nil, fmt.Errorf("harness attempt %d: %w", attempt, err)
			}

			// Combined score: Judge (semantic) + Harness (functional)
			combinedScore := (judgeResult.Score + harnessResult.OverallScore) / 2
			if combinedScore >= 0.8 {
				break
			}

			// Append harness errors for next iteration
			finalResult.Errors = append(finalResult.Errors, harnessResult.Errors...)
		} else {
			// No harness, use only Judge score
			if judgeResult.Score >= 0.8 {
				break
			}
		}

		// Feed errors back for next attempt
		graphJSON = judgeResult.GraphJSON
	}

	output := &ModernizationOutput{
		OutputPath: input.OutputPath,
		Score:      finalResult.Score,
		Errors:     finalResult.Errors,
	}

	// Include harness results if harness was run
	if input.FixturesPath != "" {
		output.HarnessScore = harnessResult.OverallScore
		output.CompileScore = harnessResult.CompileScore
		output.FunctionalScore = harnessResult.FunctionalScore
		output.DBDiffScore = harnessResult.DBDiffScore
		output.ProofPackPath = harnessResult.ProofPackPath
	}

	return output, nil
}
