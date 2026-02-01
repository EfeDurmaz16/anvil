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
}

// ModernizationOutput holds the workflow result.
type ModernizationOutput struct {
	OutputPath string
	Score      float64
	Errors     []string
}

// ModernizationWorkflow orchestrates the 4-agent pipeline.
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

	// Step 3 + 4: Architect → Judge with retry loop
	var finalResult ActivityResult
	graphJSON := specResult.GraphJSON

	for attempt := 0; attempt <= maxRetries; attempt++ {
		var archResult ActivityResult
		if err := workflow.ExecuteActivity(ctx, ArchitectActivity, input, graphJSON).Get(ctx, &archResult); err != nil {
			return nil, fmt.Errorf("architect attempt %d: %w", attempt, err)
		}

		var judgeResult ActivityResult
		if err := workflow.ExecuteActivity(ctx, JudgeActivity, input, graphJSON, archResult.FilesJSON).Get(ctx, &judgeResult); err != nil {
			return nil, fmt.Errorf("judge attempt %d: %w", attempt, err)
		}

		finalResult = judgeResult
		if judgeResult.Score >= 0.8 {
			break
		}
		// Feed errors back for next attempt
		graphJSON = judgeResult.GraphJSON
	}

	return &ModernizationOutput{
		OutputPath: input.OutputPath,
		Score:      finalResult.Score,
		Errors:     finalResult.Errors,
	}, nil
}
