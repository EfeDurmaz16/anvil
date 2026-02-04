package architect

import (
	"context"
	"fmt"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// Architect generates target language code from the enriched IR.
type Architect struct{}

func New() *Architect { return &Architect{} }

func (a *Architect) Name() string { return "architect" }

func (a *Architect) Run(ctx context.Context, ac *agents.AgentContext) (*agents.AgentResult, error) {
	result := agents.NewAgentResult()
	result.Graph = ac.Graph

	if ac.Graph == nil {
		result.Status = agents.StatusFailed
		result.AddError("architect: no graph provided")
		result.Finalize()
		return result, fmt.Errorf("architect: no graph provided")
	}

	// Count input items (functions to generate)
	totalFunctions := 0
	for _, mod := range ac.Graph.Modules {
		totalFunctions += len(mod.Functions)
	}
	result.Metrics.InputItems = totalFunctions

	targetLang := ac.Params["target"]
	result.Metadata["target_language"] = targetLang

	target, err := ac.Registry.Target(targetLang)
	if err != nil {
		result.Status = agents.StatusFailed
		result.AddError(fmt.Sprintf("registry lookup: %v", err))
		result.Finalize()
		return result, err
	}

	scaffoldFiles, err := target.Scaffold(ctx, ac.Graph)
	if err != nil {
		result.Status = agents.StatusFailed
		result.AddError(fmt.Sprintf("scaffolding: %v", err))
		result.Finalize()
		return result, fmt.Errorf("scaffolding: %w", err)
	}

	genFiles, err := target.Generate(ctx, ac.Graph, ac.LLM)
	if err != nil {
		result.Status = agents.StatusFailed
		result.AddError(fmt.Sprintf("generating: %v", err))
		result.Finalize()
		return result, fmt.Errorf("generating: %w", err)
	}

	var all []plugins.GeneratedFile
	all = append(all, scaffoldFiles...)
	all = append(all, genFiles...)

	result.GeneratedFiles = all
	result.Status = agents.StatusSuccess
	result.Metrics.OutputItems = len(all)
	result.Metadata["scaffold_files"] = fmt.Sprintf("%d", len(scaffoldFiles))
	result.Metadata["generated_files"] = fmt.Sprintf("%d", len(genFiles))

	result.Finalize()
	return result, nil
}
