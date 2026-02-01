package temporal

import (
	"context"
	"encoding/json"
	"fmt"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/agents/architect"
	"github.com/efebarandurmaz/anvil/internal/agents/cartographer"
	"github.com/efebarandurmaz/anvil/internal/agents/judge"
	"github.com/efebarandurmaz/anvil/internal/agents/specular"
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

	gOut, _ := json.Marshal(result.Graph)
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
			"generated_code": filesJSON,
		},
	}

	result, err := agent.Run(ctx, ac)
	if err != nil {
		return ActivityResult{}, err
	}

	gOut, _ := json.Marshal(result.Graph)
	return ActivityResult{
		GraphJSON: string(gOut),
		Score:     result.Score,
		Errors:    result.Errors,
	}, nil
}
