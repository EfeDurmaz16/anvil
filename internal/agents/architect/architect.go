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
	if ac.Graph == nil {
		return nil, fmt.Errorf("architect: no graph provided")
	}

	targetLang := ac.Params["target"]
	target, err := ac.Registry.Target(targetLang)
	if err != nil {
		return nil, err
	}

	scaffoldFiles, err := target.Scaffold(ctx, ac.Graph)
	if err != nil {
		return nil, fmt.Errorf("scaffolding: %w", err)
	}

	genFiles, err := target.Generate(ctx, ac.Graph, ac.LLM)
	if err != nil {
		return nil, fmt.Errorf("generating: %w", err)
	}

	var all []plugins.GeneratedFile
	all = append(all, scaffoldFiles...)
	all = append(all, genFiles...)

	return &agents.AgentResult{
		Graph:          ac.Graph,
		GeneratedFiles: all,
	}, nil
}
