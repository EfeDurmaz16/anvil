package agents

import (
	"context"

	"github.com/efebarandurmaz/anvil/internal/graph"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	"github.com/efebarandurmaz/anvil/internal/vector"
)

// Agent is the interface for all pipeline agents.
type Agent interface {
	// Name returns the agent identifier.
	Name() string
	// Run executes the agent's task.
	Run(ctx context.Context, ac *AgentContext) (*AgentResult, error)
}

// AgentContext provides shared resources to agents.
type AgentContext struct {
	Graph    *ir.SemanticGraph
	LLM      llm.Provider
	GraphDB  graph.Repository
	VectorDB vector.Repository
	Registry *plugins.Registry
	Params   map[string]string
}

// AgentResult captures agent output.
type AgentResult struct {
	Graph          *ir.SemanticGraph
	GeneratedFiles []plugins.GeneratedFile
	Score          float64
	Errors         []string
	Metadata       map[string]string
}
