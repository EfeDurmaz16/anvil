package plugins

import (
	"context"

	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/llm"
)

// GeneratedFile is a single output file produced by a target plugin.
type GeneratedFile struct {
	Path    string
	Content []byte
}

// TargetPlugin generates target language code from the IR.
type TargetPlugin interface {
	// Language returns the target language identifier (e.g. "java").
	Language() string
	// Generate produces code files from the IR, optionally using an LLM.
	Generate(ctx context.Context, graph *ir.SemanticGraph, provider llm.Provider) ([]GeneratedFile, error)
	// Scaffold produces boilerplate/project structure files.
	Scaffold(ctx context.Context, graph *ir.SemanticGraph) ([]GeneratedFile, error)
}
