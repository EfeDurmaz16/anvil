package plugins

import (
	"context"

	"github.com/efebarandurmaz/anvil/internal/ir"
)

// SourceFile represents a single input file to be parsed.
type SourceFile struct {
	Path    string
	Content []byte
}

// SourcePlugin parses source language files into the IR.
type SourcePlugin interface {
	// Language returns the source language identifier (e.g. "cobol").
	Language() string
	// Parse converts source files into a SemanticGraph.
	Parse(ctx context.Context, files []SourceFile) (*ir.SemanticGraph, error)
	// ResolveDependencies links cross-module references within the graph.
	ResolveDependencies(ctx context.Context, graph *ir.SemanticGraph) error
}

// FileExtensionsProvider is an optional interface for source plugins to declare
// which file extensions they can parse (e.g. []string{".cbl",".cpy"}).
//
// When not implemented, the Cartographer falls back to a conservative default.
type FileExtensionsProvider interface {
	FileExtensions() []string
}
