package graph

import (
	"context"

	"github.com/efebarandurmaz/anvil/internal/ir"
)

// Repository provides graph storage for the IR.
type Repository interface {
	// StoreGraph persists the entire semantic graph.
	StoreGraph(ctx context.Context, graph *ir.SemanticGraph) error
	// LoadGraph retrieves the full graph for a project.
	LoadGraph(ctx context.Context, projectID string) (*ir.SemanticGraph, error)
	// QueryCallees returns all functions called by the given function.
	QueryCallees(ctx context.Context, functionName string) ([]string, error)
	// Close releases resources.
	Close(ctx context.Context) error
}
