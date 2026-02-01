package plugins

import (
	"context"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/llm"
)

type mockSource struct{}

func (m *mockSource) Language() string                                                    { return "mock" }
func (m *mockSource) Parse(_ context.Context, _ []SourceFile) (*ir.SemanticGraph, error)  { return &ir.SemanticGraph{}, nil }
func (m *mockSource) ResolveDependencies(_ context.Context, _ *ir.SemanticGraph) error    { return nil }

type mockTarget struct{}

func (m *mockTarget) Language() string                                                                          { return "mock" }
func (m *mockTarget) Generate(_ context.Context, _ *ir.SemanticGraph, _ llm.Provider) ([]GeneratedFile, error)  { return nil, nil }
func (m *mockTarget) Scaffold(_ context.Context, _ *ir.SemanticGraph) ([]GeneratedFile, error)                  { return nil, nil }

func TestRegistry(t *testing.T) {
	r := NewRegistry()
	r.RegisterSource(&mockSource{})
	r.RegisterTarget(&mockTarget{})

	if _, err := r.Source("mock"); err != nil {
		t.Errorf("expected source, got error: %v", err)
	}
	if _, err := r.Source("unknown"); err == nil {
		t.Error("expected error for unknown source")
	}
	if _, err := r.Target("mock"); err != nil {
		t.Errorf("expected target, got error: %v", err)
	}
}
