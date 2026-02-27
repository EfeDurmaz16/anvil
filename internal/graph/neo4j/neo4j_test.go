//go:build integration

package neo4j

import (
	"context"
	"testing"
	"time"

	"github.com/efebarandurmaz/anvil/internal/graph"
	"github.com/efebarandurmaz/anvil/internal/ir"
)

// TestNeo4jImplementsRepository verifies the compile-time interface check still
// holds and is exercised at runtime via a type assertion.
func TestNeo4jImplementsRepository(t *testing.T) {
	var _ graph.Repository = (*Neo4jRepository)(nil)
}

func newTestRepo(t *testing.T) *Neo4jRepository {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	repo, err := NewNeo4j(ctx, "bolt://localhost:7687", "neo4j", "password")
	if err != nil {
		t.Skipf("neo4j not available: %v", err)
	}
	t.Cleanup(func() {
		_ = repo.Close(context.Background())
	})
	return repo
}

func TestStoreGraph(t *testing.T) {
	repo := newTestRepo(t)
	ctx := context.Background()

	g := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "main",
				Path:     "main.cbl",
				Language: "COBOL",
				Functions: []*ir.Function{
					{Name: "MAIN-PARA"},
					{Name: "PROCESS-RECORD"},
				},
			},
			{
				Name:     "utils",
				Path:     "utils.cbl",
				Language: "COBOL",
				Functions: []*ir.Function{
					{Name: "FORMAT-DATE"},
				},
			},
		},
		CallGraph: &ir.CallGraph{
			Edges: []ir.CallEdge{
				{Caller: "MAIN-PARA", Callee: "PROCESS-RECORD"},
				{Caller: "PROCESS-RECORD", Callee: "FORMAT-DATE"},
			},
		},
	}

	if err := repo.StoreGraph(ctx, g); err != nil {
		t.Fatalf("StoreGraph: %v", err)
	}
}

func TestLoadGraph(t *testing.T) {
	repo := newTestRepo(t)
	ctx := context.Background()

	// Seed some data first.
	seed := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "ledger",
				Path:     "ledger.cbl",
				Language: "COBOL",
				Functions: []*ir.Function{
					{Name: "OPEN-LEDGER"},
					{Name: "CLOSE-LEDGER"},
				},
			},
		},
	}
	if err := repo.StoreGraph(ctx, seed); err != nil {
		t.Fatalf("seed StoreGraph: %v", err)
	}

	loaded, err := repo.LoadGraph(ctx, "test-project")
	if err != nil {
		t.Fatalf("LoadGraph: %v", err)
	}
	if loaded == nil {
		t.Fatal("LoadGraph returned nil")
	}
	if len(loaded.Modules) == 0 {
		t.Fatal("expected at least one module in loaded graph")
	}
}

func TestQueryCallees(t *testing.T) {
	repo := newTestRepo(t)
	ctx := context.Background()

	g := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "query-test-mod",
				Path:     "qt.cbl",
				Language: "COBOL",
				Functions: []*ir.Function{
					{Name: "QT-CALLER"},
					{Name: "QT-CALLEE-A"},
					{Name: "QT-CALLEE-B"},
				},
			},
		},
		CallGraph: &ir.CallGraph{
			Edges: []ir.CallEdge{
				{Caller: "QT-CALLER", Callee: "QT-CALLEE-A"},
				{Caller: "QT-CALLER", Callee: "QT-CALLEE-B"},
			},
		},
	}
	if err := repo.StoreGraph(ctx, g); err != nil {
		t.Fatalf("StoreGraph: %v", err)
	}

	callees, err := repo.QueryCallees(ctx, "QT-CALLER")
	if err != nil {
		t.Fatalf("QueryCallees: %v", err)
	}

	want := map[string]bool{"QT-CALLEE-A": true, "QT-CALLEE-B": true}
	for _, c := range callees {
		delete(want, c)
	}
	if len(want) != 0 {
		t.Errorf("missing callees: %v", want)
	}
}

func TestQueryCalleesUnknownFunction(t *testing.T) {
	repo := newTestRepo(t)
	ctx := context.Background()

	callees, err := repo.QueryCallees(ctx, "NO-SUCH-FUNCTION-XYZ")
	if err != nil {
		t.Fatalf("QueryCallees on unknown function: %v", err)
	}
	if len(callees) != 0 {
		t.Errorf("expected empty callees, got %v", callees)
	}
}

func TestConnectionError(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	_, err := NewNeo4j(ctx, "bolt://localhost:19999", "neo4j", "password")
	if err == nil {
		t.Fatal("expected connection error to non-existent host, got nil")
	}
}

func TestStoreGraphEmpty(t *testing.T) {
	repo := newTestRepo(t)
	ctx := context.Background()

	g := &ir.SemanticGraph{
		Modules: []*ir.Module{},
	}
	if err := repo.StoreGraph(ctx, g); err != nil {
		t.Fatalf("StoreGraph with empty graph: %v", err)
	}
}

func TestStoreGraphNoCallGraph(t *testing.T) {
	repo := newTestRepo(t)
	ctx := context.Background()

	g := &ir.SemanticGraph{
		Modules: []*ir.Module{
			{
				Name:     "standalone",
				Path:     "standalone.cbl",
				Language: "COBOL",
				Functions: []*ir.Function{
					{Name: "STANDALONE-PARA"},
				},
			},
		},
		// CallGraph intentionally nil.
	}
	if err := repo.StoreGraph(ctx, g); err != nil {
		t.Fatalf("StoreGraph without CallGraph: %v", err)
	}
}
