//go:build integration

package qdrant

import (
	"context"
	"fmt"
	"testing"
	"time"

	"github.com/efebarandurmaz/anvil/internal/vector"
)

// TestQdrantImplementsRepository verifies the compile-time interface check still
// holds and is exercised at runtime via a type assertion.
func TestQdrantImplementsRepository(t *testing.T) {
	var _ vector.Repository = (*QdrantRepository)(nil)
}

func newTestRepo(t *testing.T) *QdrantRepository {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 10*time.Second)
	defer cancel()

	repo, err := NewQdrant(ctx, "localhost", 6334, "test-collection")
	if err != nil {
		t.Skipf("qdrant not available: %v", err)
	}
	t.Cleanup(func() {
		_ = repo.Close()
	})
	return repo
}

// makeVec creates a float32 slice of the given dimension with a fixed pattern.
func makeVec(dim int, seed float32) []float32 {
	v := make([]float32, dim)
	for i := range v {
		v[i] = seed + float32(i)*0.01
	}
	return v
}

func TestUpsertSingleDocument(t *testing.T) {
	repo := newTestRepo(t)
	ctx := context.Background()

	docs := []vector.Document{
		{
			ID:      "00000000-0000-0000-0000-000000000001",
			Content: "MAIN-PARA performs initial setup",
			Vector:  makeVec(4, 0.1),
			Metadata: map[string]string{
				"language": "COBOL",
				"module":   "main",
			},
		},
	}

	if err := repo.Upsert(ctx, docs); err != nil {
		t.Fatalf("Upsert: %v", err)
	}
}

func TestUpsertMultipleDocuments(t *testing.T) {
	repo := newTestRepo(t)
	ctx := context.Background()

	const dim = 4
	docs := make([]vector.Document, 5)
	for i := range docs {
		docs[i] = vector.Document{
			ID:      fmt.Sprintf("00000000-0000-0000-0000-0000000000%02d", i+10),
			Content: fmt.Sprintf("function content %d", i),
			Vector:  makeVec(dim, float32(i)*0.1),
			Metadata: map[string]string{
				"index": fmt.Sprintf("%d", i),
			},
		}
	}

	if err := repo.Upsert(ctx, docs); err != nil {
		t.Fatalf("Upsert batch: %v", err)
	}
}

func TestSearchReturnsResults(t *testing.T) {
	repo := newTestRepo(t)
	ctx := context.Background()

	// Seed a known document.
	queryVec := makeVec(4, 0.5)
	docs := []vector.Document{
		{
			ID:      "00000000-0000-0000-0000-000000000099",
			Content: "searchable function body",
			Vector:  queryVec,
			Metadata: map[string]string{
				"tag": "searchtest",
			},
		},
	}
	if err := repo.Upsert(ctx, docs); err != nil {
		t.Fatalf("Upsert before Search: %v", err)
	}

	results, err := repo.Search(ctx, queryVec, 5)
	if err != nil {
		t.Fatalf("Search: %v", err)
	}
	if len(results) == 0 {
		t.Fatal("expected at least one search result")
	}

	// The exact document we just upserted should score highest (identical vector).
	top := results[0]
	if top.Score < 0.99 {
		t.Errorf("expected near-perfect score for identical vector, got %.4f", top.Score)
	}
}

func TestSearchTopKRespected(t *testing.T) {
	repo := newTestRepo(t)
	ctx := context.Background()

	const dim = 4
	// Upsert 10 documents with distinct vectors.
	docs := make([]vector.Document, 10)
	for i := range docs {
		docs[i] = vector.Document{
			ID:      fmt.Sprintf("00000000-0000-0000-0000-0000000001%02d", i),
			Content: fmt.Sprintf("topk doc %d", i),
			Vector:  makeVec(dim, float32(i)*0.05),
		}
	}
	if err := repo.Upsert(ctx, docs); err != nil {
		t.Fatalf("Upsert: %v", err)
	}

	results, err := repo.Search(ctx, makeVec(dim, 0.0), 3)
	if err != nil {
		t.Fatalf("Search topK=3: %v", err)
	}
	if len(results) > 3 {
		t.Errorf("expected at most 3 results, got %d", len(results))
	}
}

func TestSearchResultFields(t *testing.T) {
	repo := newTestRepo(t)
	ctx := context.Background()

	docID := "00000000-0000-0000-0000-000000000077"
	content := "result field verification content"
	vec := makeVec(4, 0.77)

	if err := repo.Upsert(ctx, []vector.Document{
		{
			ID:      docID,
			Content: content,
			Vector:  vec,
			Metadata: map[string]string{
				"author": "test",
			},
		},
	}); err != nil {
		t.Fatalf("Upsert: %v", err)
	}

	results, err := repo.Search(ctx, vec, 1)
	if err != nil {
		t.Fatalf("Search: %v", err)
	}
	if len(results) == 0 {
		t.Fatal("no results returned")
	}

	r := results[0]
	if r.ID != docID {
		t.Errorf("ID: got %q, want %q", r.ID, docID)
	}
	if r.Content != content {
		t.Errorf("Content: got %q, want %q", r.Content, content)
	}
	if r.Score <= 0 {
		t.Errorf("Score should be positive, got %f", r.Score)
	}
}

func TestUpsertEmptySlice(t *testing.T) {
	repo := newTestRepo(t)
	ctx := context.Background()

	if err := repo.Upsert(ctx, []vector.Document{}); err != nil {
		t.Fatalf("Upsert empty slice: %v", err)
	}
}

func TestConnectionError(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()

	// grpc.NewClient is lazy; the error surfaces on the first RPC call.
	repo, err := NewQdrant(ctx, "localhost", 19998, "test-collection")
	if err != nil {
		// Some builds surface the error at construction time — that's fine.
		return
	}
	defer func() { _ = repo.Close() }()

	// Attempt an RPC that must fail because the server doesn't exist.
	err = repo.Upsert(ctx, []vector.Document{
		{
			ID:      "00000000-0000-0000-0000-000000000000",
			Content: "probe",
			Vector:  makeVec(4, 0.0),
		},
	})
	if err == nil {
		t.Fatal("expected RPC error to non-existent host, got nil")
	}
}
