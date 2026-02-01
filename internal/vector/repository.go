package vector

import "context"

// Document represents a chunk of code/text with its embedding.
type Document struct {
	ID       string
	Content  string
	Vector   []float32
	Metadata map[string]string
}

// SearchResult is a single match from a similarity search.
type SearchResult struct {
	ID       string
	Score    float32
	Content  string
	Metadata map[string]string
}

// Repository provides vector storage and similarity search.
type Repository interface {
	// Upsert inserts or updates documents.
	Upsert(ctx context.Context, docs []Document) error
	// Search finds the top-k most similar documents.
	Search(ctx context.Context, vector []float32, topK int) ([]SearchResult, error)
	// Close releases resources.
	Close() error
}
