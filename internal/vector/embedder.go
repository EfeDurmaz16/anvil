package vector

import (
	"context"
	"fmt"

	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/google/uuid"
)

// Embedder wraps an LLM provider to produce and store embeddings.
type Embedder struct {
	provider llm.Provider
	repo     Repository
}

// NewEmbedder creates an Embedder.
func NewEmbedder(provider llm.Provider, repo Repository) *Embedder {
	return &Embedder{provider: provider, repo: repo}
}

// IndexTexts embeds the given texts and upserts them into the vector store.
func (e *Embedder) IndexTexts(ctx context.Context, texts []string, metadata []map[string]string) error {
	vectors, err := e.provider.Embed(ctx, texts)
	if err != nil {
		return fmt.Errorf("embedding: %w", err)
	}
	if len(vectors) != len(texts) {
		return fmt.Errorf("embedding count mismatch: got %d, want %d", len(vectors), len(texts))
	}

	docs := make([]Document, len(texts))
	for i := range texts {
		meta := map[string]string{}
		if i < len(metadata) && metadata[i] != nil {
			meta = metadata[i]
		}
		docs[i] = Document{
			ID:       uuid.New().String(),
			Content:  texts[i],
			Vector:   vectors[i],
			Metadata: meta,
		}
	}
	return e.repo.Upsert(ctx, docs)
}
