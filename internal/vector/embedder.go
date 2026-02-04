package vector

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"fmt"

	"github.com/efebarandurmaz/anvil/internal/llm"
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
			ID:       newUUID(),
			Content:  texts[i],
			Vector:   vectors[i],
			Metadata: meta,
		}
	}
	return e.repo.Upsert(ctx, docs)
}

func newUUID() string {
	// Minimal UUIDv4 generator to avoid external dependencies.
	var b [16]byte
	_, _ = rand.Read(b[:])
	b[6] = (b[6] & 0x0f) | 0x40 // version 4
	b[8] = (b[8] & 0x3f) | 0x80 // variant 10

	var out [36]byte
	hex.Encode(out[0:8], b[0:4])
	out[8] = '-'
	hex.Encode(out[9:13], b[4:6])
	out[13] = '-'
	hex.Encode(out[14:18], b[6:8])
	out[18] = '-'
	hex.Encode(out[19:23], b[8:10])
	out[23] = '-'
	hex.Encode(out[24:36], b[10:16])
	return string(out[:])
}
