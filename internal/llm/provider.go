package llm

import "context"

// Provider is the interface all LLM backends must implement.
type Provider interface {
	// Complete sends a prompt and returns a completion.
	Complete(ctx context.Context, prompt *Prompt, opts *RequestOptions) (*Response, error)
	// Embed returns embedding vectors for the given texts.
	Embed(ctx context.Context, texts []string) ([][]float32, error)
	// Name returns the provider identifier (e.g. "anthropic", "openai").
	Name() string
}
