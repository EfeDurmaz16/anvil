package llmutil

import (
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/llm/anthropic"
	"github.com/efebarandurmaz/anvil/internal/llm/openai"
)

// RegisterDefaultProviders registers all built-in LLM provider constructors
// (anthropic, openai, and all OpenAI-compatible providers) into factory.
// Both cmd/anvil and cmd/worker call this to avoid duplicating registration
// logic across binaries.
func RegisterDefaultProviders(factory *llm.ProviderFactory) {
	factory.Register("anthropic", func(c llm.ProviderConfig) (llm.Provider, error) {
		return anthropic.New(c.APIKey, c.Model, c.BaseURL), nil
	})
	factory.Register("openai", func(c llm.ProviderConfig) (llm.Provider, error) {
		return openai.New(c.APIKey, c.Model, c.BaseURL, c.EmbedModel), nil
	})
	// All OpenAI-compatible providers
	for _, p := range []struct{ name, url string }{
		{"groq", llm.KnownProviders["groq"]},
		{"huggingface", llm.KnownProviders["huggingface"]},
		{"ollama", llm.KnownProviders["ollama"]},
		{"together", llm.KnownProviders["together"]},
		{"deepseek", llm.KnownProviders["deepseek"]},
		{"custom", ""},
	} {
		p := p
		factory.Register(p.name, func(c llm.ProviderConfig) (llm.Provider, error) {
			base := c.BaseURL
			if base == "" {
				base = p.url
			}
			return openai.New(c.APIKey, c.Model, base, c.EmbedModel), nil
		})
	}
}
