package llm

import (
	"fmt"
	"time"
)

// ProviderConfig holds all configuration needed to create any LLM provider.
type ProviderConfig struct {
	Provider   string // "anthropic", "openai", "groq", "huggingface", "ollama", "custom"
	APIKey     string
	Model      string
	BaseURL    string // Override for self-hosted / custom endpoints
	EmbedModel string // Embedding model (OpenAI-compatible providers only)

	// Timeout and retry configuration
	Timeout    time.Duration // Per-request timeout (default: 2 minutes)
	MaxRetries int           // Max retry attempts (default: 3)
	RetryDelay time.Duration // Initial retry delay for exponential backoff (default: 1s)
}

// DefaultProviderConfig returns a config with sensible defaults.
func DefaultProviderConfig() ProviderConfig {
	return ProviderConfig{
		Timeout:    2 * time.Minute,
		MaxRetries: 3,
		RetryDelay: 1 * time.Second,
	}
}

// ProviderFactory creates Provider instances from config.
type ProviderFactory struct {
	constructors map[string]ProviderConstructor
}

// ProviderConstructor builds a Provider from config.
type ProviderConstructor func(cfg ProviderConfig) (Provider, error)

// NewFactory creates a factory with built-in providers pre-registered.
func NewFactory() *ProviderFactory {
	return &ProviderFactory{
		constructors: make(map[string]ProviderConstructor),
	}
}

// Register adds a provider constructor under the given name.
func (f *ProviderFactory) Register(name string, ctor ProviderConstructor) {
	f.constructors[name] = ctor
}

// Create builds a Provider from config. Returns nil (no error) when provider is
// empty or "none", allowing LLM-free operation.
// The returned provider is automatically wrapped with retry logic if configured.
func (f *ProviderFactory) Create(cfg ProviderConfig) (Provider, error) {
	if cfg.Provider == "" || cfg.Provider == "none" {
		return nil, nil
	}

	ctor, ok := f.constructors[cfg.Provider]
	if !ok {
		return nil, fmt.Errorf("unknown LLM provider %q — registered: %v", cfg.Provider, f.names())
	}

	provider, err := ctor(cfg)
	if err != nil {
		return nil, err
	}

	// Wrap with retry logic if timeout or retries are configured
	if cfg.Timeout > 0 || cfg.MaxRetries > 0 {
		return WrapWithRetry(provider, cfg), nil
	}

	return provider, nil
}

func (f *ProviderFactory) names() []string {
	var out []string
	for k := range f.constructors {
		out = append(out, k)
	}
	return out
}

// KnownProviders documents the built-in provider presets.
// For OpenAI-compatible APIs (Groq, HuggingFace, vLLM, Ollama, Together, etc.)
// use "openai" provider with a custom base_url.
//
// Presets with default base URLs:
//
//	anthropic  → https://api.anthropic.com/v1
//	openai     → https://api.openai.com/v1
//	groq       → https://api.groq.com/openai/v1
//	huggingface→ https://api-inference.huggingface.co/v1
//	ollama     → http://localhost:11434/v1
//	together   → https://api.together.xyz/v1
//	deepseek   → https://api.deepseek.com/v1
var KnownProviders = map[string]string{
	"anthropic":   "https://api.anthropic.com/v1",
	"openai":      "https://api.openai.com/v1",
	"groq":        "https://api.groq.com/openai/v1",
	"huggingface": "https://api-inference.huggingface.co/v1",
	"ollama":      "http://localhost:11434/v1",
	"together":    "https://api.together.xyz/v1",
	"deepseek":    "https://api.deepseek.com/v1",
}
