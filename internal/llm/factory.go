package llm

import "fmt"

// ProviderConfig holds all configuration needed to create any LLM provider.
type ProviderConfig struct {
	Provider   string // "anthropic", "openai", "groq", "huggingface", "ollama", "custom"
	APIKey     string
	Model      string
	BaseURL    string // Override for self-hosted / custom endpoints
	EmbedModel string // Embedding model (OpenAI-compatible providers only)
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
func (f *ProviderFactory) Create(cfg ProviderConfig) (Provider, error) {
	if cfg.Provider == "" || cfg.Provider == "none" {
		return nil, nil
	}

	ctor, ok := f.constructors[cfg.Provider]
	if !ok {
		return nil, fmt.Errorf("unknown LLM provider %q — registered: %v", cfg.Provider, f.names())
	}
	return ctor(cfg)
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
