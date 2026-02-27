package llm

import (
	"fmt"

	"github.com/efebarandurmaz/anvil/internal/config"
)

// SetupProviders creates LLM providers from a config, including per-agent
// overrides. The returned map always has a "default" key. Providers are
// wrapped with the default rate limiter.
func SetupProviders(cfg config.LLMConfig, factory *ProviderFactory) (map[string]Provider, error) {
	makeProvider := func(lcfg config.LLMConfig, label string) (Provider, error) {
		p, err := factory.Create(ProviderConfig{
			Provider: lcfg.Provider,
			APIKey:   lcfg.APIKey,
			Model:    lcfg.Model,
			BaseURL:  lcfg.BaseURL,
		})
		if err != nil {
			return nil, fmt.Errorf("creating LLM provider for %s: %w", label, err)
		}
		if p != nil {
			p = WithRateLimit(p, DefaultRateLimitConfig())
		}
		return p, nil
	}

	// Default provider
	provider, err := makeProvider(cfg, "default")
	if err != nil {
		return nil, err
	}

	providers := map[string]Provider{"default": provider}

	// Per-agent provider overrides
	for agentName := range cfg.Agents {
		resolved := cfg.ResolveForAgent(agentName)
		agentProv, err := makeProvider(resolved, agentName)
		if err != nil {
			return nil, fmt.Errorf("agent %s: %w", agentName, err)
		}
		providers[agentName] = agentProv
	}

	return providers, nil
}
