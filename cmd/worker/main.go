package main

import (
	"fmt"
	"log"
	"os"
	"os/signal"
	"syscall"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/config"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/llm/anthropic"
	"github.com/efebarandurmaz/anvil/internal/llm/openai"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	cobolplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/cobol"
	fortranplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/fortran"
	perlplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/perl"
	goplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/golang"
	javaplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/java"
	pythonplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/python"
	tsplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/typescript"
	temporalmod "github.com/efebarandurmaz/anvil/internal/temporal"

	temporalclient "go.temporal.io/sdk/client"
)

func main() {
	configPath := "configs/anvil.yaml"
	if len(os.Args) > 1 {
		configPath = os.Args[1]
	}

	cfg, err := config.Load(configPath)
	if err != nil {
		log.Fatalf("config: %v", err)
	}

	registry := plugins.NewRegistry()
	registry.RegisterSource(cobolplugin.New())
	registry.RegisterSource(perlplugin.New())
	registry.RegisterSource(fortranplugin.New())
	registry.RegisterTarget(javaplugin.New())
	registry.RegisterTarget(pythonplugin.New())
	registry.RegisterTarget(goplugin.New())
	registry.RegisterTarget(tsplugin.New())

	// Build LLM provider via factory (supports on-prem/no-LLM operation).
	factory := llm.NewFactory()
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

	provider, err := factory.Create(llm.ProviderConfig{
		Provider: cfg.LLM.Provider,
		APIKey:   cfg.LLM.APIKey,
		Model:    cfg.LLM.Model,
		BaseURL:  cfg.LLM.BaseURL,
	})
	if err != nil {
		log.Fatalf("creating LLM provider: %v", err)
	}

	// Wire rate limiter before SetDependencies
	provider = llm.WithRateLimit(provider, llm.DefaultRateLimitConfig())

	temporalmod.SetDependencies(&temporalmod.Dependencies{
		LLM:      agents.AgentContext{LLM: provider},
		Registry: registry,
	})

	c, err := temporalclient.Dial(temporalclient.Options{
		HostPort:  cfg.Temporal.Host,
		Namespace: cfg.Temporal.Namespace,
	})
	if err != nil {
		log.Fatalf("temporal client: %v", err)
	}
	defer c.Close()

	w, err := temporalmod.StartWorker(c, cfg.Temporal.TaskQueue)
	if err != nil {
		log.Fatalf("worker: %v", err)
	}

	fmt.Printf("Worker started on task queue: %s\n", cfg.Temporal.TaskQueue)

	sig := make(chan os.Signal, 1)
	signal.Notify(sig, syscall.SIGINT, syscall.SIGTERM)
	<-sig

	w.Stop()
	fmt.Println("Worker stopped")
}
