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
	javaplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/java"
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
	registry.RegisterTarget(javaplugin.New())

	var provider llm.Provider
	switch cfg.LLM.Provider {
	case "openai":
		provider = openai.New(cfg.LLM.APIKey, cfg.LLM.Model, cfg.LLM.BaseURL, "")
	default:
		provider = anthropic.New(cfg.LLM.APIKey, cfg.LLM.Model, cfg.LLM.BaseURL)
	}

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
