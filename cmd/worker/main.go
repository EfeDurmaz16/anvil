package main

import (
	"context"
	"log/slog"
	"os"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/config"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/llmutil"
	"github.com/efebarandurmaz/anvil/internal/observability"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	cobolplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/cobol"
	fortranplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/fortran"
	perlplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/perl"
	goplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/golang"
	javaplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/java"
	pythonplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/python"
	tsplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/typescript"
	"github.com/efebarandurmaz/anvil/internal/server"
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
		slog.Error("failed to load config", "error", err)
		os.Exit(1)
	}

	// Setup structured logging.
	var logHandler slog.Handler
	if os.Getenv("ANVIL_LOG_FORMAT") == "json" || cfg.Log.Format == "json" {
		logHandler = slog.NewJSONHandler(os.Stdout, nil)
	} else {
		logHandler = slog.NewTextHandler(os.Stdout, nil)
	}
	slog.SetDefault(slog.New(logHandler))

	// Init tracing (no-op if OTLP endpoint not configured).
	tracingCfg := observability.DefaultTracingConfig()
	tracingCfg.ServiceName = "anvil-worker"
	if endpoint := os.Getenv("OTEL_EXPORTER_OTLP_ENDPOINT"); endpoint != "" {
		tracingCfg.OTLPEndpoint = endpoint
	}
	tp, err := observability.InitTracing(context.Background(), tracingCfg)
	if err != nil {
		slog.Error("failed to init tracing", "error", err)
		os.Exit(1)
	}

	// Build plugin registry.
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
	llmutil.RegisterDefaultProviders(factory)

	provider, err := factory.Create(llm.ProviderConfig{
		Provider: cfg.LLM.Provider,
		APIKey:   cfg.LLM.APIKey,
		Model:    cfg.LLM.Model,
		BaseURL:  cfg.LLM.BaseURL,
	})
	if err != nil {
		slog.Error("failed to create LLM provider", "error", err)
		os.Exit(1)
	}

	// Wire rate limiter before SetDependencies.
	provider = llm.WithRateLimit(provider, llm.DefaultRateLimitConfig())

	temporalmod.SetDependencies(&temporalmod.Dependencies{
		LLM:      agents.AgentContext{LLM: provider},
		Registry: registry,
	})

	// Create GracefulServer (health HTTP + signal handling).
	gs := server.NewGracefulServer(
		&server.HealthConfig{Version: "0.1.0"},
		server.DefaultShutdownConfig(),
	)

	// Register tracing shutdown hook (priority 80, runs before worker stop).
	gs.RegisterHook("tracing", 80, func(ctx context.Context) error {
		return tp.Shutdown(ctx)
	})

	// Start GracefulServer: begins health HTTP on :8080 and listens for signals.
	if err := gs.Start(":8080"); err != nil {
		slog.Error("failed to start graceful server", "error", err)
		os.Exit(1)
	}

	// Connect to Temporal.
	c, err := temporalclient.Dial(temporalclient.Options{
		HostPort:  cfg.Temporal.Host,
		Namespace: cfg.Temporal.Namespace,
	})
	if err != nil {
		slog.Error("failed to dial temporal", "error", err)
		os.Exit(1)
	}

	// Register Temporal health check.
	gs.Health.RegisterCheck("temporal", server.TemporalHealthChecker(func(ctx context.Context) error {
		_, err := c.CheckHealth(ctx, &temporalclient.CheckHealthRequest{})
		return err
	}))

	// Register LLM health check.
	gs.Health.RegisterCheck("llm", server.LLMHealthChecker(cfg.LLM.Provider, nil))

	// Start Temporal worker.
	w, err := temporalmod.StartWorker(c, cfg.Temporal.TaskQueue)
	if err != nil {
		slog.Error("failed to start temporal worker", "error", err)
		c.Close()
		os.Exit(1)
	}

	// Register shutdown hooks for worker and client.
	workerHook := server.TemporalWorkerShutdownHook(w.Stop)
	gs.Shutdown.RegisterHook(workerHook.Name, workerHook.Priority, workerHook.Fn)

	gs.RegisterHook("temporal-client", 30, func(ctx context.Context) error {
		c.Close()
		return nil
	})

	slog.Info("worker started", "task_queue", cfg.Temporal.TaskQueue)

	// Block until shutdown signal received and all hooks complete.
	gs.Wait()

	slog.Info("worker stopped")
}
