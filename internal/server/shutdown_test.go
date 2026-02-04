package server

import (
	"context"
	"errors"
	"sync/atomic"
	"testing"
	"time"
)

func TestDefaultShutdownConfig(t *testing.T) {
	cfg := DefaultShutdownConfig()
	if cfg.Timeout != 30*time.Second {
		t.Fatalf("expected 30s timeout, got %v", cfg.Timeout)
	}
	if len(cfg.Signals) != 2 {
		t.Fatalf("expected 2 signals, got %d", len(cfg.Signals))
	}
}

func TestNewShutdownHandler(t *testing.T) {
	h := NewShutdownHandler(nil)
	if h == nil {
		t.Fatal("expected non-nil handler")
	}
	if h.timeout != 30*time.Second {
		t.Fatalf("expected default timeout, got %v", h.timeout)
	}
}

func TestNewShutdownHandler_WithConfig(t *testing.T) {
	h := NewShutdownHandler(&ShutdownConfig{
		Timeout: 10 * time.Second,
	})
	if h.timeout != 10*time.Second {
		t.Fatalf("expected 10s timeout, got %v", h.timeout)
	}
}

func TestShutdownHandler_RegisterHook(t *testing.T) {
	h := NewShutdownHandler(nil)

	h.RegisterHook("test", 10, func(ctx context.Context) error {
		return nil
	})

	if len(h.hooks) != 1 {
		t.Fatalf("expected 1 hook, got %d", len(h.hooks))
	}
	if h.hooks[0].Name != "test" {
		t.Fatalf("expected name 'test', got %s", h.hooks[0].Name)
	}
}

func TestShutdownHandler_HookPriority(t *testing.T) {
	h := NewShutdownHandler(nil)

	h.RegisterHook("low", 100, func(ctx context.Context) error { return nil })
	h.RegisterHook("high", 10, func(ctx context.Context) error { return nil })
	h.RegisterHook("mid", 50, func(ctx context.Context) error { return nil })

	if h.hooks[0].Name != "high" {
		t.Fatalf("expected 'high' first, got %s", h.hooks[0].Name)
	}
	if h.hooks[1].Name != "mid" {
		t.Fatalf("expected 'mid' second, got %s", h.hooks[1].Name)
	}
	if h.hooks[2].Name != "low" {
		t.Fatalf("expected 'low' third, got %s", h.hooks[2].Name)
	}
}

func TestShutdownHandler_ManualShutdown(t *testing.T) {
	h := NewShutdownHandler(&ShutdownConfig{
		Timeout: 5 * time.Second,
	})

	var callOrder []string
	var mu atomic.Int32

	h.RegisterHook("first", 10, func(ctx context.Context) error {
		callOrder = append(callOrder, "first")
		mu.Add(1)
		return nil
	})
	h.RegisterHook("second", 20, func(ctx context.Context) error {
		callOrder = append(callOrder, "second")
		mu.Add(1)
		return nil
	})

	h.Start()
	h.Shutdown()

	// Wait for shutdown to complete
	select {
	case <-h.Done():
		// Good
	case <-time.After(2 * time.Second):
		t.Fatal("shutdown timed out")
	}

	if mu.Load() != 2 {
		t.Fatalf("expected 2 hooks called, got %d", mu.Load())
	}
}

func TestShutdownHandler_HookOrder(t *testing.T) {
	h := NewShutdownHandler(&ShutdownConfig{
		Timeout: 5 * time.Second,
	})

	var order []int

	h.RegisterHook("third", 30, func(ctx context.Context) error {
		order = append(order, 3)
		return nil
	})
	h.RegisterHook("first", 10, func(ctx context.Context) error {
		order = append(order, 1)
		return nil
	})
	h.RegisterHook("second", 20, func(ctx context.Context) error {
		order = append(order, 2)
		return nil
	})

	h.Start()
	h.Shutdown()
	h.Wait()

	if len(order) != 3 {
		t.Fatalf("expected 3 hooks, got %d", len(order))
	}
	if order[0] != 1 || order[1] != 2 || order[2] != 3 {
		t.Fatalf("expected order [1,2,3], got %v", order)
	}
}

func TestShutdownHandler_HookWithError(t *testing.T) {
	h := NewShutdownHandler(&ShutdownConfig{
		Timeout: 5 * time.Second,
	})

	var called bool

	h.RegisterHook("failing", 10, func(ctx context.Context) error {
		return errors.New("hook failed")
	})
	h.RegisterHook("after", 20, func(ctx context.Context) error {
		called = true
		return nil
	})

	h.Start()
	h.Shutdown()
	h.Wait()

	// Second hook should still be called even if first fails
	if !called {
		t.Fatal("expected second hook to be called despite first failing")
	}
}

func TestShutdownHandler_WaitWithTimeout_Success(t *testing.T) {
	h := NewShutdownHandler(&ShutdownConfig{
		Timeout: 5 * time.Second,
	})

	h.RegisterHook("quick", 10, func(ctx context.Context) error {
		return nil
	})

	h.Start()
	h.Shutdown()

	success := h.WaitWithTimeout(2 * time.Second)
	if !success {
		t.Fatal("expected successful wait")
	}
}

func TestShutdownHandler_WaitWithTimeout_Timeout(t *testing.T) {
	h := NewShutdownHandler(&ShutdownConfig{
		Timeout: 10 * time.Second,
	})

	h.RegisterHook("slow", 10, func(ctx context.Context) error {
		time.Sleep(5 * time.Second)
		return nil
	})

	h.Start()
	go h.Shutdown()

	success := h.WaitWithTimeout(100 * time.Millisecond)
	if success {
		t.Fatal("expected timeout")
	}
}

func TestShutdownHandler_DoubleStart(t *testing.T) {
	h := NewShutdownHandler(nil)

	h.Start()
	h.Start() // Should not panic

	if !h.started {
		t.Fatal("expected started to be true")
	}
}

func TestShutdownHandler_ShutdownBeforeStart(t *testing.T) {
	h := NewShutdownHandler(nil)

	// Should not panic
	h.Shutdown()
}

// Test common hooks

func TestHTTPServerShutdownHook(t *testing.T) {
	called := false
	hook := HTTPServerShutdownHook("test-server", func(ctx context.Context) error {
		called = true
		return nil
	})

	if hook.Name != "test-server" {
		t.Fatalf("expected name 'test-server', got %s", hook.Name)
	}
	if hook.Priority != 10 {
		t.Fatalf("expected priority 10, got %d", hook.Priority)
	}

	hook.Fn(context.Background())
	if !called {
		t.Fatal("expected hook to be called")
	}
}

func TestTemporalWorkerShutdownHook(t *testing.T) {
	stopped := false
	hook := TemporalWorkerShutdownHook(func() {
		stopped = true
	})

	if hook.Name != "temporal-worker" {
		t.Fatalf("expected name 'temporal-worker', got %s", hook.Name)
	}

	hook.Fn(context.Background())
	if !stopped {
		t.Fatal("expected worker to be stopped")
	}
}

func TestDatabaseShutdownHook(t *testing.T) {
	closed := false
	hook := DatabaseShutdownHook(func() error {
		closed = true
		return nil
	})

	if hook.Name != "database" {
		t.Fatalf("expected name 'database', got %s", hook.Name)
	}
	if hook.Priority != 90 {
		t.Fatalf("expected priority 90, got %d", hook.Priority)
	}

	hook.Fn(context.Background())
	if !closed {
		t.Fatal("expected database to be closed")
	}
}

func TestLLMProviderShutdownHook(t *testing.T) {
	cleaned := false
	hook := LLMProviderShutdownHook(func() {
		cleaned = true
	})

	if hook.Name != "llm-provider" {
		t.Fatalf("expected name 'llm-provider', got %s", hook.Name)
	}

	hook.Fn(context.Background())
	if !cleaned {
		t.Fatal("expected provider to be cleaned up")
	}
}

func TestAuditLoggerShutdownHook(t *testing.T) {
	closed := false
	hook := AuditLoggerShutdownHook(func() error {
		closed = true
		return nil
	})

	if hook.Name != "audit-logger" {
		t.Fatalf("expected name 'audit-logger', got %s", hook.Name)
	}
	if hook.Priority != 95 {
		t.Fatalf("expected priority 95, got %d", hook.Priority)
	}

	hook.Fn(context.Background())
	if !closed {
		t.Fatal("expected logger to be closed")
	}
}

func TestMetricsShutdownHook(t *testing.T) {
	flushed := false
	hook := MetricsShutdownHook(func(ctx context.Context) error {
		flushed = true
		return nil
	})

	if hook.Name != "metrics" {
		t.Fatalf("expected name 'metrics', got %s", hook.Name)
	}

	hook.Fn(context.Background())
	if !flushed {
		t.Fatal("expected metrics to be flushed")
	}
}

func TestTracingShutdownHook(t *testing.T) {
	shutdown := false
	hook := TracingShutdownHook(func(ctx context.Context) error {
		shutdown = true
		return nil
	})

	if hook.Name != "tracing" {
		t.Fatalf("expected name 'tracing', got %s", hook.Name)
	}

	hook.Fn(context.Background())
	if !shutdown {
		t.Fatal("expected tracing to be shutdown")
	}
}

// Test GracefulServer

func TestNewGracefulServer(t *testing.T) {
	g := NewGracefulServer(nil, nil)
	if g == nil {
		t.Fatal("expected non-nil server")
	}
	if g.Health == nil {
		t.Fatal("expected non-nil health server")
	}
	if g.Shutdown == nil {
		t.Fatal("expected non-nil shutdown handler")
	}
}

func TestGracefulServer_RegisterHook(t *testing.T) {
	g := NewGracefulServer(nil, nil)

	g.RegisterHook("test", 50, func(ctx context.Context) error {
		return nil
	})

	// Should have at least 2 hooks (health-server + test)
	if len(g.Shutdown.hooks) < 2 {
		t.Fatalf("expected at least 2 hooks, got %d", len(g.Shutdown.hooks))
	}
}
