package server

import (
	"context"
	"os"
	"os/signal"
	"sync"
	"syscall"
	"time"
)

// ShutdownHandler manages graceful shutdown of services.
type ShutdownHandler struct {
	mu           sync.Mutex
	hooks        []ShutdownHook
	timeout      time.Duration
	signals      []os.Signal
	shutdownCh   chan struct{}
	doneCh       chan struct{}
	started      bool
	shutdownOnce sync.Once
	doneOnce     sync.Once
}

// ShutdownHook is a function called during shutdown.
type ShutdownHook struct {
	Name     string
	Priority int // Lower priority runs first
	Fn       func(ctx context.Context) error
}

// ShutdownConfig configures the shutdown handler.
type ShutdownConfig struct {
	// Timeout for graceful shutdown (default: 30s)
	Timeout time.Duration
	// Signals to listen for (default: SIGTERM, SIGINT)
	Signals []os.Signal
}

// DefaultShutdownConfig returns default configuration.
func DefaultShutdownConfig() *ShutdownConfig {
	return &ShutdownConfig{
		Timeout: 30 * time.Second,
		Signals: []os.Signal{syscall.SIGTERM, syscall.SIGINT},
	}
}

// NewShutdownHandler creates a new shutdown handler.
func NewShutdownHandler(config *ShutdownConfig) *ShutdownHandler {
	if config == nil {
		config = DefaultShutdownConfig()
	}

	return &ShutdownHandler{
		timeout:    config.Timeout,
		signals:    config.Signals,
		shutdownCh: make(chan struct{}, 1),
		doneCh:     make(chan struct{}),
	}
}

// RegisterHook adds a shutdown hook.
func (s *ShutdownHandler) RegisterHook(name string, priority int, fn func(ctx context.Context) error) {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.hooks = append(s.hooks, ShutdownHook{
		Name:     name,
		Priority: priority,
		Fn:       fn,
	})

	// Sort by priority (lower first)
	for i := len(s.hooks) - 1; i > 0; i-- {
		if s.hooks[i].Priority < s.hooks[i-1].Priority {
			s.hooks[i], s.hooks[i-1] = s.hooks[i-1], s.hooks[i]
		}
	}
}

// Start begins listening for shutdown signals.
func (s *ShutdownHandler) Start() {
	s.mu.Lock()
	if s.started {
		s.mu.Unlock()
		return
	}
	s.started = true
	s.mu.Unlock()

	sigCh := make(chan os.Signal, 1)
	signal.Notify(sigCh, s.signals...)

	go func() {
		select {
		case sig := <-sigCh:
			signal.Stop(sigCh)
			s.shutdown(sig)
		case <-s.shutdownCh:
			signal.Stop(sigCh)
			s.shutdown(nil)
		}
	}()
}

// Shutdown triggers a manual shutdown.
func (s *ShutdownHandler) Shutdown() {
	s.mu.Lock()
	if !s.started {
		s.mu.Unlock()
		return
	}
	s.mu.Unlock()

	s.shutdownOnce.Do(func() {
		close(s.shutdownCh)
	})
}

// Wait blocks until shutdown is complete.
func (s *ShutdownHandler) Wait() {
	<-s.doneCh
}

// WaitWithTimeout blocks until shutdown is complete or timeout.
func (s *ShutdownHandler) WaitWithTimeout(timeout time.Duration) bool {
	select {
	case <-s.doneCh:
		return true
	case <-time.After(timeout):
		return false
	}
}

// Done returns a channel that closes when shutdown is complete.
func (s *ShutdownHandler) Done() <-chan struct{} {
	return s.doneCh
}

// ShutdownCh returns a channel that receives when shutdown starts.
func (s *ShutdownHandler) ShutdownCh() <-chan struct{} {
	return s.shutdownCh
}

func (s *ShutdownHandler) shutdown(_ os.Signal) {
	ctx, cancel := context.WithTimeout(context.Background(), s.timeout)
	defer cancel()

	s.mu.Lock()
	hooks := make([]ShutdownHook, len(s.hooks))
	copy(hooks, s.hooks)
	s.mu.Unlock()

	// Run hooks in order
	for _, hook := range hooks {
		if err := hook.Fn(ctx); err != nil {
			// Log error but continue with other hooks
			// In production, this would use structured logging
		}
	}

	s.doneOnce.Do(func() {
		close(s.doneCh)
	})
}

// Common shutdown hooks

// HTTPServerShutdownHook creates a hook for HTTP server shutdown.
func HTTPServerShutdownHook(name string, shutdownFn func(ctx context.Context) error) ShutdownHook {
	return ShutdownHook{
		Name:     name,
		Priority: 10, // Run early to stop accepting new connections
		Fn:       shutdownFn,
	}
}

// TemporalWorkerShutdownHook creates a hook for Temporal worker shutdown.
func TemporalWorkerShutdownHook(stopFn func()) ShutdownHook {
	return ShutdownHook{
		Name:     "temporal-worker",
		Priority: 20, // Run after HTTP servers
		Fn: func(ctx context.Context) error {
			stopFn()
			return nil
		},
	}
}

// DatabaseShutdownHook creates a hook for database connection shutdown.
func DatabaseShutdownHook(closeFn func() error) ShutdownHook {
	return ShutdownHook{
		Name:     "database",
		Priority: 90, // Run late, after workers are done
		Fn: func(ctx context.Context) error {
			return closeFn()
		},
	}
}

// LLMProviderShutdownHook creates a hook for LLM provider cleanup.
func LLMProviderShutdownHook(cleanupFn func()) ShutdownHook {
	return ShutdownHook{
		Name:     "llm-provider",
		Priority: 50,
		Fn: func(ctx context.Context) error {
			cleanupFn()
			return nil
		},
	}
}

// AuditLoggerShutdownHook creates a hook for audit logger shutdown.
func AuditLoggerShutdownHook(closeFn func() error) ShutdownHook {
	return ShutdownHook{
		Name:     "audit-logger",
		Priority: 95, // Run very late, to capture shutdown events
		Fn: func(ctx context.Context) error {
			return closeFn()
		},
	}
}

// MetricsShutdownHook creates a hook for metrics flush.
func MetricsShutdownHook(flushFn func(ctx context.Context) error) ShutdownHook {
	return ShutdownHook{
		Name:     "metrics",
		Priority: 85,
		Fn:       flushFn,
	}
}

// TracingShutdownHook creates a hook for tracing provider shutdown.
func TracingShutdownHook(shutdownFn func(ctx context.Context) error) ShutdownHook {
	return ShutdownHook{
		Name:     "tracing",
		Priority: 80,
		Fn:       shutdownFn,
	}
}

// GracefulServer combines health checks with shutdown handling.
type GracefulServer struct {
	Health   *HealthServer
	Shutdown *ShutdownHandler
}

// NewGracefulServer creates a server with health checks and graceful shutdown.
func NewGracefulServer(healthConfig *HealthConfig, shutdownConfig *ShutdownConfig) *GracefulServer {
	health := NewHealthServer(healthConfig)
	shutdown := NewShutdownHandler(shutdownConfig)

	// Register health server shutdown
	shutdown.RegisterHook("health-server", 5, func(ctx context.Context) error {
		health.Shutdown()
		return nil
	})

	// Mark as not ready when shutdown starts
	go func() {
		<-shutdown.ShutdownCh()
		health.SetReady(false)
	}()

	return &GracefulServer{
		Health:   health,
		Shutdown: shutdown,
	}
}

// Start starts the health server and shutdown handler.
func (g *GracefulServer) Start(addr string) error {
	g.Shutdown.Start()

	// Start health server in goroutine
	go func() {
		g.Health.ListenAndServe(addr)
	}()

	// Mark as ready
	g.Health.SetReady(true)

	return nil
}

// Wait waits for shutdown to complete.
func (g *GracefulServer) Wait() {
	g.Shutdown.Wait()
}

// RegisterHook adds a shutdown hook.
func (g *GracefulServer) RegisterHook(name string, priority int, fn func(ctx context.Context) error) {
	g.Shutdown.RegisterHook(name, priority, fn)
}
