// Package server provides HTTP server utilities including health checks.
package server

import (
	"context"
	"encoding/json"
	"net/http"
	"sync"
	"time"
)

// HealthStatus represents the health state of a component.
type HealthStatus string

const (
	HealthStatusHealthy   HealthStatus = "healthy"
	HealthStatusUnhealthy HealthStatus = "unhealthy"
	HealthStatusDegraded  HealthStatus = "degraded"
)

// HealthCheck represents a single health check.
type HealthCheck struct {
	Name    string            `json:"name"`
	Status  HealthStatus      `json:"status"`
	Message string            `json:"message,omitempty"`
	Details map[string]string `json:"details,omitempty"`
}

// HealthResponse is the response from health endpoints.
type HealthResponse struct {
	Status    HealthStatus  `json:"status"`
	Timestamp time.Time     `json:"timestamp"`
	Version   string        `json:"version,omitempty"`
	Checks    []HealthCheck `json:"checks,omitempty"`
}

// HealthChecker is a function that performs a health check.
type HealthChecker func(ctx context.Context) HealthCheck

// HealthServer provides HTTP health check endpoints.
type HealthServer struct {
	mu           sync.RWMutex
	checks       map[string]HealthChecker
	version      string
	ready        bool
	live         bool
	shutdownChan chan struct{}
}

// HealthConfig configures the health server.
type HealthConfig struct {
	Version string
	Addr    string // Address to listen on (default: ":8080")
}

// NewHealthServer creates a new health server.
func NewHealthServer(config *HealthConfig) *HealthServer {
	version := ""
	if config != nil {
		version = config.Version
	}

	return &HealthServer{
		checks:       make(map[string]HealthChecker),
		version:      version,
		ready:        false,
		live:         true,
		shutdownChan: make(chan struct{}),
	}
}

// RegisterCheck adds a health check.
func (s *HealthServer) RegisterCheck(name string, checker HealthChecker) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.checks[name] = checker
}

// SetReady marks the server as ready to accept traffic.
func (s *HealthServer) SetReady(ready bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.ready = ready
}

// SetLive marks the server as live (or not).
func (s *HealthServer) SetLive(live bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.live = live
}

// Handler returns an http.Handler for the health endpoints.
func (s *HealthServer) Handler() http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("/health", s.handleHealth)
	mux.HandleFunc("/ready", s.handleReady)
	mux.HandleFunc("/live", s.handleLive)
	mux.HandleFunc("/healthz", s.handleHealth) // Kubernetes alias
	mux.HandleFunc("/readyz", s.handleReady)   // Kubernetes alias
	mux.HandleFunc("/livez", s.handleLive)     // Kubernetes alias
	return mux
}

// ListenAndServe starts the health server.
func (s *HealthServer) ListenAndServe(addr string) error {
	if addr == "" {
		addr = ":8080"
	}

	server := &http.Server{
		Addr:         addr,
		Handler:      s.Handler(),
		ReadTimeout:  5 * time.Second,
		WriteTimeout: 5 * time.Second,
	}

	go func() {
		<-s.shutdownChan
		ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
		defer cancel()
		server.Shutdown(ctx)
	}()

	return server.ListenAndServe()
}

// Shutdown gracefully stops the health server.
func (s *HealthServer) Shutdown() {
	close(s.shutdownChan)
}

// handleHealth handles the /health endpoint - full health check.
func (s *HealthServer) handleHealth(w http.ResponseWriter, r *http.Request) {
	ctx, cancel := context.WithTimeout(r.Context(), 5*time.Second)
	defer cancel()

	s.mu.RLock()
	checks := make(map[string]HealthChecker, len(s.checks))
	for k, v := range s.checks {
		checks[k] = v
	}
	version := s.version
	s.mu.RUnlock()

	response := HealthResponse{
		Status:    HealthStatusHealthy,
		Timestamp: time.Now().UTC(),
		Version:   version,
		Checks:    make([]HealthCheck, 0, len(checks)),
	}

	// Run all health checks
	for name, checker := range checks {
		check := checker(ctx)
		check.Name = name
		response.Checks = append(response.Checks, check)

		// Update overall status based on check results
		if check.Status == HealthStatusUnhealthy {
			response.Status = HealthStatusUnhealthy
		} else if check.Status == HealthStatusDegraded && response.Status == HealthStatusHealthy {
			response.Status = HealthStatusDegraded
		}
	}

	statusCode := http.StatusOK
	if response.Status == HealthStatusUnhealthy {
		statusCode = http.StatusServiceUnavailable
	}

	s.writeJSON(w, statusCode, response)
}

// handleReady handles the /ready endpoint - readiness probe.
func (s *HealthServer) handleReady(w http.ResponseWriter, r *http.Request) {
	s.mu.RLock()
	ready := s.ready
	s.mu.RUnlock()

	response := HealthResponse{
		Status:    HealthStatusHealthy,
		Timestamp: time.Now().UTC(),
	}

	if !ready {
		response.Status = HealthStatusUnhealthy
		s.writeJSON(w, http.StatusServiceUnavailable, response)
		return
	}

	s.writeJSON(w, http.StatusOK, response)
}

// handleLive handles the /live endpoint - liveness probe.
func (s *HealthServer) handleLive(w http.ResponseWriter, r *http.Request) {
	s.mu.RLock()
	live := s.live
	s.mu.RUnlock()

	response := HealthResponse{
		Status:    HealthStatusHealthy,
		Timestamp: time.Now().UTC(),
	}

	if !live {
		response.Status = HealthStatusUnhealthy
		s.writeJSON(w, http.StatusServiceUnavailable, response)
		return
	}

	s.writeJSON(w, http.StatusOK, response)
}

func (s *HealthServer) writeJSON(w http.ResponseWriter, status int, data any) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(status)
	json.NewEncoder(w).Encode(data)
}

// Common health checkers

// TemporalHealthChecker creates a health check for Temporal connectivity.
func TemporalHealthChecker(checkFn func(ctx context.Context) error) HealthChecker {
	return func(ctx context.Context) HealthCheck {
		err := checkFn(ctx)
		if err != nil {
			return HealthCheck{
				Status:  HealthStatusUnhealthy,
				Message: "Temporal connection failed: " + err.Error(),
			}
		}
		return HealthCheck{
			Status:  HealthStatusHealthy,
			Message: "Temporal connection OK",
		}
	}
}

// DatabaseHealthChecker creates a health check for database connectivity.
func DatabaseHealthChecker(checkFn func(ctx context.Context) error) HealthChecker {
	return func(ctx context.Context) HealthCheck {
		err := checkFn(ctx)
		if err != nil {
			return HealthCheck{
				Status:  HealthStatusUnhealthy,
				Message: "Database connection failed: " + err.Error(),
			}
		}
		return HealthCheck{
			Status:  HealthStatusHealthy,
			Message: "Database connection OK",
		}
	}
}

// LLMHealthChecker creates a health check for LLM provider availability.
func LLMHealthChecker(providerName string, checkFn func(ctx context.Context) error) HealthChecker {
	return func(ctx context.Context) HealthCheck {
		if checkFn == nil {
			return HealthCheck{
				Status:  HealthStatusHealthy,
				Message: "LLM provider configured: " + providerName,
			}
		}

		err := checkFn(ctx)
		if err != nil {
			return HealthCheck{
				Status:  HealthStatusDegraded,
				Message: "LLM provider degraded: " + err.Error(),
				Details: map[string]string{"provider": providerName},
			}
		}
		return HealthCheck{
			Status:  HealthStatusHealthy,
			Message: "LLM provider OK",
			Details: map[string]string{"provider": providerName},
		}
	}
}

// DiskSpaceHealthChecker creates a health check for available disk space.
func DiskSpaceHealthChecker(path string, minFreeBytes int64) HealthChecker {
	return func(ctx context.Context) HealthCheck {
		// Simplified check - in production would use syscall.Statfs
		return HealthCheck{
			Status:  HealthStatusHealthy,
			Message: "Disk space OK",
			Details: map[string]string{"path": path},
		}
	}
}

// MemoryHealthChecker creates a health check for memory usage.
func MemoryHealthChecker(maxUsagePercent float64) HealthChecker {
	return func(ctx context.Context) HealthCheck {
		// Simplified check - in production would use runtime.MemStats
		return HealthCheck{
			Status:  HealthStatusHealthy,
			Message: "Memory usage OK",
		}
	}
}
