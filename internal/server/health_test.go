package server

import (
	"context"
	"encoding/json"
	"errors"
	"net/http"
	"net/http/httptest"
	"testing"
)

func TestNewHealthServer(t *testing.T) {
	s := NewHealthServer(nil)
	if s == nil {
		t.Fatal("expected non-nil server")
	}
}

func TestNewHealthServer_WithConfig(t *testing.T) {
	s := NewHealthServer(&HealthConfig{Version: "1.0.0"})
	if s.version != "1.0.0" {
		t.Fatalf("expected version 1.0.0, got %s", s.version)
	}
}

func TestHealthServer_SetReady(t *testing.T) {
	s := NewHealthServer(nil)

	// Initially not ready
	if s.ready {
		t.Fatal("expected not ready initially")
	}

	s.SetReady(true)
	if !s.ready {
		t.Fatal("expected ready after SetReady(true)")
	}

	s.SetReady(false)
	if s.ready {
		t.Fatal("expected not ready after SetReady(false)")
	}
}

func TestHealthServer_SetLive(t *testing.T) {
	s := NewHealthServer(nil)

	// Initially live
	if !s.live {
		t.Fatal("expected live initially")
	}

	s.SetLive(false)
	if s.live {
		t.Fatal("expected not live after SetLive(false)")
	}
}

func TestHealthServer_RegisterCheck(t *testing.T) {
	s := NewHealthServer(nil)

	checker := func(ctx context.Context) HealthCheck {
		return HealthCheck{Status: HealthStatusHealthy}
	}

	s.RegisterCheck("test", checker)

	if len(s.checks) != 1 {
		t.Fatalf("expected 1 check, got %d", len(s.checks))
	}
}

func TestHealthServer_HandleHealth(t *testing.T) {
	s := NewHealthServer(&HealthConfig{Version: "1.0.0"})
	s.RegisterCheck("test", func(ctx context.Context) HealthCheck {
		return HealthCheck{Status: HealthStatusHealthy, Message: "all good"}
	})

	req := httptest.NewRequest(http.MethodGet, "/health", nil)
	w := httptest.NewRecorder()

	s.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("expected 200, got %d", w.Code)
	}

	var resp HealthResponse
	if err := json.Unmarshal(w.Body.Bytes(), &resp); err != nil {
		t.Fatalf("failed to parse response: %v", err)
	}

	if resp.Status != HealthStatusHealthy {
		t.Fatalf("expected healthy, got %s", resp.Status)
	}
	if resp.Version != "1.0.0" {
		t.Fatalf("expected version 1.0.0, got %s", resp.Version)
	}
	if len(resp.Checks) != 1 {
		t.Fatalf("expected 1 check, got %d", len(resp.Checks))
	}
}

func TestHealthServer_HandleHealth_Unhealthy(t *testing.T) {
	s := NewHealthServer(nil)
	s.RegisterCheck("failing", func(ctx context.Context) HealthCheck {
		return HealthCheck{Status: HealthStatusUnhealthy, Message: "database down"}
	})

	req := httptest.NewRequest(http.MethodGet, "/health", nil)
	w := httptest.NewRecorder()

	s.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusServiceUnavailable {
		t.Fatalf("expected 503, got %d", w.Code)
	}

	var resp HealthResponse
	json.Unmarshal(w.Body.Bytes(), &resp)

	if resp.Status != HealthStatusUnhealthy {
		t.Fatalf("expected unhealthy, got %s", resp.Status)
	}
}

func TestHealthServer_HandleHealth_Degraded(t *testing.T) {
	s := NewHealthServer(nil)
	s.RegisterCheck("degraded", func(ctx context.Context) HealthCheck {
		return HealthCheck{Status: HealthStatusDegraded, Message: "high latency"}
	})

	req := httptest.NewRequest(http.MethodGet, "/health", nil)
	w := httptest.NewRecorder()

	s.Handler().ServeHTTP(w, req)

	// Degraded still returns 200
	if w.Code != http.StatusOK {
		t.Fatalf("expected 200, got %d", w.Code)
	}

	var resp HealthResponse
	json.Unmarshal(w.Body.Bytes(), &resp)

	if resp.Status != HealthStatusDegraded {
		t.Fatalf("expected degraded, got %s", resp.Status)
	}
}

func TestHealthServer_HandleReady_NotReady(t *testing.T) {
	s := NewHealthServer(nil)
	// Not setting ready

	req := httptest.NewRequest(http.MethodGet, "/ready", nil)
	w := httptest.NewRecorder()

	s.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusServiceUnavailable {
		t.Fatalf("expected 503, got %d", w.Code)
	}
}

func TestHealthServer_HandleReady_Ready(t *testing.T) {
	s := NewHealthServer(nil)
	s.SetReady(true)

	req := httptest.NewRequest(http.MethodGet, "/ready", nil)
	w := httptest.NewRecorder()

	s.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("expected 200, got %d", w.Code)
	}
}

func TestHealthServer_HandleLive_Live(t *testing.T) {
	s := NewHealthServer(nil)

	req := httptest.NewRequest(http.MethodGet, "/live", nil)
	w := httptest.NewRecorder()

	s.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusOK {
		t.Fatalf("expected 200, got %d", w.Code)
	}
}

func TestHealthServer_HandleLive_NotLive(t *testing.T) {
	s := NewHealthServer(nil)
	s.SetLive(false)

	req := httptest.NewRequest(http.MethodGet, "/live", nil)
	w := httptest.NewRecorder()

	s.Handler().ServeHTTP(w, req)

	if w.Code != http.StatusServiceUnavailable {
		t.Fatalf("expected 503, got %d", w.Code)
	}
}

func TestHealthServer_KubernetesAliases(t *testing.T) {
	s := NewHealthServer(nil)
	s.SetReady(true)

	tests := []struct {
		path string
		code int
	}{
		{"/healthz", http.StatusOK},
		{"/readyz", http.StatusOK},
		{"/livez", http.StatusOK},
	}

	for _, tt := range tests {
		t.Run(tt.path, func(t *testing.T) {
			req := httptest.NewRequest(http.MethodGet, tt.path, nil)
			w := httptest.NewRecorder()

			s.Handler().ServeHTTP(w, req)

			if w.Code != tt.code {
				t.Fatalf("expected %d, got %d", tt.code, w.Code)
			}
		})
	}
}

func TestHealthServer_MultipleChecks(t *testing.T) {
	s := NewHealthServer(nil)

	s.RegisterCheck("db", func(ctx context.Context) HealthCheck {
		return HealthCheck{Status: HealthStatusHealthy}
	})
	s.RegisterCheck("cache", func(ctx context.Context) HealthCheck {
		return HealthCheck{Status: HealthStatusHealthy}
	})
	s.RegisterCheck("queue", func(ctx context.Context) HealthCheck {
		return HealthCheck{Status: HealthStatusUnhealthy, Message: "queue down"}
	})

	req := httptest.NewRequest(http.MethodGet, "/health", nil)
	w := httptest.NewRecorder()

	s.Handler().ServeHTTP(w, req)

	// One unhealthy makes overall unhealthy
	if w.Code != http.StatusServiceUnavailable {
		t.Fatalf("expected 503, got %d", w.Code)
	}

	var resp HealthResponse
	json.Unmarshal(w.Body.Bytes(), &resp)

	if len(resp.Checks) != 3 {
		t.Fatalf("expected 3 checks, got %d", len(resp.Checks))
	}
}

// Test common health checkers

func TestTemporalHealthChecker_Healthy(t *testing.T) {
	checker := TemporalHealthChecker(func(ctx context.Context) error {
		return nil
	})

	result := checker(context.Background())
	if result.Status != HealthStatusHealthy {
		t.Fatalf("expected healthy, got %s", result.Status)
	}
}

func TestTemporalHealthChecker_Unhealthy(t *testing.T) {
	checker := TemporalHealthChecker(func(ctx context.Context) error {
		return errors.New("connection refused")
	})

	result := checker(context.Background())
	if result.Status != HealthStatusUnhealthy {
		t.Fatalf("expected unhealthy, got %s", result.Status)
	}
}

func TestDatabaseHealthChecker_Healthy(t *testing.T) {
	checker := DatabaseHealthChecker(func(ctx context.Context) error {
		return nil
	})

	result := checker(context.Background())
	if result.Status != HealthStatusHealthy {
		t.Fatalf("expected healthy, got %s", result.Status)
	}
}

func TestDatabaseHealthChecker_Unhealthy(t *testing.T) {
	checker := DatabaseHealthChecker(func(ctx context.Context) error {
		return errors.New("connection timeout")
	})

	result := checker(context.Background())
	if result.Status != HealthStatusUnhealthy {
		t.Fatalf("expected unhealthy, got %s", result.Status)
	}
}

func TestLLMHealthChecker_NilCheckFn(t *testing.T) {
	checker := LLMHealthChecker("openai", nil)

	result := checker(context.Background())
	if result.Status != HealthStatusHealthy {
		t.Fatalf("expected healthy, got %s", result.Status)
	}
}

func TestLLMHealthChecker_Degraded(t *testing.T) {
	checker := LLMHealthChecker("openai", func(ctx context.Context) error {
		return errors.New("rate limited")
	})

	result := checker(context.Background())
	if result.Status != HealthStatusDegraded {
		t.Fatalf("expected degraded, got %s", result.Status)
	}
}

func TestDiskSpaceHealthChecker(t *testing.T) {
	checker := DiskSpaceHealthChecker("/tmp", 1024*1024*100)

	result := checker(context.Background())
	if result.Status != HealthStatusHealthy {
		t.Fatalf("expected healthy, got %s", result.Status)
	}
}

func TestMemoryHealthChecker(t *testing.T) {
	checker := MemoryHealthChecker(80.0)

	result := checker(context.Background())
	if result.Status != HealthStatusHealthy {
		t.Fatalf("expected healthy, got %s", result.Status)
	}
}

func TestHealthResponse_ContentType(t *testing.T) {
	s := NewHealthServer(nil)

	req := httptest.NewRequest(http.MethodGet, "/health", nil)
	w := httptest.NewRecorder()

	s.Handler().ServeHTTP(w, req)

	contentType := w.Header().Get("Content-Type")
	if contentType != "application/json" {
		t.Fatalf("expected application/json, got %s", contentType)
	}
}
