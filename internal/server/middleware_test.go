package server

import (
	"net/http"
	"net/http/httptest"
	"testing"
)

func okHandler() http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.WriteHeader(http.StatusOK)
	})
}

func TestAPIKeyMiddleware_NoKey(t *testing.T) {
	// Dev mode: empty apiKey means all requests pass through
	h := APIKeyMiddleware("")(okHandler())
	req := httptest.NewRequest(http.MethodGet, "/", nil)
	rr := httptest.NewRecorder()
	h.ServeHTTP(rr, req)
	if rr.Code != http.StatusOK {
		t.Errorf("expected 200, got %d", rr.Code)
	}
}

func TestAPIKeyMiddleware_ValidHeader(t *testing.T) {
	h := APIKeyMiddleware("secret")(okHandler())
	req := httptest.NewRequest(http.MethodGet, "/", nil)
	req.Header.Set("X-API-Key", "secret")
	rr := httptest.NewRecorder()
	h.ServeHTTP(rr, req)
	if rr.Code != http.StatusOK {
		t.Errorf("expected 200, got %d", rr.Code)
	}
}

func TestAPIKeyMiddleware_ValidQueryParam(t *testing.T) {
	h := APIKeyMiddleware("secret")(okHandler())
	req := httptest.NewRequest(http.MethodGet, "/?api_key=secret", nil)
	rr := httptest.NewRecorder()
	h.ServeHTTP(rr, req)
	if rr.Code != http.StatusOK {
		t.Errorf("expected 200, got %d", rr.Code)
	}
}

func TestAPIKeyMiddleware_InvalidKey(t *testing.T) {
	h := APIKeyMiddleware("secret")(okHandler())
	req := httptest.NewRequest(http.MethodGet, "/", nil)
	req.Header.Set("X-API-Key", "wrong")
	rr := httptest.NewRecorder()
	h.ServeHTTP(rr, req)
	if rr.Code != http.StatusUnauthorized {
		t.Errorf("expected 401, got %d", rr.Code)
	}
}

func TestCORSMiddleware_AllowedOrigin(t *testing.T) {
	// Default (empty) origins allows localhost:*
	h := CORSMiddleware("")(okHandler())
	req := httptest.NewRequest(http.MethodGet, "/", nil)
	req.Header.Set("Origin", "http://localhost:3000")
	rr := httptest.NewRecorder()
	h.ServeHTTP(rr, req)
	if rr.Code != http.StatusOK {
		t.Errorf("expected 200, got %d", rr.Code)
	}
	if got := rr.Header().Get("Access-Control-Allow-Origin"); got != "http://localhost:3000" {
		t.Errorf("expected CORS header to be set, got %q", got)
	}
}

func TestCORSMiddleware_BlockedOrigin(t *testing.T) {
	h := CORSMiddleware("")(okHandler())
	req := httptest.NewRequest(http.MethodGet, "/", nil)
	req.Header.Set("Origin", "https://evil.com")
	rr := httptest.NewRecorder()
	h.ServeHTTP(rr, req)
	if rr.Code != http.StatusOK {
		t.Errorf("expected 200 passthrough, got %d", rr.Code)
	}
	if got := rr.Header().Get("Access-Control-Allow-Origin"); got != "" {
		t.Errorf("expected no CORS header for blocked origin, got %q", got)
	}
}

func TestCORSMiddleware_Wildcard(t *testing.T) {
	h := CORSMiddleware("*")(okHandler())
	req := httptest.NewRequest(http.MethodGet, "/", nil)
	req.Header.Set("Origin", "https://any-origin.com")
	rr := httptest.NewRecorder()
	h.ServeHTTP(rr, req)
	if got := rr.Header().Get("Access-Control-Allow-Origin"); got != "https://any-origin.com" {
		t.Errorf("expected CORS header for wildcard, got %q", got)
	}
}

func TestCORSMiddleware_Preflight(t *testing.T) {
	h := CORSMiddleware("*")(okHandler())
	req := httptest.NewRequest(http.MethodOptions, "/", nil)
	req.Header.Set("Origin", "https://any-origin.com")
	rr := httptest.NewRecorder()
	h.ServeHTTP(rr, req)
	if rr.Code != http.StatusNoContent {
		t.Errorf("expected 204 for OPTIONS preflight, got %d", rr.Code)
	}
}
