package server

import (
	"net/http"
	"os"
	"strings"
)

// APIKeyMiddleware returns middleware that validates X-API-Key header or api_key query param.
// If apiKey is empty, the middleware is a no-op (development mode).
func APIKeyMiddleware(apiKey string) func(http.Handler) http.Handler {
	return func(next http.Handler) http.Handler {
		if apiKey == "" {
			return next // No auth in dev mode
		}
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			// Check header first
			key := r.Header.Get("X-API-Key")
			if key == "" {
				// Fall back to query param
				key = r.URL.Query().Get("api_key")
			}
			if key != apiKey {
				http.Error(w, `{"error":"unauthorized"}`, http.StatusUnauthorized)
				return
			}
			next.ServeHTTP(w, r)
		})
	}
}

// CORSMiddleware returns middleware with configurable allowed origins.
// Origins is a comma-separated list. If empty, defaults to localhost only.
func CORSMiddleware(origins string) func(http.Handler) http.Handler {
	allowed := parseOrigins(origins)
	return func(next http.Handler) http.Handler {
		return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			origin := r.Header.Get("Origin")
			if origin != "" && isOriginAllowed(origin, allowed) {
				w.Header().Set("Access-Control-Allow-Origin", origin)
				w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
				w.Header().Set("Access-Control-Allow-Headers", "Content-Type, X-API-Key")
				w.Header().Set("Access-Control-Max-Age", "86400")
			}
			if r.Method == http.MethodOptions {
				w.WriteHeader(http.StatusNoContent)
				return
			}
			next.ServeHTTP(w, r)
		})
	}
}

func parseOrigins(s string) []string {
	if s == "" {
		return []string{"http://localhost:*"}
	}
	parts := strings.Split(s, ",")
	var result []string
	for _, p := range parts {
		p = strings.TrimSpace(p)
		if p != "" {
			result = append(result, p)
		}
	}
	return result
}

func isOriginAllowed(origin string, allowed []string) bool {
	for _, a := range allowed {
		if a == "*" {
			return true
		}
		if strings.Contains(a, "*") {
			// Simple wildcard match: "http://localhost:*" matches "http://localhost:3000"
			prefix := strings.Split(a, "*")[0]
			if strings.HasPrefix(origin, prefix) {
				return true
			}
		}
		if a == origin {
			return true
		}
	}
	return false
}

// FromEnv creates middleware stack from environment variables.
// ANVIL_API_KEY - API key for auth (empty = no auth)
// ANVIL_CORS_ORIGINS - comma-separated origins (empty = localhost only)
func FromEnv(next http.Handler) http.Handler {
	apiKey := os.Getenv("ANVIL_API_KEY")
	corsOrigins := os.Getenv("ANVIL_CORS_ORIGINS")

	h := CORSMiddleware(corsOrigins)(next)
	h = APIKeyMiddleware(apiKey)(h)
	return h
}
