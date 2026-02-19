package dashboard

import (
	"context"
	"embed"
	"encoding/json"
	"fmt"
	"io/fs"
	"log/slog"
	"net/http"
	"strconv"
	"strings"
	"time"
)

//go:embed static
var staticFS embed.FS

// Config holds dashboard server configuration.
type Config struct {
	ListenAddr string // e.g. ":9090"
}

// DefaultConfig returns sensible defaults.
func DefaultConfig() *Config {
	return &Config{ListenAddr: ":9090"}
}

// Server is the dashboard HTTP server.
type Server struct {
	config *Config
	store  *Store
	hub    *Hub
	server *http.Server
}

// NewServer creates a new dashboard server.
func NewServer(config *Config, store *Store, hub *Hub) *Server {
	s := &Server{
		config: config,
		store:  store,
		hub:    hub,
	}

	mux := http.NewServeMux()

	// API routes
	mux.HandleFunc("/api/migrations", s.handleMigrations)
	mux.HandleFunc("/api/migrations/", s.handleMigrationDetail)
	mux.HandleFunc("/api/stats", s.handleStats)
	mux.HandleFunc("/api/health", s.handleHealth)
	mux.HandleFunc("/api/events", s.handleSSE)

	// Static file server
	mux.HandleFunc("/", s.handleStatic)

	// Wrap with CORS and logging middleware
	handler := corsMiddleware(loggingMiddleware(mux))

	s.server = &http.Server{
		Addr:         config.ListenAddr,
		Handler:      handler,
		ReadTimeout:  15 * time.Second,
		WriteTimeout: 15 * time.Second,
		IdleTimeout:  60 * time.Second,
	}

	return s
}

// Start begins serving the dashboard.
func (s *Server) Start() error {
	slog.Info("Starting dashboard server", "addr", s.config.ListenAddr)
	if err := s.server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		return fmt.Errorf("dashboard server error: %w", err)
	}
	return nil
}

// Stop gracefully shuts down the server.
func (s *Server) Stop(ctx context.Context) error {
	slog.Info("Stopping dashboard server")
	return s.server.Shutdown(ctx)
}

// handleMigrations handles GET /api/migrations
func (s *Server) handleMigrations(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	runs := s.store.ListRuns()
	respondJSON(w, runs)
}

// handleMigrationDetail handles GET /api/migrations/{id} and GET /api/migrations/{id}/logs
func (s *Server) handleMigrationDetail(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Parse path: /api/migrations/{id} or /api/migrations/{id}/logs
	path := strings.TrimPrefix(r.URL.Path, "/api/migrations/")
	parts := strings.SplitN(path, "/", 2)

	if len(parts) == 0 || parts[0] == "" {
		http.Error(w, "Migration ID required", http.StatusBadRequest)
		return
	}

	id := parts[0]

	// Check if this is a logs request
	if len(parts) == 2 && parts[1] == "logs" {
		s.handleLogs(w, r, id)
		return
	}

	// Get single migration run
	run, ok := s.store.GetRun(id)
	if !ok {
		http.Error(w, "Migration not found", http.StatusNotFound)
		return
	}

	respondJSON(w, run)
}

// handleLogs handles GET /api/migrations/{id}/logs
func (s *Server) handleLogs(w http.ResponseWriter, r *http.Request, id string) {
	// Parse limit query parameter
	limit := 100 // default
	if limitStr := r.URL.Query().Get("limit"); limitStr != "" {
		if parsedLimit, err := strconv.Atoi(limitStr); err == nil && parsedLimit > 0 {
			limit = parsedLimit
		}
	}

	logs := s.store.GetLogs(id, limit)
	respondJSON(w, logs)
}

// handleStats handles GET /api/stats
func (s *Server) handleStats(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	stats := s.store.GetStats()
	respondJSON(w, stats)
}

// handleHealth handles GET /api/health
func (s *Server) handleHealth(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	respondJSON(w, map[string]string{
		"status": "ok",
		"time":   time.Now().Format(time.RFC3339),
	})
}

// handleSSE handles GET /api/events (Server-Sent Events)
func (s *Server) handleSSE(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Create SSE client
	client, err := NewClient(s.hub, w)
	if err != nil {
		http.Error(w, "Streaming not supported", http.StatusInternalServerError)
		return
	}

	// Register client with hub
	s.hub.Register(client)
	defer s.hub.Unregister(client)

	slog.Info("SSE client connected")

	// Send initial connection event
	connEvent := &Event{
		Type:      "connected",
		Timestamp: time.Now(),
	}
	data, _ := json.Marshal(connEvent)
	client.send(data)

	// Start keepalive in background
	go client.KeepAlive(30 * time.Second)

	// Block until client disconnects
	<-r.Context().Done()
	slog.Info("SSE client disconnected")
}

// handleStatic serves embedded static files
func (s *Server) handleStatic(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	// Get the static file system
	staticFiles, err := fs.Sub(staticFS, "static")
	if err != nil {
		slog.Error("Failed to access static files", "error", err)
		http.Error(w, "Internal server error", http.StatusInternalServerError)
		return
	}

	// Use path from URL
	path := strings.TrimPrefix(r.URL.Path, "/")
	if path == "" {
		path = "index.html"
	}

	// Try to serve the file
	http.FileServer(http.FS(staticFiles)).ServeHTTP(w, r)
}

// respondJSON writes a JSON response
func respondJSON(w http.ResponseWriter, data interface{}) {
	w.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(w).Encode(data); err != nil {
		slog.Error("Failed to encode JSON response", "error", err)
		http.Error(w, "Internal server error", http.StatusInternalServerError)
	}
}

// corsMiddleware adds CORS headers for local development
func corsMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, PUT, DELETE, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type, Authorization")

		if r.Method == http.MethodOptions {
			w.WriteHeader(http.StatusOK)
			return
		}

		next.ServeHTTP(w, r)
	})
}

// loggingMiddleware logs HTTP requests
func loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()
		next.ServeHTTP(w, r)
		slog.Debug("HTTP request",
			"method", r.Method,
			"path", r.URL.Path,
			"duration", time.Since(start),
		)
	})
}
