package proofexplorer

import (
	"context"
	"embed"
	"encoding/json"
	"fmt"
	"io/fs"
	"log/slog"
	"net/http"
	"strings"
	"time"
)

//go:embed static
var staticFS embed.FS

// Config holds explorer server configuration.
type Config struct {
	ListenAddr string   // e.g. ":9091"
	PackDirs   []string // directories to scan for proof packs
}

// DefaultConfig returns sensible defaults.
func DefaultConfig() *Config {
	return &Config{
		ListenAddr: ":9091",
		PackDirs:   []string{"."},
	}
}

// Explorer is the proof pack web explorer.
type Explorer struct {
	config *Config
	store  *Store
	server *http.Server
}

// New creates a fully wired explorer.
func New(config *Config) (*Explorer, error) {
	store := NewStore(config.PackDirs...)
	if err := store.Scan(); err != nil {
		return nil, fmt.Errorf("scanning proof packs: %w", err)
	}

	e := &Explorer{
		config: config,
		store:  store,
	}

	mux := http.NewServeMux()

	// API routes
	mux.HandleFunc("/api/packs", e.handlePacks)
	mux.HandleFunc("/api/packs/", e.handlePackDetail)
	mux.HandleFunc("/api/stats", e.handleStats)
	mux.HandleFunc("/api/health", e.handleHealth)
	mux.HandleFunc("/api/rescan", e.handleRescan)

	// Static file server
	mux.HandleFunc("/", e.handleStatic)

	handler := corsMiddleware(loggingMiddleware(mux))

	e.server = &http.Server{
		Addr:         config.ListenAddr,
		Handler:      handler,
		ReadTimeout:  15 * time.Second,
		WriteTimeout: 15 * time.Second,
		IdleTimeout:  60 * time.Second,
	}

	return e, nil
}

// Start begins serving.
func (e *Explorer) Start() error {
	slog.Info("Starting proof pack explorer", "addr", e.config.ListenAddr, "packs", e.store.PackCount())
	if err := e.server.ListenAndServe(); err != nil && err != http.ErrServerClosed {
		return fmt.Errorf("explorer server error: %w", err)
	}
	return nil
}

// Stop gracefully shuts down the server.
func (e *Explorer) Stop(ctx context.Context) error {
	return e.server.Shutdown(ctx)
}

// PackCount returns the number of loaded proof packs.
func (e *Explorer) PackCount() int {
	return e.store.PackCount()
}

// handlePacks handles GET /api/packs
func (e *Explorer) handlePacks(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}
	respondJSON(w, e.store.List())
}

// handlePackDetail handles GET /api/packs/{id}
func (e *Explorer) handlePackDetail(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	id := strings.TrimPrefix(r.URL.Path, "/api/packs/")
	if id == "" {
		http.Error(w, "Pack ID required", http.StatusBadRequest)
		return
	}

	// Check for /fixtures sub-path
	parts := strings.SplitN(id, "/", 2)
	packID := parts[0]

	pack, ok := e.store.Get(packID)
	if !ok {
		http.Error(w, "Proof pack not found", http.StatusNotFound)
		return
	}

	// If requesting /api/packs/{id}/fixtures - return just fixtures
	if len(parts) == 2 && parts[1] == "fixtures" {
		respondJSON(w, pack.Results)
		return
	}

	respondJSON(w, pack)
}

// handleStats handles GET /api/stats
func (e *Explorer) handleStats(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}
	respondJSON(w, e.store.Stats())
}

// handleHealth handles GET /api/health
func (e *Explorer) handleHealth(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}
	respondJSON(w, map[string]interface{}{
		"status":     "ok",
		"time":       time.Now().Format(time.RFC3339),
		"pack_count": e.store.PackCount(),
	})
}

// handleRescan handles POST /api/rescan - re-scans directories for new proof packs
func (e *Explorer) handleRescan(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodPost {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	if err := e.store.Scan(); err != nil {
		http.Error(w, fmt.Sprintf("Rescan failed: %v", err), http.StatusInternalServerError)
		return
	}

	respondJSON(w, map[string]interface{}{
		"status":     "ok",
		"pack_count": e.store.PackCount(),
	})
}

// handleStatic serves embedded static files
func (e *Explorer) handleStatic(w http.ResponseWriter, r *http.Request) {
	if r.Method != http.MethodGet {
		http.Error(w, "Method not allowed", http.StatusMethodNotAllowed)
		return
	}

	staticFiles, err := fs.Sub(staticFS, "static")
	if err != nil {
		http.Error(w, "Internal server error", http.StatusInternalServerError)
		return
	}

	http.FileServer(http.FS(staticFiles)).ServeHTTP(w, r)
}

func respondJSON(w http.ResponseWriter, data interface{}) {
	w.Header().Set("Content-Type", "application/json")
	if err := json.NewEncoder(w).Encode(data); err != nil {
		slog.Error("Failed to encode JSON", "error", err)
	}
}

func corsMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Access-Control-Allow-Origin", "*")
		w.Header().Set("Access-Control-Allow-Methods", "GET, POST, OPTIONS")
		w.Header().Set("Access-Control-Allow-Headers", "Content-Type")
		if r.Method == http.MethodOptions {
			w.WriteHeader(http.StatusOK)
			return
		}
		next.ServeHTTP(w, r)
	})
}

func loggingMiddleware(next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		start := time.Now()
		next.ServeHTTP(w, r)
		slog.Debug("HTTP request", "method", r.Method, "path", r.URL.Path, "duration", time.Since(start))
	})
}
