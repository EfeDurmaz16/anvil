package harness

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/http/httputil"
	"net/url"
	"os"
	"sync"
	"time"
)

// RecorderConfig configures the fixture recorder.
type RecorderConfig struct {
	TargetURL   string // Upstream service to proxy to
	ListenAddr  string // Address to listen on (default ":8090")
	OutputPath  string // JSONL file to write fixtures to
	MaxBodySize int64  // Max captured body size in bytes (default 1MB)
}

// Recorder is an HTTP reverse proxy that captures request/response pairs as JSONL fixtures.
type Recorder struct {
	config *RecorderConfig
	server *http.Server
	output *os.File
	enc    *json.Encoder
	mu     sync.Mutex
	count  int
}

type contextKey string

const reqDataKey contextKey = "recorder_req"

type capturedRequest struct {
	Method string
	Path   string
	Header map[string]string
	Body   json.RawMessage
}

// NewRecorder creates a new fixture recorder.
func NewRecorder(config *RecorderConfig) (*Recorder, error) {
	if config.ListenAddr == "" {
		config.ListenAddr = ":8090"
	}
	if config.MaxBodySize == 0 {
		config.MaxBodySize = 1 << 20
	}

	target, err := url.Parse(config.TargetURL)
	if err != nil {
		return nil, fmt.Errorf("invalid target URL: %w", err)
	}

	f, err := os.Create(config.OutputPath)
	if err != nil {
		return nil, fmt.Errorf("create output file: %w", err)
	}

	rec := &Recorder{
		config: config,
		output: f,
		enc:    json.NewEncoder(f),
	}

	proxy := httputil.NewSingleHostReverseProxy(target)
	origDirector := proxy.Director
	proxy.Director = func(req *http.Request) {
		origDirector(req)
		req.Host = target.Host
	}
	proxy.ModifyResponse = rec.recordResponse

	mux := http.NewServeMux()
	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		// Capture request body
		var bodyBytes []byte
		if r.Body != nil {
			bodyBytes, _ = io.ReadAll(io.LimitReader(r.Body, config.MaxBodySize))
			r.Body.Close()
			r.Body = io.NopCloser(bytes.NewReader(bodyBytes))
		}

		// Flatten headers to single values
		headers := make(map[string]string)
		for k, v := range r.Header {
			if len(v) > 0 {
				headers[k] = v[0]
			}
		}

		// Try to parse body as JSON, fall back to string
		var jsonBody json.RawMessage
		if len(bodyBytes) > 0 {
			if json.Valid(bodyBytes) {
				jsonBody = bodyBytes
			} else {
				quoted, _ := json.Marshal(string(bodyBytes))
				jsonBody = quoted
			}
		}

		ctx := context.WithValue(r.Context(), reqDataKey, &capturedRequest{
			Method: r.Method,
			Path:   r.URL.Path,
			Header: headers,
			Body:   jsonBody,
		})
		proxy.ServeHTTP(w, r.WithContext(ctx))
	})

	rec.server = &http.Server{
		Addr:         config.ListenAddr,
		Handler:      mux,
		ReadTimeout:  30 * time.Second,
		WriteTimeout: 60 * time.Second,
	}

	return rec, nil
}

// recordResponse captures the response and writes a fixture line.
func (rec *Recorder) recordResponse(resp *http.Response) error {
	reqData, _ := resp.Request.Context().Value(reqDataKey).(*capturedRequest)
	if reqData == nil {
		return nil
	}

	// Read response body
	bodyBytes, err := io.ReadAll(io.LimitReader(resp.Body, rec.config.MaxBodySize))
	if err != nil {
		return err
	}
	resp.Body.Close()
	resp.Body = io.NopCloser(bytes.NewReader(bodyBytes))

	// Flatten response headers
	respHeaders := make(map[string]string)
	for k, v := range resp.Header {
		if len(v) > 0 {
			respHeaders[k] = v[0]
		}
	}

	// Build response body (try JSON, fall back to string)
	var respBody json.RawMessage
	if len(bodyBytes) > 0 {
		if json.Valid(bodyBytes) {
			respBody = bodyBytes
		} else {
			quoted, _ := json.Marshal(string(bodyBytes))
			respBody = quoted
		}
	}

	rec.mu.Lock()
	rec.count++
	name := fmt.Sprintf("recorded-%03d", rec.count)
	rec.mu.Unlock()

	fixture := map[string]interface{}{
		"kind": "http",
		"name": name,
		"http": map[string]interface{}{
			"method": reqData.Method,
			"path":   reqData.Path,
			"header": reqData.Header,
			"body":   reqData.Body,
		},
		"expected_status": resp.StatusCode,
		"expected_header": respHeaders,
		"expected_body":   respBody,
	}

	rec.mu.Lock()
	err = rec.enc.Encode(fixture)
	rec.mu.Unlock()

	return err
}

// Start begins listening and proxying.
func (rec *Recorder) Start() error {
	return rec.server.ListenAndServe()
}

// Stop gracefully shuts down the recorder.
func (rec *Recorder) Stop(ctx context.Context) error {
	defer rec.output.Close()
	return rec.server.Shutdown(ctx)
}

// Count returns the number of fixtures recorded.
func (rec *Recorder) Count() int {
	rec.mu.Lock()
	defer rec.mu.Unlock()
	return rec.count
}
