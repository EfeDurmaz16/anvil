package harness

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"log"
	"net"
	"net/http"
	"net/http/httputil"
	"net/url"
	"os"
	"sync"
	"time"
)

// privateRanges lists the CIDR blocks considered private/loopback.
var privateRanges []*net.IPNet

func init() {
	cidrs := []string{
		"10.0.0.0/8",
		"172.16.0.0/12",
		"192.168.0.0/16",
		"127.0.0.0/8",
		"169.254.0.0/16",
		"::1/128",
		"fc00::/7",
	}
	for _, cidr := range cidrs {
		_, block, err := net.ParseCIDR(cidr)
		if err == nil {
			privateRanges = append(privateRanges, block)
		}
	}
}

// isPrivateIP resolves host and returns true if any resolved address falls
// within a private/loopback range (SSRF guard).
func isPrivateIP(host string) bool {
	// Strip port if present.
	h, _, err := net.SplitHostPort(host)
	if err != nil {
		// No port — use as-is.
		h = host
	}

	addrs, err := net.LookupHost(h)
	if err != nil {
		// Treat resolution failure as private to fail-safe.
		return true
	}

	for _, addr := range addrs {
		ip := net.ParseIP(addr)
		if ip == nil {
			continue
		}
		for _, block := range privateRanges {
			if block.Contains(ip) {
				return true
			}
		}
	}
	return false
}

// RecorderConfig configures the fixture recorder.
type RecorderConfig struct {
	TargetURL    string // Upstream service to proxy to
	ListenAddr   string // Address to listen on (default ":8090")
	OutputPath   string // JSONL file to write fixtures to
	MaxBodySize  int64  // Max captured body size in bytes (default 1MB)
	AllowPrivate bool   // Allow proxying to private/loopback IPs (for local dev)
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

	// Set a 30-second timeout on outbound proxy requests.
	proxy.Transport = &http.Transport{
		DialContext: (&net.Dialer{
			Timeout:   10 * time.Second,
			KeepAlive: 30 * time.Second,
		}).DialContext,
		ResponseHeaderTimeout: 30 * time.Second,
		TLSHandshakeTimeout:   10 * time.Second,
	}

	origDirector := proxy.Director
	proxy.Director = func(req *http.Request) {
		origDirector(req)
		req.Host = target.Host
	}
	proxy.ModifyResponse = rec.recordResponse
	proxy.ErrorHandler = func(w http.ResponseWriter, r *http.Request, err error) {
		log.Printf("recorder: proxy error for %s %s: %v", r.Method, r.URL.Path, err)
		http.Error(w, "Bad Gateway", http.StatusBadGateway)
	}

	mux := http.NewServeMux()
	mux.HandleFunc("/", func(w http.ResponseWriter, r *http.Request) {
		// SSRF guard: block requests that resolve to private/loopback addresses.
		if !config.AllowPrivate && isPrivateIP(target.Hostname()) {
			log.Printf("recorder: SSRF blocked — target %s resolves to a private IP", target.Hostname())
			http.Error(w, "Forbidden", http.StatusForbidden)
			return
		}

		// Apply per-request timeout via context.
		ctx, cancel := context.WithTimeout(r.Context(), 30*time.Second)
		defer cancel()
		r = r.WithContext(ctx)

		// Capture request body, applying MaxBodySize limit.
		var bodyBytes []byte
		if r.Body != nil {
			bodyBytes, _ = io.ReadAll(io.LimitReader(r.Body, config.MaxBodySize))
			r.Body.Close()
			r.Body = io.NopCloser(bytes.NewReader(bodyBytes))
		}

		// Flatten headers to single values.
		headers := make(map[string]string)
		for k, v := range r.Header {
			if len(v) > 0 {
				headers[k] = v[0]
			}
		}

		// Try to parse body as JSON, fall back to string.
		var jsonBody json.RawMessage
		if len(bodyBytes) > 0 {
			if json.Valid(bodyBytes) {
				jsonBody = bodyBytes
			} else {
				quoted, _ := json.Marshal(string(bodyBytes))
				jsonBody = quoted
			}
		}

		ctx = context.WithValue(r.Context(), reqDataKey, &capturedRequest{
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

	// Read response body, applying MaxBodySize limit.
	bodyBytes, err := io.ReadAll(io.LimitReader(resp.Body, rec.config.MaxBodySize))
	if err != nil {
		log.Printf("recorder: error reading response body: %v", err)
		return err
	}
	resp.Body.Close()
	resp.Body = io.NopCloser(bytes.NewReader(bodyBytes))

	// Flatten response headers.
	respHeaders := make(map[string]string)
	for k, v := range resp.Header {
		if len(v) > 0 {
			respHeaders[k] = v[0]
		}
	}

	// Build response body (try JSON, fall back to string).
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

	if err != nil {
		log.Printf("recorder: error writing fixture: %v", err)
	}
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
