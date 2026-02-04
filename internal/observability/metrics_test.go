package observability

import (
	"net/http/httptest"
	"strings"
	"testing"
	"time"
)

func TestNewMetricsRegistry(t *testing.T) {
	r := NewMetricsRegistry()
	if r == nil {
		t.Fatal("expected non-nil registry")
	}
}

func TestCounter_Inc(t *testing.T) {
	r := NewMetricsRegistry()
	c := r.NewCounter("test_counter", "Test counter", nil)

	c.Inc()
	c.Inc()
	c.Inc()

	if c.Value() != 3 {
		t.Fatalf("expected 3, got %f", c.Value())
	}
}

func TestCounter_Add(t *testing.T) {
	r := NewMetricsRegistry()
	c := r.NewCounter("test_counter", "Test counter", nil)

	c.Add(5)
	c.Add(3.5)

	if c.Value() != 8.5 {
		t.Fatalf("expected 8.5, got %f", c.Value())
	}
}

func TestGauge_Set(t *testing.T) {
	r := NewMetricsRegistry()
	g := r.NewGauge("test_gauge", "Test gauge", nil)

	g.Set(42)
	if g.Value() != 42 {
		t.Fatalf("expected 42, got %f", g.Value())
	}

	g.Set(10)
	if g.Value() != 10 {
		t.Fatalf("expected 10, got %f", g.Value())
	}
}

func TestGauge_IncDec(t *testing.T) {
	r := NewMetricsRegistry()
	g := r.NewGauge("test_gauge", "Test gauge", nil)

	g.Inc()
	g.Inc()
	g.Dec()

	if g.Value() != 1 {
		t.Fatalf("expected 1, got %f", g.Value())
	}
}

func TestGauge_Add(t *testing.T) {
	r := NewMetricsRegistry()
	g := r.NewGauge("test_gauge", "Test gauge", nil)

	g.Add(10)
	g.Add(-3)

	if g.Value() != 7 {
		t.Fatalf("expected 7, got %f", g.Value())
	}
}

func TestHistogram_Observe(t *testing.T) {
	r := NewMetricsRegistry()
	h := r.NewHistogram("test_histogram", "Test histogram", nil, []float64{1, 5, 10})

	h.Observe(0.5)
	h.Observe(3)
	h.Observe(7)
	h.Observe(15)

	if h.count != 4 {
		t.Fatalf("expected count 4, got %d", h.count)
	}
	if h.sum != 25.5 {
		t.Fatalf("expected sum 25.5, got %f", h.sum)
	}
}

func TestHistogram_ObserveDuration(t *testing.T) {
	r := NewMetricsRegistry()
	h := r.NewHistogram("test_histogram", "Test histogram", nil, nil)

	start := time.Now().Add(-100 * time.Millisecond)
	h.ObserveDuration(start)

	if h.count != 1 {
		t.Fatalf("expected count 1, got %d", h.count)
	}
	if h.sum < 0.1 {
		t.Fatalf("expected sum >= 0.1, got %f", h.sum)
	}
}

func TestDefaultBuckets(t *testing.T) {
	buckets := DefaultBuckets()
	if len(buckets) == 0 {
		t.Fatal("expected non-empty buckets")
	}
	// Should be in ascending order
	for i := 1; i < len(buckets); i++ {
		if buckets[i] <= buckets[i-1] {
			t.Fatal("buckets should be in ascending order")
		}
	}
}

func TestMetricsRegistry_Handler(t *testing.T) {
	r := NewMetricsRegistry()
	r.NewCounter("test_counter", "A test counter", nil).Inc()
	r.NewGauge("test_gauge", "A test gauge", nil).Set(42)

	req := httptest.NewRequest("GET", "/metrics", nil)
	w := httptest.NewRecorder()

	r.Handler().ServeHTTP(w, req)

	body := w.Body.String()

	if !strings.Contains(body, "test_counter") {
		t.Fatal("expected test_counter in output")
	}
	if !strings.Contains(body, "test_gauge") {
		t.Fatal("expected test_gauge in output")
	}
	if !strings.Contains(body, "# HELP") {
		t.Fatal("expected HELP comments")
	}
	if !strings.Contains(body, "# TYPE") {
		t.Fatal("expected TYPE comments")
	}
}

func TestMetricsRegistry_Handler_ContentType(t *testing.T) {
	r := NewMetricsRegistry()

	req := httptest.NewRequest("GET", "/metrics", nil)
	w := httptest.NewRecorder()

	r.Handler().ServeHTTP(w, req)

	ct := w.Header().Get("Content-Type")
	if !strings.Contains(ct, "text/plain") {
		t.Fatalf("expected text/plain content type, got %s", ct)
	}
}

func TestMetricsWithLabels(t *testing.T) {
	r := NewMetricsRegistry()
	labels := map[string]string{"method": "POST", "path": "/api"}
	c := r.NewCounter("http_requests", "HTTP requests", labels)
	c.Inc()

	req := httptest.NewRequest("GET", "/metrics", nil)
	w := httptest.NewRecorder()

	r.Handler().ServeHTTP(w, req)

	body := w.Body.String()
	if !strings.Contains(body, `method="POST"`) {
		t.Fatal("expected method label in output")
	}
	if !strings.Contains(body, `path="/api"`) {
		t.Fatal("expected path label in output")
	}
}

func TestHistogramOutput(t *testing.T) {
	r := NewMetricsRegistry()
	h := r.NewHistogram("request_duration", "Request duration", nil, []float64{0.1, 0.5, 1.0})
	h.Observe(0.05)
	h.Observe(0.3)
	h.Observe(0.8)

	req := httptest.NewRequest("GET", "/metrics", nil)
	w := httptest.NewRecorder()

	r.Handler().ServeHTTP(w, req)

	body := w.Body.String()
	if !strings.Contains(body, "request_duration_bucket") {
		t.Fatal("expected bucket metrics")
	}
	if !strings.Contains(body, "request_duration_sum") {
		t.Fatal("expected sum metric")
	}
	if !strings.Contains(body, "request_duration_count") {
		t.Fatal("expected count metric")
	}
	if !strings.Contains(body, `le="+Inf"`) {
		t.Fatal("expected +Inf bucket")
	}
}

// Anvil metrics tests

func TestNewAnvilMetrics(t *testing.T) {
	m := NewAnvilMetrics()
	if m == nil {
		t.Fatal("expected non-nil metrics")
	}
	if m.Registry == nil {
		t.Fatal("expected non-nil registry")
	}
}

func TestAnvilMetrics_RecordLLMRequest(t *testing.T) {
	m := NewAnvilMetrics()

	m.RecordLLMRequest(100*time.Millisecond, 500, nil)
	m.RecordLLMRequest(200*time.Millisecond, 300, nil)

	if m.LLMRequestsTotal.Value() != 2 {
		t.Fatalf("expected 2 requests, got %f", m.LLMRequestsTotal.Value())
	}
	if m.LLMTokensTotal.Value() != 800 {
		t.Fatalf("expected 800 tokens, got %f", m.LLMTokensTotal.Value())
	}
	if m.LLMErrorsTotal.Value() != 0 {
		t.Fatalf("expected 0 errors, got %f", m.LLMErrorsTotal.Value())
	}
}

func TestAnvilMetrics_RecordLLMRequest_WithError(t *testing.T) {
	m := NewAnvilMetrics()

	m.RecordLLMRequest(100*time.Millisecond, 0, errTest)

	if m.LLMErrorsTotal.Value() != 1 {
		t.Fatalf("expected 1 error, got %f", m.LLMErrorsTotal.Value())
	}
}

func TestAnvilMetrics_RecordAgentRun(t *testing.T) {
	m := NewAnvilMetrics()

	m.RecordAgentRun(5*time.Second, 0.95, nil)

	if m.AgentRunsTotal.Value() != 1 {
		t.Fatalf("expected 1 run, got %f", m.AgentRunsTotal.Value())
	}
	if m.AgentScoreGauge.Value() != 0.95 {
		t.Fatalf("expected score 0.95, got %f", m.AgentScoreGauge.Value())
	}
}

func TestAnvilMetrics_RecordFixture(t *testing.T) {
	m := NewAnvilMetrics()

	m.RecordFixture(100*time.Millisecond, true)
	m.RecordFixture(150*time.Millisecond, true)
	m.RecordFixture(200*time.Millisecond, false)

	if m.FixturesTotal.Value() != 3 {
		t.Fatalf("expected 3 fixtures, got %f", m.FixturesTotal.Value())
	}
	if m.FixturesPassedTotal.Value() != 2 {
		t.Fatalf("expected 2 passed, got %f", m.FixturesPassedTotal.Value())
	}
	if m.FixturesFailedTotal.Value() != 1 {
		t.Fatalf("expected 1 failed, got %f", m.FixturesFailedTotal.Value())
	}
}

func TestAnvilMetrics_RecordCompilation(t *testing.T) {
	m := NewAnvilMetrics()

	m.RecordCompilation(2*time.Second, true)
	m.RecordCompilation(3*time.Second, false)

	if m.CompilationsTotal.Value() != 2 {
		t.Fatalf("expected 2 compilations, got %f", m.CompilationsTotal.Value())
	}
	if m.CompileSuccessTotal.Value() != 1 {
		t.Fatalf("expected 1 success, got %f", m.CompileSuccessTotal.Value())
	}
	if m.CompileFailureTotal.Value() != 1 {
		t.Fatalf("expected 1 failure, got %f", m.CompileFailureTotal.Value())
	}
}

func TestAnvilMetrics_Handler(t *testing.T) {
	m := NewAnvilMetrics()
	m.LLMRequestsTotal.Inc()

	req := httptest.NewRequest("GET", "/metrics", nil)
	w := httptest.NewRecorder()

	m.Handler().ServeHTTP(w, req)

	body := w.Body.String()
	if !strings.Contains(body, "anvil_llm_requests_total") {
		t.Fatal("expected anvil metrics in output")
	}
}

func TestGlobalMetrics(t *testing.T) {
	m := Metrics()
	if m == nil {
		t.Fatal("expected non-nil global metrics")
	}

	// Should return same instance
	m2 := Metrics()
	if m != m2 {
		t.Fatal("expected same instance")
	}
}

func TestFormatLabels_Empty(t *testing.T) {
	result := formatLabels(nil)
	if result != "" {
		t.Fatalf("expected empty string, got %s", result)
	}

	result = formatLabels(map[string]string{})
	if result != "" {
		t.Fatalf("expected empty string, got %s", result)
	}
}

func TestFormatFloat(t *testing.T) {
	tests := []struct {
		input    float64
		expected string
	}{
		{0, "0"},
		{1, "1"},
		{42, "42"},
		{1.5, "1.5"},
	}

	for _, tt := range tests {
		result := formatFloat(tt.input)
		if result != tt.expected {
			t.Errorf("formatFloat(%f) = %s, expected %s", tt.input, result, tt.expected)
		}
	}
}

func TestFormatUint(t *testing.T) {
	tests := []struct {
		input    uint64
		expected string
	}{
		{0, "0"},
		{1, "1"},
		{42, "42"},
		{1000000, "1000000"},
	}

	for _, tt := range tests {
		result := formatUint(tt.input)
		if result != tt.expected {
			t.Errorf("formatUint(%d) = %s, expected %s", tt.input, result, tt.expected)
		}
	}
}

// Helper error for testing
var errTest = &testMetricsError{msg: "test error"}

type testMetricsError struct {
	msg string
}

func (e *testMetricsError) Error() string {
	return e.msg
}
