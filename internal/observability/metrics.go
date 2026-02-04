package observability

import (
	"net/http"
	"sync"
	"time"
)

// MetricsRegistry holds all registered metrics.
type MetricsRegistry struct {
	mu       sync.RWMutex
	counters map[string]*Counter
	gauges   map[string]*Gauge
	histos   map[string]*Histogram
}

// Counter is a monotonically increasing metric.
type Counter struct {
	name   string
	help   string
	labels map[string]string
	value  float64
	mu     sync.Mutex
}

// Gauge is a metric that can go up or down.
type Gauge struct {
	name   string
	help   string
	labels map[string]string
	value  float64
	mu     sync.Mutex
}

// Histogram tracks distribution of values.
type Histogram struct {
	name    string
	help    string
	labels  map[string]string
	buckets []float64
	counts  []uint64
	sum     float64
	count   uint64
	mu      sync.Mutex
}

// NewMetricsRegistry creates a new metrics registry.
func NewMetricsRegistry() *MetricsRegistry {
	return &MetricsRegistry{
		counters: make(map[string]*Counter),
		gauges:   make(map[string]*Gauge),
		histos:   make(map[string]*Histogram),
	}
}

// NewCounter creates and registers a counter.
func (r *MetricsRegistry) NewCounter(name, help string, labels map[string]string) *Counter {
	r.mu.Lock()
	defer r.mu.Unlock()

	c := &Counter{name: name, help: help, labels: labels}
	r.counters[name] = c
	return c
}

// NewGauge creates and registers a gauge.
func (r *MetricsRegistry) NewGauge(name, help string, labels map[string]string) *Gauge {
	r.mu.Lock()
	defer r.mu.Unlock()

	g := &Gauge{name: name, help: help, labels: labels}
	r.gauges[name] = g
	return g
}

// NewHistogram creates and registers a histogram.
func (r *MetricsRegistry) NewHistogram(name, help string, labels map[string]string, buckets []float64) *Histogram {
	r.mu.Lock()
	defer r.mu.Unlock()

	if buckets == nil {
		buckets = DefaultBuckets()
	}

	h := &Histogram{
		name:    name,
		help:    help,
		labels:  labels,
		buckets: buckets,
		counts:  make([]uint64, len(buckets)),
	}
	r.histos[name] = h
	return h
}

// DefaultBuckets returns default histogram buckets for latency.
func DefaultBuckets() []float64 {
	return []float64{0.001, 0.005, 0.01, 0.025, 0.05, 0.1, 0.25, 0.5, 1, 2.5, 5, 10}
}

// Inc increments a counter by 1.
func (c *Counter) Inc() {
	c.Add(1)
}

// Add adds a value to the counter.
func (c *Counter) Add(v float64) {
	c.mu.Lock()
	c.value += v
	c.mu.Unlock()
}

// Value returns the counter value.
func (c *Counter) Value() float64 {
	c.mu.Lock()
	defer c.mu.Unlock()
	return c.value
}

// Set sets the gauge value.
func (g *Gauge) Set(v float64) {
	g.mu.Lock()
	g.value = v
	g.mu.Unlock()
}

// Inc increments the gauge by 1.
func (g *Gauge) Inc() {
	g.Add(1)
}

// Dec decrements the gauge by 1.
func (g *Gauge) Dec() {
	g.Add(-1)
}

// Add adds a value to the gauge.
func (g *Gauge) Add(v float64) {
	g.mu.Lock()
	g.value += v
	g.mu.Unlock()
}

// Value returns the gauge value.
func (g *Gauge) Value() float64 {
	g.mu.Lock()
	defer g.mu.Unlock()
	return g.value
}

// Observe records a value in the histogram.
func (h *Histogram) Observe(v float64) {
	h.mu.Lock()
	defer h.mu.Unlock()

	h.sum += v
	h.count++

	for i, bound := range h.buckets {
		if v <= bound {
			h.counts[i]++
		}
	}
}

// ObserveDuration records a duration in the histogram.
func (h *Histogram) ObserveDuration(start time.Time) {
	h.Observe(time.Since(start).Seconds())
}

// Handler returns an HTTP handler for Prometheus metrics.
func (r *MetricsRegistry) Handler() http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, req *http.Request) {
		w.Header().Set("Content-Type", "text/plain; version=0.0.4; charset=utf-8")
		r.WritePrometheus(w)
	})
}

// WritePrometheus writes metrics in Prometheus text format.
func (r *MetricsRegistry) WritePrometheus(w http.ResponseWriter) {
	r.mu.RLock()
	defer r.mu.RUnlock()

	// Write counters
	for _, c := range r.counters {
		c.mu.Lock()
		writeMetric(w, c.name, "counter", c.help, c.labels, c.value)
		c.mu.Unlock()
	}

	// Write gauges
	for _, g := range r.gauges {
		g.mu.Lock()
		writeMetric(w, g.name, "gauge", g.help, g.labels, g.value)
		g.mu.Unlock()
	}

	// Write histograms
	for _, h := range r.histos {
		h.mu.Lock()
		writeHistogram(w, h)
		h.mu.Unlock()
	}
}

func writeMetric(w http.ResponseWriter, name, metricType, help string, labels map[string]string, value float64) {
	w.Write([]byte("# HELP " + name + " " + help + "\n"))
	w.Write([]byte("# TYPE " + name + " " + metricType + "\n"))
	w.Write([]byte(name + formatLabels(labels) + " "))
	w.Write([]byte(formatFloat(value) + "\n"))
}

func writeHistogram(w http.ResponseWriter, h *Histogram) {
	w.Write([]byte("# HELP " + h.name + " " + h.help + "\n"))
	w.Write([]byte("# TYPE " + h.name + " histogram\n"))

	// Write bucket counts
	var cumulative uint64
	for i, bound := range h.buckets {
		cumulative += h.counts[i]
		labels := copyLabels(h.labels)
		labels["le"] = formatFloat(bound)
		w.Write([]byte(h.name + "_bucket" + formatLabels(labels) + " "))
		w.Write([]byte(formatUint(cumulative) + "\n"))
	}

	// Write +Inf bucket
	labels := copyLabels(h.labels)
	labels["le"] = "+Inf"
	w.Write([]byte(h.name + "_bucket" + formatLabels(labels) + " "))
	w.Write([]byte(formatUint(h.count) + "\n"))

	// Write sum and count
	w.Write([]byte(h.name + "_sum" + formatLabels(h.labels) + " "))
	w.Write([]byte(formatFloat(h.sum) + "\n"))
	w.Write([]byte(h.name + "_count" + formatLabels(h.labels) + " "))
	w.Write([]byte(formatUint(h.count) + "\n"))
}

func formatLabels(labels map[string]string) string {
	if len(labels) == 0 {
		return ""
	}
	result := "{"
	first := true
	for k, v := range labels {
		if !first {
			result += ","
		}
		result += k + "=\"" + v + "\""
		first = false
	}
	result += "}"
	return result
}

func copyLabels(labels map[string]string) map[string]string {
	if labels == nil {
		return make(map[string]string)
	}
	result := make(map[string]string, len(labels))
	for k, v := range labels {
		result[k] = v
	}
	return result
}

func formatFloat(v float64) string {
	return string(appendFloat(nil, v))
}

func formatUint(v uint64) string {
	return string(appendUint(nil, v))
}

func appendFloat(b []byte, v float64) []byte {
	return append(b, []byte(floatToString(v))...)
}

func appendUint(b []byte, v uint64) []byte {
	return append(b, []byte(uintToString(v))...)
}

func floatToString(v float64) string {
	if v == float64(int64(v)) {
		return uintToString(uint64(v))
	}
	// Simple float formatting
	intPart := int64(v)
	fracPart := int64((v - float64(intPart)) * 1000000)
	if fracPart < 0 {
		fracPart = -fracPart
	}
	return uintToString(uint64(intPart)) + "." + padZeros(fracPart, 6)
}

func uintToString(v uint64) string {
	if v == 0 {
		return "0"
	}
	var digits [20]byte
	i := len(digits)
	for v > 0 {
		i--
		digits[i] = byte('0' + v%10)
		v /= 10
	}
	return string(digits[i:])
}

func padZeros(v int64, width int) string {
	s := uintToString(uint64(v))
	for len(s) < width {
		s = "0" + s
	}
	// Trim trailing zeros
	for len(s) > 1 && s[len(s)-1] == '0' {
		s = s[:len(s)-1]
	}
	return s
}

// Anvil-specific metrics

// AnvilMetrics contains all Anvil-specific metrics.
type AnvilMetrics struct {
	Registry *MetricsRegistry

	// LLM metrics
	LLMRequestsTotal    *Counter
	LLMRequestDuration  *Histogram
	LLMTokensTotal      *Counter
	LLMErrorsTotal      *Counter

	// Agent metrics
	AgentRunsTotal      *Counter
	AgentRunDuration    *Histogram
	AgentErrorsTotal    *Counter
	AgentScoreGauge     *Gauge

	// Harness metrics
	FixturesTotal       *Counter
	FixturesPassedTotal *Counter
	FixturesFailedTotal *Counter
	FixtureDuration     *Histogram

	// Compilation metrics
	CompilationsTotal   *Counter
	CompileSuccessTotal *Counter
	CompileFailureTotal *Counter
	CompileDuration     *Histogram

	// Active workers gauge
	ActiveWorkers       *Gauge
}

// NewAnvilMetrics creates Anvil-specific metrics.
func NewAnvilMetrics() *AnvilMetrics {
	r := NewMetricsRegistry()

	return &AnvilMetrics{
		Registry: r,

		// LLM
		LLMRequestsTotal:   r.NewCounter("anvil_llm_requests_total", "Total LLM API requests", nil),
		LLMRequestDuration: r.NewHistogram("anvil_llm_request_duration_seconds", "LLM request duration", nil, nil),
		LLMTokensTotal:     r.NewCounter("anvil_llm_tokens_total", "Total tokens used", nil),
		LLMErrorsTotal:     r.NewCounter("anvil_llm_errors_total", "Total LLM errors", nil),

		// Agents
		AgentRunsTotal:   r.NewCounter("anvil_agent_runs_total", "Total agent runs", nil),
		AgentRunDuration: r.NewHistogram("anvil_agent_run_duration_seconds", "Agent run duration", nil, nil),
		AgentErrorsTotal: r.NewCounter("anvil_agent_errors_total", "Total agent errors", nil),
		AgentScoreGauge:  r.NewGauge("anvil_agent_score", "Latest agent score", nil),

		// Harness
		FixturesTotal:       r.NewCounter("anvil_fixtures_total", "Total fixtures executed", nil),
		FixturesPassedTotal: r.NewCounter("anvil_fixtures_passed_total", "Total fixtures passed", nil),
		FixturesFailedTotal: r.NewCounter("anvil_fixtures_failed_total", "Total fixtures failed", nil),
		FixtureDuration:     r.NewHistogram("anvil_fixture_duration_seconds", "Fixture execution duration", nil, nil),

		// Compilation
		CompilationsTotal:   r.NewCounter("anvil_compilations_total", "Total compilations", nil),
		CompileSuccessTotal: r.NewCounter("anvil_compile_success_total", "Successful compilations", nil),
		CompileFailureTotal: r.NewCounter("anvil_compile_failure_total", "Failed compilations", nil),
		CompileDuration:     r.NewHistogram("anvil_compile_duration_seconds", "Compilation duration", nil, nil),

		// Workers
		ActiveWorkers: r.NewGauge("anvil_active_workers", "Number of active workers", nil),
	}
}

// Handler returns an HTTP handler for the metrics endpoint.
func (m *AnvilMetrics) Handler() http.Handler {
	return m.Registry.Handler()
}

// RecordLLMRequest records an LLM request.
func (m *AnvilMetrics) RecordLLMRequest(duration time.Duration, tokens int, err error) {
	m.LLMRequestsTotal.Inc()
	m.LLMRequestDuration.Observe(duration.Seconds())
	m.LLMTokensTotal.Add(float64(tokens))
	if err != nil {
		m.LLMErrorsTotal.Inc()
	}
}

// RecordAgentRun records an agent execution.
func (m *AnvilMetrics) RecordAgentRun(duration time.Duration, score float64, err error) {
	m.AgentRunsTotal.Inc()
	m.AgentRunDuration.Observe(duration.Seconds())
	m.AgentScoreGauge.Set(score)
	if err != nil {
		m.AgentErrorsTotal.Inc()
	}
}

// RecordFixture records a fixture execution.
func (m *AnvilMetrics) RecordFixture(duration time.Duration, passed bool) {
	m.FixturesTotal.Inc()
	m.FixtureDuration.Observe(duration.Seconds())
	if passed {
		m.FixturesPassedTotal.Inc()
	} else {
		m.FixturesFailedTotal.Inc()
	}
}

// RecordCompilation records a compilation.
func (m *AnvilMetrics) RecordCompilation(duration time.Duration, success bool) {
	m.CompilationsTotal.Inc()
	m.CompileDuration.Observe(duration.Seconds())
	if success {
		m.CompileSuccessTotal.Inc()
	} else {
		m.CompileFailureTotal.Inc()
	}
}

// Global metrics instance
var globalMetrics *AnvilMetrics
var metricsOnce sync.Once

// Metrics returns the global metrics instance.
func Metrics() *AnvilMetrics {
	metricsOnce.Do(func() {
		globalMetrics = NewAnvilMetrics()
	})
	return globalMetrics
}
