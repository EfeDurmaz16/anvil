// Package observability provides OpenTelemetry tracing and metrics for Anvil.
package observability

import (
	"context"
	"fmt"
	"time"

	"go.opentelemetry.io/otel"
	"go.opentelemetry.io/otel/attribute"
	"go.opentelemetry.io/otel/codes"
	"go.opentelemetry.io/otel/exporters/otlp/otlptrace/otlptracegrpc"
	"go.opentelemetry.io/otel/propagation"
	"go.opentelemetry.io/otel/sdk/resource"
	sdktrace "go.opentelemetry.io/otel/sdk/trace"
	semconv "go.opentelemetry.io/otel/semconv/v1.24.0"
	"go.opentelemetry.io/otel/trace"
)

const (
	// TracerName is the name used for the Anvil tracer.
	TracerName = "github.com/efebarandurmaz/anvil"
)

// TracingConfig configures the OpenTelemetry tracing.
type TracingConfig struct {
	// ServiceName is the name of the service (default: "anvil")
	ServiceName string

	// ServiceVersion is the version of the service
	ServiceVersion string

	// Environment is the deployment environment (dev, staging, prod)
	Environment string

	// OTLPEndpoint is the OTLP gRPC endpoint (e.g., "localhost:4317")
	// If empty, tracing is disabled.
	OTLPEndpoint string

	// SampleRate is the trace sampling rate (0.0 to 1.0, default: 1.0)
	SampleRate float64
}

// DefaultTracingConfig returns a default tracing configuration.
func DefaultTracingConfig() *TracingConfig {
	return &TracingConfig{
		ServiceName:    "anvil",
		ServiceVersion: "0.1.0",
		Environment:    "development",
		SampleRate:     1.0,
	}
}

// TracerProvider wraps the OpenTelemetry tracer provider.
type TracerProvider struct {
	provider *sdktrace.TracerProvider
	tracer   trace.Tracer
}

// InitTracing initializes OpenTelemetry tracing.
// Returns a no-op tracer if OTLPEndpoint is empty.
func InitTracing(ctx context.Context, cfg *TracingConfig) (*TracerProvider, error) {
	if cfg == nil {
		cfg = DefaultTracingConfig()
	}

	// If no endpoint, return no-op tracer
	if cfg.OTLPEndpoint == "" {
		return &TracerProvider{
			tracer: otel.Tracer(TracerName),
		}, nil
	}

	// Create OTLP exporter
	exporter, err := otlptracegrpc.New(ctx,
		otlptracegrpc.WithEndpoint(cfg.OTLPEndpoint),
		otlptracegrpc.WithInsecure(), // Use TLS in production
	)
	if err != nil {
		return nil, fmt.Errorf("create OTLP exporter: %w", err)
	}

	// Create resource with service info
	res, err := resource.Merge(
		resource.Default(),
		resource.NewWithAttributes(
			semconv.SchemaURL,
			semconv.ServiceName(cfg.ServiceName),
			semconv.ServiceVersion(cfg.ServiceVersion),
			semconv.DeploymentEnvironment(cfg.Environment),
		),
	)
	if err != nil {
		return nil, fmt.Errorf("create resource: %w", err)
	}

	// Create sampler
	var sampler sdktrace.Sampler
	if cfg.SampleRate >= 1.0 {
		sampler = sdktrace.AlwaysSample()
	} else if cfg.SampleRate <= 0 {
		sampler = sdktrace.NeverSample()
	} else {
		sampler = sdktrace.TraceIDRatioBased(cfg.SampleRate)
	}

	// Create trace provider
	provider := sdktrace.NewTracerProvider(
		sdktrace.WithBatcher(exporter),
		sdktrace.WithResource(res),
		sdktrace.WithSampler(sampler),
	)

	// Set global provider and propagator
	otel.SetTracerProvider(provider)
	otel.SetTextMapPropagator(propagation.NewCompositeTextMapPropagator(
		propagation.TraceContext{},
		propagation.Baggage{},
	))

	return &TracerProvider{
		provider: provider,
		tracer:   provider.Tracer(TracerName),
	}, nil
}

// Shutdown gracefully shuts down the tracer provider.
func (tp *TracerProvider) Shutdown(ctx context.Context) error {
	if tp.provider != nil {
		return tp.provider.Shutdown(ctx)
	}
	return nil
}

// Tracer returns the underlying tracer.
func (tp *TracerProvider) Tracer() trace.Tracer {
	return tp.tracer
}

// SpanKind constants for Anvil operations.
const (
	SpanKindAgent      = "agent"
	SpanKindLLM        = "llm"
	SpanKindCompile    = "compile"
	SpanKindFixture    = "fixture"
	SpanKindDBDiff     = "db_diff"
	SpanKindProofPack  = "proof_pack"
)

// StartAgentSpan starts a span for an agent operation.
func StartAgentSpan(ctx context.Context, agentName string) (context.Context, trace.Span) {
	tracer := otel.Tracer(TracerName)
	ctx, span := tracer.Start(ctx, fmt.Sprintf("agent.%s", agentName),
		trace.WithSpanKind(trace.SpanKindInternal),
		trace.WithAttributes(
			attribute.String("anvil.agent.name", agentName),
			attribute.String("anvil.span.kind", SpanKindAgent),
		),
	)
	return ctx, span
}

// StartLLMSpan starts a span for an LLM call.
func StartLLMSpan(ctx context.Context, provider, model string) (context.Context, trace.Span) {
	tracer := otel.Tracer(TracerName)
	ctx, span := tracer.Start(ctx, "llm.complete",
		trace.WithSpanKind(trace.SpanKindClient),
		trace.WithAttributes(
			attribute.String("anvil.span.kind", SpanKindLLM),
			attribute.String("llm.provider", provider),
			attribute.String("llm.model", model),
		),
	)
	return ctx, span
}

// RecordLLMMetrics records LLM call metrics on a span.
func RecordLLMMetrics(span trace.Span, inputTokens, outputTokens int, duration time.Duration) {
	span.SetAttributes(
		attribute.Int("llm.input_tokens", inputTokens),
		attribute.Int("llm.output_tokens", outputTokens),
		attribute.Int("llm.total_tokens", inputTokens+outputTokens),
		attribute.Int64("llm.duration_ms", duration.Milliseconds()),
	)
}

// StartCompileSpan starts a span for a compilation operation.
func StartCompileSpan(ctx context.Context, language string, fileCount int) (context.Context, trace.Span) {
	tracer := otel.Tracer(TracerName)
	ctx, span := tracer.Start(ctx, fmt.Sprintf("compile.%s", language),
		trace.WithSpanKind(trace.SpanKindInternal),
		trace.WithAttributes(
			attribute.String("anvil.span.kind", SpanKindCompile),
			attribute.String("compile.language", language),
			attribute.Int("compile.file_count", fileCount),
		),
	)
	return ctx, span
}

// RecordCompileResult records compilation result on a span.
func RecordCompileResult(span trace.Span, success bool, errorCount, warningCount int) {
	span.SetAttributes(
		attribute.Bool("compile.success", success),
		attribute.Int("compile.error_count", errorCount),
		attribute.Int("compile.warning_count", warningCount),
	)
	if !success {
		span.SetStatus(codes.Error, "compilation failed")
	}
}

// StartFixtureSpan starts a span for a fixture test run.
func StartFixtureSpan(ctx context.Context, fixtureName, kind string) (context.Context, trace.Span) {
	tracer := otel.Tracer(TracerName)
	ctx, span := tracer.Start(ctx, fmt.Sprintf("fixture.%s", fixtureName),
		trace.WithSpanKind(trace.SpanKindInternal),
		trace.WithAttributes(
			attribute.String("anvil.span.kind", SpanKindFixture),
			attribute.String("fixture.name", fixtureName),
			attribute.String("fixture.kind", kind),
		),
	)
	return ctx, span
}

// RecordFixtureResult records fixture test result on a span.
func RecordFixtureResult(span trace.Span, passed bool, errorMsg string) {
	span.SetAttributes(
		attribute.Bool("fixture.passed", passed),
	)
	if !passed {
		span.SetStatus(codes.Error, errorMsg)
		span.SetAttributes(attribute.String("fixture.error", errorMsg))
	}
}

// StartDBDiffSpan starts a span for a database diff operation.
func StartDBDiffSpan(ctx context.Context, tableCount int) (context.Context, trace.Span) {
	tracer := otel.Tracer(TracerName)
	ctx, span := tracer.Start(ctx, "db_diff",
		trace.WithSpanKind(trace.SpanKindInternal),
		trace.WithAttributes(
			attribute.String("anvil.span.kind", SpanKindDBDiff),
			attribute.Int("db_diff.table_count", tableCount),
		),
	)
	return ctx, span
}

// RecordDBDiffResult records DB diff result on a span.
func RecordDBDiffResult(span trace.Span, violationCount int, score float64) {
	span.SetAttributes(
		attribute.Int("db_diff.violation_count", violationCount),
		attribute.Float64("db_diff.score", score),
	)
	if violationCount > 0 {
		span.SetStatus(codes.Error, fmt.Sprintf("%d violations", violationCount))
	}
}

// StartProofPackSpan starts a span for proof pack generation.
func StartProofPackSpan(ctx context.Context) (context.Context, trace.Span) {
	tracer := otel.Tracer(TracerName)
	ctx, span := tracer.Start(ctx, "proof_pack.generate",
		trace.WithSpanKind(trace.SpanKindInternal),
		trace.WithAttributes(
			attribute.String("anvil.span.kind", SpanKindProofPack),
		),
	)
	return ctx, span
}

// RecordProofPackResult records proof pack generation result.
func RecordProofPackResult(span trace.Span, fixtureCount, passCount int, outputPath string) {
	span.SetAttributes(
		attribute.Int("proof_pack.fixture_count", fixtureCount),
		attribute.Int("proof_pack.pass_count", passCount),
		attribute.Int("proof_pack.fail_count", fixtureCount-passCount),
		attribute.String("proof_pack.output_path", outputPath),
	)
}

// RecordError records an error on a span.
func RecordError(span trace.Span, err error) {
	if err != nil {
		span.RecordError(err)
		span.SetStatus(codes.Error, err.Error())
	}
}

// SetAgentMetrics sets standard agent metrics on a span.
func SetAgentMetrics(span trace.Span, inputItems, outputItems, skippedItems int, score float64) {
	span.SetAttributes(
		attribute.Int("agent.input_items", inputItems),
		attribute.Int("agent.output_items", outputItems),
		attribute.Int("agent.skipped_items", skippedItems),
		attribute.Float64("agent.score", score),
	)
}
