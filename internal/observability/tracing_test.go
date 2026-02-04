package observability

import (
	"context"
	"errors"
	"testing"
	"time"

	"go.opentelemetry.io/otel/codes"
)

func TestDefaultTracingConfig(t *testing.T) {
	cfg := DefaultTracingConfig()
	if cfg == nil {
		t.Fatal("expected non-nil config")
	}
	if cfg.ServiceName != "anvil" {
		t.Fatalf("expected service name 'anvil', got %s", cfg.ServiceName)
	}
	if cfg.SampleRate != 1.0 {
		t.Fatalf("expected sample rate 1.0, got %f", cfg.SampleRate)
	}
}

func TestInitTracing_NoEndpoint(t *testing.T) {
	ctx := context.Background()
	tp, err := InitTracing(ctx, &TracingConfig{
		ServiceName: "test",
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if tp == nil {
		t.Fatal("expected non-nil tracer provider")
	}
	if tp.Tracer() == nil {
		t.Fatal("expected non-nil tracer")
	}
	// Should be no-op, shutdown should succeed
	if err := tp.Shutdown(ctx); err != nil {
		t.Fatalf("shutdown error: %v", err)
	}
}

func TestInitTracing_NilConfig(t *testing.T) {
	ctx := context.Background()
	tp, err := InitTracing(ctx, nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if tp == nil {
		t.Fatal("expected non-nil tracer provider")
	}
}

func TestStartAgentSpan(t *testing.T) {
	ctx := context.Background()
	ctx, span := StartAgentSpan(ctx, "cartographer")
	if span == nil {
		t.Fatal("expected non-nil span")
	}
	span.End()
}

func TestStartLLMSpan(t *testing.T) {
	ctx := context.Background()
	ctx, span := StartLLMSpan(ctx, "openai", "gpt-4")
	if span == nil {
		t.Fatal("expected non-nil span")
	}
	span.End()
}

func TestRecordLLMMetrics(t *testing.T) {
	ctx := context.Background()
	_, span := StartLLMSpan(ctx, "openai", "gpt-4")

	// Should not panic
	RecordLLMMetrics(span, 100, 200, 500*time.Millisecond)
	span.End()
}

func TestStartCompileSpan(t *testing.T) {
	ctx := context.Background()
	ctx, span := StartCompileSpan(ctx, "typescript", 5)
	if span == nil {
		t.Fatal("expected non-nil span")
	}
	span.End()
}

func TestRecordCompileResult_Success(t *testing.T) {
	ctx := context.Background()
	_, span := StartCompileSpan(ctx, "typescript", 5)

	RecordCompileResult(span, true, 0, 2)
	span.End()
}

func TestRecordCompileResult_Failure(t *testing.T) {
	ctx := context.Background()
	_, span := StartCompileSpan(ctx, "typescript", 5)

	RecordCompileResult(span, false, 3, 1)
	span.End()
}

func TestStartFixtureSpan(t *testing.T) {
	ctx := context.Background()
	ctx, span := StartFixtureSpan(ctx, "test-fixture", "api")
	if span == nil {
		t.Fatal("expected non-nil span")
	}
	span.End()
}

func TestRecordFixtureResult_Pass(t *testing.T) {
	ctx := context.Background()
	_, span := StartFixtureSpan(ctx, "test-fixture", "api")

	RecordFixtureResult(span, true, "")
	span.End()
}

func TestRecordFixtureResult_Fail(t *testing.T) {
	ctx := context.Background()
	_, span := StartFixtureSpan(ctx, "test-fixture", "api")

	RecordFixtureResult(span, false, "expected 200 got 500")
	span.End()
}

func TestStartDBDiffSpan(t *testing.T) {
	ctx := context.Background()
	ctx, span := StartDBDiffSpan(ctx, 3)
	if span == nil {
		t.Fatal("expected non-nil span")
	}
	span.End()
}

func TestRecordDBDiffResult_NoViolations(t *testing.T) {
	ctx := context.Background()
	_, span := StartDBDiffSpan(ctx, 3)

	RecordDBDiffResult(span, 0, 1.0)
	span.End()
}

func TestRecordDBDiffResult_WithViolations(t *testing.T) {
	ctx := context.Background()
	_, span := StartDBDiffSpan(ctx, 3)

	RecordDBDiffResult(span, 5, 0.8)
	span.End()
}

func TestStartProofPackSpan(t *testing.T) {
	ctx := context.Background()
	ctx, span := StartProofPackSpan(ctx)
	if span == nil {
		t.Fatal("expected non-nil span")
	}
	span.End()
}

func TestRecordProofPackResult(t *testing.T) {
	ctx := context.Background()
	_, span := StartProofPackSpan(ctx)

	RecordProofPackResult(span, 10, 9, "/tmp/proof")
	span.End()
}

func TestRecordError(t *testing.T) {
	ctx := context.Background()
	_, span := StartAgentSpan(ctx, "test")

	// Should not panic with nil
	RecordError(span, nil)

	// Should record error
	RecordError(span, errors.New("test error"))
	span.End()
}

func TestSetAgentMetrics(t *testing.T) {
	ctx := context.Background()
	_, span := StartAgentSpan(ctx, "test")

	SetAgentMetrics(span, 10, 8, 2, 0.95)
	span.End()
}

func TestSpanKindConstants(t *testing.T) {
	// Verify constants are defined
	if SpanKindAgent == "" {
		t.Fatal("SpanKindAgent should not be empty")
	}
	if SpanKindLLM == "" {
		t.Fatal("SpanKindLLM should not be empty")
	}
	if SpanKindCompile == "" {
		t.Fatal("SpanKindCompile should not be empty")
	}
	if SpanKindFixture == "" {
		t.Fatal("SpanKindFixture should not be empty")
	}
	if SpanKindDBDiff == "" {
		t.Fatal("SpanKindDBDiff should not be empty")
	}
	if SpanKindProofPack == "" {
		t.Fatal("SpanKindProofPack should not be empty")
	}
}

func TestTracerName(t *testing.T) {
	if TracerName != "github.com/efebarandurmaz/anvil" {
		t.Fatalf("unexpected tracer name: %s", TracerName)
	}
}

// Test that spans can be nested
func TestNestedSpans(t *testing.T) {
	ctx := context.Background()

	// Start agent span
	ctx, agentSpan := StartAgentSpan(ctx, "judge")

	// Start LLM span nested inside agent
	ctx, llmSpan := StartLLMSpan(ctx, "openai", "gpt-4")
	RecordLLMMetrics(llmSpan, 50, 100, 200*time.Millisecond)
	llmSpan.End()

	// Start compile span nested inside agent
	_, compileSpan := StartCompileSpan(ctx, "go", 3)
	RecordCompileResult(compileSpan, true, 0, 0)
	compileSpan.End()

	SetAgentMetrics(agentSpan, 5, 5, 0, 1.0)
	agentSpan.End()
}

// Test TracerProvider methods
func TestTracerProvider_Shutdown_NilProvider(t *testing.T) {
	tp := &TracerProvider{}
	err := tp.Shutdown(context.Background())
	if err != nil {
		t.Fatalf("expected nil error for nil provider, got: %v", err)
	}
}

// Verify codes package is correctly imported
func TestCodesPackage(t *testing.T) {
	_ = codes.Error
	_ = codes.Ok
}
