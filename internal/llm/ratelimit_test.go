package llm

import (
	"context"
	"sync/atomic"
	"testing"
	"time"
)

// mockProvider for testing
type mockProvider struct {
	name       string
	callCount  int64
	delay      time.Duration
	tokenUsage int
}

func (m *mockProvider) Name() string { return m.name }

func (m *mockProvider) Complete(ctx context.Context, prompt *Prompt, opts *RequestOptions) (*Response, error) {
	atomic.AddInt64(&m.callCount, 1)
	if m.delay > 0 {
		time.Sleep(m.delay)
	}
	return &Response{
		Content:      "test response",
		InputTokens:  m.tokenUsage / 2,
		OutputTokens: m.tokenUsage / 2,
	}, nil
}

func (m *mockProvider) Embed(ctx context.Context, texts []string) ([][]float32, error) {
	return nil, nil
}

func TestDefaultRateLimitConfig(t *testing.T) {
	cfg := DefaultRateLimitConfig()
	if cfg.RequestsPerMinute != 25 {
		t.Fatalf("expected 25 RPM, got %d", cfg.RequestsPerMinute)
	}
	if cfg.TokensPerMinute != 25000 {
		t.Fatalf("expected 25000 TPM, got %d", cfg.TokensPerMinute)
	}
	if cfg.BurstSize != 3 {
		t.Fatalf("expected burst 3, got %d", cfg.BurstSize)
	}
}

func TestRateLimitProvider_Name(t *testing.T) {
	mock := &mockProvider{name: "test-provider"}
	rl := NewRateLimitProvider(mock, nil)

	if rl.Name() != "test-provider" {
		t.Fatalf("expected 'test-provider', got %s", rl.Name())
	}
}

func TestRateLimitProvider_Complete(t *testing.T) {
	mock := &mockProvider{name: "test", tokenUsage: 100}
	rl := NewRateLimitProvider(mock, &RateLimitConfig{
		RequestsPerMinute: 100,
		TokensPerMinute:   10000,
		BurstSize:         5,
	})

	ctx := context.Background()
	resp, err := rl.Complete(ctx, &Prompt{Messages: []Message{{Role: "user", Content: "test"}}}, nil)

	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if resp == nil {
		t.Fatal("expected response")
	}
	if mock.callCount != 1 {
		t.Fatalf("expected 1 call, got %d", mock.callCount)
	}
}

func TestRateLimitProvider_BurstAllowed(t *testing.T) {
	mock := &mockProvider{name: "test", tokenUsage: 100}
	rl := NewRateLimitProvider(mock, &RateLimitConfig{
		RequestsPerMinute: 60,
		TokensPerMinute:   100000,
		BurstSize:         5,
	})

	ctx := context.Background()

	// Should allow burst of 5 requests quickly
	for i := 0; i < 5; i++ {
		_, err := rl.Complete(ctx, &Prompt{Messages: []Message{{Role: "user", Content: "test"}}}, nil)
		if err != nil {
			t.Fatalf("unexpected error on request %d: %v", i, err)
		}
	}

	if mock.callCount != 5 {
		t.Fatalf("expected 5 calls, got %d", mock.callCount)
	}
}

func TestRateLimitProvider_Stats(t *testing.T) {
	mock := &mockProvider{name: "test", tokenUsage: 1000}
	rl := NewRateLimitProvider(mock, &RateLimitConfig{
		RequestsPerMinute: 60,
		TokensPerMinute:   100000,
		BurstSize:         10,
	})

	ctx := context.Background()

	// Make a request
	rl.Complete(ctx, &Prompt{Messages: []Message{{Role: "user", Content: "test"}}}, nil)

	stats := rl.Stats()
	if stats.RequestsInWindow != 1 {
		t.Fatalf("expected 1 request in window, got %d", stats.RequestsInWindow)
	}
	if stats.TokensInWindow != 1000 {
		t.Fatalf("expected 1000 tokens in window, got %d", stats.TokensInWindow)
	}
}

func TestRateLimitProvider_TokenTracking(t *testing.T) {
	mock := &mockProvider{name: "test", tokenUsage: 5000}
	rl := NewRateLimitProvider(mock, &RateLimitConfig{
		RequestsPerMinute: 100,
		TokensPerMinute:   10000,
		BurstSize:         10,
	})

	ctx := context.Background()

	// Two requests should use 10000 tokens total
	rl.Complete(ctx, &Prompt{Messages: []Message{{Role: "user", Content: "test"}}}, nil)
	rl.Complete(ctx, &Prompt{Messages: []Message{{Role: "user", Content: "test"}}}, nil)

	stats := rl.Stats()
	if stats.TokensInWindow != 10000 {
		t.Fatalf("expected 10000 tokens, got %d", stats.TokensInWindow)
	}
	if stats.RemainingTokens != 0 {
		t.Fatalf("expected 0 remaining tokens, got %d", stats.RemainingTokens)
	}
}

func TestRateLimitProvider_ContextCancellation(t *testing.T) {
	mock := &mockProvider{name: "test", tokenUsage: 100}
	rl := NewRateLimitProvider(mock, &RateLimitConfig{
		RequestsPerMinute: 6000, // High rate but low burst
		TokensPerMinute:   100000,
		BurstSize:         1,
	})

	ctx := context.Background()

	// Use up burst
	rl.Complete(ctx, &Prompt{Messages: []Message{{Role: "user", Content: "test"}}}, nil)

	// Next request should block briefly, so use a cancelled context
	cancelCtx, cancel := context.WithCancel(context.Background())
	cancel() // Cancel immediately

	_, err := rl.Complete(cancelCtx, &Prompt{Messages: []Message{{Role: "user", Content: "test"}}}, nil)
	if err != context.Canceled {
		t.Fatalf("expected context.Canceled, got %v", err)
	}
}

func TestRateLimitProvider_UnlimitedRequests(t *testing.T) {
	mock := &mockProvider{name: "test", tokenUsage: 100}
	rl := NewRateLimitProvider(mock, &RateLimitConfig{
		RequestsPerMinute: 0, // Unlimited
		TokensPerMinute:   0, // Unlimited
		BurstSize:         0,
	})

	ctx := context.Background()

	// Should allow many requests without limiting
	for i := 0; i < 20; i++ {
		_, err := rl.Complete(ctx, &Prompt{Messages: []Message{{Role: "user", Content: "test"}}}, nil)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
	}

	if mock.callCount != 20 {
		t.Fatalf("expected 20 calls, got %d", mock.callCount)
	}
}

func TestWithRateLimit(t *testing.T) {
	mock := &mockProvider{name: "test"}

	// With nil provider
	p := WithRateLimit(nil, nil)
	if p != nil {
		t.Fatal("expected nil for nil provider")
	}

	// With valid provider
	p = WithRateLimit(mock, &RateLimitConfig{RequestsPerMinute: 60})
	if p == nil {
		t.Fatal("expected non-nil provider")
	}
	if p.Name() != "test" {
		t.Fatalf("expected 'test', got %s", p.Name())
	}
}

func TestRateLimitProvider_NilConfig(t *testing.T) {
	mock := &mockProvider{name: "test"}
	rl := NewRateLimitProvider(mock, nil)

	// Should use defaults
	stats := rl.Stats()
	if rl.config.RequestsPerMinute != 25 {
		t.Fatalf("expected default 25 RPM, got %d", rl.config.RequestsPerMinute)
	}
	if stats.RemainingRequests != 3 { // Default burst size
		t.Fatalf("expected 3 remaining requests, got %d", stats.RemainingRequests)
	}
}
