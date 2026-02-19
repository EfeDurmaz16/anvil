package llm

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"testing"
	"time"
)

func TestDefaultRetryConfig(t *testing.T) {
	cfg := DefaultRetryConfig()

	if cfg.MaxRetries != 8 {
		t.Errorf("expected 8 max retries, got %d", cfg.MaxRetries)
	}
	if cfg.RetryDelay != 2*time.Second {
		t.Errorf("expected 2 second retry delay, got %v", cfg.RetryDelay)
	}
	if cfg.MaxDelay != 30*time.Second {
		t.Errorf("expected 30 second max delay, got %v", cfg.MaxDelay)
	}
	if cfg.Timeout != 2*time.Minute {
		t.Errorf("expected 2 minute timeout, got %v", cfg.Timeout)
	}
}

func TestNewRetryProvider_NilConfig(t *testing.T) {
	inner := &mockRetryProvider{name: "test"}
	retry := NewRetryProvider(inner, nil)

	if retry == nil {
		t.Fatal("expected non-nil retry provider")
	}
	if retry.config == nil {
		t.Fatal("expected config to be set")
	}
	// Should use defaults
	if retry.config.MaxRetries != 8 {
		t.Errorf("expected default 8 retries, got %d", retry.config.MaxRetries)
	}
}

func TestRetryProvider_Name(t *testing.T) {
	inner := &mockRetryProvider{name: "test-provider"}
	retry := NewRetryProvider(inner, nil)

	if retry.Name() != "test-provider" {
		t.Errorf("expected 'test-provider', got %s", retry.Name())
	}
}

func TestRetryProvider_Complete_SucceedsFirstTry(t *testing.T) {
	inner := &mockRetryProvider{
		name: "test",
		responses: []*Response{
			{Content: "success", InputTokens: 10, OutputTokens: 20},
		},
	}

	retry := NewRetryProvider(inner, &RetryConfig{
		MaxRetries: 3,
		RetryDelay: 10 * time.Millisecond,
		MaxDelay:   1 * time.Second,
		Timeout:    5 * time.Second,
	})

	ctx := context.Background()
	resp, err := retry.Complete(ctx, &Prompt{}, nil)

	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if resp == nil {
		t.Fatal("expected response")
	}
	if resp.Content != "success" {
		t.Errorf("expected 'success', got %q", resp.Content)
	}
	if inner.calls != 1 {
		t.Errorf("expected 1 call, got %d", inner.calls)
	}
}

func TestRetryProvider_Complete_RetriesOnRetryableError(t *testing.T) {
	inner := &mockRetryProvider{
		name: "test",
		errors: []error{
			errors.New("500 Internal Server Error"),
			errors.New("503 Service Unavailable"),
		},
		responses: []*Response{
			{Content: "success after retries"},
		},
	}

	retry := NewRetryProvider(inner, &RetryConfig{
		MaxRetries: 3,
		RetryDelay: 10 * time.Millisecond,
		MaxDelay:   1 * time.Second,
		Timeout:    5 * time.Second,
	})

	ctx := context.Background()
	resp, err := retry.Complete(ctx, &Prompt{}, nil)

	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if resp == nil {
		t.Fatal("expected response")
	}
	if inner.calls != 3 {
		t.Errorf("expected 3 calls (2 failures + 1 success), got %d", inner.calls)
	}
}

func TestRetryProvider_Complete_FailsNonRetryableError(t *testing.T) {
	inner := &mockRetryProvider{
		name: "test",
		errors: []error{
			errors.New("401 Unauthorized"),
		},
	}

	retry := NewRetryProvider(inner, &RetryConfig{
		MaxRetries: 3,
		RetryDelay: 10 * time.Millisecond,
		MaxDelay:   1 * time.Second,
		Timeout:    5 * time.Second,
	})

	ctx := context.Background()
	_, err := retry.Complete(ctx, &Prompt{}, nil)

	if err == nil {
		t.Fatal("expected error")
	}
	if !strings.Contains(err.Error(), "non-retryable") {
		t.Errorf("expected 'non-retryable' in error, got: %v", err)
	}
	if inner.calls != 1 {
		t.Errorf("expected 1 call (no retries), got %d", inner.calls)
	}
}

func TestRetryProvider_Complete_RespectsMaxRetries(t *testing.T) {
	inner := &mockRetryProvider{
		name: "test",
		errors: []error{
			errors.New("500"),
			errors.New("500"),
			errors.New("500"),
			errors.New("500"),
			errors.New("500"), // More errors than max retries
		},
	}

	retry := NewRetryProvider(inner, &RetryConfig{
		MaxRetries: 2,
		RetryDelay: 10 * time.Millisecond,
		MaxDelay:   1 * time.Second,
		Timeout:    5 * time.Second,
	})

	ctx := context.Background()
	_, err := retry.Complete(ctx, &Prompt{}, nil)

	if err == nil {
		t.Fatal("expected error")
	}
	if !strings.Contains(err.Error(), "max retries") {
		t.Errorf("expected 'max retries' in error, got: %v", err)
	}
	// Should be called 3 times: initial + 2 retries
	if inner.calls != 3 {
		t.Errorf("expected 3 calls (initial + 2 retries), got %d", inner.calls)
	}
}

func TestRetryProvider_Complete_RespectsContextCancellation(t *testing.T) {
	inner := &mockRetryProvider{
		name: "test",
		errors: []error{
			errors.New("500"),
		},
	}

	retry := NewRetryProvider(inner, &RetryConfig{
		MaxRetries: 3,
		RetryDelay: 100 * time.Millisecond,
		MaxDelay:   1 * time.Second,
		Timeout:    5 * time.Second,
	})

	ctx, cancel := context.WithCancel(context.Background())
	cancel() // Cancel immediately

	_, err := retry.Complete(ctx, &Prompt{}, nil)

	if !errors.Is(err, context.Canceled) {
		t.Errorf("expected context.Canceled, got: %v", err)
	}
}

func TestRetryProvider_Embed_FollowsRetryLogic(t *testing.T) {
	inner := &mockRetryProvider{
		name: "test",
		embedErrors: []error{
			errors.New("503 Service Unavailable"),
		},
		embedResponses: [][][]float32{
			{{0.1, 0.2, 0.3}},
		},
	}

	retry := NewRetryProvider(inner, &RetryConfig{
		MaxRetries: 3,
		RetryDelay: 10 * time.Millisecond,
		MaxDelay:   1 * time.Second,
		Timeout:    5 * time.Second,
	})

	ctx := context.Background()
	embeddings, err := retry.Embed(ctx, []string{"test"})

	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if len(embeddings) != 1 {
		t.Fatalf("expected 1 embedding, got %d", len(embeddings))
	}
	if inner.embedCalls != 2 {
		t.Errorf("expected 2 calls (1 failure + 1 success), got %d", inner.embedCalls)
	}
}

func TestRetryProvider_IsRetryable_ContextCanceled(t *testing.T) {
	retry := NewRetryProvider(&mockRetryProvider{}, nil)

	if retry.isRetryable(context.Canceled) {
		t.Error("context.Canceled should not be retryable")
	}
}

func TestRetryProvider_IsRetryable_DeadlineExceeded(t *testing.T) {
	retry := NewRetryProvider(&mockRetryProvider{}, nil)

	if !retry.isRetryable(context.DeadlineExceeded) {
		t.Error("context.DeadlineExceeded should be retryable")
	}
}

func TestRetryProvider_IsRetryable_429(t *testing.T) {
	retry := NewRetryProvider(&mockRetryProvider{}, nil)

	if !retry.isRetryable(errors.New("429 Too Many Requests")) {
		t.Error("429 errors should be retryable")
	}

	if !retry.isRetryable(errors.New("Too Many Requests")) {
		t.Error("'Too Many Requests' should be retryable")
	}
}

func TestRetryProvider_IsRetryable_429_DailyLimit(t *testing.T) {
	retry := NewRetryProvider(&mockRetryProvider{}, nil)

	if retry.isRetryable(errors.New("429 tokens per day exceeded")) {
		t.Error("daily token limit (TPD) should not be retryable")
	}

	if retry.isRetryable(errors.New("429 TPD limit reached")) {
		t.Error("TPD errors should not be retryable")
	}
}

func TestRetryProvider_IsRetryable_5xxErrors(t *testing.T) {
	retry := NewRetryProvider(&mockRetryProvider{}, nil)

	testCases := []string{
		"500 Internal Server Error",
		"502 Bad Gateway",
		"503 Service Unavailable",
		"504 Gateway Timeout",
	}

	for _, errMsg := range testCases {
		if !retry.isRetryable(errors.New(errMsg)) {
			t.Errorf("%s should be retryable", errMsg)
		}
	}
}

func TestRetryProvider_IsRetryable_4xxErrors(t *testing.T) {
	retry := NewRetryProvider(&mockRetryProvider{}, nil)

	testCases := []string{
		"400 Bad Request",
		"401 Unauthorized",
		"403 Forbidden",
		"404 Not Found",
	}

	for _, errMsg := range testCases {
		if retry.isRetryable(errors.New(errMsg)) {
			t.Errorf("%s should not be retryable", errMsg)
		}
	}
}

func TestRetryProvider_CalculateBackoff_ExponentialGrowth(t *testing.T) {
	retry := NewRetryProvider(&mockRetryProvider{}, &RetryConfig{
		RetryDelay: 100 * time.Millisecond,
		MaxDelay:   10 * time.Second,
	})

	// First retry: base delay (100ms) + jitter
	delay1 := retry.calculateBackoff(1)
	if delay1 < 100*time.Millisecond || delay1 > 125*time.Millisecond {
		t.Errorf("first backoff out of range: %v", delay1)
	}

	// Second retry: 200ms + jitter
	delay2 := retry.calculateBackoff(2)
	if delay2 < 200*time.Millisecond || delay2 > 250*time.Millisecond {
		t.Errorf("second backoff out of range: %v", delay2)
	}

	// Third retry: 400ms + jitter
	delay3 := retry.calculateBackoff(3)
	if delay3 < 400*time.Millisecond || delay3 > 500*time.Millisecond {
		t.Errorf("third backoff out of range: %v", delay3)
	}
}

func TestRetryProvider_CalculateBackoff_MaxCap(t *testing.T) {
	retry := NewRetryProvider(&mockRetryProvider{}, &RetryConfig{
		RetryDelay: 1 * time.Second,
		MaxDelay:   5 * time.Second,
	})

	// After enough attempts, should cap at MaxDelay
	delay := retry.calculateBackoff(10)
	// Should be capped at 5s + 25% jitter = max 6.25s
	if delay > 7*time.Second {
		t.Errorf("backoff should be capped, got: %v", delay)
	}
}

func TestWrapWithRetry_NilProvider(t *testing.T) {
	result := WrapWithRetry(nil, ProviderConfig{})
	if result != nil {
		t.Error("expected nil for nil provider")
	}
}

func TestWrapWithRetry_ValidProvider(t *testing.T) {
	inner := &mockRetryProvider{name: "test"}
	cfg := ProviderConfig{
		Timeout:    3 * time.Minute,
		MaxRetries: 5,
		RetryDelay: 2 * time.Second,
	}

	result := WrapWithRetry(inner, cfg)
	if result == nil {
		t.Fatal("expected non-nil provider")
	}

	retry, ok := result.(*RetryProvider)
	if !ok {
		t.Fatalf("expected RetryProvider, got %T", result)
	}

	if retry.config.Timeout != 3*time.Minute {
		t.Errorf("expected 3 minute timeout, got %v", retry.config.Timeout)
	}
	if retry.config.MaxRetries != 5 {
		t.Errorf("expected 5 retries, got %d", retry.config.MaxRetries)
	}
	if retry.config.RetryDelay != 2*time.Second {
		t.Errorf("expected 2s retry delay, got %v", retry.config.RetryDelay)
	}
}

// mockRetryProvider is a mock that can be configured to fail N times then succeed
type mockRetryProvider struct {
	name           string
	responses      []*Response
	errors         []error
	embedResponses [][][]float32
	embedErrors    []error
	calls          int
	embedCalls     int
}

func (m *mockRetryProvider) Name() string {
	return m.name
}

func (m *mockRetryProvider) Complete(ctx context.Context, prompt *Prompt, opts *RequestOptions) (*Response, error) {
	m.calls++

	// Return errors first
	if len(m.errors) > 0 {
		err := m.errors[0]
		m.errors = m.errors[1:]
		return nil, err
	}

	// Then return responses
	if len(m.responses) > 0 {
		resp := m.responses[0]
		m.responses = m.responses[1:]
		return resp, nil
	}

	return nil, fmt.Errorf("mock: no more responses configured")
}

func (m *mockRetryProvider) Embed(ctx context.Context, texts []string) ([][]float32, error) {
	m.embedCalls++

	// Return errors first
	if len(m.embedErrors) > 0 {
		err := m.embedErrors[0]
		m.embedErrors = m.embedErrors[1:]
		return nil, err
	}

	// Then return responses
	if len(m.embedResponses) > 0 {
		resp := m.embedResponses[0]
		m.embedResponses = m.embedResponses[1:]
		return resp, nil
	}

	return nil, fmt.Errorf("mock: no more embed responses configured")
}
