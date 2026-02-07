package llm

import (
	"context"
	"errors"
	"fmt"
	"net"
	"net/http"
	"strings"
	"time"
)

// RetryConfig configures retry behavior for LLM calls.
type RetryConfig struct {
	MaxRetries int           // Maximum number of retry attempts (0 = no retries)
	RetryDelay time.Duration // Initial delay between retries
	MaxDelay   time.Duration // Maximum delay between retries (caps exponential backoff)
	Timeout    time.Duration // Per-request timeout
}

// DefaultRetryConfig returns a sensible default configuration.
func DefaultRetryConfig() *RetryConfig {
	return &RetryConfig{
		MaxRetries: 8,
		RetryDelay: 2 * time.Second,
		MaxDelay:   30 * time.Second,
		Timeout:    2 * time.Minute,
	}
}

// RetryProvider wraps a Provider with timeout and retry logic.
type RetryProvider struct {
	inner  Provider
	config *RetryConfig
}

// NewRetryProvider wraps an existing provider with retry logic.
func NewRetryProvider(inner Provider, config *RetryConfig) *RetryProvider {
	if config == nil {
		config = DefaultRetryConfig()
	}
	return &RetryProvider{
		inner:  inner,
		config: config,
	}
}

// Name returns the underlying provider name.
func (r *RetryProvider) Name() string {
	return r.inner.Name()
}

// Complete sends a prompt with timeout and retry logic.
func (r *RetryProvider) Complete(ctx context.Context, prompt *Prompt, opts *RequestOptions) (*Response, error) {
	var lastErr error

	for attempt := 0; attempt <= r.config.MaxRetries; attempt++ {
		if attempt > 0 {
			// Calculate backoff delay with exponential increase
			delay := r.calculateBackoff(attempt)
			select {
			case <-ctx.Done():
				return nil, ctx.Err()
			case <-time.After(delay):
			}
		}

		// Create timeout context for this attempt
		attemptCtx, cancel := context.WithTimeout(ctx, r.config.Timeout)
		resp, err := r.inner.Complete(attemptCtx, prompt, opts)
		cancel()

		if err == nil {
			return resp, nil
		}

		lastErr = err

		// Check if error is retryable
		if !r.isRetryable(err) {
			return nil, fmt.Errorf("non-retryable error: %w", err)
		}

		// Check if parent context is cancelled
		if ctx.Err() != nil {
			return nil, ctx.Err()
		}
	}

	return nil, fmt.Errorf("max retries (%d) exceeded: %w", r.config.MaxRetries, lastErr)
}

// Embed sends an embedding request with timeout and retry logic.
func (r *RetryProvider) Embed(ctx context.Context, texts []string) ([][]float32, error) {
	var lastErr error

	for attempt := 0; attempt <= r.config.MaxRetries; attempt++ {
		if attempt > 0 {
			delay := r.calculateBackoff(attempt)
			select {
			case <-ctx.Done():
				return nil, ctx.Err()
			case <-time.After(delay):
			}
		}

		attemptCtx, cancel := context.WithTimeout(ctx, r.config.Timeout)
		embeddings, err := r.inner.Embed(attemptCtx, texts)
		cancel()

		if err == nil {
			return embeddings, nil
		}

		lastErr = err

		if !r.isRetryable(err) {
			return nil, fmt.Errorf("non-retryable error: %w", err)
		}

		if ctx.Err() != nil {
			return nil, ctx.Err()
		}
	}

	return nil, fmt.Errorf("max retries (%d) exceeded: %w", r.config.MaxRetries, lastErr)
}

// calculateBackoff returns the delay for the given attempt using exponential backoff.
func (r *RetryProvider) calculateBackoff(attempt int) time.Duration {
	// Exponential backoff: delay * 2^(attempt-1)
	delay := r.config.RetryDelay
	for i := 1; i < attempt; i++ {
		delay *= 2
		if delay > r.config.MaxDelay {
			delay = r.config.MaxDelay
			break
		}
	}
	return delay
}

// isRetryable determines if an error should trigger a retry.
func (r *RetryProvider) isRetryable(err error) bool {
	if err == nil {
		return false
	}

	// Context errors are not retryable (caller cancelled)
	if errors.Is(err, context.Canceled) {
		return false
	}

	// Timeout errors are retryable
	if errors.Is(err, context.DeadlineExceeded) {
		return true
	}

	// Network errors are generally retryable
	var netErr net.Error
	if errors.As(err, &netErr) {
		return netErr.Timeout()
	}

	// Check for specific HTTP status codes in error message
	errStr := err.Error()

	// Rate limiting (429) - retryable, UNLESS it's a daily token limit (TPD)
	if strings.Contains(errStr, "429") || strings.Contains(errStr, "Too Many Requests") {
		// Daily token limits (TPD) won't reset with retries - don't waste time
		if strings.Contains(errStr, "tokens per day") || strings.Contains(errStr, "TPD") {
			return false
		}
		return true
	}

	// Server errors (5xx) - retryable
	if strings.Contains(errStr, "500") ||
		strings.Contains(errStr, "502") ||
		strings.Contains(errStr, "503") ||
		strings.Contains(errStr, "504") ||
		strings.Contains(errStr, http.StatusText(http.StatusInternalServerError)) ||
		strings.Contains(errStr, http.StatusText(http.StatusBadGateway)) ||
		strings.Contains(errStr, http.StatusText(http.StatusServiceUnavailable)) ||
		strings.Contains(errStr, http.StatusText(http.StatusGatewayTimeout)) {
		return true
	}

	// Client errors (4xx except 429) - not retryable
	if strings.Contains(errStr, "400") ||
		strings.Contains(errStr, "401") ||
		strings.Contains(errStr, "403") ||
		strings.Contains(errStr, "404") {
		return false
	}

	// Default: retry on unknown errors (conservative approach for LLM calls)
	return true
}

// WrapWithRetry is a convenience function to wrap a provider with retry logic from config.
func WrapWithRetry(provider Provider, cfg ProviderConfig) Provider {
	if provider == nil {
		return nil
	}

	// Use defaults if not specified
	timeout := cfg.Timeout
	if timeout == 0 {
		timeout = 2 * time.Minute
	}

	maxRetries := cfg.MaxRetries
	if maxRetries == 0 && cfg.Timeout == 0 {
		// Only use default if neither was explicitly set
		maxRetries = 3
	}

	retryDelay := cfg.RetryDelay
	if retryDelay == 0 {
		retryDelay = 1 * time.Second
	}

	config := &RetryConfig{
		MaxRetries: maxRetries,
		RetryDelay: retryDelay,
		MaxDelay:   30 * time.Second,
		Timeout:    timeout,
	}

	return NewRetryProvider(provider, config)
}
