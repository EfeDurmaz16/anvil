package llm

import (
	"context"
	"sync"
	"time"
)

// RateLimitConfig configures rate limiting for LLM providers.
type RateLimitConfig struct {
	// RequestsPerMinute limits the number of API calls per minute (0 = unlimited)
	RequestsPerMinute int
	// TokensPerMinute limits total tokens per minute (0 = unlimited)
	TokensPerMinute int
	// BurstSize allows temporary burst above the rate limit
	BurstSize int
}

// DefaultRateLimitConfig returns sensible defaults for most providers.
func DefaultRateLimitConfig() *RateLimitConfig {
	return &RateLimitConfig{
		RequestsPerMinute: 25,    // conservative for free-tier cloud APIs (Groq etc.)
		TokensPerMinute:   25000, // Groq free tier: 6K-30K TPM depending on model
		BurstSize:         3,
	}
}

// RateLimitProvider wraps a provider with rate limiting.
type RateLimitProvider struct {
	inner  Provider
	config *RateLimitConfig

	mu              sync.Mutex
	requestTokens   int       // Available request tokens
	tokenBudget     int       // Available token budget
	lastRefill      time.Time // Last time tokens were refilled
	requestsInWindow int      // Requests in current window
	tokensInWindow   int      // Tokens used in current window
	windowStart     time.Time // Start of current window
}

// NewRateLimitProvider creates a rate-limited provider wrapper.
func NewRateLimitProvider(inner Provider, config *RateLimitConfig) *RateLimitProvider {
	if config == nil {
		config = DefaultRateLimitConfig()
	}

	burstSize := config.BurstSize
	if burstSize <= 0 {
		burstSize = 1
	}

	return &RateLimitProvider{
		inner:         inner,
		config:        config,
		requestTokens: burstSize,
		tokenBudget:   config.TokensPerMinute,
		lastRefill:    time.Now(),
		windowStart:   time.Now(),
	}
}

// Name returns the underlying provider name.
func (r *RateLimitProvider) Name() string {
	return r.inner.Name()
}

// Complete rate-limits and delegates to the inner provider.
func (r *RateLimitProvider) Complete(ctx context.Context, prompt *Prompt, opts *RequestOptions) (*Response, error) {
	// Wait for rate limit clearance
	if err := r.waitForCapacity(ctx); err != nil {
		return nil, err
	}

	// Make the actual request
	resp, err := r.inner.Complete(ctx, prompt, opts)

	// Track token usage for token-based rate limiting
	if err == nil && resp != nil {
		r.trackTokenUsage(resp.InputTokens + resp.OutputTokens)
	}

	return resp, err
}

// Embed delegates to the inner provider (if supported).
func (r *RateLimitProvider) Embed(ctx context.Context, texts []string) ([][]float32, error) {
	if embedder, ok := r.inner.(interface {
		Embed(context.Context, []string) ([][]float32, error)
	}); ok {
		// Wait for rate limit
		if err := r.waitForCapacity(ctx); err != nil {
			return nil, err
		}
		return embedder.Embed(ctx, texts)
	}
	return nil, nil
}

// waitForCapacity blocks until rate limit allows a request.
func (r *RateLimitProvider) waitForCapacity(ctx context.Context) error {
	for {
		r.mu.Lock()
		r.refillTokens()

		// If unlimited (both 0), allow immediately
		if r.config.RequestsPerMinute == 0 && r.config.TokensPerMinute == 0 {
			r.requestsInWindow++
			r.mu.Unlock()
			return nil
		}

		// Check if we have capacity
		hasRequestCapacity := r.config.RequestsPerMinute == 0 || r.requestTokens > 0
		hasTokenCapacity := r.config.TokensPerMinute == 0 || r.tokenBudget > 0

		if hasRequestCapacity && hasTokenCapacity {
			if r.config.RequestsPerMinute > 0 {
				r.requestTokens--
			}
			r.requestsInWindow++
			r.mu.Unlock()
			return nil
		}

		// Calculate wait time
		waitTime := r.calculateWaitTime()
		r.mu.Unlock()

		// Wait with context
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-time.After(waitTime):
			// Continue to check again
		}
	}
}

// refillTokens adds tokens based on elapsed time.
func (r *RateLimitProvider) refillTokens() {
	now := time.Now()
	elapsed := now.Sub(r.lastRefill)

	// Refill request tokens
	if r.config.RequestsPerMinute > 0 {
		tokensToAdd := int(elapsed.Minutes() * float64(r.config.RequestsPerMinute))
		if tokensToAdd > 0 {
			r.requestTokens += tokensToAdd
			maxTokens := r.config.BurstSize
			if maxTokens <= 0 {
				maxTokens = r.config.RequestsPerMinute / 6 // ~10 second burst
				if maxTokens < 1 {
					maxTokens = 1
				}
			}
			if r.requestTokens > maxTokens {
				r.requestTokens = maxTokens
			}
		}
	}

	// Reset window if a minute has passed
	if now.Sub(r.windowStart) >= time.Minute {
		r.windowStart = now
		r.requestsInWindow = 0
		r.tokensInWindow = 0
		r.tokenBudget = r.config.TokensPerMinute
	}

	r.lastRefill = now
}

// trackTokenUsage records token consumption.
func (r *RateLimitProvider) trackTokenUsage(tokens int) {
	r.mu.Lock()
	defer r.mu.Unlock()

	r.tokensInWindow += tokens
	r.tokenBudget -= tokens
	if r.tokenBudget < 0 {
		r.tokenBudget = 0
	}
}

// calculateWaitTime estimates how long to wait before retry.
func (r *RateLimitProvider) calculateWaitTime() time.Duration {
	// If no request tokens, calculate when we'll get one
	if r.config.RequestsPerMinute > 0 && r.requestTokens <= 0 {
		tokensPerSecond := float64(r.config.RequestsPerMinute) / 60.0
		if tokensPerSecond > 0 {
			waitSeconds := 1.0 / tokensPerSecond
			return time.Duration(waitSeconds * float64(time.Second))
		}
	}

	// If out of token budget, wait until window resets
	if r.config.TokensPerMinute > 0 && r.tokenBudget <= 0 {
		remaining := time.Minute - time.Since(r.windowStart)
		if remaining > 0 {
			return remaining
		}
	}

	// Default short wait
	return 100 * time.Millisecond
}

// Stats returns current rate limiting statistics.
func (r *RateLimitProvider) Stats() RateLimitStats {
	r.mu.Lock()
	defer r.mu.Unlock()

	return RateLimitStats{
		RequestsInWindow:  r.requestsInWindow,
		TokensInWindow:    r.tokensInWindow,
		RemainingRequests: r.requestTokens,
		RemainingTokens:   r.tokenBudget,
		WindowStart:       r.windowStart,
	}
}

// RateLimitStats contains rate limiting statistics.
type RateLimitStats struct {
	RequestsInWindow  int
	TokensInWindow    int
	RemainingRequests int
	RemainingTokens   int
	WindowStart       time.Time
}

// WithRateLimit wraps a provider with rate limiting.
func WithRateLimit(p Provider, config *RateLimitConfig) Provider {
	if p == nil {
		return nil
	}
	return NewRateLimitProvider(p, config)
}
