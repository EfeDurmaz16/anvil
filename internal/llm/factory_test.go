package llm

import (
	"context"
	"errors"
	"testing"
	"time"
)

func TestNewFactory(t *testing.T) {
	f := NewFactory()
	if f == nil {
		t.Fatal("expected non-nil factory")
	}
	if f.constructors == nil {
		t.Fatal("expected constructors map to be initialized")
	}
	if len(f.constructors) != 0 {
		t.Fatalf("expected empty factory, got %d constructors", len(f.constructors))
	}
}

func TestFactoryRegister(t *testing.T) {
	f := NewFactory()
	called := false
	ctor := func(cfg ProviderConfig) (Provider, error) {
		called = true
		return nil, nil
	}

	f.Register("test-provider", ctor)

	if len(f.constructors) != 1 {
		t.Fatalf("expected 1 constructor, got %d", len(f.constructors))
	}

	// Verify constructor is actually registered
	f.constructors["test-provider"](ProviderConfig{})
	if !called {
		t.Fatal("constructor was not called")
	}
}

func TestFactoryCreate_EmptyProvider(t *testing.T) {
	f := NewFactory()

	// Empty provider
	p, err := f.Create(ProviderConfig{Provider: ""})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if p != nil {
		t.Fatal("expected nil provider for empty string")
	}
}

func TestFactoryCreate_NoneProvider(t *testing.T) {
	f := NewFactory()

	// "none" provider
	p, err := f.Create(ProviderConfig{Provider: "none"})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if p != nil {
		t.Fatal("expected nil provider for 'none'")
	}
}

func TestFactoryCreate_UnknownProvider(t *testing.T) {
	f := NewFactory()
	f.Register("provider1", func(cfg ProviderConfig) (Provider, error) {
		return nil, nil
	})
	f.Register("provider2", func(cfg ProviderConfig) (Provider, error) {
		return nil, nil
	})

	_, err := f.Create(ProviderConfig{Provider: "unknown"})
	if err == nil {
		t.Fatal("expected error for unknown provider")
	}

	errMsg := err.Error()
	if errMsg == "" {
		t.Fatal("expected non-empty error message")
	}
	// Error should mention unknown provider and list registered ones
	t.Logf("Error message: %s", errMsg)
}

func TestFactoryCreate_RegisteredProvider(t *testing.T) {
	f := NewFactory()
	expectedProvider := &mockTestProvider{name: "test"}

	f.Register("test", func(cfg ProviderConfig) (Provider, error) {
		return expectedProvider, nil
	})

	p, err := f.Create(ProviderConfig{Provider: "test"})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if p == nil {
		t.Fatal("expected non-nil provider")
	}
}

func TestFactoryCreate_ConstructorError(t *testing.T) {
	f := NewFactory()
	expectedErr := errors.New("constructor failed")

	f.Register("failing", func(cfg ProviderConfig) (Provider, error) {
		return nil, expectedErr
	})

	p, err := f.Create(ProviderConfig{Provider: "failing"})
	if err == nil {
		t.Fatal("expected error from constructor")
	}
	if !errors.Is(err, expectedErr) {
		t.Fatalf("expected constructor error, got: %v", err)
	}
	if p != nil {
		t.Fatal("expected nil provider on error")
	}
}

func TestFactoryCreate_WithTimeoutWrapsRetry(t *testing.T) {
	f := NewFactory()
	inner := &mockTestProvider{name: "inner"}

	f.Register("test", func(cfg ProviderConfig) (Provider, error) {
		return inner, nil
	})

	p, err := f.Create(ProviderConfig{
		Provider: "test",
		Timeout:  5 * time.Second,
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if p == nil {
		t.Fatal("expected non-nil provider")
	}

	// Should be wrapped with RetryProvider
	retry, ok := p.(*RetryProvider)
	if !ok {
		t.Fatalf("expected RetryProvider wrapper, got %T", p)
	}
	if retry.inner.Name() != inner.Name() {
		t.Fatal("expected inner provider to match")
	}
}

func TestFactoryCreate_WithMaxRetriesWrapsRetry(t *testing.T) {
	f := NewFactory()
	inner := &mockTestProvider{name: "inner"}

	f.Register("test", func(cfg ProviderConfig) (Provider, error) {
		return inner, nil
	})

	p, err := f.Create(ProviderConfig{
		Provider:   "test",
		MaxRetries: 5,
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Should be wrapped with RetryProvider
	retry, ok := p.(*RetryProvider)
	if !ok {
		t.Fatalf("expected RetryProvider wrapper, got %T", p)
	}
	if retry.config.MaxRetries != 5 {
		t.Fatalf("expected 5 retries, got %d", retry.config.MaxRetries)
	}
}

func TestDefaultProviderConfig(t *testing.T) {
	cfg := DefaultProviderConfig()

	if cfg.Timeout != 2*time.Minute {
		t.Errorf("expected 2 minute timeout, got %v", cfg.Timeout)
	}
	if cfg.MaxRetries != 3 {
		t.Errorf("expected 3 max retries, got %d", cfg.MaxRetries)
	}
	if cfg.RetryDelay != 1*time.Second {
		t.Errorf("expected 1 second retry delay, got %v", cfg.RetryDelay)
	}
}

func TestKnownProviders(t *testing.T) {
	expectedProviders := map[string]string{
		"anthropic":   "https://api.anthropic.com/v1",
		"openai":      "https://api.openai.com/v1",
		"groq":        "https://api.groq.com/openai/v1",
		"huggingface": "https://api-inference.huggingface.co/v1",
		"ollama":      "http://localhost:11434/v1",
		"together":    "https://api.together.xyz/v1",
		"deepseek":    "https://api.deepseek.com/v1",
	}

	if len(KnownProviders) != len(expectedProviders) {
		t.Errorf("expected %d known providers, got %d", len(expectedProviders), len(KnownProviders))
	}

	for name, expectedURL := range expectedProviders {
		url, ok := KnownProviders[name]
		if !ok {
			t.Errorf("expected provider %q to be in KnownProviders", name)
			continue
		}
		if url != expectedURL {
			t.Errorf("provider %q: expected URL %q, got %q", name, expectedURL, url)
		}
	}
}

// mockTestProvider is a simple mock for testing
type mockTestProvider struct {
	name string
}

func (m *mockTestProvider) Name() string {
	return m.name
}

func (m *mockTestProvider) Complete(_ context.Context, _ *Prompt, _ *RequestOptions) (*Response, error) {
	return &Response{Content: "test"}, nil
}

func (m *mockTestProvider) Embed(_ context.Context, _ []string) ([][]float32, error) {
	return nil, nil
}
