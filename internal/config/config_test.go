package config

import (
	"strings"
	"testing"
)

func TestValidate_Empty(t *testing.T) {
	cfg := &Config{}
	warnings := cfg.Validate()
	if len(warnings) != 0 {
		t.Errorf("empty config should have no warnings, got %v", warnings)
	}
}

func TestValidate_MissingAPIKey(t *testing.T) {
	cfg := &Config{
		LLM: LLMConfig{Provider: "openai"},
	}
	warnings := cfg.Validate()
	found := false
	for _, w := range warnings {
		if strings.Contains(w, "api_key") {
			found = true
			break
		}
	}
	if !found {
		t.Error("expected warning about missing api_key")
	}
}

func TestValidate_InvalidTemperature(t *testing.T) {
	tests := []struct {
		name string
		temp float64
		want bool // true = should warn
	}{
		{"zero", 0, false},
		{"normal", 0.7, false},
		{"max", 2.0, false},
		{"negative", -1, true},
		{"too_high", 3.0, true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cfg := &Config{LLM: LLMConfig{Temperature: tt.temp}}
			warnings := cfg.Validate()
			hasWarn := false
			for _, w := range warnings {
				if strings.Contains(w, "temperature") {
					hasWarn = true
				}
			}
			if hasWarn != tt.want {
				t.Errorf("temperature=%.1f: hasWarn=%v, want=%v", tt.temp, hasWarn, tt.want)
			}
		})
	}
}

func TestValidate_NegativeMaxTokens(t *testing.T) {
	cfg := &Config{LLM: LLMConfig{MaxTokens: -100}}
	warnings := cfg.Validate()
	found := false
	for _, w := range warnings {
		if strings.Contains(w, "max_tokens") {
			found = true
		}
	}
	if !found {
		t.Error("expected warning about negative max_tokens")
	}
}

func TestValidate_NoneProvider(t *testing.T) {
	// "none" provider with no API key should not warn
	cfg := &Config{LLM: LLMConfig{Provider: "none"}}
	warnings := cfg.Validate()
	for _, w := range warnings {
		if strings.Contains(w, "api_key") {
			t.Error("'none' provider should not warn about missing api_key")
		}
	}
}

func TestResolveForAgent(t *testing.T) {
	cfg := LLMConfig{
		Provider: "openai",
		Model:    "gpt-4",
		APIKey:   "key1",
		Agents: map[string]LLMAgentOverride{
			"judge": {Provider: "anthropic", Model: "claude-3"},
		},
	}

	// Should override provider and model
	resolved := cfg.ResolveForAgent("judge")
	if resolved.Provider != "anthropic" {
		t.Errorf("expected provider=anthropic, got %s", resolved.Provider)
	}
	if resolved.Model != "claude-3" {
		t.Errorf("expected model=claude-3, got %s", resolved.Model)
	}
	// Should inherit API key
	if resolved.APIKey != "key1" {
		t.Errorf("expected inherited api_key=key1, got %s", resolved.APIKey)
	}

	// Unknown agent should return base config
	base := cfg.ResolveForAgent("unknown")
	if base.Provider != "openai" {
		t.Errorf("expected base provider=openai, got %s", base.Provider)
	}
}
