package config

import (
	"os"
	"path/filepath"
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

// TestDefaultValues verifies that an empty Config struct has safe zero-value defaults.
func TestDefaultValues(t *testing.T) {
	cfg := &Config{}

	// LLM defaults
	if cfg.LLM.Provider != "" {
		t.Errorf("default LLM.Provider should be empty, got %q", cfg.LLM.Provider)
	}
	if cfg.LLM.Temperature != 0 {
		t.Errorf("default LLM.Temperature should be 0, got %f", cfg.LLM.Temperature)
	}
	if cfg.LLM.MaxTokens != 0 {
		t.Errorf("default LLM.MaxTokens should be 0, got %d", cfg.LLM.MaxTokens)
	}

	// Graph defaults
	if cfg.Graph.URI != "" {
		t.Errorf("default Graph.URI should be empty, got %q", cfg.Graph.URI)
	}

	// Vector defaults (reserved for future use)
	if cfg.Vector.Host != "" {
		t.Errorf("default Vector.Host should be empty, got %q", cfg.Vector.Host)
	}
	if cfg.Vector.Port != 0 {
		t.Errorf("default Vector.Port should be 0, got %d", cfg.Vector.Port)
	}

	// Temporal defaults
	if cfg.Temporal.Host != "" {
		t.Errorf("default Temporal.Host should be empty, got %q", cfg.Temporal.Host)
	}
	if cfg.Temporal.Namespace != "" {
		t.Errorf("default Temporal.Namespace should be empty, got %q", cfg.Temporal.Namespace)
	}
	if cfg.Temporal.TaskQueue != "" {
		t.Errorf("default Temporal.TaskQueue should be empty, got %q", cfg.Temporal.TaskQueue)
	}

	// Log defaults
	if cfg.Log.Level != "" {
		t.Errorf("default Log.Level should be empty, got %q", cfg.Log.Level)
	}
	if cfg.Log.Format != "" {
		t.Errorf("default Log.Format should be empty, got %q", cfg.Log.Format)
	}

	// Empty config should produce no validation warnings
	warnings := cfg.Validate()
	if len(warnings) != 0 {
		t.Errorf("empty config should have no warnings, got %v", warnings)
	}
}

// TestLoadFromYAML verifies that Load() correctly reads a YAML file into Config.
func TestLoadFromYAML(t *testing.T) {
	yaml := `
llm:
  provider: openai
  model: gpt-4o
  api_key: test-key-123
  base_url: https://api.openai.com/v1
  temperature: 0.7
  max_tokens: 4096
  agents:
    judge:
      provider: anthropic
      model: claude-3-5-sonnet-20241022
graph:
  uri: neo4j://localhost:7687
  username: neo4j
  password: secret
vector:
  host: localhost
  port: 6333
  collection: anvil-embeddings
temporal:
  host: localhost:7233
  namespace: anvil
  task_queue: migration
log:
  level: info
  format: json
`
	dir := t.TempDir()
	cfgPath := filepath.Join(dir, "config.yaml")
	if err := os.WriteFile(cfgPath, []byte(yaml), 0600); err != nil {
		t.Fatalf("failed to write temp config: %v", err)
	}

	cfg, err := Load(cfgPath)
	if err != nil {
		t.Fatalf("Load() error: %v", err)
	}

	// LLM fields
	if cfg.LLM.Provider != "openai" {
		t.Errorf("LLM.Provider: got %q, want %q", cfg.LLM.Provider, "openai")
	}
	if cfg.LLM.Model != "gpt-4o" {
		t.Errorf("LLM.Model: got %q, want %q", cfg.LLM.Model, "gpt-4o")
	}
	if cfg.LLM.APIKey != "test-key-123" {
		t.Errorf("LLM.APIKey: got %q, want %q", cfg.LLM.APIKey, "test-key-123")
	}
	if cfg.LLM.BaseURL != "https://api.openai.com/v1" {
		t.Errorf("LLM.BaseURL: got %q, want %q", cfg.LLM.BaseURL, "https://api.openai.com/v1")
	}
	if cfg.LLM.Temperature != 0.7 {
		t.Errorf("LLM.Temperature: got %f, want 0.7", cfg.LLM.Temperature)
	}
	if cfg.LLM.MaxTokens != 4096 {
		t.Errorf("LLM.MaxTokens: got %d, want 4096", cfg.LLM.MaxTokens)
	}

	// Per-agent override
	judgeOverride, ok := cfg.LLM.Agents["judge"]
	if !ok {
		t.Fatal("expected agents.judge override to be present")
	}
	if judgeOverride.Provider != "anthropic" {
		t.Errorf("agents.judge.provider: got %q, want %q", judgeOverride.Provider, "anthropic")
	}

	// Graph fields
	if cfg.Graph.URI != "neo4j://localhost:7687" {
		t.Errorf("Graph.URI: got %q, want %q", cfg.Graph.URI, "neo4j://localhost:7687")
	}
	if cfg.Graph.Username != "neo4j" {
		t.Errorf("Graph.Username: got %q, want %q", cfg.Graph.Username, "neo4j")
	}
	if cfg.Graph.Password != "secret" {
		t.Errorf("Graph.Password: got %q, want %q", cfg.Graph.Password, "secret")
	}

	// Vector fields (reserved for future use)
	if cfg.Vector.Host != "localhost" {
		t.Errorf("Vector.Host: got %q, want %q", cfg.Vector.Host, "localhost")
	}
	if cfg.Vector.Port != 6333 {
		t.Errorf("Vector.Port: got %d, want 6333", cfg.Vector.Port)
	}
	if cfg.Vector.Collection != "anvil-embeddings" {
		t.Errorf("Vector.Collection: got %q, want %q", cfg.Vector.Collection, "anvil-embeddings")
	}

	// Temporal fields
	if cfg.Temporal.Host != "localhost:7233" {
		t.Errorf("Temporal.Host: got %q, want %q", cfg.Temporal.Host, "localhost:7233")
	}
	if cfg.Temporal.Namespace != "anvil" {
		t.Errorf("Temporal.Namespace: got %q, want %q", cfg.Temporal.Namespace, "anvil")
	}
	if cfg.Temporal.TaskQueue != "migration" {
		t.Errorf("Temporal.TaskQueue: got %q, want %q", cfg.Temporal.TaskQueue, "migration")
	}

	// Log fields
	if cfg.Log.Level != "info" {
		t.Errorf("Log.Level: got %q, want %q", cfg.Log.Level, "info")
	}
	if cfg.Log.Format != "json" {
		t.Errorf("Log.Format: got %q, want %q", cfg.Log.Format, "json")
	}
}

// TestLoadFromYAML_Missing verifies that Load() returns an error for a missing file.
func TestLoadFromYAML_Missing(t *testing.T) {
	_, err := Load("/nonexistent/path/config.yaml")
	if err == nil {
		t.Error("Load() should return error for missing file")
	}
	if !strings.Contains(err.Error(), "reading config") {
		t.Errorf("error should mention 'reading config', got: %v", err)
	}
}

// TestLoadFromYAML_EnvOverride verifies that ANVIL_ env vars override YAML values.
func TestLoadFromYAML_EnvOverride(t *testing.T) {
	yaml := `
llm:
  provider: openai
  model: gpt-4o
  api_key: yaml-key
`
	dir := t.TempDir()
	cfgPath := filepath.Join(dir, "config.yaml")
	if err := os.WriteFile(cfgPath, []byte(yaml), 0600); err != nil {
		t.Fatalf("failed to write temp config: %v", err)
	}

	t.Setenv("ANVIL_LLM_API_KEY", "env-key-override")

	cfg, err := Load(cfgPath)
	if err != nil {
		t.Fatalf("Load() error: %v", err)
	}

	if cfg.LLM.APIKey != "env-key-override" {
		t.Errorf("env override: LLM.APIKey got %q, want %q", cfg.LLM.APIKey, "env-key-override")
	}
}
