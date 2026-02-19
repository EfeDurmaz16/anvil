package config

import (
	"fmt"
	"os"
	"strings"

	"github.com/spf13/viper"
)

// Config holds all application configuration.
type Config struct {
	LLM      LLMConfig      `mapstructure:"llm"`
	Graph    GraphConfig    `mapstructure:"graph"`
	Vector   VectorConfig   `mapstructure:"vector"`
	Temporal TemporalConfig `mapstructure:"temporal"`
	Log      LogConfig      `mapstructure:"log"`
}

type LLMConfig struct {
	Provider    string  `mapstructure:"provider"`
	Model       string  `mapstructure:"model"`
	APIKey      string  `mapstructure:"api_key"`
	BaseURL     string  `mapstructure:"base_url"`
	Temperature float64 `mapstructure:"temperature"`
	MaxTokens   int     `mapstructure:"max_tokens"`

	// Per-agent overrides. Keys are agent names (e.g. "judge", "specular", "architect").
	// Each override inherits unset fields from the top-level LLM config.
	Agents map[string]LLMAgentOverride `mapstructure:"agents"`
}

// LLMAgentOverride allows per-agent LLM provider configuration.
type LLMAgentOverride struct {
	Provider string `mapstructure:"provider"`
	Model    string `mapstructure:"model"`
	APIKey   string `mapstructure:"api_key"`
	BaseURL  string `mapstructure:"base_url"`
}

// ResolveForAgent returns an LLMConfig with agent-specific overrides applied.
func (c LLMConfig) ResolveForAgent(agentName string) LLMConfig {
	override, ok := c.Agents[agentName]
	if !ok {
		return c
	}
	resolved := c
	if override.Provider != "" {
		resolved.Provider = override.Provider
	}
	if override.Model != "" {
		resolved.Model = override.Model
	}
	if override.APIKey != "" {
		resolved.APIKey = override.APIKey
	}
	if override.BaseURL != "" {
		resolved.BaseURL = override.BaseURL
	}
	return resolved
}

type GraphConfig struct {
	URI      string `mapstructure:"uri"`
	Username string `mapstructure:"username"`
	Password string `mapstructure:"password"`
}

type VectorConfig struct {
	Host       string `mapstructure:"host"`
	Port       int    `mapstructure:"port"`
	Collection string `mapstructure:"collection"`
}

type TemporalConfig struct {
	Host      string `mapstructure:"host"`
	Namespace string `mapstructure:"namespace"`
	TaskQueue string `mapstructure:"task_queue"`
}

type LogConfig struct {
	Level  string `mapstructure:"level"`
	Format string `mapstructure:"format"`
}

// Validate checks configuration for issues and returns warnings.
func (c *Config) Validate() []string {
	var warnings []string

	// Check for empty API key with active provider (skip "none" provider)
	if c.LLM.Provider != "" && c.LLM.Provider != "none" && c.LLM.APIKey == "" {
		warnings = append(warnings, fmt.Sprintf("LLM provider '%s' is configured but api_key is empty", c.LLM.Provider))
	}

	// Check temperature range [0, 2.0]
	if c.LLM.Temperature < 0 || c.LLM.Temperature > 2.0 {
		warnings = append(warnings, fmt.Sprintf("LLM temperature %.2f is outside recommended range [0.0, 2.0]", c.LLM.Temperature))
	}

	// Check for negative max_tokens
	if c.LLM.MaxTokens < 0 {
		warnings = append(warnings, fmt.Sprintf("LLM max_tokens %d is negative", c.LLM.MaxTokens))
	}

	return warnings
}

// Load reads configuration from file and environment.
func Load(path string) (*Config, error) {
	v := viper.New()
	v.SetConfigFile(path)
	v.SetEnvPrefix("ANVIL")
	v.SetEnvKeyReplacer(strings.NewReplacer(".", "_"))
	v.AutomaticEnv()

	if err := v.ReadInConfig(); err != nil {
		return nil, fmt.Errorf("reading config: %w", err)
	}

	var cfg Config
	if err := v.Unmarshal(&cfg); err != nil {
		return nil, fmt.Errorf("unmarshalling config: %w", err)
	}

	// Validate configuration and print warnings
	if warnings := cfg.Validate(); len(warnings) > 0 {
		for _, warning := range warnings {
			fmt.Fprintf(os.Stderr, "Warning: %s\n", warning)
		}
	}

	return &cfg, nil
}
