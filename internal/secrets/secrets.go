// Package secrets provides unified secrets management with multiple backends.
package secrets

import (
	"context"
	"fmt"
	"os"
	"strings"
	"sync"
)

// SecretKey identifies common secret types.
type SecretKey string

const (
	SecretLLMAPIKey     SecretKey = "llm_api_key"
	SecretDBPassword    SecretKey = "db_password"
	SecretDBConnString  SecretKey = "db_connection_string"
	SecretTemporalToken SecretKey = "temporal_token"
	SecretWebhookSecret SecretKey = "webhook_secret"
	SecretEncryptionKey SecretKey = "encryption_key"
)

// Provider is the interface for secret backends.
type Provider interface {
	// Get retrieves a secret by key.
	Get(ctx context.Context, key string) (string, error)
	// Set stores a secret (not all providers support this).
	Set(ctx context.Context, key, value string) error
	// Delete removes a secret (not all providers support this).
	Delete(ctx context.Context, key string) error
	// Name returns the provider name.
	Name() string
}

// Config configures the secrets manager.
type Config struct {
	// Provider specifies which backend to use: "env", "vault", "file"
	Provider string
	// VaultConfig for HashiCorp Vault backend
	VaultConfig *VaultConfig
	// FileConfig for file-based backend (development only)
	FileConfig *FileConfig
	// Prefix for environment variable names (default: "ANVIL_")
	EnvPrefix string
}

// DefaultConfig returns default secrets configuration (env-based).
func DefaultConfig() *Config {
	return &Config{
		Provider:  "env",
		EnvPrefix: "ANVIL_",
	}
}

// Manager provides unified access to secrets from multiple backends.
type Manager struct {
	primary   Provider
	fallback  Provider
	cache     map[string]string
	cacheMu   sync.RWMutex
	useCache  bool
}

// NewManager creates a secrets manager with the specified configuration.
func NewManager(cfg *Config) (*Manager, error) {
	if cfg == nil {
		cfg = DefaultConfig()
	}

	var primary Provider
	var err error

	switch cfg.Provider {
	case "vault":
		if cfg.VaultConfig == nil {
			return nil, fmt.Errorf("vault config required for vault provider")
		}
		primary, err = NewVaultProvider(cfg.VaultConfig)
		if err != nil {
			return nil, fmt.Errorf("create vault provider: %w", err)
		}
	case "file":
		if cfg.FileConfig == nil {
			return nil, fmt.Errorf("file config required for file provider")
		}
		primary, err = NewFileProvider(cfg.FileConfig)
		if err != nil {
			return nil, fmt.Errorf("create file provider: %w", err)
		}
	case "env", "":
		primary = NewEnvProvider(cfg.EnvPrefix)
	default:
		return nil, fmt.Errorf("unknown secrets provider: %s", cfg.Provider)
	}

	// Always use env as fallback
	fallback := NewEnvProvider(cfg.EnvPrefix)

	return &Manager{
		primary:  primary,
		fallback: fallback,
		cache:    make(map[string]string),
		useCache: true,
	}, nil
}

// Get retrieves a secret, trying primary then fallback.
func (m *Manager) Get(ctx context.Context, key string) (string, error) {
	// Check cache first
	if m.useCache {
		m.cacheMu.RLock()
		if val, ok := m.cache[key]; ok {
			m.cacheMu.RUnlock()
			return val, nil
		}
		m.cacheMu.RUnlock()
	}

	// Try primary provider
	val, err := m.primary.Get(ctx, key)
	if err == nil && val != "" {
		m.cacheSet(key, val)
		return val, nil
	}

	// Try fallback
	if m.fallback != nil {
		val, err = m.fallback.Get(ctx, key)
		if err == nil && val != "" {
			m.cacheSet(key, val)
			return val, nil
		}
	}

	return "", fmt.Errorf("secret not found: %s", key)
}

// GetOrDefault retrieves a secret or returns a default value.
func (m *Manager) GetOrDefault(ctx context.Context, key, defaultVal string) string {
	val, err := m.Get(ctx, key)
	if err != nil || val == "" {
		return defaultVal
	}
	return val
}

// MustGet retrieves a secret or panics if not found.
func (m *Manager) MustGet(ctx context.Context, key string) string {
	val, err := m.Get(ctx, key)
	if err != nil {
		panic(fmt.Sprintf("required secret not found: %s", key))
	}
	return val
}

// Set stores a secret in the primary provider.
func (m *Manager) Set(ctx context.Context, key, value string) error {
	err := m.primary.Set(ctx, key, value)
	if err != nil {
		return err
	}
	m.cacheSet(key, value)
	return nil
}

// Delete removes a secret from the primary provider.
func (m *Manager) Delete(ctx context.Context, key string) error {
	err := m.primary.Delete(ctx, key)
	if err != nil {
		return err
	}
	m.cacheMu.Lock()
	delete(m.cache, key)
	m.cacheMu.Unlock()
	return nil
}

// ClearCache clears the secrets cache.
func (m *Manager) ClearCache() {
	m.cacheMu.Lock()
	m.cache = make(map[string]string)
	m.cacheMu.Unlock()
}

// DisableCache disables caching (useful for testing).
func (m *Manager) DisableCache() {
	m.useCache = false
}

func (m *Manager) cacheSet(key, value string) {
	if m.useCache {
		m.cacheMu.Lock()
		m.cache[key] = value
		m.cacheMu.Unlock()
	}
}

// EnvProvider reads secrets from environment variables.
type EnvProvider struct {
	prefix string
}

// NewEnvProvider creates an environment-based secrets provider.
func NewEnvProvider(prefix string) *EnvProvider {
	if prefix == "" {
		prefix = "ANVIL_"
	}
	return &EnvProvider{prefix: prefix}
}

func (p *EnvProvider) Name() string { return "env" }

func (p *EnvProvider) Get(ctx context.Context, key string) (string, error) {
	// Try with prefix first
	envKey := p.prefix + strings.ToUpper(key)
	if val := os.Getenv(envKey); val != "" {
		return val, nil
	}
	// Try without prefix
	if val := os.Getenv(strings.ToUpper(key)); val != "" {
		return val, nil
	}
	return "", fmt.Errorf("env var not found: %s", envKey)
}

func (p *EnvProvider) Set(ctx context.Context, key, value string) error {
	envKey := p.prefix + strings.ToUpper(key)
	return os.Setenv(envKey, value)
}

func (p *EnvProvider) Delete(ctx context.Context, key string) error {
	envKey := p.prefix + strings.ToUpper(key)
	return os.Unsetenv(envKey)
}

// Global manager instance
var globalManager *Manager
var managerOnce sync.Once

// Init initializes the global secrets manager.
func Init(cfg *Config) error {
	var err error
	managerOnce.Do(func() {
		globalManager, err = NewManager(cfg)
	})
	return err
}

// Get retrieves a secret using the global manager.
func Get(ctx context.Context, key string) (string, error) {
	if globalManager == nil {
		// Auto-initialize with defaults
		if err := Init(nil); err != nil {
			return "", err
		}
	}
	return globalManager.Get(ctx, key)
}

// GetOrDefault retrieves a secret or returns default using global manager.
func GetOrDefault(ctx context.Context, key, defaultVal string) string {
	if globalManager == nil {
		Init(nil)
	}
	return globalManager.GetOrDefault(ctx, key, defaultVal)
}

// MustGet retrieves a secret or panics using global manager.
func MustGet(ctx context.Context, key string) string {
	if globalManager == nil {
		Init(nil)
	}
	return globalManager.MustGet(ctx, key)
}
