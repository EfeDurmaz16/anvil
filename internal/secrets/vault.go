package secrets

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"strings"
	"time"
)

// VaultConfig configures the HashiCorp Vault provider.
type VaultConfig struct {
	// Address is the Vault server address (e.g., "http://localhost:8200")
	Address string
	// Token is the Vault authentication token
	Token string
	// MountPath is the secrets engine mount path (default: "secret")
	MountPath string
	// SecretPath is the path under the mount for Anvil secrets (default: "anvil")
	SecretPath string
	// Timeout for Vault API requests
	Timeout time.Duration
}

// DefaultVaultConfig returns default Vault configuration.
func DefaultVaultConfig() *VaultConfig {
	return &VaultConfig{
		Address:    "http://localhost:8200",
		MountPath:  "secret",
		SecretPath: "anvil",
		Timeout:    10 * time.Second,
	}
}

// VaultProvider reads secrets from HashiCorp Vault.
type VaultProvider struct {
	config *VaultConfig
	client *http.Client
}

// NewVaultProvider creates a Vault secrets provider.
func NewVaultProvider(config *VaultConfig) (*VaultProvider, error) {
	if config == nil {
		config = DefaultVaultConfig()
	}
	if config.Address == "" {
		return nil, fmt.Errorf("vault address required")
	}
	if config.Token == "" {
		return nil, fmt.Errorf("vault token required")
	}
	if config.MountPath == "" {
		config.MountPath = "secret"
	}
	if config.SecretPath == "" {
		config.SecretPath = "anvil"
	}
	if config.Timeout == 0 {
		config.Timeout = 10 * time.Second
	}

	return &VaultProvider{
		config: config,
		client: &http.Client{Timeout: config.Timeout},
	}, nil
}

func (p *VaultProvider) Name() string { return "vault" }

func (p *VaultProvider) Get(ctx context.Context, key string) (string, error) {
	// Build URL for KV v2 secrets engine
	url := fmt.Sprintf("%s/v1/%s/data/%s",
		strings.TrimSuffix(p.config.Address, "/"),
		p.config.MountPath,
		p.config.SecretPath,
	)

	req, err := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	if err != nil {
		return "", fmt.Errorf("create request: %w", err)
	}
	req.Header.Set("X-Vault-Token", p.config.Token)

	resp, err := p.client.Do(req)
	if err != nil {
		return "", fmt.Errorf("vault request: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode == http.StatusNotFound {
		return "", fmt.Errorf("secret path not found: %s", p.config.SecretPath)
	}
	if resp.StatusCode != http.StatusOK {
		body, _ := io.ReadAll(resp.Body)
		return "", fmt.Errorf("vault error %d: %s", resp.StatusCode, body)
	}

	var result struct {
		Data struct {
			Data map[string]interface{} `json:"data"`
		} `json:"data"`
	}

	if err := json.NewDecoder(resp.Body).Decode(&result); err != nil {
		return "", fmt.Errorf("decode response: %w", err)
	}

	val, ok := result.Data.Data[key]
	if !ok {
		return "", fmt.Errorf("key not found in vault: %s", key)
	}

	if strVal, ok := val.(string); ok {
		return strVal, nil
	}

	return fmt.Sprintf("%v", val), nil
}

func (p *VaultProvider) Set(ctx context.Context, key, value string) error {
	// First, get existing data
	existingData := make(map[string]interface{})

	url := fmt.Sprintf("%s/v1/%s/data/%s",
		strings.TrimSuffix(p.config.Address, "/"),
		p.config.MountPath,
		p.config.SecretPath,
	)

	// Try to read existing data (ignore errors for new paths)
	getReq, _ := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	getReq.Header.Set("X-Vault-Token", p.config.Token)
	if resp, err := p.client.Do(getReq); err == nil {
		defer resp.Body.Close()
		if resp.StatusCode == http.StatusOK {
			var result struct {
				Data struct {
					Data map[string]interface{} `json:"data"`
				} `json:"data"`
			}
			if json.NewDecoder(resp.Body).Decode(&result) == nil {
				existingData = result.Data.Data
			}
		}
	}

	// Update with new key
	existingData[key] = value

	// Write back
	payload := map[string]interface{}{
		"data": existingData,
	}
	body, err := json.Marshal(payload)
	if err != nil {
		return fmt.Errorf("marshal payload: %w", err)
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodPost, url, strings.NewReader(string(body)))
	if err != nil {
		return fmt.Errorf("create request: %w", err)
	}
	req.Header.Set("X-Vault-Token", p.config.Token)
	req.Header.Set("Content-Type", "application/json")

	resp, err := p.client.Do(req)
	if err != nil {
		return fmt.Errorf("vault request: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusNoContent {
		respBody, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("vault error %d: %s", resp.StatusCode, respBody)
	}

	return nil
}

func (p *VaultProvider) Delete(ctx context.Context, key string) error {
	// For KV v2, we need to get existing data, remove the key, and write back
	existingData := make(map[string]interface{})

	url := fmt.Sprintf("%s/v1/%s/data/%s",
		strings.TrimSuffix(p.config.Address, "/"),
		p.config.MountPath,
		p.config.SecretPath,
	)

	// Get existing data
	getReq, _ := http.NewRequestWithContext(ctx, http.MethodGet, url, nil)
	getReq.Header.Set("X-Vault-Token", p.config.Token)
	if resp, err := p.client.Do(getReq); err == nil {
		defer resp.Body.Close()
		if resp.StatusCode == http.StatusOK {
			var result struct {
				Data struct {
					Data map[string]interface{} `json:"data"`
				} `json:"data"`
			}
			if json.NewDecoder(resp.Body).Decode(&result) == nil {
				existingData = result.Data.Data
			}
		}
	}

	// Remove the key
	delete(existingData, key)

	// Write back
	payload := map[string]interface{}{
		"data": existingData,
	}
	body, err := json.Marshal(payload)
	if err != nil {
		return fmt.Errorf("marshal payload: %w", err)
	}

	req, err := http.NewRequestWithContext(ctx, http.MethodPost, url, strings.NewReader(string(body)))
	if err != nil {
		return fmt.Errorf("create request: %w", err)
	}
	req.Header.Set("X-Vault-Token", p.config.Token)
	req.Header.Set("Content-Type", "application/json")

	resp, err := p.client.Do(req)
	if err != nil {
		return fmt.Errorf("vault request: %w", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK && resp.StatusCode != http.StatusNoContent {
		respBody, _ := io.ReadAll(resp.Body)
		return fmt.Errorf("vault error %d: %s", resp.StatusCode, respBody)
	}

	return nil
}
