package secrets

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sync"
)

// FileConfig configures the file-based secrets provider.
// WARNING: This provider is for development/testing only. Do not use in production.
type FileConfig struct {
	// Path is the path to the secrets file (JSON format)
	Path string
	// CreateIfMissing creates the file if it doesn't exist
	CreateIfMissing bool
}

// FileProvider reads secrets from a JSON file.
// WARNING: This is for development only. Use Vault or env vars in production.
type FileProvider struct {
	config *FileConfig
	mu     sync.RWMutex
	data   map[string]string
}

// NewFileProvider creates a file-based secrets provider.
func NewFileProvider(config *FileConfig) (*FileProvider, error) {
	if config == nil || config.Path == "" {
		return nil, fmt.Errorf("file path required")
	}

	p := &FileProvider{
		config: config,
		data:   make(map[string]string),
	}

	// Load existing file
	if err := p.load(); err != nil {
		if os.IsNotExist(err) && config.CreateIfMissing {
			// Create empty file
			if err := p.save(); err != nil {
				return nil, fmt.Errorf("create secrets file: %w", err)
			}
		} else if !os.IsNotExist(err) {
			return nil, fmt.Errorf("load secrets file: %w", err)
		}
	}

	return p, nil
}

func (p *FileProvider) Name() string { return "file" }

func (p *FileProvider) Get(ctx context.Context, key string) (string, error) {
	p.mu.RLock()
	defer p.mu.RUnlock()

	val, ok := p.data[key]
	if !ok {
		return "", fmt.Errorf("secret not found: %s", key)
	}
	return val, nil
}

func (p *FileProvider) Set(ctx context.Context, key, value string) error {
	p.mu.Lock()
	defer p.mu.Unlock()

	p.data[key] = value
	return p.save()
}

func (p *FileProvider) Delete(ctx context.Context, key string) error {
	p.mu.Lock()
	defer p.mu.Unlock()

	delete(p.data, key)
	return p.save()
}

func (p *FileProvider) load() error {
	data, err := os.ReadFile(p.config.Path)
	if err != nil {
		return err
	}

	return json.Unmarshal(data, &p.data)
}

func (p *FileProvider) save() error {
	// Ensure directory exists
	dir := filepath.Dir(p.config.Path)
	if err := os.MkdirAll(dir, 0700); err != nil {
		return fmt.Errorf("create directory: %w", err)
	}

	data, err := json.MarshalIndent(p.data, "", "  ")
	if err != nil {
		return fmt.Errorf("marshal secrets: %w", err)
	}

	// Write with restrictive permissions
	return os.WriteFile(p.config.Path, data, 0600)
}

// Reload reloads secrets from the file.
func (p *FileProvider) Reload() error {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.load()
}
