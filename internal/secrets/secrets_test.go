package secrets

import (
	"context"
	"os"
	"path/filepath"
	"testing"
)

// ==================== EnvProvider Tests ====================

func TestEnvProvider_Name(t *testing.T) {
	p := NewEnvProvider("")
	if p.Name() != "env" {
		t.Fatalf("expected 'env', got %s", p.Name())
	}
}

func TestEnvProvider_Get_WithPrefix(t *testing.T) {
	os.Setenv("ANVIL_TEST_SECRET", "secret_value")
	defer os.Unsetenv("ANVIL_TEST_SECRET")

	p := NewEnvProvider("ANVIL_")
	val, err := p.Get(context.Background(), "test_secret")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if val != "secret_value" {
		t.Fatalf("expected 'secret_value', got %s", val)
	}
}

func TestEnvProvider_Get_WithoutPrefix(t *testing.T) {
	os.Setenv("TEST_SECRET_NO_PREFIX", "direct_value")
	defer os.Unsetenv("TEST_SECRET_NO_PREFIX")

	p := NewEnvProvider("ANVIL_")
	val, err := p.Get(context.Background(), "test_secret_no_prefix")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if val != "direct_value" {
		t.Fatalf("expected 'direct_value', got %s", val)
	}
}

func TestEnvProvider_Get_NotFound(t *testing.T) {
	p := NewEnvProvider("ANVIL_")
	_, err := p.Get(context.Background(), "nonexistent_secret_xyz")
	if err == nil {
		t.Fatal("expected error for missing secret")
	}
}

func TestEnvProvider_Set(t *testing.T) {
	p := NewEnvProvider("ANVIL_")
	err := p.Set(context.Background(), "set_test", "new_value")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	defer os.Unsetenv("ANVIL_SET_TEST")

	if os.Getenv("ANVIL_SET_TEST") != "new_value" {
		t.Fatal("expected env var to be set")
	}
}

func TestEnvProvider_Delete(t *testing.T) {
	os.Setenv("ANVIL_DELETE_TEST", "to_delete")

	p := NewEnvProvider("ANVIL_")
	err := p.Delete(context.Background(), "delete_test")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if os.Getenv("ANVIL_DELETE_TEST") != "" {
		t.Fatal("expected env var to be deleted")
	}
}

func TestEnvProvider_DefaultPrefix(t *testing.T) {
	p := NewEnvProvider("")
	if p.prefix != "ANVIL_" {
		t.Fatalf("expected default prefix 'ANVIL_', got %s", p.prefix)
	}
}

// ==================== FileProvider Tests ====================

func TestFileProvider_Name(t *testing.T) {
	tmpDir := t.TempDir()
	p, err := NewFileProvider(&FileConfig{
		Path:            filepath.Join(tmpDir, "secrets.json"),
		CreateIfMissing: true,
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if p.Name() != "file" {
		t.Fatalf("expected 'file', got %s", p.Name())
	}
}

func TestFileProvider_CreateIfMissing(t *testing.T) {
	tmpDir := t.TempDir()
	secretsPath := filepath.Join(tmpDir, "secrets.json")

	_, err := NewFileProvider(&FileConfig{
		Path:            secretsPath,
		CreateIfMissing: true,
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	if _, err := os.Stat(secretsPath); os.IsNotExist(err) {
		t.Fatal("expected file to be created")
	}
}

func TestFileProvider_GetSet(t *testing.T) {
	tmpDir := t.TempDir()
	p, err := NewFileProvider(&FileConfig{
		Path:            filepath.Join(tmpDir, "secrets.json"),
		CreateIfMissing: true,
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	ctx := context.Background()

	// Set a secret
	err = p.Set(ctx, "api_key", "my_secret_key")
	if err != nil {
		t.Fatalf("unexpected error setting secret: %v", err)
	}

	// Get the secret
	val, err := p.Get(ctx, "api_key")
	if err != nil {
		t.Fatalf("unexpected error getting secret: %v", err)
	}
	if val != "my_secret_key" {
		t.Fatalf("expected 'my_secret_key', got %s", val)
	}
}

func TestFileProvider_Get_NotFound(t *testing.T) {
	tmpDir := t.TempDir()
	p, err := NewFileProvider(&FileConfig{
		Path:            filepath.Join(tmpDir, "secrets.json"),
		CreateIfMissing: true,
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	_, err = p.Get(context.Background(), "nonexistent")
	if err == nil {
		t.Fatal("expected error for missing secret")
	}
}

func TestFileProvider_Delete(t *testing.T) {
	tmpDir := t.TempDir()
	p, err := NewFileProvider(&FileConfig{
		Path:            filepath.Join(tmpDir, "secrets.json"),
		CreateIfMissing: true,
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	ctx := context.Background()

	// Set then delete
	p.Set(ctx, "to_delete", "value")
	err = p.Delete(ctx, "to_delete")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Verify deleted
	_, err = p.Get(ctx, "to_delete")
	if err == nil {
		t.Fatal("expected error for deleted secret")
	}
}

func TestFileProvider_Reload(t *testing.T) {
	tmpDir := t.TempDir()
	secretsPath := filepath.Join(tmpDir, "secrets.json")

	p, err := NewFileProvider(&FileConfig{
		Path:            secretsPath,
		CreateIfMissing: true,
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	ctx := context.Background()
	p.Set(ctx, "key1", "value1")

	// Manually modify file
	os.WriteFile(secretsPath, []byte(`{"key1":"modified","key2":"new"}`), 0600)

	// Reload
	err = p.Reload()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Check values
	val, _ := p.Get(ctx, "key1")
	if val != "modified" {
		t.Fatalf("expected 'modified', got %s", val)
	}

	val, _ = p.Get(ctx, "key2")
	if val != "new" {
		t.Fatalf("expected 'new', got %s", val)
	}
}

func TestFileProvider_MissingPath(t *testing.T) {
	_, err := NewFileProvider(&FileConfig{Path: ""})
	if err == nil {
		t.Fatal("expected error for empty path")
	}
}

func TestFileProvider_NilConfig(t *testing.T) {
	_, err := NewFileProvider(nil)
	if err == nil {
		t.Fatal("expected error for nil config")
	}
}

// ==================== Manager Tests ====================

func TestManager_DefaultConfig(t *testing.T) {
	cfg := DefaultConfig()
	if cfg.Provider != "env" {
		t.Fatalf("expected 'env' provider, got %s", cfg.Provider)
	}
	if cfg.EnvPrefix != "ANVIL_" {
		t.Fatalf("expected 'ANVIL_' prefix, got %s", cfg.EnvPrefix)
	}
}

func TestManager_EnvProvider(t *testing.T) {
	os.Setenv("ANVIL_MANAGER_TEST", "manager_value")
	defer os.Unsetenv("ANVIL_MANAGER_TEST")

	m, err := NewManager(&Config{Provider: "env", EnvPrefix: "ANVIL_"})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	val, err := m.Get(context.Background(), "manager_test")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if val != "manager_value" {
		t.Fatalf("expected 'manager_value', got %s", val)
	}
}

func TestManager_FileProvider(t *testing.T) {
	tmpDir := t.TempDir()
	secretsPath := filepath.Join(tmpDir, "secrets.json")

	m, err := NewManager(&Config{
		Provider: "file",
		FileConfig: &FileConfig{
			Path:            secretsPath,
			CreateIfMissing: true,
		},
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	ctx := context.Background()
	m.Set(ctx, "file_key", "file_value")

	val, err := m.Get(ctx, "file_key")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if val != "file_value" {
		t.Fatalf("expected 'file_value', got %s", val)
	}
}

func TestManager_Fallback(t *testing.T) {
	// Set up env var as fallback
	os.Setenv("ANVIL_FALLBACK_TEST", "fallback_value")
	defer os.Unsetenv("ANVIL_FALLBACK_TEST")

	tmpDir := t.TempDir()
	m, err := NewManager(&Config{
		Provider: "file",
		FileConfig: &FileConfig{
			Path:            filepath.Join(tmpDir, "secrets.json"),
			CreateIfMissing: true,
		},
		EnvPrefix: "ANVIL_",
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Key not in file, should fall back to env
	val, err := m.Get(context.Background(), "fallback_test")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if val != "fallback_value" {
		t.Fatalf("expected 'fallback_value', got %s", val)
	}
}

func TestManager_GetOrDefault(t *testing.T) {
	m, _ := NewManager(&Config{Provider: "env", EnvPrefix: "ANVIL_"})

	val := m.GetOrDefault(context.Background(), "nonexistent_key_xyz", "default_val")
	if val != "default_val" {
		t.Fatalf("expected 'default_val', got %s", val)
	}
}

func TestManager_MustGet_Panic(t *testing.T) {
	m, _ := NewManager(&Config{Provider: "env", EnvPrefix: "ANVIL_"})

	defer func() {
		if r := recover(); r == nil {
			t.Fatal("expected panic for missing required secret")
		}
	}()

	m.MustGet(context.Background(), "definitely_missing_secret_xyz")
}

func TestManager_Cache(t *testing.T) {
	os.Setenv("ANVIL_CACHE_TEST", "cached_value")
	defer os.Unsetenv("ANVIL_CACHE_TEST")

	m, _ := NewManager(&Config{Provider: "env", EnvPrefix: "ANVIL_"})
	ctx := context.Background()

	// First get populates cache
	m.Get(ctx, "cache_test")

	// Change env var
	os.Setenv("ANVIL_CACHE_TEST", "new_value")

	// Should still get cached value
	val, _ := m.Get(ctx, "cache_test")
	if val != "cached_value" {
		t.Fatalf("expected cached 'cached_value', got %s", val)
	}

	// Clear cache
	m.ClearCache()

	// Now should get new value
	val, _ = m.Get(ctx, "cache_test")
	if val != "new_value" {
		t.Fatalf("expected 'new_value' after cache clear, got %s", val)
	}
}

func TestManager_DisableCache(t *testing.T) {
	os.Setenv("ANVIL_NOCACHE_TEST", "initial")
	defer os.Unsetenv("ANVIL_NOCACHE_TEST")

	m, _ := NewManager(&Config{Provider: "env", EnvPrefix: "ANVIL_"})
	m.DisableCache()

	ctx := context.Background()
	m.Get(ctx, "nocache_test")

	os.Setenv("ANVIL_NOCACHE_TEST", "changed")

	val, _ := m.Get(ctx, "nocache_test")
	if val != "changed" {
		t.Fatalf("expected 'changed' with cache disabled, got %s", val)
	}
}

func TestManager_Delete(t *testing.T) {
	tmpDir := t.TempDir()
	m, _ := NewManager(&Config{
		Provider: "file",
		FileConfig: &FileConfig{
			Path:            filepath.Join(tmpDir, "secrets.json"),
			CreateIfMissing: true,
		},
	})

	ctx := context.Background()
	m.Set(ctx, "delete_me", "value")

	err := m.Delete(ctx, "delete_me")
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	_, err = m.Get(ctx, "delete_me")
	if err == nil {
		t.Fatal("expected error for deleted secret")
	}
}

func TestManager_UnknownProvider(t *testing.T) {
	_, err := NewManager(&Config{Provider: "unknown_provider"})
	if err == nil {
		t.Fatal("expected error for unknown provider")
	}
}

func TestManager_VaultWithoutConfig(t *testing.T) {
	_, err := NewManager(&Config{Provider: "vault"})
	if err == nil {
		t.Fatal("expected error for vault without config")
	}
}

func TestManager_FileWithoutConfig(t *testing.T) {
	_, err := NewManager(&Config{Provider: "file"})
	if err == nil {
		t.Fatal("expected error for file without config")
	}
}

// ==================== SecretKey Constants Tests ====================

func TestSecretKeyConstants(t *testing.T) {
	keys := []SecretKey{
		SecretLLMAPIKey,
		SecretDBPassword,
		SecretDBConnString,
		SecretTemporalToken,
		SecretWebhookSecret,
		SecretEncryptionKey,
	}

	for _, k := range keys {
		if k == "" {
			t.Fatal("secret key constant should not be empty")
		}
	}
}
