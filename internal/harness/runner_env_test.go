package harness

import (
	"os"
	"strings"
	"testing"
)

func TestFilterEnv(t *testing.T) {
	// Save and restore env after test
	orig := os.Environ()
	defer func() {
		os.Clearenv()
		for _, e := range orig {
			k, v, _ := strings.Cut(e, "=")
			os.Setenv(k, v)
		}
	}()

	// Clear environment and set controlled vars
	os.Clearenv()
	os.Setenv("PATH", "/usr/bin:/bin")
	os.Setenv("HOME", "/home/user")
	os.Setenv("GOPATH", "/go")
	os.Setenv("ANVIL_API_KEY", "anvil-secret")
	os.Setenv("MY_SECRET_KEY", "supersecret")
	os.Setenv("MY_API_KEY_VALUE", "apivalue")
	os.Setenv("ANTHROPIC_API_KEY", "anthro-key")
	os.Setenv("GITHUB_TOKEN", "ghtoken")
	os.Setenv("NORMAL_VAR", "normal")

	result := filterEnv()

	// Build a map for easy lookup
	resultMap := make(map[string]string)
	for _, entry := range result {
		k, v, _ := strings.Cut(entry, "=")
		resultMap[k] = v
	}

	t.Run("PATH passes through", func(t *testing.T) {
		if v, ok := resultMap["PATH"]; !ok || v == "" {
			t.Errorf("expected PATH to pass through, got %q", v)
		}
	})

	t.Run("HOME passes through", func(t *testing.T) {
		if v, ok := resultMap["HOME"]; !ok || v == "" {
			t.Errorf("expected HOME to pass through, got %q", v)
		}
	})

	t.Run("GOPATH passes through", func(t *testing.T) {
		if _, ok := resultMap["GOPATH"]; !ok {
			t.Error("expected GOPATH to pass through")
		}
	})

	t.Run("NORMAL_VAR passes through", func(t *testing.T) {
		if _, ok := resultMap["NORMAL_VAR"]; !ok {
			t.Error("expected NORMAL_VAR to pass through")
		}
	})

	t.Run("ANVIL_API_KEY is filtered", func(t *testing.T) {
		if _, ok := resultMap["ANVIL_API_KEY"]; ok {
			t.Error("expected ANVIL_API_KEY to be filtered out")
		}
	})

	t.Run("MY_SECRET_KEY is filtered", func(t *testing.T) {
		if _, ok := resultMap["MY_SECRET_KEY"]; ok {
			t.Error("expected MY_SECRET_KEY to be filtered out")
		}
	})

	t.Run("MY_API_KEY_VALUE is filtered", func(t *testing.T) {
		if _, ok := resultMap["MY_API_KEY_VALUE"]; ok {
			t.Error("expected MY_API_KEY_VALUE to be filtered out")
		}
	})

	t.Run("ANTHROPIC_API_KEY is filtered", func(t *testing.T) {
		if _, ok := resultMap["ANTHROPIC_API_KEY"]; ok {
			t.Error("expected ANTHROPIC_API_KEY to be filtered out")
		}
	})

	t.Run("TOKEN vars are filtered", func(t *testing.T) {
		if _, ok := resultMap["GITHUB_TOKEN"]; ok {
			t.Error("expected GITHUB_TOKEN to be filtered out")
		}
	})
}
