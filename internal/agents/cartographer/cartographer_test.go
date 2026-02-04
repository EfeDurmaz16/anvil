package cartographer

import (
	"context"
	"os"
	"path/filepath"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	fortranplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/fortran"
)

func TestLoadSourceFiles_UsesPluginExtensions(t *testing.T) {
	dir := t.TempDir()

	// Fortran extension should be picked up.
	fortranPath := filepath.Join(dir, "hello.f90")
	if err := os.WriteFile(fortranPath, []byte("program hello\nend program hello\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	// A non-Fortran file should be ignored for directory scans.
	ignoredPath := filepath.Join(dir, "ignored.cbl")
	if err := os.WriteFile(ignoredPath, []byte("IDENTIFICATION DIVISION.\n"), 0o644); err != nil {
		t.Fatal(err)
	}

	reg := plugins.NewRegistry()
	reg.RegisterSource(fortranplugin.New())

	agent := New()
	res, err := agent.Run(context.Background(), &agents.AgentContext{
		Registry: reg,
		Params: map[string]string{
			"source": "fortran",
			"input":  dir,
		},
	})
	if err != nil {
		t.Fatal(err)
	}
	if res == nil || res.Graph == nil {
		t.Fatal("expected graph")
	}
	if len(res.Graph.Modules) != 1 {
		t.Fatalf("expected 1 module, got %d", len(res.Graph.Modules))
	}
	if res.Graph.Modules[0].Path != fortranPath {
		t.Fatalf("expected module path %q, got %q", fortranPath, res.Graph.Modules[0].Path)
	}
}
