package harness

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
)

// ManifestFile is the default filename that target plugins should generate at the
// root of the scaffold.
const ManifestFile = "anvil.manifest.json"

// Manifest describes how to compile/typecheck and run fixtures for a generated
// target project. This keeps the harness target-language-agnostic.
type Manifest struct {
	Version  string `json:"version"`
	Language string `json:"language"`

	// Compile is a list of commands executed in order.
	Compile []Command `json:"compile,omitempty"`

	// RunFixture is the command executed per fixture. The harness passes the
	// fixture JSON on stdin; the runner must print JSON describing ActualOutput
	// on stdout.
	RunFixture Command `json:"run_fixture"`
}

type Command struct {
	Cmd  string            `json:"cmd"`
	Args []string          `json:"args,omitempty"`
	Dir  string            `json:"dir,omitempty"` // relative to project root
	Env  map[string]string `json:"env,omitempty"`
}

func LoadManifest(projectDir string) (*Manifest, string, error) {
	path := filepath.Join(projectDir, ManifestFile)
	b, err := os.ReadFile(path)
	if err != nil {
		return nil, path, err
	}
	var m Manifest
	if err := json.Unmarshal(b, &m); err != nil {
		return nil, path, fmt.Errorf("manifest: invalid JSON: %w", err)
	}
	if m.RunFixture.Cmd == "" {
		return nil, path, fmt.Errorf("manifest: missing run_fixture.cmd")
	}
	return &m, path, nil
}
