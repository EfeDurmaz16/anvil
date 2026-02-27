package harness

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"
)

// Runner executes generated code and captures output for comparison.
// New target languages should not require harness code changes: provide
// `anvil.manifest.json` + a small runner program in the generated project.
type Runner interface {
	Name() string
	Compile(ctx context.Context, projectDir string) (*CompileResult, error)
	RunFixture(ctx context.Context, projectDir string, fixture Fixture) (*RunResult, error)
	Cleanup() error
}

type CompileResult struct {
	Success  bool          `json:"success"`
	Errors   []string      `json:"errors,omitempty"`
	Warnings []string      `json:"warnings,omitempty"`
	Duration time.Duration `json:"duration_ms"`
	Log      string        `json:"log,omitempty"`
}

type RunResult struct {
	FixtureName string        `json:"fixture_name"`
	Success     bool          `json:"success"`
	Output      *ActualOutput `json:"output,omitempty"`
	Duration    time.Duration `json:"duration_ms"`
	Error       string        `json:"error,omitempty"`
	Log         string        `json:"log,omitempty"`
}

// ActualOutput is the canonical runtime output understood by the harness.
// The target runner prints a JSON object matching this shape.
type ActualOutput struct {
	Status  int               `json:"status,omitempty"`
	Headers map[string]string `json:"headers,omitempty"`
	Body    json.RawMessage   `json:"body,omitempty"`

	Stdout   []byte `json:"stdout,omitempty"`
	Stderr   []byte `json:"stderr,omitempty"`
	ExitCode int    `json:"exit_code,omitempty"`
}

type RunnerConfig struct {
	Timeout time.Duration     `json:"timeout"`
	Env     map[string]string `json:"env,omitempty"`
}

func DefaultRunnerConfig() *RunnerConfig {
	return &RunnerConfig{
		Timeout: 5 * time.Minute,
		Env:     make(map[string]string),
	}
}

// ManifestRunner uses an `anvil.manifest.json` to compile and execute fixtures.
type ManifestRunner struct {
	cfg        *RunnerConfig
	manifest   *Manifest
	projectDir string
}

func NewManifestRunner(projectDir string, cfg *RunnerConfig) (*ManifestRunner, error) {
	if cfg == nil {
		cfg = DefaultRunnerConfig()
	}
	m, _, err := LoadManifest(projectDir)
	if err != nil {
		return nil, err
	}
	return &ManifestRunner{cfg: cfg, manifest: m, projectDir: projectDir}, nil
}

func (r *ManifestRunner) Name() string { return "manifest:" + r.manifest.Language }

func (r *ManifestRunner) Compile(ctx context.Context, projectDir string) (*CompileResult, error) {
	start := time.Now()
	res := &CompileResult{Success: true}

	for _, c := range r.manifest.Compile {
		stdout, stderr, err := r.exec(ctx, projectDir, c, nil)
		res.Log += string(stdout) + string(stderr)
		if err != nil {
			res.Success = false
			res.Errors = append(res.Errors, err.Error())
			res.Duration = time.Since(start)
			return res, nil
		}
	}

	res.Duration = time.Since(start)
	return res, nil
}

func (r *ManifestRunner) RunFixture(ctx context.Context, projectDir string, fixture Fixture) (*RunResult, error) {
	start := time.Now()
	res := &RunResult{
		FixtureName: fixture.Name,
		Success:     false,
		Output:      &ActualOutput{},
	}

	in, err := json.Marshal(fixture)
	if err != nil {
		res.Error = err.Error()
		res.Duration = time.Since(start)
		return res, nil
	}

	stdout, stderr, err := r.exec(ctx, projectDir, r.manifest.RunFixture, in)
	res.Log = string(stdout) + string(stderr)
	res.Duration = time.Since(start)
	if err != nil {
		res.Error = err.Error()
		res.Output.Stderr = stderr
		res.Output.Stdout = stdout
		return res, nil
	}

	// Expect stdout to be JSON for ActualOutput. If not parseable, keep raw.
	var out ActualOutput
	if uerr := json.Unmarshal(stdout, &out); uerr == nil {
		res.Output = &out
	} else {
		res.Output.Stdout = stdout
		res.Output.Stderr = stderr
	}

	res.Success = true
	return res, nil
}

func (r *ManifestRunner) Cleanup() error { return nil }

// envWhitelist contains safe environment variable prefixes/names for child processes.
var envWhitelist = map[string]bool{
	"PATH": true, "HOME": true, "USER": true, "TMPDIR": true,
	"LANG": true, "LC_ALL": true, "LC_CTYPE": true,
	"GOPATH": true, "GOROOT": true, "GOMODCACHE": true,
	"JAVA_HOME": true, "PYTHONPATH": true, "NODE_PATH": true,
	"TERM": true, "SHELL": true,
}

// sensitivePatterns are substrings that indicate a sensitive env var.
var sensitivePatterns = []string{"API_KEY", "SECRET", "TOKEN", "PASSWORD", "CREDENTIAL"}

func filterEnv() []string {
	var filtered []string
	for _, entry := range os.Environ() {
		key, _, ok := strings.Cut(entry, "=")
		if !ok {
			continue
		}
		upperKey := strings.ToUpper(key)

		// Block ANVIL_ internal vars
		if strings.HasPrefix(upperKey, "ANVIL_") {
			continue
		}

		// Block sensitive patterns
		sensitive := false
		for _, pat := range sensitivePatterns {
			if strings.Contains(upperKey, pat) {
				sensitive = true
				break
			}
		}
		if sensitive {
			continue
		}

		filtered = append(filtered, entry)
	}
	return filtered
}

// allowedCommands is the whitelist of permitted command base names.
var allowedCommands = map[string]bool{
	"node": true, "npx": true, "tsc": true,
	"python": true, "python3": true,
	"go":     true,
	"javac": true, "mvn": true, "gradle": true,
	"npm": true, "pip": true,
}

// validateCommand checks that the command is in the whitelist, is not an
// absolute path, and that the working directory does not contain "..".
func validateCommand(c Command) error {
	// Reject absolute paths in command
	if filepath.IsAbs(c.Cmd) {
		return fmt.Errorf("absolute command paths not allowed: %q", c.Cmd)
	}

	// Check command base name against whitelist
	base := filepath.Base(c.Cmd)
	if !allowedCommands[base] {
		return fmt.Errorf("command %q not in allowed list", c.Cmd)
	}

	// Reject ".." in working directory
	if c.Dir != "" && strings.Contains(filepath.Clean(c.Dir), "..") {
		return fmt.Errorf("directory traversal not allowed in Dir: %q", c.Dir)
	}

	return nil
}

func (r *ManifestRunner) exec(ctx context.Context, projectDir string, c Command, stdin []byte) ([]byte, []byte, error) {
	if err := validateCommand(c); err != nil {
		return nil, nil, err
	}

	timeout := r.cfg.Timeout
	if timeout <= 0 {
		timeout = 5 * time.Minute
	}
	ctx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()

	cmd := exec.CommandContext(ctx, c.Cmd, c.Args...)
	cmd.Dir = projectDir
	if c.Dir != "" {
		cmd.Dir = filepath.Join(projectDir, c.Dir)
	}

	env := filterEnv()
	for k, v := range r.cfg.Env {
		env = append(env, fmt.Sprintf("%s=%s", k, v))
	}
	for k, v := range c.Env {
		env = append(env, fmt.Sprintf("%s=%s", k, v))
	}
	cmd.Env = env

	if stdin != nil {
		cmd.Stdin = bytes.NewReader(stdin)
	}

	var stdout bytes.Buffer
	var stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr

	err := cmd.Run()
	if err != nil {
		return stdout.Bytes(), stderr.Bytes(), fmt.Errorf("%s %v failed: %w\n%s%s", c.Cmd, c.Args, err, stdout.Bytes(), stderr.Bytes())
	}
	return stdout.Bytes(), stderr.Bytes(), nil
}
