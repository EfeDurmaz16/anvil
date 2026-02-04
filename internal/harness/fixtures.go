package harness

import (
	"bufio"
	"encoding/json"
	"fmt"
	"io"
)

// FixtureKind describes the type of recorded scenario.
type FixtureKind string

const (
	FixtureHTTP  FixtureKind = "http"
	FixtureBatch FixtureKind = "batch"
)

// Fixture is a single record/replay test case.
//
// V1 intentionally keeps the schema small and tool-agnostic; it can be produced by
// an API gateway/middleware recorder or by existing regression test exporters.
type Fixture struct {
	Kind FixtureKind `json:"kind"`
	Name string      `json:"name"`

	// CorrelationID ties together logs, DB diffs, and replay runs.
	CorrelationID string `json:"correlation_id,omitempty"`

	HTTP *HTTPFixture `json:"http,omitempty"`
	// Batch is reserved for file-based batch scenarios.
	Batch *BatchFixture `json:"batch,omitempty"`

	// Normalize rules for this fixture (optional overrides).
	Normalize *NormalizeRules `json:"normalize,omitempty"`
}

type HTTPFixture struct {
	Method string            `json:"method"`
	Path   string            `json:"path"`
	Header map[string]string `json:"header,omitempty"`
	Body   json.RawMessage   `json:"body,omitempty"`

	ExpectedStatus int               `json:"expected_status"`
	ExpectedHeader map[string]string `json:"expected_header,omitempty"`
	ExpectedBody   json.RawMessage   `json:"expected_body,omitempty"`
}

type BatchFixture struct {
	// Inputs/Outputs are tool-defined paths relative to the proof pack root.
	Inputs  []string `json:"inputs,omitempty"`
	Outputs []string `json:"outputs,omitempty"`

	// ExpectedOutputHashes optionally pins output deterministically.
	ExpectedOutputHashes map[string]string `json:"expected_output_hashes,omitempty"`
}

// ReadJSONL reads fixtures from a JSON Lines stream.
func ReadJSONL(r io.Reader) ([]Fixture, error) {
	var out []Fixture
	sc := bufio.NewScanner(r)
	// Allow reasonably large fixtures (body payloads).
	sc.Buffer(make([]byte, 0, 64*1024), 10*1024*1024)

	lineNo := 0
	for sc.Scan() {
		lineNo++
		b := sc.Bytes()
		if len(b) == 0 {
			continue
		}
		var f Fixture
		if err := json.Unmarshal(b, &f); err != nil {
			return nil, fmt.Errorf("fixtures: invalid JSONL at line %d: %w", lineNo, err)
		}
		if f.Kind == "" {
			return nil, fmt.Errorf("fixtures: missing kind at line %d", lineNo)
		}
		if f.Name == "" {
			return nil, fmt.Errorf("fixtures: missing name at line %d", lineNo)
		}
		out = append(out, f)
	}
	if err := sc.Err(); err != nil {
		return nil, err
	}
	return out, nil
}
