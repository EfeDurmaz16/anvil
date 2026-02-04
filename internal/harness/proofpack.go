package harness

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"time"
)

// ProofPack is a minimal, tool-agnostic evidence bundle for a single run.
// It is designed to be archived and reviewed by enterprise teams.
type ProofPack struct {
	StartedAt time.Time `json:"started_at"`
	EndedAt   time.Time `json:"ended_at"`
	Pass      bool      `json:"pass"`

	FixtureCount int `json:"fixture_count"`
	PassCount    int `json:"pass_count"`
	FailCount    int `json:"fail_count"`

	Results []FixtureResult `json:"results"`
}

type FixtureResult struct {
	Name          string      `json:"name"`
	Kind          FixtureKind `json:"kind"`
	CorrelationID string      `json:"correlation_id,omitempty"`
	Diff          DiffResult  `json:"diff"`
}

func (p *ProofPack) Write(dir string) error {
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return err
	}
	out := filepath.Join(dir, "summary.json")
	b, err := json.MarshalIndent(p, "", "  ")
	if err != nil {
		return err
	}
	if err := os.WriteFile(out, b, 0o644); err != nil {
		return err
	}
	return nil
}

func NewProofPack() *ProofPack {
	return &ProofPack{StartedAt: time.Now()}
}

func (p *ProofPack) Finish() {
	p.EndedAt = time.Now()
	p.FixtureCount = len(p.Results)
	p.PassCount = 0
	for _, r := range p.Results {
		if r.Diff.Pass {
			p.PassCount++
		}
	}
	p.FailCount = p.FixtureCount - p.PassCount
	p.Pass = p.FailCount == 0
}

func (p *ProofPack) AddResult(f Fixture, diff DiffResult) {
	p.Results = append(p.Results, FixtureResult{
		Name:          f.Name,
		Kind:          f.Kind,
		CorrelationID: f.CorrelationID,
		Diff:          diff,
	})
}

func (p *ProofPack) String() string {
	p.Finish()
	return fmt.Sprintf("proof: %d fixtures, %d pass, %d fail", p.FixtureCount, p.PassCount, p.FailCount)
}
