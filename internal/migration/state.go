package migration

import (
	"encoding/json"
	"os"
	"path/filepath"
	"time"
)

// MigrationState stores the state of a migration run for incremental processing.
type MigrationState struct {
	// Version for schema compatibility
	Version string `json:"version"`
	// Timestamp of last successful run
	LastRun time.Time `json:"last_run"`
	// SourceLanguage used in the migration
	SourceLanguage string `json:"source_language"`
	// TargetLanguage used in the migration
	TargetLanguage string `json:"target_language"`
	// Fingerprints maps file path → fingerprint from last run
	Fingerprints map[string]*Fingerprint `json:"fingerprints"`
}

const stateVersion = "1.0.0"
const stateFileName = ".anvil-state.json"

// NewMigrationState creates a new empty state.
func NewMigrationState(sourceLang, targetLang string) *MigrationState {
	return &MigrationState{
		Version:        stateVersion,
		LastRun:        time.Now(),
		SourceLanguage: sourceLang,
		TargetLanguage: targetLang,
		Fingerprints:   make(map[string]*Fingerprint),
	}
}

// LoadState loads migration state from the output directory.
// Returns nil (no error) if no state file exists (first run).
func LoadState(outputDir string) (*MigrationState, error) {
	path := filepath.Join(outputDir, stateFileName)
	data, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return nil, nil // First run, no previous state
		}
		return nil, err
	}

	var state MigrationState
	if err := json.Unmarshal(data, &state); err != nil {
		return nil, err
	}

	return &state, nil
}

// Save persists the migration state to the output directory.
func (s *MigrationState) Save(outputDir string) error {
	s.LastRun = time.Now()

	data, err := json.MarshalIndent(s, "", "  ")
	if err != nil {
		return err
	}

	path := filepath.Join(outputDir, stateFileName)
	return os.WriteFile(path, data, 0o644)
}

// ChangedFiles compares current fingerprints against stored state
// and returns the paths of files that have changed.
// If previousState is nil (first run), all files are considered changed.
func ChangedFiles(current map[string]*Fingerprint, previousState *MigrationState) []string {
	if previousState == nil {
		// First run: everything is new
		changed := make([]string, 0, len(current))
		for path := range current {
			changed = append(changed, path)
		}
		return changed
	}

	var changed []string
	for path, fp := range current {
		prev, exists := previousState.Fingerprints[path]
		if !exists || prev.CompositeHash != fp.CompositeHash {
			changed = append(changed, path)
		}
	}

	return changed
}
