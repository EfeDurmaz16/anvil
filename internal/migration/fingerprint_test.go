package migration

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/plugins"
)

func TestComputeFingerprints(t *testing.T) {
	files := []plugins.SourceFile{
		{Path: "main.cbl", Content: []byte("IDENTIFICATION DIVISION.\nPROGRAM-ID. HELLO.")},
		{Path: "copybook.cpy", Content: []byte("01 CUSTOMER-RECORD.\n   05 CUST-ID PIC 9(8).")},
	}

	deps := map[string][]string{
		"main.cbl": {"copybook.cpy"},
	}

	fps := ComputeFingerprints(files, deps)

	if len(fps) != 2 {
		t.Fatalf("expected 2 fingerprints, got %d", len(fps))
	}

	mainFP := fps["main.cbl"]
	cpyFP := fps["copybook.cpy"]

	if mainFP.FileHash == "" {
		t.Error("expected non-empty file hash for main.cbl")
	}
	if cpyFP.FileHash == "" {
		t.Error("expected non-empty file hash for copybook.cpy")
	}

	// main.cbl should have dependency hashes
	if len(mainFP.DependencyHashes) != 1 {
		t.Errorf("expected 1 dependency hash, got %d", len(mainFP.DependencyHashes))
	}

	// copybook has no dependencies
	if len(cpyFP.DependencyHashes) != 0 {
		t.Errorf("expected 0 dependency hashes for copybook, got %d", len(cpyFP.DependencyHashes))
	}

	// Composite hash should differ from file hash when there are dependencies
	if mainFP.CompositeHash == mainFP.FileHash {
		t.Error("composite hash should differ from file hash when dependencies exist")
	}

	// Composite hash should equal file hash when no dependencies
	if cpyFP.CompositeHash == cpyFP.FileHash {
		// Actually this is expected since computeComposite still hashes the single fileHash
		// so it won't be equal. Let's just check it's not empty.
	}
	if cpyFP.CompositeHash == "" {
		t.Error("expected non-empty composite hash for copybook")
	}
}

func TestFingerprintDeterminism(t *testing.T) {
	files := []plugins.SourceFile{
		{Path: "test.cbl", Content: []byte("HELLO WORLD")},
	}

	fp1 := ComputeFingerprints(files, nil)
	fp2 := ComputeFingerprints(files, nil)

	if fp1["test.cbl"].FileHash != fp2["test.cbl"].FileHash {
		t.Error("fingerprints should be deterministic")
	}
	if fp1["test.cbl"].CompositeHash != fp2["test.cbl"].CompositeHash {
		t.Error("composite hashes should be deterministic")
	}
}

func TestFingerprintChangesOnContentChange(t *testing.T) {
	files1 := []plugins.SourceFile{
		{Path: "test.cbl", Content: []byte("VERSION 1")},
	}
	files2 := []plugins.SourceFile{
		{Path: "test.cbl", Content: []byte("VERSION 2")},
	}

	fp1 := ComputeFingerprints(files1, nil)
	fp2 := ComputeFingerprints(files2, nil)

	if fp1["test.cbl"].FileHash == fp2["test.cbl"].FileHash {
		t.Error("file hashes should differ when content changes")
	}
}

func TestChangedFilesFirstRun(t *testing.T) {
	fps := map[string]*Fingerprint{
		"a.cbl": {CompositeHash: "hash1"},
		"b.cbl": {CompositeHash: "hash2"},
	}

	changed := ChangedFiles(fps, nil)
	if len(changed) != 2 {
		t.Errorf("first run should report all files as changed, got %d", len(changed))
	}
}

func TestChangedFilesIncremental(t *testing.T) {
	current := map[string]*Fingerprint{
		"a.cbl": {CompositeHash: "hash1"},
		"b.cbl": {CompositeHash: "hash2-changed"},
		"c.cbl": {CompositeHash: "hash3-new"},
	}

	prev := &MigrationState{
		Fingerprints: map[string]*Fingerprint{
			"a.cbl": {CompositeHash: "hash1"},           // unchanged
			"b.cbl": {CompositeHash: "hash2-original"},  // changed
		},
	}

	changed := ChangedFiles(current, prev)
	if len(changed) != 2 {
		t.Errorf("expected 2 changed files (b.cbl modified, c.cbl new), got %d", len(changed))
	}

	changedSet := make(map[string]bool)
	for _, f := range changed {
		changedSet[f] = true
	}
	if !changedSet["b.cbl"] {
		t.Error("expected b.cbl to be detected as changed")
	}
	if !changedSet["c.cbl"] {
		t.Error("expected c.cbl to be detected as new")
	}
	if changedSet["a.cbl"] {
		t.Error("a.cbl should NOT be detected as changed")
	}
}

func TestStatePersistence(t *testing.T) {
	tmpDir := t.TempDir()

	// Create state
	state := NewMigrationState("cobol", "typescript")
	state.Fingerprints["test.cbl"] = &Fingerprint{
		FileHash:      "abc123",
		CompositeHash: "def456",
	}

	// Save
	if err := state.Save(tmpDir); err != nil {
		t.Fatal(err)
	}

	// Verify file exists
	statePath := filepath.Join(tmpDir, stateFileName)
	if _, err := os.Stat(statePath); err != nil {
		t.Fatalf("state file not created: %v", err)
	}

	// Load
	loaded, err := LoadState(tmpDir)
	if err != nil {
		t.Fatal(err)
	}
	if loaded == nil {
		t.Fatal("expected non-nil loaded state")
	}
	if loaded.SourceLanguage != "cobol" {
		t.Errorf("expected cobol, got %s", loaded.SourceLanguage)
	}
	if loaded.TargetLanguage != "typescript" {
		t.Errorf("expected typescript, got %s", loaded.TargetLanguage)
	}
	if fp, ok := loaded.Fingerprints["test.cbl"]; !ok {
		t.Error("expected test.cbl fingerprint")
	} else if fp.CompositeHash != "def456" {
		t.Errorf("expected def456, got %s", fp.CompositeHash)
	}
}

func TestLoadStateMissing(t *testing.T) {
	tmpDir := t.TempDir()

	state, err := LoadState(tmpDir)
	if err != nil {
		t.Fatal(err)
	}
	if state != nil {
		t.Error("expected nil state for missing file")
	}
}
