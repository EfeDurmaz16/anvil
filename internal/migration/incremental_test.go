package migration

import (
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// Helper to create test source files
func makeSourceFile(path, content string) plugins.SourceFile {
	return plugins.SourceFile{
		Path:    path,
		Content: []byte(content),
	}
}

// TestIncrementalRunner_Analyze_FirstRun tests the initial run with no previous state.
func TestIncrementalRunner_Analyze_FirstRun(t *testing.T) {
	tmpDir := t.TempDir()

	cfg := &IncrementalConfig{
		OutputDir:      tmpDir,
		SourceLanguage: "cobol",
		TargetLanguage: "typescript",
	}

	runner := NewIncrementalRunner(cfg)

	files := []plugins.SourceFile{
		makeSourceFile("file1.cbl", "PROGRAM-ID. FILE1."),
		makeSourceFile("file2.cbl", "PROGRAM-ID. FILE2."),
		makeSourceFile("file3.cbl", "PROGRAM-ID. FILE3."),
	}

	result, err := runner.Analyze(files)
	if err != nil {
		t.Fatalf("Analyze failed: %v", err)
	}

	if !result.IsFirstRun {
		t.Error("Expected IsFirstRun to be true")
	}

	if result.TotalFiles != 3 {
		t.Errorf("Expected TotalFiles=3, got %d", result.TotalFiles)
	}

	if len(result.NewFiles) != 3 {
		t.Errorf("Expected 3 new files, got %d", len(result.NewFiles))
	}

	if len(result.ChangedFiles) != 0 {
		t.Errorf("Expected 0 changed files, got %d", len(result.ChangedFiles))
	}

	if len(result.UnchangedFiles) != 0 {
		t.Errorf("Expected 0 unchanged files, got %d", len(result.UnchangedFiles))
	}

	if len(result.DeletedFiles) != 0 {
		t.Errorf("Expected 0 deleted files, got %d", len(result.DeletedFiles))
	}

	if result.Skipped != 0 {
		t.Errorf("Expected Skipped=0, got %d", result.Skipped)
	}

	// Verify all files are marked as new
	expectedNew := map[string]bool{
		"file1.cbl": true,
		"file2.cbl": true,
		"file3.cbl": true,
	}
	for _, f := range result.NewFiles {
		if !expectedNew[f] {
			t.Errorf("Unexpected new file: %s", f)
		}
		delete(expectedNew, f)
	}
	if len(expectedNew) > 0 {
		t.Errorf("Missing new files: %v", expectedNew)
	}
}

// TestIncrementalRunner_Analyze_IncrementalRun tests subsequent runs with changes.
func TestIncrementalRunner_Analyze_IncrementalRun(t *testing.T) {
	tmpDir := t.TempDir()

	cfg := &IncrementalConfig{
		OutputDir:      tmpDir,
		SourceLanguage: "cobol",
		TargetLanguage: "typescript",
	}

	runner := NewIncrementalRunner(cfg)

	// Initial run
	initialFiles := []plugins.SourceFile{
		makeSourceFile("file1.cbl", "PROGRAM-ID. FILE1."),
		makeSourceFile("file2.cbl", "PROGRAM-ID. FILE2."),
		makeSourceFile("file3.cbl", "PROGRAM-ID. FILE3."),
	}

	_, err := runner.Analyze(initialFiles)
	if err != nil {
		t.Fatalf("Initial Analyze failed: %v", err)
	}

	// Save state
	err = runner.SaveState(initialFiles)
	if err != nil {
		t.Fatalf("SaveState failed: %v", err)
	}

	// Second run with one file changed
	secondFiles := []plugins.SourceFile{
		makeSourceFile("file1.cbl", "PROGRAM-ID. FILE1-MODIFIED."), // Changed
		makeSourceFile("file2.cbl", "PROGRAM-ID. FILE2."),           // Unchanged
		makeSourceFile("file3.cbl", "PROGRAM-ID. FILE3."),           // Unchanged
	}

	result, err := runner.Analyze(secondFiles)
	if err != nil {
		t.Fatalf("Second Analyze failed: %v", err)
	}

	if result.IsFirstRun {
		t.Error("Expected IsFirstRun to be false")
	}

	if result.TotalFiles != 3 {
		t.Errorf("Expected TotalFiles=3, got %d", result.TotalFiles)
	}

	if len(result.ChangedFiles) != 1 {
		t.Errorf("Expected 1 changed file, got %d", len(result.ChangedFiles))
	} else if result.ChangedFiles[0] != "file1.cbl" {
		t.Errorf("Expected file1.cbl to be changed, got %s", result.ChangedFiles[0])
	}

	if len(result.UnchangedFiles) != 2 {
		t.Errorf("Expected 2 unchanged files, got %d", len(result.UnchangedFiles))
	}

	if result.Skipped != 2 {
		t.Errorf("Expected Skipped=2, got %d", result.Skipped)
	}

	if len(result.NewFiles) != 0 {
		t.Errorf("Expected 0 new files, got %d", len(result.NewFiles))
	}

	if len(result.DeletedFiles) != 0 {
		t.Errorf("Expected 0 deleted files, got %d", len(result.DeletedFiles))
	}
}

// TestIncrementalRunner_Analyze_ForceAll tests force-all mode.
func TestIncrementalRunner_Analyze_ForceAll(t *testing.T) {
	tmpDir := t.TempDir()

	cfg := &IncrementalConfig{
		OutputDir:      tmpDir,
		SourceLanguage: "cobol",
		TargetLanguage: "typescript",
		ForceAll:       true,
	}

	runner := NewIncrementalRunner(cfg)

	// Save state first
	initialFiles := []plugins.SourceFile{
		makeSourceFile("file1.cbl", "PROGRAM-ID. FILE1."),
		makeSourceFile("file2.cbl", "PROGRAM-ID. FILE2."),
	}

	err := runner.SaveState(initialFiles)
	if err != nil {
		t.Fatalf("SaveState failed: %v", err)
	}

	// Analyze with ForceAll - should mark everything as changed
	result, err := runner.Analyze(initialFiles)
	if err != nil {
		t.Fatalf("Analyze failed: %v", err)
	}

	if !result.ForcedFull {
		t.Error("Expected ForcedFull to be true")
	}

	if len(result.ChangedFiles) != 2 {
		t.Errorf("Expected 2 changed files, got %d", len(result.ChangedFiles))
	}

	// In force mode, files should be in ChangedFiles not NewFiles
	if len(result.NewFiles) != 0 {
		t.Errorf("Expected 0 new files in force mode, got %d", len(result.NewFiles))
	}

	if len(result.UnchangedFiles) != 0 {
		t.Errorf("Expected 0 unchanged files in force mode, got %d", len(result.UnchangedFiles))
	}

	if result.Skipped != 0 {
		t.Errorf("Expected Skipped=0 in force mode, got %d", result.Skipped)
	}
}

// TestIncrementalRunner_Analyze_EmptyFileList tests empty input.
func TestIncrementalRunner_Analyze_EmptyFileList(t *testing.T) {
	tmpDir := t.TempDir()

	cfg := &IncrementalConfig{
		OutputDir:      tmpDir,
		SourceLanguage: "cobol",
		TargetLanguage: "typescript",
	}

	runner := NewIncrementalRunner(cfg)

	result, err := runner.Analyze([]plugins.SourceFile{})
	if err != nil {
		t.Fatalf("Analyze failed: %v", err)
	}

	if result.TotalFiles != 0 {
		t.Errorf("Expected TotalFiles=0, got %d", result.TotalFiles)
	}

	if len(result.NewFiles) != 0 {
		t.Errorf("Expected 0 new files, got %d", len(result.NewFiles))
	}
}

// TestIncrementalRunner_Analyze_DeletedFiles tests detection of deleted files.
func TestIncrementalRunner_Analyze_DeletedFiles(t *testing.T) {
	tmpDir := t.TempDir()

	cfg := &IncrementalConfig{
		OutputDir:      tmpDir,
		SourceLanguage: "cobol",
		TargetLanguage: "typescript",
	}

	runner := NewIncrementalRunner(cfg)

	// Initial run with 3 files
	initialFiles := []plugins.SourceFile{
		makeSourceFile("file1.cbl", "PROGRAM-ID. FILE1."),
		makeSourceFile("file2.cbl", "PROGRAM-ID. FILE2."),
		makeSourceFile("file3.cbl", "PROGRAM-ID. FILE3."),
	}

	_, err := runner.Analyze(initialFiles)
	if err != nil {
		t.Fatalf("Initial Analyze failed: %v", err)
	}

	err = runner.SaveState(initialFiles)
	if err != nil {
		t.Fatalf("SaveState failed: %v", err)
	}

	// Second run with file2 removed
	secondFiles := []plugins.SourceFile{
		makeSourceFile("file1.cbl", "PROGRAM-ID. FILE1."),
		makeSourceFile("file3.cbl", "PROGRAM-ID. FILE3."),
	}

	result, err := runner.Analyze(secondFiles)
	if err != nil {
		t.Fatalf("Second Analyze failed: %v", err)
	}

	if len(result.DeletedFiles) != 1 {
		t.Errorf("Expected 1 deleted file, got %d", len(result.DeletedFiles))
	} else if result.DeletedFiles[0] != "file2.cbl" {
		t.Errorf("Expected file2.cbl to be deleted, got %s", result.DeletedFiles[0])
	}

	if result.TotalFiles != 2 {
		t.Errorf("Expected TotalFiles=2, got %d", result.TotalFiles)
	}
}

// TestIncrementalRunner_FilterFiles tests filtering to only changed/new files.
func TestIncrementalRunner_FilterFiles(t *testing.T) {
	tmpDir := t.TempDir()

	cfg := &IncrementalConfig{
		OutputDir:      tmpDir,
		SourceLanguage: "cobol",
		TargetLanguage: "typescript",
	}

	runner := NewIncrementalRunner(cfg)

	initialFiles := []plugins.SourceFile{
		makeSourceFile("file1.cbl", "PROGRAM-ID. FILE1."),
		makeSourceFile("file2.cbl", "PROGRAM-ID. FILE2."),
		makeSourceFile("file3.cbl", "PROGRAM-ID. FILE3."),
	}

	err := runner.SaveState(initialFiles)
	if err != nil {
		t.Fatalf("SaveState failed: %v", err)
	}

	// Second run with one changed
	secondFiles := []plugins.SourceFile{
		makeSourceFile("file1.cbl", "PROGRAM-ID. FILE1-MODIFIED."),
		makeSourceFile("file2.cbl", "PROGRAM-ID. FILE2."),
		makeSourceFile("file3.cbl", "PROGRAM-ID. FILE3."),
	}

	result, err := runner.Analyze(secondFiles)
	if err != nil {
		t.Fatalf("Analyze failed: %v", err)
	}

	filtered := runner.FilterFiles(secondFiles, result)

	if len(filtered) != 1 {
		t.Errorf("Expected 1 filtered file, got %d", len(filtered))
	} else if filtered[0].Path != "file1.cbl" {
		t.Errorf("Expected file1.cbl, got %s", filtered[0].Path)
	}
}

// TestIncrementalRunner_FilesToMigrate tests getting the list of files needing migration.
func TestIncrementalRunner_FilesToMigrate(t *testing.T) {
	tmpDir := t.TempDir()

	cfg := &IncrementalConfig{
		OutputDir:      tmpDir,
		SourceLanguage: "cobol",
		TargetLanguage: "typescript",
	}

	runner := NewIncrementalRunner(cfg)

	result := &IncrementalResult{
		ChangedFiles: []string{"changed1.cbl", "changed2.cbl"},
		NewFiles:     []string{"new1.cbl"},
	}

	toMigrate := runner.FilesToMigrate(result)

	if len(toMigrate) != 3 {
		t.Errorf("Expected 3 files to migrate, got %d", len(toMigrate))
	}

	// Should be sorted
	expected := []string{"changed1.cbl", "changed2.cbl", "new1.cbl"}
	for i, exp := range expected {
		if toMigrate[i] != exp {
			t.Errorf("Expected toMigrate[%d]=%s, got %s", i, exp, toMigrate[i])
		}
	}
}

// TestIncrementalRunner_FilterFiles_EmptyResult tests filtering with empty result.
func TestIncrementalRunner_FilterFiles_EmptyResult(t *testing.T) {
	tmpDir := t.TempDir()

	cfg := &IncrementalConfig{
		OutputDir:      tmpDir,
		SourceLanguage: "cobol",
		TargetLanguage: "typescript",
	}

	runner := NewIncrementalRunner(cfg)

	files := []plugins.SourceFile{
		makeSourceFile("file1.cbl", "PROGRAM-ID. FILE1."),
	}

	result := &IncrementalResult{
		ChangedFiles: []string{},
		NewFiles:     []string{},
	}

	filtered := runner.FilterFiles(files, result)

	if len(filtered) != 0 {
		t.Errorf("Expected 0 filtered files, got %d", len(filtered))
	}
}

// TestIncrementalRunner_SaveState tests state persistence.
func TestIncrementalRunner_SaveState(t *testing.T) {
	tmpDir := t.TempDir()

	cfg := &IncrementalConfig{
		OutputDir:      tmpDir,
		SourceLanguage: "cobol",
		TargetLanguage: "typescript",
	}

	runner := NewIncrementalRunner(cfg)

	files := []plugins.SourceFile{
		makeSourceFile("file1.cbl", "PROGRAM-ID. FILE1."),
		makeSourceFile("file2.cbl", "PROGRAM-ID. FILE2."),
	}

	err := runner.SaveState(files)
	if err != nil {
		t.Fatalf("SaveState failed: %v", err)
	}

	// Verify state file exists
	statePath := filepath.Join(tmpDir, stateFileName)
	if _, err := os.Stat(statePath); os.IsNotExist(err) {
		t.Errorf("State file not created at %s", statePath)
	}

	// Load and verify
	state, err := LoadState(tmpDir)
	if err != nil {
		t.Fatalf("LoadState failed: %v", err)
	}

	if state == nil {
		t.Fatal("Loaded state is nil")
	}

	if len(state.Fingerprints) != 2 {
		t.Errorf("Expected 2 fingerprints, got %d", len(state.Fingerprints))
	}

	if _, exists := state.Fingerprints["file1.cbl"]; !exists {
		t.Error("Expected file1.cbl in fingerprints")
	}

	if _, exists := state.Fingerprints["file2.cbl"]; !exists {
		t.Error("Expected file2.cbl in fingerprints")
	}
}

// TestIncrementalRunner_SaveState_SubsequentAnalyze tests that saved state is used correctly.
func TestIncrementalRunner_SaveState_SubsequentAnalyze(t *testing.T) {
	tmpDir := t.TempDir()

	cfg := &IncrementalConfig{
		OutputDir:      tmpDir,
		SourceLanguage: "cobol",
		TargetLanguage: "typescript",
	}

	runner := NewIncrementalRunner(cfg)

	files := []plugins.SourceFile{
		makeSourceFile("file1.cbl", "PROGRAM-ID. FILE1."),
		makeSourceFile("file2.cbl", "PROGRAM-ID. FILE2."),
	}

	err := runner.SaveState(files)
	if err != nil {
		t.Fatalf("SaveState failed: %v", err)
	}

	// Analyze same files again - should all be unchanged
	result, err := runner.Analyze(files)
	if err != nil {
		t.Fatalf("Analyze failed: %v", err)
	}

	if len(result.UnchangedFiles) != 2 {
		t.Errorf("Expected 2 unchanged files, got %d", len(result.UnchangedFiles))
	}

	if len(result.ChangedFiles) != 0 {
		t.Errorf("Expected 0 changed files, got %d", len(result.ChangedFiles))
	}

	if len(result.NewFiles) != 0 {
		t.Errorf("Expected 0 new files, got %d", len(result.NewFiles))
	}
}

// TestLoadCache_EmptyDir tests loading cache from empty directory.
func TestLoadCache_EmptyDir(t *testing.T) {
	tmpDir := t.TempDir()

	cache, err := LoadCache(tmpDir)
	if err != nil {
		t.Fatalf("LoadCache failed: %v", err)
	}

	if cache == nil {
		t.Fatal("Expected non-nil cache")
	}

	if cache.Version != cacheVersion {
		t.Errorf("Expected version %s, got %s", cacheVersion, cache.Version)
	}

	if len(cache.Entries) != 0 {
		t.Errorf("Expected 0 entries, got %d", len(cache.Entries))
	}
}

// TestResultCache_PutGet tests cache put/get operations.
func TestResultCache_PutGet(t *testing.T) {
	cache := &ResultCache{
		Version:   cacheVersion,
		Entries:   make(map[string]*CacheEntry),
		CreatedAt: time.Now(),
		UpdatedAt: time.Now(),
	}

	entry := &CacheEntry{
		SourcePath:    "test.cbl",
		CompositeHash: "abc123",
		OutputPath:    "test.ts",
		GeneratedAt:   time.Now(),
		TokensUsed:    1000,
		Score:         0.95,
		Success:       true,
	}

	cache.Put(entry)

	// Get with correct hash
	retrieved, found := cache.Get("test.cbl", "abc123")
	if !found {
		t.Error("Expected to find entry")
	}
	if retrieved.SourcePath != "test.cbl" {
		t.Errorf("Expected SourcePath=test.cbl, got %s", retrieved.SourcePath)
	}
	if retrieved.TokensUsed != 1000 {
		t.Errorf("Expected TokensUsed=1000, got %d", retrieved.TokensUsed)
	}
}

// TestResultCache_GetWrongHash tests cache get with wrong hash.
func TestResultCache_GetWrongHash(t *testing.T) {
	cache := &ResultCache{
		Version:   cacheVersion,
		Entries:   make(map[string]*CacheEntry),
		CreatedAt: time.Now(),
		UpdatedAt: time.Now(),
	}

	entry := &CacheEntry{
		SourcePath:    "test.cbl",
		CompositeHash: "abc123",
		OutputPath:    "test.ts",
		GeneratedAt:   time.Now(),
	}

	cache.Put(entry)

	// Get with wrong hash
	_, found := cache.Get("test.cbl", "wronghash")
	if found {
		t.Error("Expected not to find entry with wrong hash")
	}

	// Get non-existent path
	_, found = cache.Get("nonexistent.cbl", "abc123")
	if found {
		t.Error("Expected not to find non-existent entry")
	}
}

// TestResultCache_Remove tests cache removal.
func TestResultCache_Remove(t *testing.T) {
	cache := &ResultCache{
		Version:   cacheVersion,
		Entries:   make(map[string]*CacheEntry),
		CreatedAt: time.Now(),
		UpdatedAt: time.Now(),
	}

	entry := &CacheEntry{
		SourcePath:    "test.cbl",
		CompositeHash: "abc123",
		OutputPath:    "test.ts",
	}

	cache.Put(entry)

	if len(cache.Entries) != 1 {
		t.Errorf("Expected 1 entry, got %d", len(cache.Entries))
	}

	cache.Remove("test.cbl")

	if len(cache.Entries) != 0 {
		t.Errorf("Expected 0 entries after removal, got %d", len(cache.Entries))
	}

	_, found := cache.Get("test.cbl", "abc123")
	if found {
		t.Error("Expected entry to be removed")
	}
}

// TestResultCache_Prune tests pruning of stale entries.
func TestResultCache_Prune(t *testing.T) {
	cache := &ResultCache{
		Version:   cacheVersion,
		Entries:   make(map[string]*CacheEntry),
		CreatedAt: time.Now(),
		UpdatedAt: time.Now(),
	}

	cache.Put(&CacheEntry{SourcePath: "file1.cbl", CompositeHash: "hash1"})
	cache.Put(&CacheEntry{SourcePath: "file2.cbl", CompositeHash: "hash2"})
	cache.Put(&CacheEntry{SourcePath: "file3.cbl", CompositeHash: "hash3"})

	currentFiles := map[string]bool{
		"file1.cbl": true,
		"file3.cbl": true,
		// file2.cbl is missing - should be pruned
	}

	pruned := cache.Prune(currentFiles)

	if pruned != 1 {
		t.Errorf("Expected 1 pruned entry, got %d", pruned)
	}

	if len(cache.Entries) != 2 {
		t.Errorf("Expected 2 remaining entries, got %d", len(cache.Entries))
	}

	if _, exists := cache.Entries["file2.cbl"]; exists {
		t.Error("Expected file2.cbl to be pruned")
	}

	if _, exists := cache.Entries["file1.cbl"]; !exists {
		t.Error("Expected file1.cbl to remain")
	}

	if _, exists := cache.Entries["file3.cbl"]; !exists {
		t.Error("Expected file3.cbl to remain")
	}
}

// TestResultCache_Stats tests cache statistics.
func TestResultCache_Stats(t *testing.T) {
	cache := &ResultCache{
		Version:   cacheVersion,
		Entries:   make(map[string]*CacheEntry),
		CreatedAt: time.Now(),
		UpdatedAt: time.Now(),
	}

	cache.Put(&CacheEntry{
		SourcePath: "file1.cbl",
		Success:    true,
		TokensUsed: 1000,
	})
	cache.Put(&CacheEntry{
		SourcePath: "file2.cbl",
		Success:    true,
		TokensUsed: 1500,
	})
	cache.Put(&CacheEntry{
		SourcePath: "file3.cbl",
		Success:    false,
		TokensUsed: 500,
	})

	stats := cache.Stats()

	if stats.TotalEntries != 3 {
		t.Errorf("Expected TotalEntries=3, got %d", stats.TotalEntries)
	}

	if stats.SuccessEntries != 2 {
		t.Errorf("Expected SuccessEntries=2, got %d", stats.SuccessEntries)
	}

	if stats.FailedEntries != 1 {
		t.Errorf("Expected FailedEntries=1, got %d", stats.FailedEntries)
	}

	if stats.TotalTokens != 3000 {
		t.Errorf("Expected TotalTokens=3000, got %d", stats.TotalTokens)
	}
}

// TestResultCache_SaveLoad tests cache persistence.
func TestResultCache_SaveLoad(t *testing.T) {
	tmpDir := t.TempDir()

	cache := &ResultCache{
		Version:   cacheVersion,
		Entries:   make(map[string]*CacheEntry),
		CreatedAt: time.Now(),
		UpdatedAt: time.Now(),
	}

	cache.Put(&CacheEntry{
		SourcePath:    "test.cbl",
		CompositeHash: "abc123",
		OutputPath:    "test.ts",
		TokensUsed:    1000,
		Score:         0.95,
		Success:       true,
	})

	err := cache.Save(tmpDir)
	if err != nil {
		t.Fatalf("Save failed: %v", err)
	}

	// Verify file exists
	cachePath := filepath.Join(tmpDir, cacheFileName)
	if _, err := os.Stat(cachePath); os.IsNotExist(err) {
		t.Errorf("Cache file not created at %s", cachePath)
	}

	// Load and verify
	loaded, err := LoadCache(tmpDir)
	if err != nil {
		t.Fatalf("LoadCache failed: %v", err)
	}

	if loaded.Version != cacheVersion {
		t.Errorf("Expected version %s, got %s", cacheVersion, loaded.Version)
	}

	if len(loaded.Entries) != 1 {
		t.Errorf("Expected 1 entry, got %d", len(loaded.Entries))
	}

	entry, found := loaded.Get("test.cbl", "abc123")
	if !found {
		t.Error("Expected to find entry after load")
	}

	if entry.TokensUsed != 1000 {
		t.Errorf("Expected TokensUsed=1000, got %d", entry.TokensUsed)
	}

	if entry.Score != 0.95 {
		t.Errorf("Expected Score=0.95, got %f", entry.Score)
	}
}

// TestFormatIncrementalReport_FirstRun tests report formatting for first run.
func TestFormatIncrementalReport_FirstRun(t *testing.T) {
	result := &IncrementalResult{
		TotalFiles: 3,
		NewFiles:   []string{"file1.cbl", "file2.cbl", "file3.cbl"},
		IsFirstRun: true,
		Duration:   100 * time.Millisecond,
	}

	report := FormatIncrementalReport(result)

	if report == "" {
		t.Error("Expected non-empty report")
	}

	// Check for key sections
	expectedStrings := []string{
		"Incremental Migration Report",
		"Mode: First Run",
		"Total Files:     3",
		"New Files:       3",
		"file1.cbl",
		"file2.cbl",
		"file3.cbl",
		"+ file1.cbl", // New file marker
	}

	for _, expected := range expectedStrings {
		if !containsString(report, expected) {
			t.Errorf("Expected report to contain %q", expected)
		}
	}
}

// TestFormatIncrementalReport_IncrementalRun tests report formatting for incremental run.
func TestFormatIncrementalReport_IncrementalRun(t *testing.T) {
	result := &IncrementalResult{
		TotalFiles:     5,
		ChangedFiles:   []string{"changed1.cbl", "changed2.cbl"},
		NewFiles:       []string{"new1.cbl"},
		UnchangedFiles: []string{"unchanged1.cbl", "unchanged2.cbl"},
		DeletedFiles:   []string{"deleted1.cbl"},
		Skipped:        2,
		Duration:       50 * time.Millisecond,
		IsFirstRun:     false,
		ForcedFull:     false,
	}

	report := FormatIncrementalReport(result)

	expectedStrings := []string{
		"Mode: Incremental",
		"Total Files:     5",
		"Changed Files:   2",
		"New Files:       1",
		"Unchanged:       2",
		"Deleted:         1",
		"Changed Files:",
		"~ changed1.cbl", // Changed file marker
		"New Files:",
		"+ new1.cbl", // New file marker
		"Deleted Files:",
		"- deleted1.cbl", // Deleted file marker
		"Skip Rate:",
	}

	for _, expected := range expectedStrings {
		if !containsString(report, expected) {
			t.Errorf("Expected report to contain %q", expected)
		}
	}
}

// TestEndToEnd_MultiRunScenario tests a complete multi-run scenario.
func TestEndToEnd_MultiRunScenario(t *testing.T) {
	tmpDir := t.TempDir()

	cfg := &IncrementalConfig{
		OutputDir:      tmpDir,
		SourceLanguage: "cobol",
		TargetLanguage: "typescript",
	}

	runner := NewIncrementalRunner(cfg)

	// Run 1: Initial run - all files are new
	run1Files := []plugins.SourceFile{
		makeSourceFile("file1.cbl", "PROGRAM-ID. FILE1."),
		makeSourceFile("file2.cbl", "PROGRAM-ID. FILE2."),
		makeSourceFile("file3.cbl", "PROGRAM-ID. FILE3."),
	}

	result1, err := runner.Analyze(run1Files)
	if err != nil {
		t.Fatalf("Run 1 Analyze failed: %v", err)
	}

	if !result1.IsFirstRun {
		t.Error("Run 1: Expected IsFirstRun=true")
	}

	if len(result1.NewFiles) != 3 {
		t.Errorf("Run 1: Expected 3 new files, got %d", len(result1.NewFiles))
	}

	err = runner.SaveState(run1Files)
	if err != nil {
		t.Fatalf("Run 1 SaveState failed: %v", err)
	}

	// Run 2: Modify one file
	run2Files := []plugins.SourceFile{
		makeSourceFile("file1.cbl", "PROGRAM-ID. FILE1-MODIFIED."), // Changed
		makeSourceFile("file2.cbl", "PROGRAM-ID. FILE2."),           // Unchanged
		makeSourceFile("file3.cbl", "PROGRAM-ID. FILE3."),           // Unchanged
	}

	result2, err := runner.Analyze(run2Files)
	if err != nil {
		t.Fatalf("Run 2 Analyze failed: %v", err)
	}

	if result2.IsFirstRun {
		t.Error("Run 2: Expected IsFirstRun=false")
	}

	if len(result2.ChangedFiles) != 1 || result2.ChangedFiles[0] != "file1.cbl" {
		t.Errorf("Run 2: Expected 1 changed file (file1.cbl), got %v", result2.ChangedFiles)
	}

	if len(result2.UnchangedFiles) != 2 {
		t.Errorf("Run 2: Expected 2 unchanged files, got %d", len(result2.UnchangedFiles))
	}

	if result2.Skipped != 2 {
		t.Errorf("Run 2: Expected Skipped=2, got %d", result2.Skipped)
	}

	err = runner.SaveState(run2Files)
	if err != nil {
		t.Fatalf("Run 2 SaveState failed: %v", err)
	}

	// Run 3: Add a new file
	run3Files := []plugins.SourceFile{
		makeSourceFile("file1.cbl", "PROGRAM-ID. FILE1-MODIFIED."), // Unchanged
		makeSourceFile("file2.cbl", "PROGRAM-ID. FILE2."),           // Unchanged
		makeSourceFile("file3.cbl", "PROGRAM-ID. FILE3."),           // Unchanged
		makeSourceFile("file4.cbl", "PROGRAM-ID. FILE4."),           // New
	}

	result3, err := runner.Analyze(run3Files)
	if err != nil {
		t.Fatalf("Run 3 Analyze failed: %v", err)
	}

	if len(result3.NewFiles) != 1 || result3.NewFiles[0] != "file4.cbl" {
		t.Errorf("Run 3: Expected 1 new file (file4.cbl), got %v", result3.NewFiles)
	}

	if len(result3.UnchangedFiles) != 3 {
		t.Errorf("Run 3: Expected 3 unchanged files, got %d", len(result3.UnchangedFiles))
	}

	if len(result3.ChangedFiles) != 0 {
		t.Errorf("Run 3: Expected 0 changed files, got %d", len(result3.ChangedFiles))
	}

	// Verify total files to migrate in run 3
	toMigrate := runner.FilesToMigrate(result3)
	if len(toMigrate) != 1 || toMigrate[0] != "file4.cbl" {
		t.Errorf("Run 3: Expected to migrate only file4.cbl, got %v", toMigrate)
	}
}

// Helper function to check if a string contains a substring
func containsString(s, substr string) bool {
	return len(s) > 0 && len(substr) > 0 && (s == substr || len(s) >= len(substr) && findSubstring(s, substr))
}

func findSubstring(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
