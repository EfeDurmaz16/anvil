package snapshot

import (
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/efebarandurmaz/anvil/internal/plugins"
)

func makeTestSnapshot(id string, score float64, files []FileEntry) *Snapshot {
	return &Snapshot{
		ID:          id,
		CreatedAt:   time.Now(),
		SourceLang:  "cobol",
		TargetLang:  "java",
		InputPath:   "/test/input",
		ContentHash: "testhash-" + id,
		Score:       score,
		Status:      "success",
		Provenance: &Provenance{
			Provider:      "anthropic",
			Model:         "claude-3-opus",
			TotalLLMCalls: 5,
			TotalTokens:   1000,
		},
		AgentStages: []AgentStageInfo{
			{Name: "cartographer", Status: "success", Score: 0.9, LLMCalls: 2, PromptTokens: 200, CompletionTokens: 300},
			{Name: "architect", Status: "success", Score: 0.85, LLMCalls: 3, PromptTokens: 400, CompletionTokens: 100},
		},
		FileManifest: files,
		Metadata:     map[string]string{"test": "true"},
	}
}

func makeTestFiles() []plugins.GeneratedFile {
	return []plugins.GeneratedFile{
		{Path: "Main.java", Content: []byte("public class Main {\n  public static void main(String[] args) {\n    System.out.println(\"Hello\");\n  }\n}\n")},
		{Path: "Utils.java", Content: []byte("public class Utils {\n  public static int add(int a, int b) {\n    return a + b;\n  }\n}\n")},
	}
}

func TestContentHash(t *testing.T) {
	content := []byte("hello world")
	h1 := ContentHash(content)
	h2 := ContentHash(content)
	if h1 != h2 {
		t.Fatalf("ContentHash not deterministic: %s != %s", h1, h2)
	}
	if len(h1) != 64 { // SHA-256 hex is 64 chars
		t.Fatalf("unexpected hash length: %d", len(h1))
	}
	// Different content should produce different hash
	h3 := ContentHash([]byte("different"))
	if h1 == h3 {
		t.Fatal("different content produced same hash")
	}
}

func TestNewStore(t *testing.T) {
	dir := t.TempDir()
	store, err := NewStore(filepath.Join(dir, "store"))
	if err != nil {
		t.Fatalf("NewStore failed: %v", err)
	}
	if store == nil {
		t.Fatal("store is nil")
	}
	// Verify directories exist
	if _, err := os.Stat(filepath.Join(dir, "store", "snapshots")); err != nil {
		t.Fatalf("snapshots dir missing: %v", err)
	}
	if _, err := os.Stat(filepath.Join(dir, "store", "objects")); err != nil {
		t.Fatalf("objects dir missing: %v", err)
	}
}

func TestStoreSaveAndLoad(t *testing.T) {
	dir := t.TempDir()
	store, err := NewStore(filepath.Join(dir, "store"))
	if err != nil {
		t.Fatalf("NewStore: %v", err)
	}

	files := makeTestFiles()
	fileEntries := make([]FileEntry, len(files))
	for i, f := range files {
		fileEntries[i] = FileEntry{
			Path:        f.Path,
			ContentHash: ContentHash(f.Content),
			Size:        len(f.Content),
		}
	}

	snap := makeTestSnapshot("snap1", 0.85, fileEntries)

	if err := store.Save(snap, files); err != nil {
		t.Fatalf("Save: %v", err)
	}

	loaded, err := store.Load("snap1")
	if err != nil {
		t.Fatalf("Load: %v", err)
	}

	if loaded.ID != snap.ID {
		t.Fatalf("ID mismatch: got %s, want %s", loaded.ID, snap.ID)
	}
	if loaded.Score != snap.Score {
		t.Fatalf("Score mismatch: got %f, want %f", loaded.Score, snap.Score)
	}
	if loaded.SourceLang != snap.SourceLang {
		t.Fatalf("SourceLang mismatch: got %s, want %s", loaded.SourceLang, snap.SourceLang)
	}
	if len(loaded.FileManifest) != len(snap.FileManifest) {
		t.Fatalf("FileManifest length mismatch: got %d, want %d", len(loaded.FileManifest), len(snap.FileManifest))
	}
}

func TestStoreLoadFiles(t *testing.T) {
	dir := t.TempDir()
	store, err := NewStore(filepath.Join(dir, "store"))
	if err != nil {
		t.Fatalf("NewStore: %v", err)
	}

	files := makeTestFiles()
	fileEntries := make([]FileEntry, len(files))
	for i, f := range files {
		fileEntries[i] = FileEntry{
			Path:        f.Path,
			ContentHash: ContentHash(f.Content),
			Size:        len(f.Content),
		}
	}

	snap := makeTestSnapshot("snap-files", 0.9, fileEntries)
	if err := store.Save(snap, files); err != nil {
		t.Fatalf("Save: %v", err)
	}

	loadedFiles, err := store.LoadFiles(snap)
	if err != nil {
		t.Fatalf("LoadFiles: %v", err)
	}

	if len(loadedFiles) != len(files) {
		t.Fatalf("file count mismatch: got %d, want %d", len(loadedFiles), len(files))
	}

	for i, f := range loadedFiles {
		if f.Path != files[i].Path {
			t.Errorf("file %d path mismatch: got %s, want %s", i, f.Path, files[i].Path)
		}
		if string(f.Content) != string(files[i].Content) {
			t.Errorf("file %d content mismatch", i)
		}
	}
}

func TestStoreList(t *testing.T) {
	dir := t.TempDir()
	store, err := NewStore(filepath.Join(dir, "store"))
	if err != nil {
		t.Fatalf("NewStore: %v", err)
	}

	snap1 := makeTestSnapshot("snap-a", 0.7, nil)
	snap1.CreatedAt = time.Now().Add(-time.Hour)
	snap2 := makeTestSnapshot("snap-b", 0.9, nil)
	snap2.CreatedAt = time.Now()

	store.Save(snap1, nil)
	store.Save(snap2, nil)

	list := store.List()
	if len(list) != 2 {
		t.Fatalf("expected 2 snapshots, got %d", len(list))
	}
	// Should be newest first
	if list[0].ID != "snap-b" {
		t.Fatalf("expected newest first, got %s", list[0].ID)
	}
}

func TestStoreTag(t *testing.T) {
	dir := t.TempDir()
	store, err := NewStore(filepath.Join(dir, "store"))
	if err != nil {
		t.Fatalf("NewStore: %v", err)
	}

	snap := makeTestSnapshot("snap-tag", 0.8, nil)
	store.Save(snap, nil)

	if err := store.Tag("snap-tag", "v1.0"); err != nil {
		t.Fatalf("Tag: %v", err)
	}

	loaded, err := store.Load("snap-tag")
	if err != nil {
		t.Fatalf("Load: %v", err)
	}
	if loaded.Tag != "v1.0" {
		t.Fatalf("tag mismatch: got %s, want v1.0", loaded.Tag)
	}
}

func TestStoreDelete(t *testing.T) {
	dir := t.TempDir()
	store, err := NewStore(filepath.Join(dir, "store"))
	if err != nil {
		t.Fatalf("NewStore: %v", err)
	}

	snap := makeTestSnapshot("snap-del", 0.5, nil)
	store.Save(snap, nil)

	if err := store.Delete("snap-del"); err != nil {
		t.Fatalf("Delete: %v", err)
	}

	if _, err := store.Load("snap-del"); err == nil {
		t.Fatal("expected error loading deleted snapshot")
	}

	list := store.List()
	if len(list) != 0 {
		t.Fatalf("expected empty list after delete, got %d", len(list))
	}
}

func TestStoreRestore(t *testing.T) {
	dir := t.TempDir()
	store, err := NewStore(filepath.Join(dir, "store"))
	if err != nil {
		t.Fatalf("NewStore: %v", err)
	}

	files := makeTestFiles()
	fileEntries := make([]FileEntry, len(files))
	for i, f := range files {
		fileEntries[i] = FileEntry{
			Path:        f.Path,
			ContentHash: ContentHash(f.Content),
			Size:        len(f.Content),
		}
	}

	snap := makeTestSnapshot("snap-restore", 0.95, fileEntries)
	store.Save(snap, files)

	targetDir := filepath.Join(dir, "restored")
	if err := store.Restore(snap, targetDir); err != nil {
		t.Fatalf("Restore: %v", err)
	}

	// Verify files exist
	for _, f := range files {
		content, err := os.ReadFile(filepath.Join(targetDir, f.Path))
		if err != nil {
			t.Fatalf("read restored file %s: %v", f.Path, err)
		}
		if string(content) != string(f.Content) {
			t.Errorf("restored file %s content mismatch", f.Path)
		}
	}
}

func TestDiffIdentical(t *testing.T) {
	entries := []FileEntry{{Path: "a.java", ContentHash: "abc123", Size: 100}}
	snap1 := makeTestSnapshot("id1", 0.8, entries)
	snap2 := makeTestSnapshot("id2", 0.8, entries)

	d, err := Diff(snap1, snap2, nil)
	if err != nil {
		t.Fatalf("Diff: %v", err)
	}

	if len(d.FileDiffs) != 0 {
		t.Fatalf("expected no file diffs for identical snapshots, got %d", len(d.FileDiffs))
	}
	if d.ScoreDelta != 0 {
		t.Fatalf("expected 0 score delta, got %f", d.ScoreDelta)
	}
}

func TestDiffFileAdded(t *testing.T) {
	snap1 := makeTestSnapshot("old", 0.7, []FileEntry{
		{Path: "a.java", ContentHash: "aaa", Size: 50},
	})
	snap2 := makeTestSnapshot("new", 0.9, []FileEntry{
		{Path: "a.java", ContentHash: "aaa", Size: 50},
		{Path: "b.java", ContentHash: "bbb", Size: 100},
	})

	d, err := Diff(snap1, snap2, nil)
	if err != nil {
		t.Fatalf("Diff: %v", err)
	}

	if d.Summary.FilesAdded != 1 {
		t.Fatalf("expected 1 file added, got %d", d.Summary.FilesAdded)
	}
	found := false
	for _, fd := range d.FileDiffs {
		if fd.Path == "b.java" && fd.Type == DiffAdded {
			found = true
		}
	}
	if !found {
		t.Fatal("expected b.java in added files")
	}
}

func TestDiffFileRemoved(t *testing.T) {
	snap1 := makeTestSnapshot("old", 0.7, []FileEntry{
		{Path: "a.java", ContentHash: "aaa", Size: 50},
		{Path: "b.java", ContentHash: "bbb", Size: 100},
	})
	snap2 := makeTestSnapshot("new", 0.8, []FileEntry{
		{Path: "a.java", ContentHash: "aaa", Size: 50},
	})

	d, err := Diff(snap1, snap2, nil)
	if err != nil {
		t.Fatalf("Diff: %v", err)
	}

	if d.Summary.FilesRemoved != 1 {
		t.Fatalf("expected 1 file removed, got %d", d.Summary.FilesRemoved)
	}
}

func TestDiffFileModified(t *testing.T) {
	snap1 := makeTestSnapshot("old", 0.7, []FileEntry{
		{Path: "a.java", ContentHash: "aaa", Size: 50},
	})
	snap2 := makeTestSnapshot("new", 0.9, []FileEntry{
		{Path: "a.java", ContentHash: "bbb", Size: 75},
	})

	d, err := Diff(snap1, snap2, nil)
	if err != nil {
		t.Fatalf("Diff: %v", err)
	}

	if d.Summary.FilesModified != 1 {
		t.Fatalf("expected 1 file modified, got %d", d.Summary.FilesModified)
	}
	if d.FileDiffs[0].SizeDelta != 25 {
		t.Fatalf("expected size delta 25, got %d", d.FileDiffs[0].SizeDelta)
	}
}

func TestDiffWithLineDiffs(t *testing.T) {
	dir := t.TempDir()
	store, err := NewStore(filepath.Join(dir, "store"))
	if err != nil {
		t.Fatalf("NewStore: %v", err)
	}

	oldContent := []byte("line1\nline2\nline3\n")
	newContent := []byte("line1\nmodified\nline3\nnewline\n")

	oldFiles := []plugins.GeneratedFile{{Path: "test.java", Content: oldContent}}
	newFiles := []plugins.GeneratedFile{{Path: "test.java", Content: newContent}}

	snap1 := makeTestSnapshot("v1", 0.7, []FileEntry{
		{Path: "test.java", ContentHash: ContentHash(oldContent), Size: len(oldContent)},
	})
	snap2 := makeTestSnapshot("v2", 0.9, []FileEntry{
		{Path: "test.java", ContentHash: ContentHash(newContent), Size: len(newContent)},
	})

	store.Save(snap1, oldFiles)
	store.Save(snap2, newFiles)

	d, err := Diff(snap1, snap2, store)
	if err != nil {
		t.Fatalf("Diff: %v", err)
	}

	if len(d.FileDiffs) != 1 {
		t.Fatalf("expected 1 file diff, got %d", len(d.FileDiffs))
	}
	if d.FileDiffs[0].LinesAdded == 0 {
		t.Fatal("expected lines added > 0")
	}
}

func TestDiffStages(t *testing.T) {
	snap1 := makeTestSnapshot("old", 0.7, nil)
	snap2 := makeTestSnapshot("new", 0.9, nil)
	// Give snap2 different stage scores
	snap2.AgentStages[0].Score = 0.95
	snap2.AgentStages[0].LLMCalls = 4
	snap2.AgentStages[1].Status = "partial"

	d, err := Diff(snap1, snap2, nil)
	if err != nil {
		t.Fatalf("Diff: %v", err)
	}

	if len(d.StageDiffs) != 2 {
		t.Fatalf("expected 2 stage diffs, got %d", len(d.StageDiffs))
	}

	// Check cartographer delta
	for _, sd := range d.StageDiffs {
		if sd.Name == "cartographer" {
			expected := 0.05
			if sd.ScoreDelta < expected-0.001 || sd.ScoreDelta > expected+0.001 {
				t.Errorf("cartographer score delta: got %f, want ~0.05", sd.ScoreDelta)
			}
			if sd.LLMCallsDelta != 2 { // 4 - 2
				t.Errorf("cartographer LLM calls delta: got %d, want 2", sd.LLMCallsDelta)
			}
		}
		if sd.Name == "architect" && !sd.StatusChanged {
			t.Error("expected architect status changed")
		}
	}
}

func TestFormatDiff(t *testing.T) {
	d := &SnapshotDiff{
		OldID:      "abc123",
		NewID:      "def456",
		ScoreDelta: 0.15,
		FileDiffs: []FileDiff{
			{Path: "Main.java", Type: DiffModified, LinesAdded: 10, LinesRemoved: 3, HunkCount: 2},
			{Path: "New.java", Type: DiffAdded},
		},
		StageDiffs: []StageDiff{
			{Name: "architect", ScoreDelta: 0.1, LLMCallsDelta: 1, TokensDelta: 200},
		},
		Summary: DiffSummary{FilesAdded: 1, FilesModified: 1, TotalAdded: 10, TotalRemoved: 3, ScoreImproved: true},
	}

	output := FormatDiff(d)
	if output == "" {
		t.Fatal("FormatDiff returned empty string")
	}
	// Basic checks
	if !containsStr(output, "abc123") || !containsStr(output, "def456") {
		t.Error("expected snapshot IDs in output")
	}
	if !containsStr(output, "Main.java") {
		t.Error("expected Main.java in output")
	}
	if !containsStr(output, "architect") {
		t.Error("expected architect stage in output")
	}
}

func TestContentDeduplication(t *testing.T) {
	dir := t.TempDir()
	store, err := NewStore(filepath.Join(dir, "store"))
	if err != nil {
		t.Fatalf("NewStore: %v", err)
	}

	content := []byte("shared content across snapshots")
	hash := ContentHash(content)
	files := []plugins.GeneratedFile{{Path: "shared.java", Content: content}}
	entries := []FileEntry{{Path: "shared.java", ContentHash: hash, Size: len(content)}}

	snap1 := makeTestSnapshot("s1", 0.8, entries)
	snap2 := makeTestSnapshot("s2", 0.9, entries)

	store.Save(snap1, files)
	store.Save(snap2, files)

	// The object should only be stored once
	objPath := filepath.Join(dir, "store", "objects", hash[:2], hash[2:])
	info, err := os.Stat(objPath)
	if err != nil {
		t.Fatalf("object not found: %v", err)
	}
	if info.Size() != int64(len(content)) {
		t.Fatalf("object size mismatch: got %d, want %d", info.Size(), len(content))
	}
}

func containsStr(s, substr string) bool {
	return len(s) >= len(substr) && (s == substr || len(s) > 0 && containsSubstring(s, substr))
}

func containsSubstring(s, substr string) bool {
	for i := 0; i <= len(s)-len(substr); i++ {
		if s[i:i+len(substr)] == substr {
			return true
		}
	}
	return false
}
