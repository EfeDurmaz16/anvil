package snapshot

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"sync"
	"time"

	"github.com/efebarandurmaz/anvil/internal/plugins"
)

const (
	snapshotsDir = "snapshots"
	objectsDir   = "objects"
	indexFile     = "index.json"
)

// Store provides content-addressable storage for migration snapshots.
type Store struct {
	mu      sync.RWMutex
	rootDir string
	index   *SnapshotIndex
}

// NewStore creates or opens a snapshot store at the given directory.
func NewStore(rootDir string) (*Store, error) {
	s := &Store{rootDir: rootDir}

	// Create directory structure
	dirs := []string{
		filepath.Join(rootDir, snapshotsDir),
		filepath.Join(rootDir, objectsDir),
	}
	for _, dir := range dirs {
		if err := os.MkdirAll(dir, 0o755); err != nil {
			return nil, fmt.Errorf("create store directory %s: %w", dir, err)
		}
	}

	// Load or create index
	if err := s.loadIndex(); err != nil {
		s.index = &SnapshotIndex{
			Snapshots: []SnapshotSummary{},
			UpdatedAt: time.Now(),
		}
	}

	return s, nil
}

// Save persists a snapshot and its associated generated files.
func (s *Store) Save(snap *Snapshot, files []plugins.GeneratedFile) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	// Store each file as a content-addressed object
	for _, f := range files {
		hash := ContentHash(f.Content)
		if err := s.writeObject(hash, f.Content); err != nil {
			return fmt.Errorf("store object %s: %w", f.Path, err)
		}
	}

	// Store the snapshot metadata
	snapDir := filepath.Join(s.rootDir, snapshotsDir, snap.ID)
	if err := os.MkdirAll(snapDir, 0o755); err != nil {
		return fmt.Errorf("create snapshot dir: %w", err)
	}

	snapData, err := json.MarshalIndent(snap, "", "  ")
	if err != nil {
		return fmt.Errorf("marshal snapshot: %w", err)
	}

	if err := os.WriteFile(filepath.Join(snapDir, "snapshot.json"), snapData, 0o644); err != nil {
		return fmt.Errorf("write snapshot: %w", err)
	}

	// Update index
	s.index.Snapshots = append(s.index.Snapshots, snap.Summary())
	s.index.UpdatedAt = time.Now()
	return s.saveIndex()
}

// Load retrieves a snapshot by ID.
func (s *Store) Load(id string) (*Snapshot, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	snapPath := filepath.Join(s.rootDir, snapshotsDir, id, "snapshot.json")
	data, err := os.ReadFile(snapPath)
	if err != nil {
		return nil, fmt.Errorf("read snapshot %s: %w", id, err)
	}

	var snap Snapshot
	if err := json.Unmarshal(data, &snap); err != nil {
		return nil, fmt.Errorf("unmarshal snapshot %s: %w", id, err)
	}

	return &snap, nil
}

// LoadFiles retrieves the generated files from a snapshot using content-addressed objects.
func (s *Store) LoadFiles(snap *Snapshot) ([]plugins.GeneratedFile, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	var files []plugins.GeneratedFile
	for _, entry := range snap.FileManifest {
		content, err := s.readObject(entry.ContentHash)
		if err != nil {
			return nil, fmt.Errorf("read object for %s: %w", entry.Path, err)
		}
		files = append(files, plugins.GeneratedFile{
			Path:    entry.Path,
			Content: content,
		})
	}

	return files, nil
}

// List returns all snapshot summaries, newest first.
func (s *Store) List() []SnapshotSummary {
	s.mu.RLock()
	defer s.mu.RUnlock()

	result := make([]SnapshotSummary, len(s.index.Snapshots))
	copy(result, s.index.Snapshots)

	sort.Slice(result, func(i, j int) bool {
		return result[i].CreatedAt.After(result[j].CreatedAt)
	})

	return result
}

// FindByTag returns the snapshot with the given tag.
func (s *Store) FindByTag(tag string) (*Snapshot, error) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	for _, summary := range s.index.Snapshots {
		if summary.Tag == tag {
			s.mu.RUnlock()
			snap, err := s.Load(summary.ID)
			s.mu.RLock()
			return snap, err
		}
	}
	return nil, fmt.Errorf("snapshot with tag %q not found", tag)
}

// Tag assigns a tag to a snapshot.
func (s *Store) Tag(id, tag string) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	// Load, update, save the snapshot
	snapPath := filepath.Join(s.rootDir, snapshotsDir, id, "snapshot.json")
	data, err := os.ReadFile(snapPath)
	if err != nil {
		return fmt.Errorf("read snapshot %s: %w", id, err)
	}

	var snap Snapshot
	if err := json.Unmarshal(data, &snap); err != nil {
		return fmt.Errorf("unmarshal snapshot: %w", err)
	}

	snap.Tag = tag
	snapData, err := json.MarshalIndent(&snap, "", "  ")
	if err != nil {
		return fmt.Errorf("marshal snapshot: %w", err)
	}

	if err := os.WriteFile(snapPath, snapData, 0o644); err != nil {
		return fmt.Errorf("write snapshot: %w", err)
	}

	// Update index
	for i, summary := range s.index.Snapshots {
		if summary.ID == id {
			s.index.Snapshots[i].Tag = tag
			break
		}
	}
	s.index.UpdatedAt = time.Now()
	return s.saveIndex()
}

// Delete removes a snapshot and its unique objects.
func (s *Store) Delete(id string) error {
	s.mu.Lock()
	defer s.mu.Unlock()

	snapDir := filepath.Join(s.rootDir, snapshotsDir, id)
	if err := os.RemoveAll(snapDir); err != nil {
		return fmt.Errorf("remove snapshot dir: %w", err)
	}

	// Remove from index
	filtered := s.index.Snapshots[:0]
	for _, summary := range s.index.Snapshots {
		if summary.ID != id {
			filtered = append(filtered, summary)
		}
	}
	s.index.Snapshots = filtered
	s.index.UpdatedAt = time.Now()

	return s.saveIndex()
}

// Restore writes a snapshot's files back to the target directory.
func (s *Store) Restore(snap *Snapshot, targetDir string) error {
	files, err := s.LoadFiles(snap)
	if err != nil {
		return fmt.Errorf("load files: %w", err)
	}

	for _, f := range files {
		outPath := filepath.Join(targetDir, f.Path)
		if err := os.MkdirAll(filepath.Dir(outPath), 0o755); err != nil {
			return fmt.Errorf("create dir for %s: %w", f.Path, err)
		}
		if err := os.WriteFile(outPath, f.Content, 0o644); err != nil {
			return fmt.Errorf("write %s: %w", f.Path, err)
		}
	}

	return nil
}

// writeObject stores content by its hash.
func (s *Store) writeObject(hash string, content []byte) error {
	prefix := hash[:2]
	dir := filepath.Join(s.rootDir, objectsDir, prefix)
	if err := os.MkdirAll(dir, 0o755); err != nil {
		return err
	}

	objPath := filepath.Join(dir, hash[2:])
	if _, err := os.Stat(objPath); err == nil {
		return nil // Already exists (content-addressable dedup)
	}

	return os.WriteFile(objPath, content, 0o644)
}

// readObject retrieves content by its hash.
func (s *Store) readObject(hash string) ([]byte, error) {
	prefix := hash[:2]
	objPath := filepath.Join(s.rootDir, objectsDir, prefix, hash[2:])
	return os.ReadFile(objPath)
}

func (s *Store) loadIndex() error {
	data, err := os.ReadFile(filepath.Join(s.rootDir, indexFile))
	if err != nil {
		return err
	}
	s.index = &SnapshotIndex{}
	return json.Unmarshal(data, s.index)
}

func (s *Store) saveIndex() error {
	data, err := json.MarshalIndent(s.index, "", "  ")
	if err != nil {
		return err
	}
	return os.WriteFile(filepath.Join(s.rootDir, indexFile), data, 0o644)
}
