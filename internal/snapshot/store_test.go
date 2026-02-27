package snapshot

import (
	"os"
	"sync"
	"testing"
	"time"

	"github.com/efebarandurmaz/anvil/internal/plugins"
)

func TestFindByTag_Concurrent(t *testing.T) {
	dir := t.TempDir()

	store, err := NewStore(dir)
	if err != nil {
		t.Fatalf("NewStore: %v", err)
	}

	// Create and save a snapshot
	snap := &Snapshot{
		ID:        "test-snap-1",
		CreatedAt: time.Now(),
		Tag:       "",
	}
	files := []plugins.GeneratedFile{
		{Path: "out/main.go", Content: []byte("package main")},
	}
	if err := store.Save(snap, files); err != nil {
		t.Fatalf("Save: %v", err)
	}

	// Tag the snapshot
	if err := store.Tag(snap.ID, "release-1"); err != nil {
		t.Fatalf("Tag: %v", err)
	}

	// Run 10 concurrent FindByTag calls to verify no deadlock
	var wg sync.WaitGroup
	errs := make([]error, 10)
	for i := 0; i < 10; i++ {
		wg.Add(1)
		go func(idx int) {
			defer wg.Done()
			s, err := store.FindByTag("release-1")
			if err != nil {
				errs[idx] = err
				return
			}
			if s.ID != snap.ID {
				t.Errorf("goroutine %d: got snapshot ID %q, want %q", idx, s.ID, snap.ID)
			}
		}(i)
	}

	done := make(chan struct{})
	go func() {
		wg.Wait()
		close(done)
	}()

	select {
	case <-done:
		// success
	case <-time.After(5 * time.Second):
		t.Fatal("TestFindByTag_Concurrent: deadlock detected (timed out after 5s)")
	}

	for i, e := range errs {
		if e != nil {
			t.Errorf("goroutine %d returned error: %v", i, e)
		}
	}
}

func TestFindByTag_NotFound(t *testing.T) {
	dir, err := os.MkdirTemp("", "snapshot-test-*")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)

	store, err := NewStore(dir)
	if err != nil {
		t.Fatalf("NewStore: %v", err)
	}

	_, err = store.FindByTag("nonexistent")
	if err == nil {
		t.Error("expected error for missing tag, got nil")
	}
}
