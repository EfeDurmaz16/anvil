package cli

import (
	"testing"
)

func TestListSnapshots_EmptyStore(t *testing.T) {
	tmpDir := t.TempDir()
	err := ListSnapshots(tmpDir)
	if err != nil {
		t.Errorf("unexpected error on empty store: %v", err)
	}
}

func TestShowSnapshot_NotFound(t *testing.T) {
	tmpDir := t.TempDir()
	err := ShowSnapshot(tmpDir, "nonexistentid")
	if err == nil {
		t.Error("expected error when showing nonexistent snapshot")
	}
}

func TestDeleteSnapshot_NoError(t *testing.T) {
	tmpDir := t.TempDir()
	// DeleteSnapshot is idempotent — no error on nonexistent ID
	err := DeleteSnapshot(tmpDir, "nonexistentid")
	if err != nil {
		t.Errorf("unexpected error: %v", err)
	}
}

func TestTagSnapshot_NotFound(t *testing.T) {
	tmpDir := t.TempDir()
	err := TagSnapshot(tmpDir, "nonexistentid", "mytag")
	if err == nil {
		t.Error("expected error when tagging nonexistent snapshot")
	}
}
