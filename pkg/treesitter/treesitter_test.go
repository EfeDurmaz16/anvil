package treesitter

import "testing"

func TestLanguageRegistry(t *testing.T) {
	// Register a test language
	Register("test-lang", func() uintptr { return 0 })

	fn, ok := GetLanguage("test-lang")
	if !ok {
		t.Fatal("expected test-lang to be registered")
	}
	if fn == nil {
		t.Fatal("expected non-nil language func")
	}

	_, ok = GetLanguage("nonexistent")
	if ok {
		t.Error("expected nonexistent language to not be found")
	}

	langs := Languages()
	found := false
	for _, l := range langs {
		if l == "test-lang" {
			found = true
		}
	}
	if !found {
		t.Error("expected test-lang in Languages() result")
	}
}

func TestStubParser(t *testing.T) {
	// Without CGO, NewParser should return an error
	p, err := NewParser("cobol")
	if err == nil {
		// If we're running with CGO+treesitter, skip this test
		p.Close()
		t.Skip("tree-sitter is enabled, stub test not applicable")
	}

	// Verify stub node methods don't panic
	n := &Node{}
	if n.Type() != "" {
		t.Error("expected empty type from stub")
	}
	if !n.IsNull() {
		t.Error("expected IsNull true from stub")
	}
	if n.ChildCount() != 0 {
		t.Error("expected 0 children from stub")
	}
	if n.StartLine() != 0 {
		t.Error("expected 0 start line from stub")
	}

	// Verify cursor stub
	cursor := n.Walk()
	if cursor.GotoFirstChild() {
		t.Error("expected false from stub cursor GotoFirstChild")
	}
	cursor.Close()
}
