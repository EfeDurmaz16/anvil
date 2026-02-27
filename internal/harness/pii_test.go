package harness

import (
	"strings"
	"testing"
)

func TestPIIDetector_ArrayIndexBeyondNine(t *testing.T) {
	detector := NewPIIDetector(&PIIDetectorConfig{
		EnabledTypes: []PIIType{PIITypeSSN},
		MaskingStyle: MaskingStyleRedact,
	})

	// Build a 12-element array where elements 10 and 11 contain PII
	data := map[string]any{
		"data": []any{
			"no-pii-0",
			"no-pii-1",
			"no-pii-2",
			"no-pii-3",
			"no-pii-4",
			"no-pii-5",
			"no-pii-6",
			"no-pii-7",
			"no-pii-8",
			"no-pii-9",
			"SSN is 123-45-6789",  // index 10
			"SSN is 987-65-4321",  // index 11
		},
	}

	matches := detector.DetectInMap(data)
	if len(matches) == 0 {
		t.Fatal("expected PII matches, got none")
	}

	for _, m := range matches {
		if !strings.Contains(m.Field, "[10]") && !strings.Contains(m.Field, "[11]") {
			t.Errorf("unexpected field path %q: must contain [10] or [11]", m.Field)
		}
		// Explicitly assert no wrong single-char index like [:]
		if strings.Contains(m.Field, "[:") || strings.Contains(m.Field, "[;") {
			t.Errorf("field path %q contains wrong index character (single rune overflow bug)", m.Field)
		}
	}

	// Verify both indices are represented
	foundTen := false
	foundEleven := false
	for _, m := range matches {
		if strings.Contains(m.Field, "[10]") {
			foundTen = true
		}
		if strings.Contains(m.Field, "[11]") {
			foundEleven = true
		}
	}
	if !foundTen {
		t.Error("expected a match with field path containing [10]")
	}
	if !foundEleven {
		t.Error("expected a match with field path containing [11]")
	}
}
