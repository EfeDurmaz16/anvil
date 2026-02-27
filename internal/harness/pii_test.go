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
			"SSN is 123-45-6789", // index 10
			"SSN is 987-65-4321", // index 11
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

// TestPIIDetector_EmptyBody verifies no matches on empty / whitespace input.
func TestPIIDetector_EmptyBody(t *testing.T) {
	detector := NewPIIDetector(nil)

	matches := detector.DetectInText("")
	if len(matches) != 0 {
		t.Errorf("expected 0 matches on empty string, got %d", len(matches))
	}

	matches = detector.DetectInText("   ")
	if len(matches) != 0 {
		t.Errorf("expected 0 matches on whitespace, got %d", len(matches))
	}

	mapMatches := detector.DetectInMap(map[string]any{})
	if len(mapMatches) != 0 {
		t.Errorf("expected 0 matches on empty map, got %d", len(mapMatches))
	}
}

// TestPIIDetector_NestedObjectsWithPII verifies deep field paths are reported correctly.
func TestPIIDetector_NestedObjectsWithPII(t *testing.T) {
	detector := NewPIIDetector(&PIIDetectorConfig{
		EnabledTypes: []PIIType{PIITypeEmail, PIITypeSSN},
		MaskingStyle: MaskingStyleRedact,
	})

	data := map[string]any{
		"user": map[string]any{
			"profile": map[string]any{
				"email": "deep.user@example.com",
				"ssn":   "111-22-3333",
			},
		},
	}

	matches := detector.DetectInMap(data)
	if len(matches) != 2 {
		t.Fatalf("expected 2 matches, got %d", len(matches))
	}

	fields := make(map[string]bool)
	for _, m := range matches {
		fields[m.Field] = true
	}

	if !fields["user.profile.email"] {
		t.Error("expected match with field path user.profile.email")
	}
	if !fields["user.profile.ssn"] {
		t.Error("expected match with field path user.profile.ssn")
	}
}

// TestPIIDetector_ArrayOfObjectsWithPII verifies arrays of objects get correct indexed paths.
func TestPIIDetector_ArrayOfObjectsWithPII(t *testing.T) {
	detector := NewPIIDetector(&PIIDetectorConfig{
		EnabledTypes: []PIIType{PIITypeEmail},
		MaskingStyle: MaskingStyleRedact,
	})

	data := map[string]any{
		"users": []any{
			map[string]any{"email": "user0@example.com", "name": "Alice"},
			map[string]any{"email": "user1@example.com", "name": "Bob"},
			map[string]any{"email": "user2@example.com", "name": "Charlie"},
		},
	}

	matches := detector.DetectInMap(data)
	if len(matches) != 3 {
		t.Fatalf("expected 3 matches, got %d", len(matches))
	}

	fields := make(map[string]bool)
	for _, m := range matches {
		fields[m.Field] = true
	}

	for i := 0; i < 3; i++ {
		expected := "users[" + string(rune('0'+i)) + "].email"
		if !fields[expected] {
			t.Errorf("expected field path %q not found in matches", expected)
		}
	}
}

// TestPIIDetector_SSNPattern verifies SSN detection and various masking styles.
func TestPIIDetector_SSNPattern(t *testing.T) {
	tests := []struct {
		name         string
		text         string
		maskingStyle MaskingStyle
		wantMatch    bool
		wantMasked   string
	}{
		{
			name:         "redact SSN",
			text:         "My SSN is 123-45-6789.",
			maskingStyle: MaskingStyleRedact,
			wantMatch:    true,
			wantMasked:   "[REDACTED:ssn]",
		},
		{
			name:         "partial SSN",
			text:         "SSN: 123-45-6789",
			maskingStyle: MaskingStylePartial,
			wantMatch:    true,
			wantMasked:   "***-**-6789",
		},
		{
			name:         "no SSN",
			text:         "Nothing sensitive here",
			maskingStyle: MaskingStyleRedact,
			wantMatch:    false,
		},
	}

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			detector := NewPIIDetector(&PIIDetectorConfig{
				EnabledTypes: []PIIType{PIITypeSSN},
				MaskingStyle: tc.maskingStyle,
			})
			matches := detector.DetectInText(tc.text)
			if tc.wantMatch && len(matches) == 0 {
				t.Fatalf("expected SSN match, got none")
			}
			if !tc.wantMatch && len(matches) != 0 {
				t.Fatalf("expected no matches, got %d", len(matches))
			}
			if tc.wantMatch {
				if matches[0].Masked != tc.wantMasked {
					t.Errorf("expected masked=%q, got %q", tc.wantMasked, matches[0].Masked)
				}
				if matches[0].Type != PIITypeSSN {
					t.Errorf("expected type=ssn, got %q", matches[0].Type)
				}
			}
		})
	}
}

// TestPIIDetector_EmailPattern verifies email detection and partial masking.
func TestPIIDetector_EmailPattern(t *testing.T) {
	detector := NewPIIDetector(&PIIDetectorConfig{
		EnabledTypes: []PIIType{PIITypeEmail},
		MaskingStyle: MaskingStylePartial,
	})

	texts := []struct {
		input      string
		wantCount  int
		wantMasked string
	}{
		{"Contact alice@example.com for info", 1, "a***@example.com"},
		{"No email here", 0, ""},
		{"Two emails: a@b.com and c@d.org", 2, ""},
	}

	for _, tc := range texts {
		matches := detector.DetectInText(tc.input)
		if len(matches) != tc.wantCount {
			t.Errorf("input %q: expected %d match(es), got %d", tc.input, tc.wantCount, len(matches))
			continue
		}
		if tc.wantMasked != "" && len(matches) > 0 {
			if matches[0].Masked != tc.wantMasked {
				t.Errorf("expected masked=%q, got %q", tc.wantMasked, matches[0].Masked)
			}
		}
	}
}

// TestPIIDetector_PhonePattern verifies US phone number detection.
func TestPIIDetector_PhonePattern(t *testing.T) {
	detector := NewPIIDetector(&PIIDetectorConfig{
		EnabledTypes: []PIIType{PIITypePhone},
		MaskingStyle: MaskingStyleRedact,
	})

	validPhones := []string{
		"Call me at 555-867-5309",
		"Phone: (800) 555-1234",
		"Reach us at 800.555.6789",
	}

	for _, text := range validPhones {
		matches := detector.DetectInText(text)
		if len(matches) == 0 {
			t.Errorf("expected phone match in %q, got none", text)
			continue
		}
		if matches[0].Type != PIITypePhone {
			t.Errorf("expected type=phone, got %q", matches[0].Type)
		}
		if matches[0].Masked != "[REDACTED:phone]" {
			t.Errorf("expected [REDACTED:phone], got %q", matches[0].Masked)
		}
	}
}

// TestPIIDetector_CreditCardPattern verifies credit card detection and partial masking.
func TestPIIDetector_CreditCardPattern(t *testing.T) {
	detector := NewPIIDetector(&PIIDetectorConfig{
		EnabledTypes: []PIIType{PIITypeCreditCard},
		MaskingStyle: MaskingStylePartial,
	})

	texts := []struct {
		input      string
		wantMatch  bool
		wantMasked string
	}{
		{"Card: 4111-1111-1111-1111", true, "****-****-****-1111"},
		{"Card: 4111111111111111", true, "****-****-****-1111"},
		{"No card here", false, ""},
	}

	for _, tc := range texts {
		matches := detector.DetectInText(tc.input)
		if tc.wantMatch && len(matches) == 0 {
			t.Errorf("expected credit card match in %q, got none", tc.input)
			continue
		}
		if !tc.wantMatch && len(matches) != 0 {
			t.Errorf("expected no match in %q, got %d", tc.input, len(matches))
			continue
		}
		if tc.wantMatch && tc.wantMasked != "" {
			if matches[0].Masked != tc.wantMasked {
				t.Errorf("expected masked=%q, got %q", tc.wantMasked, matches[0].Masked)
			}
		}
	}
}

// TestPIIDetector_MaskText_MultiPII verifies MaskText replaces multiple PII types in-place.
func TestPIIDetector_MaskText_MultiPII(t *testing.T) {
	detector := NewPIIDetector(&PIIDetectorConfig{
		EnabledTypes: []PIIType{PIITypeSSN, PIITypeEmail},
		MaskingStyle: MaskingStyleRedact,
	})

	input := "User email is alice@example.com and SSN is 123-45-6789."
	masked, matches := detector.MaskText(input)

	if len(matches) < 2 {
		t.Fatalf("expected at least 2 matches, got %d", len(matches))
	}
	if strings.Contains(masked, "alice@example.com") {
		t.Error("masked text still contains original email")
	}
	if strings.Contains(masked, "123-45-6789") {
		t.Error("masked text still contains original SSN")
	}
	if !strings.Contains(masked, "[REDACTED:") {
		t.Error("masked text does not contain [REDACTED: token")
	}
}

// TestPIIDetector_MaskMap_NestedAndPreservesOriginal verifies MaskMap masks nested values and returns masked copy without mutating original.
func TestPIIDetector_MaskMap_NestedAndPreservesOriginal(t *testing.T) {
	detector := NewPIIDetector(&PIIDetectorConfig{
		EnabledTypes: []PIIType{PIITypeEmail},
		MaskingStyle: MaskingStyleRedact,
	})

	original := map[string]any{
		"id":    42,
		"email": "bob@example.com",
		"meta":  map[string]any{"contact": "charlie@example.com"},
	}

	masked, matches := detector.MaskMap(original)

	if len(matches) != 2 {
		t.Fatalf("expected 2 matches, got %d", len(matches))
	}

	// Original must not be mutated.
	if original["email"] != "bob@example.com" {
		t.Error("original map was mutated")
	}

	// Masked copy must not contain original emails.
	if masked["email"] == "bob@example.com" {
		t.Error("masked map still contains original email")
	}
	meta, ok := masked["meta"].(map[string]any)
	if !ok {
		t.Fatal("masked meta is not a map")
	}
	if meta["contact"] == "charlie@example.com" {
		t.Error("masked nested map still contains original email")
	}
	// Non-PII field must be preserved.
	if masked["id"] != 42 {
		t.Errorf("expected id=42, got %v", masked["id"])
	}
}

// TestPIIDetector_HashAndTokenizeMasking verifies hash and tokenize styles produce stable outputs.
func TestPIIDetector_HashAndTokenizeMasking(t *testing.T) {
	text := "SSN: 123-45-6789"

	hashDetector := NewPIIDetector(&PIIDetectorConfig{
		EnabledTypes: []PIIType{PIITypeSSN},
		MaskingStyle: MaskingStyleHash,
	})
	tokenDetector := NewPIIDetector(&PIIDetectorConfig{
		EnabledTypes: []PIIType{PIITypeSSN},
		MaskingStyle: MaskingStyleTokenize,
	})

	hashMatches := hashDetector.DetectInText(text)
	if len(hashMatches) == 0 {
		t.Fatal("hash: expected SSN match")
	}
	if !strings.HasPrefix(hashMatches[0].Masked, "HASH:") {
		t.Errorf("hash: expected HASH: prefix, got %q", hashMatches[0].Masked)
	}

	tokenMatches := tokenDetector.DetectInText(text)
	if len(tokenMatches) == 0 {
		t.Fatal("token: expected SSN match")
	}
	if !strings.HasPrefix(tokenMatches[0].Masked, "TOKEN:ssn:") {
		t.Errorf("token: expected TOKEN:ssn: prefix, got %q", tokenMatches[0].Masked)
	}

	// Same input should produce same hash/token (stable).
	hashMatches2 := hashDetector.DetectInText(text)
	if hashMatches[0].Masked != hashMatches2[0].Masked {
		t.Error("hash masking is not deterministic")
	}
}

// TestPIIDetector_EnabledTypesFilter_SSNOnly verifies that only enabled types are detected when SSN-only is configured.
func TestPIIDetector_EnabledTypesFilter_SSNOnly(t *testing.T) {
	// Enable only SSN — email should be ignored.
	detector := NewPIIDetector(&PIIDetectorConfig{
		EnabledTypes: []PIIType{PIITypeSSN},
		MaskingStyle: MaskingStyleRedact,
	})

	text := "Email: alice@example.com, SSN: 111-22-3333"
	matches := detector.DetectInText(text)

	for _, m := range matches {
		if m.Type == PIITypeEmail {
			t.Errorf("expected email to be filtered out, but got match: %+v", m)
		}
	}

	ssnFound := false
	for _, m := range matches {
		if m.Type == PIITypeSSN {
			ssnFound = true
		}
	}
	if !ssnFound {
		t.Error("expected SSN to be detected when EnabledTypes=[ssn]")
	}
}

// TestPIIDetector_PreserveLengthRedact verifies PreserveLength replaces with stars.
func TestPIIDetector_PreserveLengthRedact(t *testing.T) {
	detector := NewPIIDetector(&PIIDetectorConfig{
		EnabledTypes:   []PIIType{PIITypeSSN},
		MaskingStyle:   MaskingStyleRedact,
		PreserveLength: true,
	})

	ssn := "123-45-6789"
	text := "SSN: " + ssn
	matches := detector.DetectInText(text)
	if len(matches) == 0 {
		t.Fatal("expected SSN match")
	}

	if len(matches[0].Masked) != len(ssn) {
		t.Errorf("expected masked length=%d, got %d (masked=%q)", len(ssn), len(matches[0].Masked), matches[0].Masked)
	}
	if !strings.ContainsRune(matches[0].Masked, '*') {
		t.Errorf("expected masked to contain '*', got %q", matches[0].Masked)
	}
}

// TestPIIDetector_MaskFixture verifies MaskFixture masks HTTP bodies.
func TestPIIDetector_MaskFixture(t *testing.T) {
	detector := NewPIIDetector(&PIIDetectorConfig{
		EnabledTypes: []PIIType{PIITypeSSN},
		MaskingStyle: MaskingStyleRedact,
	})

	fixture := &Fixture{
		Kind: FixtureHTTP,
		Name: "test-fixture",
		HTTP: &HTTPFixture{
			Method: "POST",
			Path:   "/submit",
			Body:   []byte(`{"ssn":"123-45-6789"}`),
			ExpectedBody: []byte(`{"confirmed":"123-45-6789"}`),
		},
	}

	masked, matches := detector.MaskFixture(fixture)

	if len(matches) == 0 {
		t.Fatal("expected PII matches from fixture")
	}
	if strings.Contains(string(masked.HTTP.Body), "123-45-6789") {
		t.Error("masked fixture body still contains SSN")
	}
	if strings.Contains(string(masked.HTTP.ExpectedBody), "123-45-6789") {
		t.Error("masked fixture expected body still contains SSN")
	}
	// Original fixture must not be mutated.
	if !strings.Contains(string(fixture.HTTP.Body), "123-45-6789") {
		t.Error("original fixture body was mutated")
	}
}

// TestPIIDetector_LargeArray verifies indices beyond 9 work for arrays of any size.
func TestPIIDetector_LargeArray(t *testing.T) {
	detector := NewPIIDetector(&PIIDetectorConfig{
		EnabledTypes: []PIIType{PIITypeEmail},
		MaskingStyle: MaskingStyleRedact,
	})

	// 20-element array; only index 19 has PII.
	items := make([]any, 20)
	for i := range items {
		items[i] = "safe"
	}
	items[19] = "contact@big-index.com"

	data := map[string]any{"items": items}
	matches := detector.DetectInMap(data)

	if len(matches) != 1 {
		t.Fatalf("expected 1 match, got %d", len(matches))
	}
	if !strings.Contains(matches[0].Field, "[19]") {
		t.Errorf("expected field path to contain [19], got %q", matches[0].Field)
	}
}
