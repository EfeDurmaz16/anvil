package harness

import (
	"bytes"
	"encoding/json"
	"fmt"
	"sort"
	"strings"
)

type DiffKind string

const (
	DiffNone     DiffKind = "none"
	DiffKindHTTP DiffKind = "http"
)

type DiffResult struct {
	Kind   DiffKind `json:"kind"`
	Pass   bool     `json:"pass"`
	Reason string   `json:"reason,omitempty"`
}

type HTTPActual struct {
	Status int
	Header map[string]string
	Body   json.RawMessage
}

func DiffHTTP(f Fixture, actual HTTPActual, defaultRules *NormalizeRules) DiffResult {
	if f.HTTP == nil {
		return DiffResult{Kind: DiffKindHTTP, Pass: false, Reason: "missing http fixture"}
	}

	rules := defaultRules
	if f.Normalize != nil {
		rules = f.Normalize
	}

	if actual.Status != f.HTTP.ExpectedStatus {
		return DiffResult{Kind: DiffKindHTTP, Pass: false, Reason: fmt.Sprintf("status mismatch: got %d want %d", actual.Status, f.HTTP.ExpectedStatus)}
	}

	// Header compare (optional).
	if len(f.HTTP.ExpectedHeader) > 0 {
		ignoreHeader := map[string]bool{}
		if rules != nil {
			ignoreHeader = rules.normalizedIgnoreHeaderSet()
		}

		for k, want := range f.HTTP.ExpectedHeader {
			if ignoreHeader[strings.ToLower(k)] {
				continue
			}
			got := ""
			if actual.Header != nil {
				got = actual.Header[k]
				if got == "" {
					got = actual.Header[strings.ToLower(k)]
				}
			}
			if got != want {
				return DiffResult{Kind: DiffKindHTTP, Pass: false, Reason: fmt.Sprintf("header mismatch %q: got %q want %q", k, got, want)}
			}
		}
	}

	// Body compare (optional).
	if len(f.HTTP.ExpectedBody) > 0 {
		wantNorm, err := normalizeJSONTopLevel(f.HTTP.ExpectedBody, rules)
		if err != nil {
			return DiffResult{Kind: DiffKindHTTP, Pass: false, Reason: "normalize expected body failed: " + err.Error()}
		}
		gotNorm, err := normalizeJSONTopLevel(actual.Body, rules)
		if err != nil {
			return DiffResult{Kind: DiffKindHTTP, Pass: false, Reason: "normalize actual body failed: " + err.Error()}
		}

		if !jsonEqual(wantNorm, gotNorm) {
			return DiffResult{Kind: DiffKindHTTP, Pass: false, Reason: "body mismatch"}
		}
	}

	return DiffResult{Kind: DiffKindHTTP, Pass: true}
}

func jsonEqual(a, b []byte) bool {
	// Compare normalized JSON structures; fall back to raw bytes on parse errors.
	var ao any
	var bo any
	if err := json.Unmarshal(a, &ao); err != nil {
		return bytes.Equal(bytes.TrimSpace(a), bytes.TrimSpace(b))
	}
	if err := json.Unmarshal(b, &bo); err != nil {
		return bytes.Equal(bytes.TrimSpace(a), bytes.TrimSpace(b))
	}
	return canonicalJSON(ao) == canonicalJSON(bo)
}

func canonicalJSON(v any) string {
	// Deterministic stringify for objects (maps) by sorting keys.
	switch t := v.(type) {
	case map[string]any:
		keys := make([]string, 0, len(t))
		for k := range t {
			keys = append(keys, k)
		}
		sort.Strings(keys)
		var b strings.Builder
		b.WriteString("{")
		for i, k := range keys {
			if i > 0 {
				b.WriteString(",")
			}
			kk, _ := json.Marshal(k)
			b.Write(kk)
			b.WriteString(":")
			b.WriteString(canonicalJSON(t[k]))
		}
		b.WriteString("}")
		return b.String()
	case []any:
		var b strings.Builder
		b.WriteString("[")
		for i, it := range t {
			if i > 0 {
				b.WriteString(",")
			}
			b.WriteString(canonicalJSON(it))
		}
		b.WriteString("]")
		return b.String()
	default:
		enc, _ := json.Marshal(t)
		return string(enc)
	}
}
