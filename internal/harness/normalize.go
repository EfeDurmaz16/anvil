package harness

import (
	"encoding/json"
	"strings"
)

// NormalizeRules defines deterministic normalization for comparison.
//
// V1 only supports a minimal ignore list for top-level JSON fields and header keys.
// This is enough to ignore timestamps, request ids, correlation ids, etc.
type NormalizeRules struct {
	IgnoreJSONFields   []string `json:"ignore_json_fields,omitempty"`
	IgnoreHeaderFields []string `json:"ignore_header_fields,omitempty"`
}

func (r *NormalizeRules) normalizedIgnoreJSONSet() map[string]bool {
	out := make(map[string]bool)
	for _, f := range r.IgnoreJSONFields {
		f = strings.TrimSpace(f)
		if f == "" {
			continue
		}
		out[f] = true
	}
	return out
}

func (r *NormalizeRules) normalizedIgnoreHeaderSet() map[string]bool {
	out := make(map[string]bool)
	for _, h := range r.IgnoreHeaderFields {
		h = strings.ToLower(strings.TrimSpace(h))
		if h == "" {
			continue
		}
		out[h] = true
	}
	return out
}

func normalizeJSONTopLevel(b json.RawMessage, rules *NormalizeRules) (json.RawMessage, error) {
	if rules == nil || len(rules.IgnoreJSONFields) == 0 || len(b) == 0 {
		return b, nil
	}

	var obj map[string]any
	if err := json.Unmarshal(b, &obj); err != nil {
		// Not an object → don't normalize.
		return b, nil
	}

	ignore := rules.normalizedIgnoreJSONSet()
	for k := range obj {
		if ignore[k] {
			delete(obj, k)
		}
	}

	out, err := json.Marshal(obj)
	if err != nil {
		return nil, err
	}
	return json.RawMessage(out), nil
}
