package llm

import "strings"

// StripThinkingTags removes <think>...</think> blocks from LLM output.
// Some models (e.g. qwen3) wrap their reasoning in these tags.
func StripThinkingTags(s string) string {
	for {
		start := strings.Index(s, "<think>")
		if start == -1 {
			break
		}
		end := strings.Index(s, "</think>")
		if end == -1 {
			s = strings.TrimSpace(s[:start])
			break
		}
		s = s[:start] + s[end+len("</think>"):]
	}
	return strings.TrimSpace(s)
}
