// Package llmutil provides shared utilities for LLM output processing and
// identifier case conversion used across target language plugins.
package llmutil

import (
	"strings"

	"github.com/efebarandurmaz/anvil/internal/llm"
)

// StripThinkingTags removes <think>...</think> blocks from LLM output.
// Delegates to llm.StripThinkingTags.
func StripThinkingTags(s string) string {
	return llm.StripThinkingTags(s)
}

// StripMarkdownFences removes markdown code fences (``` ... ```) from LLM
// output. It first strips thinking tags, then removes the outermost fence pair
// if present.
func StripMarkdownFences(s string) string {
	s = StripThinkingTags(s)

	lines := strings.Split(s, "\n")

	// Find and remove leading fence.
	start := 0
	for i, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "```") {
			start = i + 1
			break
		}
	}

	// Find and remove trailing fence.
	end := len(lines)
	for i := len(lines) - 1; i >= start; i-- {
		trimmed := strings.TrimSpace(lines[i])
		if strings.HasPrefix(trimmed, "```") {
			end = i
			break
		}
	}

	// If no fences found, return original.
	if start == 0 && end == len(lines) {
		return s
	}

	return strings.Join(lines[start:end], "\n")
}

// SanitizeLLMOutput strips import/export statements, unwraps function/class
// wrappers, and removes trailing prose that LLMs commonly include in TypeScript
// (and similarly structured) output.
func SanitizeLLMOutput(s string) string {
	lines := strings.Split(s, "\n")
	var cleaned []string
	hadExport := false
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)
		// Skip import/require statements.
		if strings.HasPrefix(trimmed, "import ") || strings.HasPrefix(trimmed, "import{") {
			continue
		}
		if strings.Contains(trimmed, "require(") && (strings.HasPrefix(trimmed, "const ") || strings.HasPrefix(trimmed, "let ") || strings.HasPrefix(trimmed, "var ")) {
			continue
		}
		// Strip "export " prefix from export statements rather than dropping the line.
		if strings.HasPrefix(trimmed, "export ") {
			// Find the position of "export " in the original line and remove it.
			idx := strings.Index(line, "export ")
			if idx >= 0 {
				line = line[:idx] + line[idx+len("export "):]
				hadExport = true
			}
		}
		// Strip trailing prose (non-code text after the implementation).
		if strings.HasPrefix(trimmed, "This ") || strings.HasPrefix(trimmed, "Note:") || strings.HasPrefix(trimmed, "The above") || strings.HasPrefix(trimmed, "Here ") {
			break
		}
		cleaned = append(cleaned, line)
	}

	// Trim leading/trailing empty lines.
	for len(cleaned) > 0 && strings.TrimSpace(cleaned[0]) == "" {
		cleaned = cleaned[1:]
	}
	for len(cleaned) > 0 && strings.TrimSpace(cleaned[len(cleaned)-1]) == "" {
		cleaned = cleaned[:len(cleaned)-1]
	}

	// Unwrap function/class wrappers: if first line is a function/class
	// declaration and last line is its closing brace, extract only the body.
	// Skip unwrap if we stripped an export keyword — the function IS the code.
	if len(cleaned) >= 3 && !hadExport {
		first := strings.TrimSpace(cleaned[0])
		last := strings.TrimSpace(cleaned[len(cleaned)-1])
		isFuncDecl := (strings.HasPrefix(first, "function ") ||
			strings.HasPrefix(first, "async function ") ||
			strings.HasPrefix(first, "class ")) &&
			strings.HasSuffix(first, "{")
		if isFuncDecl && last == "}" {
			// Extract body, removing one level of indentation.
			body := cleaned[1 : len(cleaned)-1]
			var unwrapped []string
			for _, line := range body {
				if strings.HasPrefix(line, "\t") {
					line = line[1:]
				} else if strings.HasPrefix(line, "  ") {
					line = line[2:]
				}
				unwrapped = append(unwrapped, line)
			}
			cleaned = unwrapped
		}
	}

	// Trim again after unwrapping.
	for len(cleaned) > 0 && strings.TrimSpace(cleaned[0]) == "" {
		cleaned = cleaned[1:]
	}
	for len(cleaned) > 0 && strings.TrimSpace(cleaned[len(cleaned)-1]) == "" {
		cleaned = cleaned[:len(cleaned)-1]
	}

	return strings.Join(cleaned, "\n")
}

// ToPascalCase converts an identifier using common separators (-, _, space, .)
// into PascalCase. Returns "Generated" for empty input.
func ToPascalCase(name string) string {
	parts := strings.FieldsFunc(name, func(r rune) bool {
		return r == '-' || r == '_' || r == ' ' || r == '.'
	})
	var out string
	for _, p := range parts {
		if p == "" {
			continue
		}
		out += strings.ToUpper(p[:1]) + strings.ToLower(p[1:])
	}
	if out == "" {
		return "Generated"
	}
	return out
}

// ToCamelCase converts an identifier to camelCase. Returns "unnamed" for empty
// input. For a single word, only the first character is lowercased (remaining
// chars are preserved as-is). For multiple words, the first word is fully
// lowercased and subsequent words have their first char uppercased and the rest
// lowercased.
func ToCamelCase(name string) string {
	parts := strings.FieldsFunc(name, func(r rune) bool {
		return r == '-' || r == '_' || r == ' ' || r == '.'
	})
	if len(parts) == 0 {
		return "unnamed"
	}
	if len(parts) == 1 {
		p := parts[0]
		return strings.ToLower(p[:1]) + p[1:]
	}
	var out string
	for i, p := range parts {
		if p == "" {
			continue
		}
		if i == 0 {
			out += strings.ToLower(p)
		} else {
			out += strings.ToUpper(p[:1]) + strings.ToLower(p[1:])
		}
	}
	if out == "" {
		return "unnamed"
	}
	return out
}
