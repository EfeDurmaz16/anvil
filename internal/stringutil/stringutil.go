package stringutil

import "strings"

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

// ToCamelCase converts an identifier to camelCase. Returns "unnamed" for empty input.
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

// ToSnakeCase converts an identifier to snake_case.
func ToSnakeCase(name string) string {
	parts := strings.FieldsFunc(name, func(r rune) bool {
		return r == '-' || r == '_' || r == ' ' || r == '.'
	})
	for i := range parts {
		parts[i] = strings.ToLower(parts[i])
	}
	return strings.Join(parts, "_")
}

// ToKebabCase converts an identifier to kebab-case.
func ToKebabCase(name string) string {
	parts := strings.FieldsFunc(name, func(r rune) bool {
		return r == '-' || r == '_' || r == ' ' || r == '.'
	})
	for i := range parts {
		parts[i] = strings.ToLower(parts[i])
	}
	return strings.Join(parts, "-")
}

// Dedup returns a new slice with duplicates removed, preserving order.
func Dedup(ss []string) []string {
	seen := make(map[string]bool)
	var result []string
	for _, s := range ss {
		if !seen[s] {
			seen[s] = true
			result = append(result, s)
		}
	}
	return result
}
