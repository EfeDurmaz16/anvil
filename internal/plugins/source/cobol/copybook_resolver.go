package cobol

import (
	"fmt"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// CopybookResolver expands COPY statements in COBOL source text.
// It operates at the text level (like a C preprocessor) - expanding
// copybook content inline before the source is parsed.
type CopybookResolver struct {
	// copybooks maps uppercase copybook name → file content lines
	copybooks map[string][]string
	// searchPaths for finding copybook files
	searchPaths []string
	// resolved tracks which copybooks were successfully resolved
	resolved []string
	// warnings collects non-fatal issues (e.g. missing system copybooks)
	warnings []string
}

// NewCopybookResolver creates a resolver from the available source files.
// It indexes all .cpy files by their base name (uppercase, without extension).
func NewCopybookResolver(files []plugins.SourceFile) *CopybookResolver {
	r := &CopybookResolver{
		copybooks: make(map[string][]string),
	}

	// Build a search path set from copybook directories
	pathSet := make(map[string]bool)

	for _, f := range files {
		ext := strings.ToLower(filepath.Ext(f.Path))
		if ext == ".cpy" {
			name := strings.ToUpper(strings.TrimSuffix(filepath.Base(f.Path), ext))
			r.copybooks[name] = strings.Split(string(f.Content), "\n")
			dir := filepath.Dir(f.Path)
			if !pathSet[dir] {
				pathSet[dir] = true
				r.searchPaths = append(r.searchPaths, dir)
			}
		}
	}

	return r
}

// Known system copybooks that are proprietary (IBM CICS, etc.)
// These are warned about but not treated as errors.
var systemCopybooks = map[string]bool{
	"DFHAID":      true,
	"DFHBMSCA":    true,
	"DFHATTR":     true,
	"DFHEIBLK":    true,
	"DFHCOMMAREA": true,
}

// copyPattern matches COPY statements. It handles:
// - COPY MEMBERNAME.
// - COPY MEMBERNAME REPLACING ...
// The statement may span multiple lines (terminated by period).
var copyPattern = regexp.MustCompile(`(?i)^\s*COPY\s+([\w-]+)`)

// ExpandSource expands all COPY statements in the given source lines.
// Returns the expanded source as a single string ready for parsing.
func (r *CopybookResolver) ExpandSource(lines []string) string {
	expanded := r.expandLines(lines, 0)
	return strings.Join(expanded, "\n")
}

// Warnings returns any non-fatal warnings from the expansion process.
func (r *CopybookResolver) Warnings() []string {
	return r.warnings
}

// Resolved returns the list of copybook names that were successfully resolved.
func (r *CopybookResolver) Resolved() []string {
	return r.resolved
}

const maxCopyDepth = 10

func (r *CopybookResolver) expandLines(lines []string, depth int) []string {
	if depth > maxCopyDepth {
		r.warnings = append(r.warnings, "copybook expansion exceeded max depth")
		return lines
	}

	var result []string
	i := 0
	for i < len(lines) {
		line := lines[i]

		// Skip fixed-format comments (column 7 = * or /)
		if len(line) >= 7 && (line[6] == '*' || line[6] == '/') {
			result = append(result, line)
			i++
			continue
		}

		// Check for COPY statement
		upper := strings.ToUpper(strings.TrimSpace(line))
		if !strings.Contains(upper, "COPY ") {
			result = append(result, line)
			i++
			continue
		}

		// Accumulate multi-line COPY statement (terminated by period)
		stmtLines := []string{line}
		fullStmt := strings.TrimSpace(line)
		for !strings.HasSuffix(strings.TrimSpace(fullStmt), ".") && i+1 < len(lines) {
			i++
			stmtLines = append(stmtLines, lines[i])
			fullStmt = fullStmt + " " + strings.TrimSpace(lines[i])
		}

		// Parse the COPY statement
		copyName, replacings, ok := parseCopyStatement(fullStmt)
		if !ok {
			// Not a valid COPY statement, keep original lines
			result = append(result, stmtLines...)
			i++
			continue
		}

		// Look up the copybook
		cpyLines, found := r.copybooks[strings.ToUpper(copyName)]
		if !found {
			if systemCopybooks[strings.ToUpper(copyName)] {
				r.warnings = append(r.warnings, fmt.Sprintf("system copybook %s not available (skipped)", copyName))
			} else {
				r.warnings = append(r.warnings, fmt.Sprintf("copybook %s not found (skipped)", copyName))
			}
			// Keep the original COPY line as a comment
			for _, sl := range stmtLines {
				result = append(result, toComment(sl))
			}
			i++
			continue
		}

		r.resolved = append(r.resolved, copyName)

		// Apply REPLACING if present
		expanded := make([]string, len(cpyLines))
		copy(expanded, cpyLines)
		if len(replacings) > 0 {
			expanded = applyReplacings(expanded, replacings)
		}

		// Recursively expand nested COPY statements
		expanded = r.expandLines(expanded, depth+1)
		result = append(result, expanded...)
		i++
	}

	return result
}

// replacing represents a single REPLACING operand pair.
type replacing struct {
	mode    string // "" (exact), "LEADING", "TRAILING"
	oldText string
	newText string
}

// parseCopyStatement parses a complete COPY statement (potentially multi-line, already joined).
// Returns the copybook name, any REPLACING directives, and whether parsing succeeded.
func parseCopyStatement(stmt string) (string, []replacing, bool) {
	// Remove trailing period
	stmt = strings.TrimSuffix(strings.TrimSpace(stmt), ".")
	upper := strings.ToUpper(stmt)

	// Find COPY keyword
	idx := strings.Index(upper, "COPY ")
	if idx < 0 {
		return "", nil, false
	}
	rest := strings.TrimSpace(stmt[idx+5:])
	upperRest := strings.ToUpper(rest)

	// Extract copybook name (first token)
	fields := strings.Fields(rest)
	if len(fields) == 0 {
		return "", nil, false
	}
	copyName := strings.TrimSuffix(fields[0], ".")

	// Check for REPLACING
	repIdx := strings.Index(upperRest, "REPLACING")
	if repIdx < 0 {
		return copyName, nil, true
	}

	// Parse REPLACING clause
	repText := rest[repIdx+9:]
	replacings := parseReplacings(repText)

	return copyName, replacings, true
}

// parseReplacings parses the REPLACING clause text.
// Handles: REPLACING ==old== BY ==new== ==old2== BY ==new2==
// Also handles: REPLACING LEADING ==old== BY ==new==
func parseReplacings(text string) []replacing {
	var result []replacing

	// Tokenize by splitting on == delimiters
	// Pattern: [LEADING|TRAILING] ==pseudo-text== BY ==pseudo-text==
	parts := strings.Split(text, "==")

	// parts will be: ["", "old", " BY ", "new", " ", "old2", " BY ", "new2", ...]
	// Or with LEADING: [" LEADING ", "old", " BY ", "new", ...]
	i := 0
	for i < len(parts) {
		part := strings.TrimSpace(parts[i])
		upperPart := strings.ToUpper(part)

		mode := ""
		if strings.Contains(upperPart, "LEADING") {
			mode = "LEADING"
		} else if strings.Contains(upperPart, "TRAILING") {
			mode = "TRAILING"
		}

		// Skip non-pseudo-text parts
		if i+1 >= len(parts) {
			break
		}

		// Check if next is a pseudo-text (old value)
		if strings.Contains(upperPart, "BY") && i > 0 {
			// This is a BY separator, skip
			i++
			continue
		}

		// Try to find old BY new pattern
		if i+3 < len(parts) {
			oldText := strings.TrimSpace(parts[i+1])
			byPart := strings.TrimSpace(strings.ToUpper(parts[i+2]))
			if strings.Contains(byPart, "BY") {
				newText := strings.TrimSpace(parts[i+3])
				result = append(result, replacing{
					mode:    mode,
					oldText: oldText,
					newText: newText,
				})
				i += 4
				continue
			}
		}

		i++
	}

	return result
}

// applyReplacings applies REPLACING directives to copybook lines.
func applyReplacings(lines []string, replacings []replacing) []string {
	result := make([]string, len(lines))
	for i, line := range lines {
		modified := line
		for _, rep := range replacings {
			switch rep.mode {
			case "LEADING":
				modified = applyLeadingReplace(modified, rep.oldText, rep.newText)
			case "TRAILING":
				modified = applyTrailingReplace(modified, rep.oldText, rep.newText)
			default:
				modified = strings.ReplaceAll(modified, rep.oldText, rep.newText)
			}
		}
		result[i] = modified
	}
	return result
}

// applyLeadingReplace replaces oldText at the beginning of each word/token in the line.
func applyLeadingReplace(line, oldText, newText string) string {
	// For LEADING, we replace the prefix of identifiers
	// e.g., LEADING ==PREFIX== BY ==PLAYER== turns PREFIX-X into PLAYER-X
	words := strings.Fields(line)
	for i, word := range words {
		cleanWord := strings.TrimSuffix(word, ".")
		if strings.HasPrefix(strings.ToUpper(cleanWord), strings.ToUpper(oldText)) {
			replacement := newText + cleanWord[len(oldText):]
			if strings.HasSuffix(word, ".") {
				replacement += "."
			}
			words[i] = replacement
		}
	}
	// Preserve original indentation
	indent := ""
	for _, ch := range line {
		if ch == ' ' || ch == '\t' {
			indent += string(ch)
		} else {
			break
		}
	}
	return indent + strings.Join(words, " ")
}

// applyTrailingReplace replaces oldText at the end of each word/token in the line.
func applyTrailingReplace(line, oldText, newText string) string {
	words := strings.Fields(line)
	for i, word := range words {
		cleanWord := strings.TrimSuffix(word, ".")
		if strings.HasSuffix(strings.ToUpper(cleanWord), strings.ToUpper(oldText)) {
			replacement := cleanWord[:len(cleanWord)-len(oldText)] + newText
			if strings.HasSuffix(word, ".") {
				replacement += "."
			}
			words[i] = replacement
		}
	}
	indent := ""
	for _, ch := range line {
		if ch == ' ' || ch == '\t' {
			indent += string(ch)
		} else {
			break
		}
	}
	return indent + strings.Join(words, " ")
}

// toComment converts a COBOL line to a comment by putting * in column 7.
func toComment(line string) string {
	if len(line) < 7 {
		return "      * " + strings.TrimSpace(line)
	}
	return line[:6] + "*" + line[7:]
}
