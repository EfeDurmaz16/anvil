package cobol

import (
	"context"
	"path/filepath"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// Plugin implements SourcePlugin for COBOL.
type Plugin struct{}

func New() *Plugin { return &Plugin{} }

func (p *Plugin) Language() string { return "cobol" }

func (p *Plugin) FileExtensions() []string { return []string{".cbl", ".cob", ".cpy"} }

func (p *Plugin) Parse(ctx context.Context, files []plugins.SourceFile) (*ir.SemanticGraph, error) {
	graph := &ir.SemanticGraph{
		CallGraph: &ir.CallGraph{},
		Metadata:  map[string]string{"source_language": "cobol"},
	}

	// Pass 1: Index copybooks and build resolver
	resolver := NewCopybookResolver(files)

	// Still extract standalone copybook data types
	for _, f := range files {
		ext := strings.ToLower(filepath.Ext(f.Path))
		if ext == ".cpy" {
			dt := parseCopybook(f)
			graph.DataTypes = append(graph.DataTypes, dt...)
		}
	}

	// Pass 2: Parse programs with COPY expansion
	for _, f := range files {
		ext := strings.ToLower(filepath.Ext(f.Path))
		if ext == ".cpy" {
			continue
		}

		// Preprocess: join continuation lines before any expansion
		rawLines := strings.Split(string(f.Content), "\n")
		joinedLines := joinContinuationLines(rawLines)

		// Expand COPY statements in raw source text
		expandedSource := resolver.ExpandSource(joinedLines)

		mod := parseProgramFromSource(f.Path, expandedSource)
		graph.Modules = append(graph.Modules, mod)

		for _, fn := range mod.Functions {
			for _, call := range fn.Calls {
				graph.CallGraph.Edges = append(graph.CallGraph.Edges, ir.CallEdge{
					Caller: fn.Name,
					Callee: call,
				})
			}
		}
	}

	// Record resolver warnings in graph metadata
	if warnings := resolver.Warnings(); len(warnings) > 0 {
		graph.Metadata["copybook_warnings"] = strings.Join(warnings, "; ")
	}
	if resolved := resolver.Resolved(); len(resolved) > 0 {
		graph.Metadata["copybooks_resolved"] = strings.Join(resolved, ",")
	}

	return graph, nil
}

func (p *Plugin) ResolveDependencies(ctx context.Context, graph *ir.SemanticGraph) error {
	// Link copybook data types into modules that reference them.
	typeMap := make(map[string]*ir.DataType)
	for _, dt := range graph.DataTypes {
		typeMap[dt.Name] = dt
	}
	for _, mod := range graph.Modules {
		for i, dt := range mod.DataTypes {
			if resolved, ok := typeMap[dt.Name]; ok && dt.Kind == ir.TypeUnknown {
				mod.DataTypes[i] = resolved
			}
		}
	}
	return nil
}

// isFixedFormat reports whether the lines look like fixed-format COBOL.
// Heuristic: if more than half of non-empty lines have a fixed-format indicator
// in column 7 (index 6) — i.e. '*', '/', '-', or a space with content at
// column 8+ — treat as fixed-format. This handles short fixed-format lines
// that don't reach column 72.
func isFixedFormat(lines []string) bool {
	total, fixed := 0, 0
	for _, l := range lines {
		if strings.TrimSpace(l) == "" {
			continue
		}
		total++
		if len(l) >= 7 && (l[6] == '*' || l[6] == '/' || l[6] == '-' || (l[6] == ' ' && len(l) > 7)) {
			fixed++
		}
	}
	if total == 0 {
		return false
	}
	return fixed*2 >= total
}

// preprocessLines is the canonical pre-processing step for COBOL source.
// It handles fixed-format column conventions when applicable:
//   - Lines with '*' or '/' in column 7 (index 6) are comment lines and are dropped.
//   - Lines with '-' in column 7 are continuation lines: their content area
//     (columns 12–72, index 11 onward) is appended to the previous result line.
//     For string-literal continuations the first non-space character of the
//     content area begins the appended text; for non-literal continuations
//     leading spaces are preserved as-is.
//
// If the source does not appear to be fixed-format (free-format COBOL), the
// column rules are skipped and lines are returned unchanged (after stripping
// inline comments that start with *> which is the free-format comment marker).
func preprocessLines(lines []string) []string {
	fixed := isFixedFormat(lines)
	result := make([]string, 0, len(lines))

	for _, line := range lines {
		if fixed && len(line) >= 7 {
			indicator := line[6]

			// Comment line: drop entirely
			if indicator == '*' || indicator == '/' {
				continue
			}

			// Continuation line: merge into previous
			if indicator == '-' {
				var cont string
				if len(line) > 11 {
					content := line[11:]
					// Trim to column 72 boundary if line is full-width
					if len(content) > 61 {
						content = content[:61]
					}
					// For string literal continuation the first non-space char
					// of the content area is the start of the appended text.
					// For non-literal continuation preserve leading spaces.
					cont = strings.TrimLeft(content, " \t")
				}
				if len(result) > 0 {
					result[len(result)-1] = result[len(result)-1] + cont
				}
				continue
			}
		}

		// Free-format inline comment (*>) — strip from the line
		if !fixed {
			if idx := strings.Index(line, "*>"); idx >= 0 {
				line = line[:idx]
			}
		}

		result = append(result, line)
	}
	return result
}

// joinContinuationLines is kept for backwards compatibility.
// New code should prefer preprocessLines.
func joinContinuationLines(lines []string) []string {
	return preprocessLines(lines)
}

func parseProgram(f plugins.SourceFile) *ir.Module {
	content := string(f.Content)
	lines := joinContinuationLines(strings.Split(content, "\n"))

	mod := &ir.Module{
		Name:     programName(f.Path, lines),
		Path:     f.Path,
		Language: "cobol",
		Metadata: map[string]string{},
	}

	mod.DataTypes = parseDataDivision(lines)
	fns, ios := parseProcedureDivision(lines)
	mod.Functions = fns
	mod.IOContracts = ios

	return mod
}

func parseProgramFromSource(path string, source string) *ir.Module {
	lines := joinContinuationLines(strings.Split(source, "\n"))

	mod := &ir.Module{
		Name:     programName(path, lines),
		Path:     path,
		Language: "cobol",
		Metadata: map[string]string{},
	}

	mod.DataTypes = parseDataDivision(lines)
	fns, ios := parseProcedureDivision(lines)
	mod.Functions = fns
	mod.IOContracts = ios

	return mod
}

func programName(path string, lines []string) string {
	for _, line := range lines {
		upper := strings.ToUpper(strings.TrimSpace(line))
		if strings.HasPrefix(upper, "PROGRAM-ID.") {
			name := strings.TrimPrefix(upper, "PROGRAM-ID.")
			name = strings.TrimSuffix(strings.TrimSpace(name), ".")
			return strings.TrimSpace(name)
		}
	}
	base := filepath.Base(path)
	return strings.TrimSuffix(base, filepath.Ext(base))
}
