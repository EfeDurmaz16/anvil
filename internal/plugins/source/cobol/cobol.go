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

		// Expand COPY statements in raw source text
		lines := strings.Split(string(f.Content), "\n")
		expandedSource := resolver.ExpandSource(lines)

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

func parseProgram(f plugins.SourceFile) *ir.Module {
	content := string(f.Content)
	lines := strings.Split(content, "\n")

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
	lines := strings.Split(source, "\n")

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
