package perl

import (
	"context"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// Plugin implements SourcePlugin for Perl.
type Plugin struct{}

func New() *Plugin { return &Plugin{} }

func (p *Plugin) Language() string { return "perl" }

func (p *Plugin) FileExtensions() []string { return []string{".pl", ".pm"} }

func (p *Plugin) Parse(ctx context.Context, files []plugins.SourceFile) (*ir.SemanticGraph, error) {
	graph := &ir.SemanticGraph{
		CallGraph: &ir.CallGraph{},
		Metadata:  map[string]string{"source_language": "perl"},
	}

	for _, f := range files {
		mod := parseModule(f)
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

	return graph, nil
}

func (p *Plugin) ResolveDependencies(ctx context.Context, graph *ir.SemanticGraph) error {
	// Link module dependencies (use/require statements)
	// Could resolve imported modules, but for now just track them
	return nil
}

func parseModule(f plugins.SourceFile) *ir.Module {
	content := stripPOD(string(f.Content))
	lines := strings.Split(content, "\n")

	mod := &ir.Module{
		Name:     moduleName(f.Path, lines),
		Path:     f.Path,
		Language: "perl",
		Metadata: map[string]string{},
	}

	// Detect Moose/Moo OOP usage
	oop := detectOOPFramework(lines)
	if oop != "" {
		mod.Metadata["oop_framework"] = oop
	}

	// Track module-level dependencies (use/require)
	deps := parseModuleDependencies(lines)
	if len(deps) > 0 {
		mod.Metadata["dependencies"] = strings.Join(deps, ",")
	}

	// Parse functions (subs), data types (variables), and I/O
	fns, dataTypes, ios := parseSubs(lines)
	mod.Functions = fns
	mod.DataTypes = dataTypes
	mod.IOContracts = ios

	return mod
}

func moduleName(path string, lines []string) string {
	// Look for package declaration
	for _, line := range lines {
		line = strings.TrimSpace(line)
		if strings.HasPrefix(line, "package ") {
			// Extract package name: "package Foo::Bar;" -> "Foo::Bar"
			line = strings.TrimPrefix(line, "package ")
			line = strings.TrimSuffix(line, ";")
			return strings.TrimSpace(line)
		}
	}
	// Default to filename without extension
	base := filepath.Base(path)
	return strings.TrimSuffix(base, filepath.Ext(base))
}

func parseModuleDependencies(lines []string) []string {
	var deps []string
	seen := make(map[string]bool)

	usePattern := regexp.MustCompile(`^\s*use\s+([\w:]+)`)
	requirePattern := regexp.MustCompile(`^\s*require\s+([\w:]+)`)

	for _, line := range lines {
		line = stripComment(line)
		if line == "" {
			continue
		}

		// use Foo::Bar;
		if m := usePattern.FindStringSubmatch(line); len(m) > 1 {
			dep := m[1]
			if !seen[dep] && !isCoreMod(dep) {
				seen[dep] = true
				deps = append(deps, dep)
			}
		}

		// require Foo::Bar;
		if m := requirePattern.FindStringSubmatch(line); len(m) > 1 {
			dep := m[1]
			if !seen[dep] && !isCoreMod(dep) {
				seen[dep] = true
				deps = append(deps, dep)
			}
		}
	}

	return deps
}

// podDirectives is the set of known POD directive keywords.
var podDirectives = map[string]bool{
	"head1": true, "head2": true, "head3": true, "head4": true,
	"pod": true, "over": true, "item": true, "back": true,
	"begin": true, "end": true, "for": true, "encoding": true,
}

// stripPOD removes POD documentation blocks from Perl source.
// POD blocks start with =<directive> at the start of a line and end with =cut.
func stripPOD(source string) string {
	lines := strings.Split(source, "\n")
	var result []string
	inPOD := false

	for _, line := range lines {
		// POD directives must start at the beginning of the line (no leading whitespace)
		if !inPOD {
			if strings.HasPrefix(line, "=") {
				// Check if the word after = is a known POD directive
				rest := line[1:]
				word := rest
				if idx := strings.IndexAny(rest, " \t\r\n"); idx >= 0 {
					word = rest[:idx]
				}
				if podDirectives[word] {
					inPOD = true
					continue
				}
			}
			result = append(result, line)
		} else {
			// In POD mode: skip until =cut
			trimmed := strings.TrimSpace(line)
			if trimmed == "=cut" {
				inPOD = false
			}
		}
	}

	return strings.Join(result, "\n")
}

var (
	// sub foo { ... }
	// sub bar ($arg1, $arg2) { ... }
	subPattern = regexp.MustCompile(`^\s*sub\s+([\w_]+)\s*(\([^)]*\))?\s*\{`)

	// my ($a, $b) = @_; parameter extraction
	atUnderscorePattern = regexp.MustCompile(`my\s*\(([^)]+)\)\s*=\s*@_`)

	// my $self = shift; or my $arg = shift;
	shiftPattern = regexp.MustCompile(`^\s*my\s+\$(\w+)\s*=\s*shift\s*;`)

	// my @args = @_;
	arrayAtUnderscorePattern = regexp.MustCompile(`my\s+@(\w+)\s*=\s*@_`)

	// my $name = sub { ... } closure detection
	closurePattern = regexp.MustCompile(`^\s*my\s+\$(\w+)\s*=\s*sub\s*\{`)

	// Moose/Moo attribute declarations: has 'name' => (...)
	mooseHasPattern = regexp.MustCompile(`^\s*has\s+['"]?([\w_]+)['"]?\s*=>`)

	// Moose/Moo role application: with 'RoleName'
	mooseWithPattern = regexp.MustCompile(`^\s*with\s+['"]?([\w:]+)['"]?`)

	// Moose/Moo extends: extends 'BaseClass'
	mooseExtendsPattern = regexp.MustCompile(`^\s*extends\s+['"]?([\w:]+)['"]?`)

	// Function calls: foo(), bar($x), Foo::Bar->method(), $obj->method()
	funcCallPattern   = regexp.MustCompile(`\b([\w:]+)\s*\(`)
	methodCallPattern = regexp.MustCompile(`->([\w_]+)\s*\(`)

	// Variable declarations: my $var, our @array, local %hash
	myVarPattern    = regexp.MustCompile(`\bmy\s+(\$|@|%)([\w_]+)`)
	ourVarPattern   = regexp.MustCompile(`\bour\s+(\$|@|%)([\w_]+)`)
	localVarPattern = regexp.MustCompile(`\blocal\s+(\$|@|%)([\w_]+)`)

	// File I/O: open(FH, ...), close(FH)
	openPattern  = regexp.MustCompile(`\bopen\s*\(\s*(?:my\s+)?(\$?\w+)\s*,\s*['"]?([<>+|]+)?['"]?`)
	closePattern = regexp.MustCompile(`\bclose\s*\(\s*(\$?\w+)\s*\)`)

	// DBI database calls
	dbiConnectPattern = regexp.MustCompile(`\bDBI->connect\b`)
	dbiPreparePattern = regexp.MustCompile(`->(prepare|do|selectall_arrayref|selectrow_array)`)
	dbiExecutePattern = regexp.MustCompile(`->(execute|fetchrow|fetch)\b`)
)

// detectOOPFramework checks if the source uses Moose, Moo, or Mouse OOP frameworks.
func detectOOPFramework(lines []string) string {
	for _, line := range lines {
		stripped := stripComment(line)
		if strings.Contains(stripped, "use Moose") {
			return "Moose"
		}
		if strings.Contains(stripped, "use Moo") && !strings.Contains(stripped, "use Moo::") {
			return "Moo"
		}
		if strings.Contains(stripped, "use Mouse") {
			return "Mouse"
		}
		if strings.Contains(stripped, "use Moose::Role") {
			return "Moose::Role"
		}
		if strings.Contains(stripped, "use Moo::Role") {
			return "Moo::Role"
		}
	}
	return ""
}

func parseSubs(lines []string) ([]*ir.Function, []*ir.DataType, []*ir.IOContract) {
	var functions []*ir.Function
	var dataTypes []*ir.DataType
	var ioContracts []*ir.IOContract

	var currentFn *ir.Function
	var bodyLines []string
	braceDepth := 0
	inSub := false

	varsSeen := make(map[string]bool)

	flush := func() {
		if currentFn != nil {
			currentFn.Body = strings.Join(bodyLines, "\n")
			// Extract parameters from body @_ patterns if not already set
			if len(currentFn.Parameters) == 0 {
				currentFn.Parameters = extractParametersFromBody(bodyLines)
			}
			// Deduplicate calls
			currentFn.Calls = dedup(currentFn.Calls)
			functions = append(functions, currentFn)
		}
	}

	for _, line := range lines {
		originalLine := line
		line = stripComment(line)
		if line == "" {
			continue
		}

		// Detect closure: my $name = sub { ... }
		if m := closurePattern.FindStringSubmatch(line); len(m) > 1 && !inSub {
			flush()
			closureName := m[1]
			currentFn = &ir.Function{
				Name:     closureName,
				Metadata: map[string]string{"kind": "closure"},
			}
			bodyLines = []string{originalLine}
			inSub = true
			braceDepth = countBraces(line)
			continue
		}

		// Detect nested closure inside a sub body: record it as a separate function
		if m := closurePattern.FindStringSubmatch(line); len(m) > 1 && inSub {
			closureName := m[1]
			functions = append(functions, &ir.Function{
				Name:     closureName,
				Metadata: map[string]string{"kind": "closure"},
			})
		}

		// Detect sub declaration
		if m := subPattern.FindStringSubmatch(line); len(m) > 1 {
			flush()
			name := m[1]
			params := ""
			if len(m) > 2 {
				params = m[2]
			}

			currentFn = &ir.Function{
				Name:     name,
				Metadata: map[string]string{},
			}

			// Parse parameters from prototype signature: sub foo ($a, $b) { ... }
			if params != "" {
				currentFn.Parameters = parseParameters(params)
			}

			// Detect Moose/Moo accessor modifiers
			if name == "BUILD" || name == "BUILDARGS" || name == "DEMOLISH" {
				currentFn.Metadata["moose_lifecycle"] = name
			}

			bodyLines = []string{originalLine}
			inSub = true
			braceDepth = countBraces(line)
			continue
		}

		// Detect Moose/Moo attribute declarations (outside subs)
		if !inSub {
			if m := mooseHasPattern.FindStringSubmatch(line); len(m) > 1 {
				attrName := m[1]
				varsSeen["moose_attr_"+attrName] = true
				dataTypes = append(dataTypes, &ir.DataType{
					Name:     attrName,
					Kind:     ir.TypeUnknown,
					Metadata: map[string]string{"kind": "moose_attribute"},
				})
			}
		}

		// Track brace depth when inside a sub
		if inSub {
			bodyLines = append(bodyLines, originalLine)
			braceDepth += countBraces(line)

			if braceDepth <= 0 {
				// End of current sub
				flush()
				currentFn = nil
				bodyLines = nil
				inSub = false
				braceDepth = 0
				continue
			}

			// Extract function calls
			if currentFn != nil {
				extractCalls(line, currentFn)
				extractIO(line, currentFn, &ioContracts)
			}
		}

		// Parse variable declarations (module-level or within subs)
		extractVariables(line, &dataTypes, varsSeen)
	}

	flush()

	return functions, dataTypes, ioContracts
}

// extractParametersFromBody scans the body lines of a sub for common @_ parameter
// assignment patterns and returns the extracted parameters.
//
// Supported patterns:
//   - my ($self, $name, $value) = @_;
//   - my ($arg1, $arg2) = @_;
//   - my $self = shift; my $name = shift;
//   - my @args = @_;
func extractParametersFromBody(bodyLines []string) []*ir.Parameter {
	var params []*ir.Parameter

	// Try: my ($a, $b, ...) = @_;
	// Uses the package-level atUnderscorePattern.
	for _, line := range bodyLines {
		stripped := stripComment(line)
		if m := atUnderscorePattern.FindStringSubmatch(stripped); len(m) > 1 {
			parts := strings.Split(m[1], ",")
			for _, part := range parts {
				part = strings.TrimSpace(part)
				if part == "" {
					continue
				}
				name := strings.TrimPrefix(part, "$")
				name = strings.TrimPrefix(name, "@")
				name = strings.TrimPrefix(name, "%")
				name = strings.TrimSpace(name)
				params = append(params, &ir.Parameter{
					Name: name,
					Type: &ir.DataType{Name: "scalar", Kind: ir.TypeUnknown},
				})
			}
			// Found the primary @_ assignment; stop looking for more.
			return params
		}
	}

	// Try: my @args = @_;
	for _, line := range bodyLines {
		stripped := stripComment(line)
		if m := arrayAtUnderscorePattern.FindStringSubmatch(stripped); len(m) > 1 {
			name := m[1]
			return []*ir.Parameter{
				{
					Name: name,
					Type: &ir.DataType{Name: "array", Kind: ir.TypeArray},
				},
			}
		}
	}

	// Try: sequential my $x = shift; statements at the top of the body.
	for _, line := range bodyLines {
		stripped := stripComment(line)
		if m := shiftPattern.FindStringSubmatch(stripped); len(m) > 1 {
			name := m[1]
			params = append(params, &ir.Parameter{
				Name: name,
				Type: &ir.DataType{Name: "scalar", Kind: ir.TypeUnknown},
			})
		} else if len(params) > 0 {
			// Stop collecting shift params once we hit a non-shift line
			// (ignore blank / comment-only lines which stripComment returns "")
			if stripped != "" {
				break
			}
		}
	}

	return params
}

func parseParameters(params string) []*ir.Parameter {
	// Remove parentheses and split by comma
	params = strings.Trim(params, "()")
	if params == "" {
		return nil
	}

	var result []*ir.Parameter
	parts := strings.Split(params, ",")
	for _, part := range parts {
		part = strings.TrimSpace(part)
		if part == "" {
			continue
		}
		// Remove sigil if present: $arg -> arg
		name := strings.TrimPrefix(part, "$")
		name = strings.TrimPrefix(name, "@")
		name = strings.TrimPrefix(name, "%")

		result = append(result, &ir.Parameter{
			Name: name,
			Type: &ir.DataType{
				Name: "scalar",
				Kind: ir.TypeUnknown,
			},
		})
	}
	return result
}

func extractCalls(line string, fn *ir.Function) {
	// Function calls: foo(), bar($x)
	matches := funcCallPattern.FindAllStringSubmatch(line, -1)
	for _, m := range matches {
		if len(m) > 1 {
			callee := m[1]
			// Filter out built-in functions and keywords
			if !isPerlBuiltin(callee) {
				fn.Calls = append(fn.Calls, callee)
			}
		}
	}

	// Method calls: $obj->method(), Foo->method()
	matches = methodCallPattern.FindAllStringSubmatch(line, -1)
	for _, m := range matches {
		if len(m) > 1 {
			method := m[1]
			fn.Calls = append(fn.Calls, method)
		}
	}
}

func extractVariables(line string, dataTypes *[]*ir.DataType, seen map[string]bool) {
	// my $var, my @array, my %hash
	patterns := []*regexp.Regexp{myVarPattern, ourVarPattern, localVarPattern}
	for _, pattern := range patterns {
		matches := pattern.FindAllStringSubmatch(line, -1)
		for _, m := range matches {
			if len(m) > 2 {
				sigil := m[1]
				name := m[2]
				fullName := sigil + name

				if seen[fullName] {
					continue
				}
				seen[fullName] = true

				kind := ir.TypeUnknown
				switch sigil {
				case "$":
					kind = ir.TypeString // scalar (could be string, int, etc.)
				case "@":
					kind = ir.TypeArray
				case "%":
					kind = ir.TypeStruct // hash
				}

				*dataTypes = append(*dataTypes, &ir.DataType{
					Name:     fullName,
					Kind:     kind,
					Metadata: map[string]string{"sigil": sigil},
				})
			}
		}
	}
}

func extractIO(line string, fn *ir.Function, ioContracts *[]*ir.IOContract) {
	if fn == nil {
		return
	}

	// File I/O: open(FH, '<', 'file.txt') or open(FH, '>', 'file.txt')
	if m := openPattern.FindStringSubmatch(line); len(m) > 1 {
		fh := m[1]
		mode := "<" // default read
		if len(m) > 2 && m[2] != "" {
			mode = m[2]
		}

		dir := ir.IORead
		if strings.Contains(mode, ">") {
			if strings.Contains(mode, ">>") {
				dir = ir.IOWrite // append
			} else {
				dir = ir.IOWrite
			}
		} else if strings.Contains(mode, "+") {
			dir = ir.IOReadWrite
		}

		*ioContracts = append(*ioContracts, &ir.IOContract{
			Name:      fh,
			Kind:      ir.IOFile,
			Direction: dir,
			Metadata:  map[string]string{"function": fn.Name, "mode": mode},
		})
	}

	// close(FH)
	if m := closePattern.FindStringSubmatch(line); len(m) > 1 {
		fh := m[1]
		*ioContracts = append(*ioContracts, &ir.IOContract{
			Name:      fh,
			Kind:      ir.IOFile,
			Direction: ir.IOReadWrite,
			Metadata:  map[string]string{"function": fn.Name, "operation": "close"},
		})
	}

	// DBI database operations
	if dbiConnectPattern.MatchString(line) {
		*ioContracts = append(*ioContracts, &ir.IOContract{
			Name:      fn.Name + "_db_connect",
			Kind:      ir.IODB,
			Direction: ir.IOReadWrite,
			Metadata:  map[string]string{"function": fn.Name, "operation": "connect"},
		})
	}

	if dbiPreparePattern.MatchString(line) {
		dir := ir.IORead
		if strings.Contains(strings.ToUpper(line), "INSERT") ||
			strings.Contains(strings.ToUpper(line), "UPDATE") ||
			strings.Contains(strings.ToUpper(line), "DELETE") {
			dir = ir.IOWrite
		}
		*ioContracts = append(*ioContracts, &ir.IOContract{
			Name:      fn.Name + "_db_query",
			Kind:      ir.IODB,
			Direction: dir,
			Metadata:  map[string]string{"function": fn.Name, "operation": "query"},
		})
	}

	if dbiExecutePattern.MatchString(line) {
		*ioContracts = append(*ioContracts, &ir.IOContract{
			Name:      fn.Name + "_db_execute",
			Kind:      ir.IODB,
			Direction: ir.IOReadWrite,
			Metadata:  map[string]string{"function": fn.Name, "operation": "execute"},
		})
	}
}

func stripComment(line string) string {
	// Remove # comments (but not inside strings - simplified)
	if idx := strings.Index(line, "#"); idx >= 0 {
		// Simple heuristic: if # is not inside quotes
		before := line[:idx]
		if strings.Count(before, `"`)%2 == 0 && strings.Count(before, `'`)%2 == 0 {
			line = before
		}
	}
	return strings.TrimSpace(line)
}

func countBraces(line string) int {
	return strings.Count(line, "{") - strings.Count(line, "}")
}

func dedup(ss []string) []string {
	seen := make(map[string]bool)
	var out []string
	for _, s := range ss {
		if !seen[s] {
			seen[s] = true
			out = append(out, s)
		}
	}
	return out
}

func isCoreMod(name string) bool {
	// Common Perl core modules to skip
	core := map[string]bool{
		"strict": true, "warnings": true, "vars": true,
		"Carp": true, "Exporter": true, "base": true, "parent": true,
		"constant": true, "lib": true, "utf8": true,
		"Data::Dumper": true, "Storable": true, "Scalar::Util": true,
		"List::Util": true, "File::Spec": true, "File::Path": true,
		"IO::File": true, "IO::Handle": true,
	}
	return core[name]
}

func isPerlBuiltin(name string) bool {
	// Common Perl built-in functions
	builtins := map[string]bool{
		"print": true, "printf": true, "say": true, "warn": true, "die": true,
		"open": true, "close": true, "read": true, "write": true, "seek": true,
		"chomp": true, "chop": true, "split": true, "join": true, "substr": true,
		"length": true, "defined": true, "exists": true, "delete": true,
		"push": true, "pop": true, "shift": true, "unshift": true,
		"keys": true, "values": true, "each": true, "grep": true, "map": true,
		"sort": true, "reverse": true, "sprintf": true,
		"localtime": true, "gmtime": true, "time": true, "sleep": true,
		"eval": true, "require": true, "use": true, "package": true,
		"if": true, "unless": true, "while": true, "until": true, "for": true,
		"foreach": true, "do": true, "return": true, "next": true, "last": true,
		"redo": true, "goto": true, "sub": true, "my": true, "our": true,
		"local": true, "new": true, "bless": true, "ref": true, "isa": true,
	}
	return builtins[name]
}
