package fortran

import (
	"context"
	"path/filepath"
	"regexp"
	"strconv"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// Plugin implements SourcePlugin for Fortran.
type Plugin struct{}

func New() *Plugin { return &Plugin{} }

func (p *Plugin) Language() string { return "fortran" }

func (p *Plugin) FileExtensions() []string {
	return []string{".f", ".for", ".f77", ".f90", ".f95", ".f03", ".f08"}
}

func (p *Plugin) Parse(ctx context.Context, files []plugins.SourceFile) (*ir.SemanticGraph, error) {
	graph := &ir.SemanticGraph{
		CallGraph: &ir.CallGraph{},
		Metadata:  map[string]string{"source_language": "fortran"},
	}

	for _, f := range files {
		mod := parseFile(f)
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
	// Build module map for USE statement resolution
	moduleMap := make(map[string]*ir.Module)
	for _, mod := range graph.Modules {
		moduleMap[strings.ToUpper(mod.Name)] = mod
	}

	// Resolve USE statements
	for _, mod := range graph.Modules {
		if usedModules, ok := mod.Metadata["use_modules"]; ok {
			modules := strings.Split(usedModules, ",")
			for _, usedName := range modules {
				if usedMod, exists := moduleMap[strings.ToUpper(usedName)]; exists {
					// Link used module's data types
					for _, dt := range usedMod.DataTypes {
						// Add reference to used type
						mod.DataTypes = append(mod.DataTypes, &ir.DataType{
							Name:     dt.Name,
							Kind:     dt.Kind,
							Metadata: map[string]string{"from_module": usedMod.Name},
						})
					}
				}
			}
		}
	}

	return nil
}

var (
	// Program unit patterns
	programPattern    = regexp.MustCompile(`(?i)^\s*program\s+([\w_]+)`)
	modulePattern     = regexp.MustCompile(`(?i)^\s*module\s+([\w_]+)`)
	subroutinePattern = regexp.MustCompile(`(?i)^\s*subroutine\s+([\w_]+)(?:\s*\((.*?)\))?`)
	functionPattern   = regexp.MustCompile(`(?i)^\s*(?:(?:integer|real|double\s+precision|complex|logical|character(?:\s*\*\s*\d+)?|type\s*\([\w_]+\))\s+)?function\s+([\w_]+)(?:\s*\((.*?)\))?`)
	endPattern        = regexp.MustCompile(`(?i)^\s*end\s*(program|module|subroutine|function)?`)
	containsPattern   = regexp.MustCompile(`(?i)^\s*contains\s*$`)

	// USE statements
	usePattern = regexp.MustCompile(`(?i)^\s*use\s+([\w_]+)`)

	// Variable declarations
	integerDeclPattern   = regexp.MustCompile(`(?i)^\s*integer(?:\s*\*\s*(\d+)|\s*\(kind\s*=\s*(\d+)\))?(?:\s*,\s*(?:intent\s*\(\s*(in|out|inout)\s*\)|dimension\s*\([^)]*\)|parameter|allocatable|save|target|pointer))*\s*::\s*([\w_]+(?:\s*,\s*[\w_]+)*)`)
	realDeclPattern      = regexp.MustCompile(`(?i)^\s*real(?:\s*\*\s*(\d+)|\s*\(kind\s*=\s*(\d+)\))?(?:\s*,\s*(?:intent\s*\(\s*(in|out|inout)\s*\)|dimension\s*\([^)]*\)|parameter|allocatable|save|target|pointer))*\s*::\s*([\w_]+(?:\s*,\s*[\w_]+)*)`)
	doubleDeclPattern    = regexp.MustCompile(`(?i)^\s*double\s+precision(?:\s*,\s*intent\s*\(\s*(in|out|inout)\s*\))?\s*::\s*([\w_]+(?:\s*,\s*[\w_]+)*)`)
	complexDeclPattern   = regexp.MustCompile(`(?i)^\s*complex(?:\s*\*\s*(\d+))?(?:\s*,\s*intent\s*\(\s*(in|out|inout)\s*\))?\s*::\s*([\w_]+(?:\s*,\s*[\w_]+)*)`)
	logicalDeclPattern   = regexp.MustCompile(`(?i)^\s*logical(?:\s*,\s*intent\s*\(\s*(in|out|inout)\s*\))?\s*::\s*([\w_]+(?:\s*,\s*[\w_]+)*)`)
	characterDeclPattern = regexp.MustCompile(`(?i)^\s*character(?:\s*\*\s*(\d+)|\s*\((?:len\s*=\s*)?(\d+|\*)\))?(?:\s*,\s*(?:intent\s*\(\s*(in|out|inout)\s*\)|dimension\s*\([^)]*\)|parameter|allocatable|save|target|pointer))*\s*::\s*([\w_]+(?:\s*,\s*[\w_]+)*)`)
	typeDeclPattern      = regexp.MustCompile(`(?i)^\s*type\s*\(\s*([\w_]+)\s*\)(?:\s*,\s*intent\s*\(\s*(in|out|inout)\s*\))?\s*::\s*([\w_]+(?:\s*,\s*[\w_]+)*)`)

	// Array dimension pattern
	dimensionPattern = regexp.MustCompile(`(?i),\s*dimension\s*\((.*?)\)`)

	// Call statements
	callPattern = regexp.MustCompile(`(?i)^\s*call\s+([\w_]+)`)

	// I/O statements
	openPattern  = regexp.MustCompile(`(?i)^\s*open\s*\(`)
	readPattern  = regexp.MustCompile(`(?i)^\s*read\s*\(`)
	writePattern = regexp.MustCompile(`(?i)^\s*write\s*\(`)
	printPattern = regexp.MustCompile(`(?i)^\s*print\s+`)

	// Comment pattern: ! anywhere means rest-of-line comment in free form.
	// Fixed-form: C/c/* in column 1 only (checked separately with original line).
	freeFormCommentPattern  = regexp.MustCompile(`^\s*!`)
	fixedFormCommentPattern = regexp.MustCompile(`^[Cc*]`)
)

func parseFile(f plugins.SourceFile) *ir.Module {
	content := string(f.Content)
	lines := strings.Split(content, "\n")

	mod := &ir.Module{
		Name:     extractModuleName(f.Path, lines),
		Path:     f.Path,
		Language: "fortran",
		Metadata: map[string]string{},
	}

	parseContext := &parseState{
		module:      mod,
		lines:       lines,
		currentLine: 0,
		usedModules: []string{},
	}

	parseProgram(parseContext)

	if len(parseContext.usedModules) > 0 {
		mod.Metadata["use_modules"] = strings.Join(parseContext.usedModules, ",")
	}

	return mod
}

type parseState struct {
	module      *ir.Module
	lines       []string
	currentLine int
	currentFunc *ir.Function
	usedModules []string
	inContains  bool
}

// isComment checks if a line is a Fortran comment.
// Free form: line starts with ! (possibly after whitespace).
// Fixed form: C, c, or * in column 1 (no leading whitespace).
func isComment(line string) bool {
	if freeFormCommentPattern.MatchString(line) {
		return true
	}
	if len(line) > 0 && fixedFormCommentPattern.MatchString(line) {
		return true
	}
	return false
}

func extractModuleName(path string, lines []string) string {
	for _, line := range lines {
		if isComment(line) {
			continue
		}
		line = strings.TrimSpace(line)

		if m := programPattern.FindStringSubmatch(line); len(m) > 1 {
			return strings.ToUpper(m[1])
		}
		if m := modulePattern.FindStringSubmatch(line); len(m) > 1 {
			return strings.ToUpper(m[1])
		}
	}

	// Default to filename
	base := filepath.Base(path)
	return strings.ToUpper(strings.TrimSuffix(base, filepath.Ext(base)))
}

func parseProgram(ctx *parseState) {
	var mainCalls []string

	for ctx.currentLine < len(ctx.lines) {
		line := ctx.lines[ctx.currentLine]
		trimmed := strings.TrimSpace(line)

		// Skip comments and empty lines
		if trimmed == "" || isComment(line) {
			ctx.currentLine++
			continue
		}

		// USE statements
		if m := usePattern.FindStringSubmatch(trimmed); len(m) > 1 {
			ctx.usedModules = append(ctx.usedModules, m[1])
			ctx.currentLine++
			continue
		}

		// CONTAINS keyword - marks internal subprograms
		if containsPattern.MatchString(trimmed) {
			ctx.inContains = true
			ctx.currentLine++
			continue
		}

		// Subroutine
		if m := subroutinePattern.FindStringSubmatch(trimmed); len(m) > 1 {
			parseSubroutine(ctx, m[1], m[2])
			continue
		}

		// Function
		if m := functionPattern.FindStringSubmatch(trimmed); len(m) > 1 {
			parseFunction(ctx, m[1], m[2])
			continue
		}

		// Variable declarations at module level
		if !ctx.inContains && ctx.currentFunc == nil {
			if dt := parseVariableDeclaration(trimmed); dt != nil {
				ctx.module.DataTypes = append(ctx.module.DataTypes, dt...)
			}
		}

		// Call and I/O at main program level
		if ctx.currentFunc == nil && !ctx.inContains {
			if m := callPattern.FindStringSubmatch(trimmed); len(m) > 1 {
				mainCalls = append(mainCalls, strings.ToUpper(m[1]))
			}
			if io := parseIOStatement(trimmed, "MAIN"); io != nil {
				ctx.module.IOContracts = append(ctx.module.IOContracts, io)
			}
		}

		ctx.currentLine++
	}

	if len(mainCalls) > 0 {
		mainFn := &ir.Function{
			Name:     "MAIN",
			Calls:    dedup(mainCalls),
			Metadata: map[string]string{"kind": "program"},
		}
		ctx.module.Functions = append([]*ir.Function{mainFn}, ctx.module.Functions...)
	}
}

func parseSubroutine(ctx *parseState, name string, params string) {
	fn := &ir.Function{
		Name:     strings.ToUpper(name),
		Metadata: map[string]string{"kind": "subroutine"},
	}

	// Parse parameters
	if params != "" {
		paramList := strings.Split(params, ",")
		for _, p := range paramList {
			p = strings.TrimSpace(p)
			if p != "" {
				fn.Parameters = append(fn.Parameters, &ir.Parameter{
					Name: strings.ToUpper(p),
					Type: &ir.DataType{Kind: ir.TypeUnknown},
				})
			}
		}
	}

	ctx.currentFunc = fn
	ctx.currentLine++

	// Parse body
	bodyLines := []string{}
	for ctx.currentLine < len(ctx.lines) {
		line := ctx.lines[ctx.currentLine]
		trimmed := strings.TrimSpace(line)

		if trimmed == "" || isComment(line) {
			ctx.currentLine++
			continue
		}

		// End of subroutine
		if endPattern.MatchString(trimmed) {
			m := endPattern.FindStringSubmatch(trimmed)
			if len(m) > 1 && (m[1] == "" || strings.EqualFold(m[1], "subroutine")) {
				ctx.currentLine++
				break
			}
		}

		// Contains - nested functions
		if containsPattern.MatchString(trimmed) {
			ctx.currentLine++
			// Parse internal procedures
			for ctx.currentLine < len(ctx.lines) {
				line := ctx.lines[ctx.currentLine]
				trimmed := strings.TrimSpace(line)

				if endPattern.MatchString(trimmed) {
					break
				}

				if m := subroutinePattern.FindStringSubmatch(trimmed); len(m) > 1 {
					parseSubroutine(ctx, m[1], m[2])
					continue
				}
				if m := functionPattern.FindStringSubmatch(trimmed); len(m) > 1 {
					parseFunction(ctx, m[1], m[2])
					continue
				}
				ctx.currentLine++
			}
			continue
		}

		bodyLines = append(bodyLines, line)

		// Update parameter types from declarations
		if dt := parseVariableDeclaration(trimmed); dt != nil {
			for _, declType := range dt {
				for _, param := range fn.Parameters {
					if param.Name == declType.Name {
						param.Type = declType
						break
					}
				}
			}
		}

		// Call statements
		if m := callPattern.FindStringSubmatch(trimmed); len(m) > 1 {
			fn.Calls = append(fn.Calls, strings.ToUpper(m[1]))
		}

		// I/O statements
		if io := parseIOStatement(trimmed, fn.Name); io != nil {
			ctx.module.IOContracts = append(ctx.module.IOContracts, io)
		}

		ctx.currentLine++
	}

	fn.Body = strings.Join(bodyLines, "\n")
	fn.Calls = dedup(fn.Calls)
	ctx.module.Functions = append(ctx.module.Functions, fn)
	ctx.currentFunc = nil
}

func parseFunction(ctx *parseState, name string, params string) {
	fn := &ir.Function{
		Name:     strings.ToUpper(name),
		Returns:  &ir.DataType{Kind: ir.TypeUnknown}, // Will be refined by result declaration
		Metadata: map[string]string{"kind": "function"},
	}

	// Parse parameters
	if params != "" {
		paramList := strings.Split(params, ",")
		for _, p := range paramList {
			p = strings.TrimSpace(p)
			if p != "" {
				fn.Parameters = append(fn.Parameters, &ir.Parameter{
					Name: strings.ToUpper(p),
					Type: &ir.DataType{Kind: ir.TypeUnknown},
				})
			}
		}
	}

	ctx.currentFunc = fn
	ctx.currentLine++

	// Parse body
	bodyLines := []string{}
	for ctx.currentLine < len(ctx.lines) {
		line := ctx.lines[ctx.currentLine]
		trimmed := strings.TrimSpace(line)

		if trimmed == "" || isComment(line) {
			ctx.currentLine++
			continue
		}

		// End of function
		if endPattern.MatchString(trimmed) {
			m := endPattern.FindStringSubmatch(trimmed)
			if len(m) > 1 && (m[1] == "" || strings.EqualFold(m[1], "function")) {
				ctx.currentLine++
				break
			}
		}

		// Contains - nested functions
		if containsPattern.MatchString(trimmed) {
			ctx.currentLine++
			for ctx.currentLine < len(ctx.lines) {
				line := ctx.lines[ctx.currentLine]
				trimmed := strings.TrimSpace(line)

				if endPattern.MatchString(trimmed) {
					break
				}

				if m := subroutinePattern.FindStringSubmatch(trimmed); len(m) > 1 {
					parseSubroutine(ctx, m[1], m[2])
					continue
				}
				if m := functionPattern.FindStringSubmatch(trimmed); len(m) > 1 {
					parseFunction(ctx, m[1], m[2])
					continue
				}
				ctx.currentLine++
			}
			continue
		}

		bodyLines = append(bodyLines, line)

		// Update parameter types from declarations
		if dt := parseVariableDeclaration(trimmed); dt != nil {
			for _, declType := range dt {
				// Check if it's the function return value
				if declType.Name == fn.Name {
					fn.Returns = declType
				}
				// Check parameters
				for _, param := range fn.Parameters {
					if param.Name == declType.Name {
						param.Type = declType
						break
					}
				}
			}
		}

		// Call statements
		if m := callPattern.FindStringSubmatch(trimmed); len(m) > 1 {
			fn.Calls = append(fn.Calls, strings.ToUpper(m[1]))
		}

		// I/O statements
		if io := parseIOStatement(trimmed, fn.Name); io != nil {
			ctx.module.IOContracts = append(ctx.module.IOContracts, io)
		}

		ctx.currentLine++
	}

	fn.Body = strings.Join(bodyLines, "\n")
	fn.Calls = dedup(fn.Calls)
	ctx.module.Functions = append(ctx.module.Functions, fn)
	ctx.currentFunc = nil
}

func parseVariableDeclaration(line string) []*ir.DataType {
	var types []*ir.DataType

	// Check for array dimensions
	isDimensioned := dimensionPattern.MatchString(line)

	// INTEGER
	if m := integerDeclPattern.FindStringSubmatch(line); len(m) > 4 {
		size := 4 // default integer size
		if m[1] != "" {
			size, _ = strconv.Atoi(m[1])
		} else if m[2] != "" {
			size, _ = strconv.Atoi(m[2])
		}
		intent := m[3]
		names := strings.Split(m[4], ",")
		for _, name := range names {
			name = strings.TrimSpace(name)
			dt := &ir.DataType{
				Name:     strings.ToUpper(name),
				Kind:     ir.TypeInteger,
				Size:     size,
				Metadata: map[string]string{},
			}
			if intent != "" {
				dt.Metadata["intent"] = strings.ToLower(intent)
			}
			if isDimensioned {
				dt = wrapAsArray(dt, line)
			}
			types = append(types, dt)
		}
		return types
	}

	// REAL
	if m := realDeclPattern.FindStringSubmatch(line); len(m) > 3 {
		size := 4 // default real size
		if m[1] != "" {
			size, _ = strconv.Atoi(m[1])
		} else if m[2] != "" {
			size, _ = strconv.Atoi(m[2])
		}
		intent := m[3]
		names := strings.Split(m[4], ",")
		for _, name := range names {
			name = strings.TrimSpace(name)
			dt := &ir.DataType{
				Name:     strings.ToUpper(name),
				Kind:     ir.TypeDecimal,
				Size:     size,
				Metadata: map[string]string{},
			}
			if intent != "" {
				dt.Metadata["intent"] = strings.ToLower(intent)
			}
			if isDimensioned {
				dt = wrapAsArray(dt, line)
			}
			types = append(types, dt)
		}
		return types
	}

	// DOUBLE PRECISION
	if m := doubleDeclPattern.FindStringSubmatch(line); len(m) > 2 {
		intent := m[1]
		names := strings.Split(m[2], ",")
		for _, name := range names {
			name = strings.TrimSpace(name)
			dt := &ir.DataType{
				Name:     strings.ToUpper(name),
				Kind:     ir.TypeDecimal,
				Size:     8,
				Metadata: map[string]string{},
			}
			if intent != "" {
				dt.Metadata["intent"] = strings.ToLower(intent)
			}
			if isDimensioned {
				dt = wrapAsArray(dt, line)
			}
			types = append(types, dt)
		}
		return types
	}

	// COMPLEX
	if m := complexDeclPattern.FindStringSubmatch(line); len(m) > 3 {
		size := 8 // default complex size (2x float)
		if m[1] != "" {
			size, _ = strconv.Atoi(m[1])
		}
		intent := m[2]
		names := strings.Split(m[3], ",")
		for _, name := range names {
			name = strings.TrimSpace(name)
			dt := &ir.DataType{
				Name:     strings.ToUpper(name),
				Kind:     ir.TypeStruct, // Complex as struct with real/imag
				Size:     size,
				Metadata: map[string]string{"fortran_type": "complex"},
			}
			if intent != "" {
				dt.Metadata["intent"] = strings.ToLower(intent)
			}
			if isDimensioned {
				dt = wrapAsArray(dt, line)
			}
			types = append(types, dt)
		}
		return types
	}

	// LOGICAL
	if m := logicalDeclPattern.FindStringSubmatch(line); len(m) > 2 {
		intent := m[1]
		names := strings.Split(m[2], ",")
		for _, name := range names {
			name = strings.TrimSpace(name)
			dt := &ir.DataType{
				Name:     strings.ToUpper(name),
				Kind:     ir.TypeBoolean,
				Metadata: map[string]string{},
			}
			if intent != "" {
				dt.Metadata["intent"] = strings.ToLower(intent)
			}
			if isDimensioned {
				dt = wrapAsArray(dt, line)
			}
			types = append(types, dt)
		}
		return types
	}

	// CHARACTER
	if m := characterDeclPattern.FindStringSubmatch(line); len(m) > 4 {
		size := 1
		if m[1] != "" {
			size, _ = strconv.Atoi(m[1])
		} else if m[2] != "" && m[2] != "*" {
			size, _ = strconv.Atoi(m[2])
		} else if m[2] == "*" {
			size = -1 // Variable length
		}
		intent := m[3]
		names := strings.Split(m[4], ",")
		for _, name := range names {
			name = strings.TrimSpace(name)
			dt := &ir.DataType{
				Name:     strings.ToUpper(name),
				Kind:     ir.TypeString,
				Size:     size,
				Metadata: map[string]string{},
			}
			if intent != "" {
				dt.Metadata["intent"] = strings.ToLower(intent)
			}
			if isDimensioned {
				dt = wrapAsArray(dt, line)
			}
			types = append(types, dt)
		}
		return types
	}

	// TYPE (user-defined)
	if m := typeDeclPattern.FindStringSubmatch(line); len(m) > 3 {
		typeName := m[1]
		intent := m[2]
		names := strings.Split(m[3], ",")
		for _, name := range names {
			name = strings.TrimSpace(name)
			dt := &ir.DataType{
				Name:     strings.ToUpper(name),
				Kind:     ir.TypeStruct,
				Metadata: map[string]string{"fortran_type": typeName},
			}
			if intent != "" {
				dt.Metadata["intent"] = strings.ToLower(intent)
			}
			if isDimensioned {
				dt = wrapAsArray(dt, line)
			}
			types = append(types, dt)
		}
		return types
	}

	return nil
}

func wrapAsArray(dt *ir.DataType, line string) *ir.DataType {
	if m := dimensionPattern.FindStringSubmatch(line); len(m) > 1 {
		return &ir.DataType{
			Name:        dt.Name,
			Kind:        ir.TypeArray,
			ElementType: dt,
			Metadata:    map[string]string{"dimensions": m[1]},
		}
	}
	return dt
}

func parseIOStatement(line string, functionName string) *ir.IOContract {
	upper := strings.ToUpper(line)

	// OPEN statement
	if openPattern.MatchString(upper) {
		return &ir.IOContract{
			Name:      functionName + "_file_open",
			Kind:      ir.IOFile,
			Direction: ir.IOReadWrite,
			Metadata:  map[string]string{"function": functionName, "operation": "open"},
		}
	}

	// READ statement
	if readPattern.MatchString(upper) {
		kind := ir.IOFile
		if strings.Contains(upper, "READ(") && strings.Contains(upper, "*") {
			kind = ir.IOScreen // stdin
		}
		return &ir.IOContract{
			Name:      functionName + "_read",
			Kind:      kind,
			Direction: ir.IORead,
			Metadata:  map[string]string{"function": functionName},
		}
	}

	// WRITE statement
	if writePattern.MatchString(upper) {
		kind := ir.IOFile
		if strings.Contains(upper, "WRITE(") && strings.Contains(upper, "*") {
			kind = ir.IOScreen // stdout
		}
		return &ir.IOContract{
			Name:      functionName + "_write",
			Kind:      kind,
			Direction: ir.IOWrite,
			Metadata:  map[string]string{"function": functionName},
		}
	}

	// PRINT statement
	if printPattern.MatchString(upper) {
		return &ir.IOContract{
			Name:      functionName + "_print",
			Kind:      ir.IOScreen,
			Direction: ir.IOWrite,
			Metadata:  map[string]string{"function": functionName},
		}
	}

	return nil
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
