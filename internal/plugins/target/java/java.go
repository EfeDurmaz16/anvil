package java

import (
	"context"
	"fmt"
	"strings"
	"sync"

	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// Plugin implements TargetPlugin for Java Spring Boot.
type Plugin struct{}

func New() *Plugin { return &Plugin{} }

func (p *Plugin) Language() string { return "java" }

func (p *Plugin) Generate(ctx context.Context, graph *ir.SemanticGraph, provider llm.Provider) ([]plugins.GeneratedFile, error) {
	var files []plugins.GeneratedFile

	// Generate modules concurrently with worker pool
	const maxConcurrent = 1
	sem := make(chan struct{}, maxConcurrent)
	var wg sync.WaitGroup
	var mu sync.Mutex

	moduleFiles := make([]plugins.GeneratedFile, len(graph.Modules))

	for i, mod := range graph.Modules {
		wg.Add(1)
		sem <- struct{}{} // acquire
		go func(idx int, m *ir.Module) {
			defer wg.Done()
			defer func() { <-sem }() // release

			className := toClassName(m.Name)
			var content string

			if provider != nil {
				var err error
				content, err = generateWithLLM(ctx, provider, m, className, graph)
				if err != nil {
					// Fall back to template-based generation
					content = generateFromTemplate(m, className)
				}
			} else {
				content = generateFromTemplate(m, className)
			}

			mu.Lock()
			moduleFiles[idx] = plugins.GeneratedFile{
				Path:    fmt.Sprintf("src/main/java/com/anvil/generated/%s.java", className),
				Content: []byte(content),
			}
			mu.Unlock()
		}(i, mod)
	}
	wg.Wait()

	// Add module files in order
	files = append(files, moduleFiles...)

	// Generate type classes
	for _, dt := range graph.DataTypes {
		if dt.Kind == ir.TypeStruct {
			content := generateTypeClass(dt)
			files = append(files, plugins.GeneratedFile{
				Path:    fmt.Sprintf("src/main/java/com/anvil/generated/model/%s.java", toClassName(dt.Name)),
				Content: []byte(content),
			})
		}
	}

	return files, nil
}

func (p *Plugin) Scaffold(ctx context.Context, graph *ir.SemanticGraph) ([]plugins.GeneratedFile, error) {
	return []plugins.GeneratedFile{
		{Path: "anvil.manifest.json", Content: []byte(anvilManifestJSON)},
		{Path: "pom.xml", Content: []byte(pomXML)},
		{Path: "src/main/java/com/anvil/generated/Application.java", Content: []byte(applicationJava)},
		{Path: "src/main/java/com/anvil/generated/AnvilRunner.java", Content: []byte(anvilRunnerJava)},
	}, nil
}

// stripMarkdownFences removes markdown code fences from LLM output.
func stripMarkdownFences(s string) string {
	s = llm.StripThinkingTags(s)
	lines := strings.Split(s, "\n")

	// Find and remove leading fence
	start := 0
	for i, line := range lines {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "```") {
			start = i + 1
			break
		}
	}

	// Find and remove trailing fence
	end := len(lines)
	for i := len(lines) - 1; i >= start; i-- {
		trimmed := strings.TrimSpace(lines[i])
		if strings.HasPrefix(trimmed, "```") {
			end = i
			break
		}
	}

	// If no fences found, return original
	if start == 0 && end == len(lines) {
		return s
	}

	return strings.Join(lines[start:end], "\n")
}

func generateWithLLM(ctx context.Context, provider llm.Provider, mod *ir.Module, className string, graph *ir.SemanticGraph) (string, error) {
	srcLang := mod.Language
	if srcLang == "" {
		srcLang = "legacy"
	}

	moduleCtx := buildModuleContext(mod, className, graph)

	prompt := &llm.Prompt{
		SystemPrompt: fmt.Sprintf(`You are a %s to Java Spring Boot migration expert.

CRITICAL OUTPUT FORMAT RULES:
- Output a complete, compilable Java class including the package declaration and all imports
- Annotate the class with @Service (default), or @RestController if it handles HTTP, or @Repository if it handles persistence
- Use @Autowired for dependency injection where applicable
- Preserve all business logic from the original %s source code exactly
- Add brief Javadoc comments for each method explaining its business purpose
- Do NOT wrap the output in markdown code fences
- Do NOT include any explanation or prose outside the Java source

Spring Boot annotation guidance:
- @Service: business logic classes (most common)
- @RestController + @RequestMapping: HTTP endpoint handlers
- @Repository: database access classes
- @Autowired: inject dependencies
- @Transactional: methods that modify persistent state
- Use BigDecimal for decimal arithmetic to preserve precision`, srcLang, srcLang),
		Messages: []llm.Message{
			{Role: llm.RoleUser, Content: fmt.Sprintf("Convert this %s module to a Java Spring Boot class:\n\n%s", srcLang, moduleCtx)},
		},
	}
	resp, err := provider.Complete(ctx, prompt, nil)
	if err != nil {
		return "", err
	}
	raw := stripMarkdownFences(resp.Content)
	return sanitizeLLMOutput(raw), nil
}

// buildModuleContext assembles a rich context string for the LLM including
// function bodies, business rules, data types, and parameter information.
func buildModuleContext(mod *ir.Module, className string, graph *ir.SemanticGraph) string {
	srcLang := mod.Language
	if srcLang == "" {
		srcLang = "legacy"
	}

	var b strings.Builder

	b.WriteString(fmt.Sprintf("Module: %s\n", mod.Name))
	b.WriteString(fmt.Sprintf("Target class name: %s\n", className))
	b.WriteString(fmt.Sprintf("Source language: %s\n", srcLang))

	// IO contracts hint for annotation selection
	if len(mod.IOContracts) > 0 {
		b.WriteString("I/O contracts:\n")
		for _, io := range mod.IOContracts {
			b.WriteString(fmt.Sprintf("  - %s (%s, %s)\n", io.Name, io.Kind, io.Direction))
		}
	}

	// Collect business rules relevant to this module
	var moduleRules []*ir.BusinessRule
	if graph != nil {
		for _, rule := range graph.BusinessRules {
			if rule == nil {
				continue
			}
			// Match rules that reference this module by name prefix
			if strings.HasPrefix(rule.SourceRef, mod.Name+".") || rule.SourceRef == mod.Name {
				moduleRules = append(moduleRules, rule)
			}
		}
	}
	if len(moduleRules) > 0 {
		b.WriteString("\nBusiness Rules for this module:\n")
		for _, rule := range moduleRules {
			b.WriteString(fmt.Sprintf("  - [%.0f%% confidence] %s\n", rule.Confidence*100, rule.Description))
		}
	}

	// Collect data types referenced by any function in this module
	if graph != nil && len(graph.DataTypes) > 0 {
		relevant := collectRelevantDataTypes(mod, graph.DataTypes)
		if len(relevant) > 0 {
			b.WriteString("\nData structures referenced by this module:\n")
			for _, dt := range relevant {
				b.WriteString(fmt.Sprintf("  %s (struct):\n", dt.Name))
				for _, f := range dt.Fields {
					if f == nil {
						continue
					}
					b.WriteString(fmt.Sprintf("    - %s: %s\n", f.Name, describeDataType(f)))
				}
			}
		}
	}

	// Functions with full bodies
	b.WriteString(fmt.Sprintf("\nFunctions (%d total):\n", len(mod.Functions)))
	for _, fn := range mod.Functions {
		b.WriteString(fmt.Sprintf("\n--- Function: %s ---\n", fn.Name))

		if len(fn.Parameters) > 0 {
			b.WriteString("Parameters:\n")
			for _, p := range fn.Parameters {
				b.WriteString(fmt.Sprintf("  - %s: %s\n", p.Name, describeDataType(p.Type)))
			}
		}

		if fn.Returns != nil {
			b.WriteString(fmt.Sprintf("Returns: %s\n", describeDataType(fn.Returns)))
		}

		if len(fn.Calls) > 0 {
			b.WriteString(fmt.Sprintf("Calls: %s\n", strings.Join(fn.Calls, ", ")))
		}

		// Business rules scoped to this function
		if graph != nil {
			ref := fmt.Sprintf("%s.%s", mod.Name, fn.Name)
			for _, rule := range graph.BusinessRules {
				if rule != nil && rule.SourceRef == ref {
					b.WriteString(fmt.Sprintf("Business Rule: %s (confidence: %.2f)\n", rule.Description, rule.Confidence))
				}
			}
		}

		if fn.Body != "" {
			b.WriteString(fmt.Sprintf("Original %s body:\n%s\n", srcLang, fn.Body))
		} else {
			b.WriteString("(no body available)\n")
		}
	}

	return b.String()
}

// collectRelevantDataTypes returns data types from the global graph that are
// referenced by any function body or parameter in the given module.
func collectRelevantDataTypes(mod *ir.Module, allTypes []*ir.DataType) []*ir.DataType {
	seen := make(map[string]bool)
	var result []*ir.DataType

	for _, fn := range mod.Functions {
		bodyUpper := strings.ToUpper(fn.Body)
		for _, dt := range allTypes {
			if dt == nil || dt.Kind != ir.TypeStruct || len(dt.Fields) == 0 {
				continue
			}
			if seen[dt.Name] {
				continue
			}
			if fn.Body != "" && strings.Contains(bodyUpper, strings.ToUpper(dt.Name)) {
				seen[dt.Name] = true
				result = append(result, dt)
				continue
			}
			// Also check parameters
			for _, p := range fn.Parameters {
				if p.Type != nil && strings.EqualFold(p.Type.Name, dt.Name) {
					seen[dt.Name] = true
					result = append(result, dt)
					break
				}
			}
		}
	}

	// Limit to avoid overwhelming the LLM
	if len(result) > 8 {
		result = result[:8]
	}
	return result
}

// sanitizeLLMOutput strips stray package/import lines that duplicate what the
// template already provides, and removes trailing prose.
func sanitizeLLMOutput(s string) string {
	lines := strings.Split(s, "\n")
	var cleaned []string
	inCode := true
	for _, line := range lines {
		trimmed := strings.TrimSpace(line)

		// Stop at trailing prose lines
		if inCode && (strings.HasPrefix(trimmed, "This class") ||
			strings.HasPrefix(trimmed, "This code") ||
			strings.HasPrefix(trimmed, "Note:") ||
			strings.HasPrefix(trimmed, "The above") ||
			strings.HasPrefix(trimmed, "Here is") ||
			strings.HasPrefix(trimmed, "Here's")) {
			inCode = false
		}
		if !inCode {
			continue
		}
		cleaned = append(cleaned, line)
	}

	// Trim leading/trailing blank lines
	for len(cleaned) > 0 && strings.TrimSpace(cleaned[0]) == "" {
		cleaned = cleaned[1:]
	}
	for len(cleaned) > 0 && strings.TrimSpace(cleaned[len(cleaned)-1]) == "" {
		cleaned = cleaned[:len(cleaned)-1]
	}

	return strings.Join(cleaned, "\n")
}

// describeDataType creates a human-readable description of an IR data type.
func describeDataType(dt *ir.DataType) string {
	if dt == nil {
		return "unknown"
	}
	switch dt.Kind {
	case ir.TypeString:
		return "String"
	case ir.TypeInteger:
		return "int/long"
	case ir.TypeDecimal:
		return "BigDecimal"
	case ir.TypeBoolean:
		return "boolean"
	case ir.TypeArray:
		return "List<" + describeDataType(dt.ElementType) + ">"
	case ir.TypeStruct:
		return "struct " + dt.Name
	default:
		return "unknown"
	}
}
