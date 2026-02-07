package java

import (
	"context"
	"fmt"
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
				content, err = generateWithLLM(ctx, provider, m, className)
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

// stripThinkingTags removes <think>...</think> blocks from LLM output (e.g. qwen3).
func stripThinkingTags(s string) string {
	for {
		start := -1
		tag := "<think>"
		for i := 0; i <= len(s)-len(tag); i++ {
			if s[i:i+len(tag)] == tag {
				start = i
				break
			}
		}
		if start == -1 {
			break
		}
		endTag := "</think>"
		end := -1
		for i := start; i <= len(s)-len(endTag); i++ {
			if s[i:i+len(endTag)] == endTag {
				end = i
				break
			}
		}
		if end == -1 {
			s = trimSpace(s[:start])
			break
		}
		s = s[:start] + s[end+len(endTag):]
	}
	return trimSpace(s)
}

// stripMarkdownFences removes markdown code fences from LLM output.
func stripMarkdownFences(s string) string {
	s = stripThinkingTags(s)
	lines := make([]string, 0)
	for _, line := range splitLines(s) {
		lines = append(lines, line)
	}

	// Find and remove leading fence
	start := 0
	for i, line := range lines {
		trimmed := trimSpace(line)
		if hasPrefix(trimmed, "```") {
			start = i + 1
			break
		}
	}

	// Find and remove trailing fence
	end := len(lines)
	for i := len(lines) - 1; i >= start; i-- {
		trimmed := trimSpace(lines[i])
		if hasPrefix(trimmed, "```") {
			end = i
			break
		}
	}

	// If no fences found, return original
	if start == 0 && end == len(lines) {
		return s
	}

	return joinLines(lines[start:end])
}

func splitLines(s string) []string {
	result := []string{}
	start := 0
	for i := 0; i < len(s); i++ {
		if s[i] == '\n' {
			result = append(result, s[start:i])
			start = i + 1
		}
	}
	if start < len(s) {
		result = append(result, s[start:])
	}
	return result
}

func joinLines(lines []string) string {
	result := ""
	for i, line := range lines {
		if i > 0 {
			result += "\n"
		}
		result += line
	}
	return result
}

func trimSpace(s string) string {
	start := 0
	end := len(s)
	for start < end && (s[start] == ' ' || s[start] == '\t' || s[start] == '\n' || s[start] == '\r') {
		start++
	}
	for end > start && (s[end-1] == ' ' || s[end-1] == '\t' || s[end-1] == '\n' || s[end-1] == '\r') {
		end--
	}
	return s[start:end]
}

func hasPrefix(s, prefix string) bool {
	return len(s) >= len(prefix) && s[:len(prefix)] == prefix
}

func generateWithLLM(ctx context.Context, provider llm.Provider, mod *ir.Module, className string) (string, error) {
	prompt := &llm.Prompt{
		SystemPrompt: "You are a legacy-to-Java migration expert. Convert the given module to a Java Spring Boot service class. Output only the Java source code, no explanation.",
		Messages: []llm.Message{
			{Role: llm.RoleUser, Content: fmt.Sprintf("Convert this module to Java:\nModule: %s\nFunctions: %v\nGenerate class name: %s", mod.Name, functionSummary(mod), className)},
		},
	}
	resp, err := provider.Complete(ctx, prompt, nil)
	if err != nil {
		return "", err
	}
	return stripMarkdownFences(resp.Content), nil
}

func functionSummary(mod *ir.Module) string {
	var parts []string
	for _, fn := range mod.Functions {
		parts = append(parts, fmt.Sprintf("%s (calls: %v)", fn.Name, fn.Calls))
	}
	return fmt.Sprintf("[%s]", joinStrings(parts, ", "))
}

func joinStrings(s []string, sep string) string {
	result := ""
	for i, v := range s {
		if i > 0 {
			result += sep
		}
		result += v
	}
	return result
}
