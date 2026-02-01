package java

import (
	"context"
	"fmt"

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

	for _, mod := range graph.Modules {
		className := toClassName(mod.Name)

		if provider != nil {
			content, err := generateWithLLM(ctx, provider, mod, className)
			if err == nil {
				files = append(files, plugins.GeneratedFile{
					Path:    fmt.Sprintf("src/main/java/com/anvil/generated/%s.java", className),
					Content: []byte(content),
				})
				continue
			}
			// Fall back to template-based generation
		}

		content := generateFromTemplate(mod, className)
		files = append(files, plugins.GeneratedFile{
			Path:    fmt.Sprintf("src/main/java/com/anvil/generated/%s.java", className),
			Content: []byte(content),
		})
	}

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
		{Path: "pom.xml", Content: []byte(pomXML)},
		{Path: "src/main/java/com/anvil/generated/Application.java", Content: []byte(applicationJava)},
	}, nil
}

func generateWithLLM(ctx context.Context, provider llm.Provider, mod *ir.Module, className string) (string, error) {
	prompt := &llm.Prompt{
		SystemPrompt: "You are a COBOL-to-Java migration expert. Convert the given COBOL module to a Java Spring Boot service class. Output only the Java source code, no explanation.",
		Messages: []llm.Message{
			{Role: llm.RoleUser, Content: fmt.Sprintf("Convert this COBOL module to Java:\nModule: %s\nFunctions: %v\nGenerate class name: %s", mod.Name, functionSummary(mod), className)},
		},
	}
	resp, err := provider.Complete(ctx, prompt, nil)
	if err != nil {
		return "", err
	}
	return resp.Content, nil
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
