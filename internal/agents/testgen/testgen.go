package testgen

import (
	"context"
	"fmt"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	"github.com/efebarandurmaz/anvil/internal/stringutil"
)

// TestGen generates test files for the code produced by the Architect agent.
type TestGen struct{}

func New() *TestGen { return &TestGen{} }

func (t *TestGen) Name() string { return "testgen" }

func (t *TestGen) Run(ctx context.Context, ac *agents.AgentContext) (*agents.AgentResult, error) {
	result := agents.NewAgentResult()
	result.Graph = ac.Graph

	if ac.Graph == nil {
		result.Status = agents.StatusFailed
		result.AddError("testgen: no graph provided")
		result.Finalize()
		return result, fmt.Errorf("testgen: no graph provided")
	}

	targetLang := ac.Params["target"]
	result.Metadata["target_language"] = targetLang

	// Get generated files from params (passed as JSON by the pipeline)
	generatedFilesJSON := ac.Params["generated_files"]
	if generatedFilesJSON == "" {
		result.Status = agents.StatusFailed
		result.AddError("testgen: no generated_files in params")
		result.Finalize()
		return result, fmt.Errorf("testgen: no generated_files in params")
	}

	var testFiles []plugins.GeneratedFile

	for _, mod := range ac.Graph.Modules {
		for _, fn := range mod.Functions {
			testFile := generateTestFile(ctx, mod, fn, targetLang, ac.LLM, ac.DefaultOpts)
			if testFile != nil {
				testFiles = append(testFiles, *testFile)
			}
		}
	}

	// If no LLM, generate stub tests
	if len(testFiles) == 0 {
		testFiles = generateStubTests(ac.Graph, targetLang)
	}

	result.GeneratedFiles = testFiles
	result.Status = agents.StatusSuccess
	result.Metrics.InputItems = countFunctions(ac.Graph)
	result.Metrics.OutputItems = len(testFiles)
	result.Metadata["test_files"] = fmt.Sprintf("%d", len(testFiles))

	result.Finalize()
	return result, nil
}

func countFunctions(graph *ir.SemanticGraph) int {
	total := 0
	for _, mod := range graph.Modules {
		total += len(mod.Functions)
	}
	return total
}

func generateTestFile(ctx context.Context, mod *ir.Module, fn *ir.Function, targetLang string, provider llm.Provider, opts *llm.RequestOptions) *plugins.GeneratedFile {
	if provider == nil {
		return nil // Will use stub generation instead
	}

	testCode := generateTestWithLLM(ctx, mod, fn, targetLang, provider, opts)
	if testCode == "" {
		return nil
	}

	path := testFilePath(mod.Name, fn.Name, targetLang)
	return &plugins.GeneratedFile{
		Path:    path,
		Content: []byte(testCode),
	}
}

func generateTestWithLLM(ctx context.Context, mod *ir.Module, fn *ir.Function, targetLang string, provider llm.Provider, opts *llm.RequestOptions) string {
	// Build test generation context
	var context_ strings.Builder
	context_.WriteString(fmt.Sprintf("Module: %s\n", mod.Name))
	context_.WriteString(fmt.Sprintf("Function: %s\n", fn.Name))
	context_.WriteString(fmt.Sprintf("Target Language: %s\n", targetLang))

	if fn.Body != "" {
		context_.WriteString(fmt.Sprintf("Original Source Body:\n%s\n", fn.Body))
	}

	if len(fn.Parameters) > 0 {
		context_.WriteString("Parameters:\n")
		for _, p := range fn.Parameters {
			context_.WriteString(fmt.Sprintf("  - %s: %s\n", p.Name, p.Type.Kind))
		}
	}

	prompt := &llm.Prompt{
		SystemPrompt: fmt.Sprintf(`You are a test generation expert. Generate comprehensive unit tests for a %s function that was migrated from legacy code.
Rules:
1. Test all normal execution paths
2. Test edge cases and boundary conditions
3. Test error handling paths
4. Use the standard testing framework for the target language
5. Include descriptive test names
6. Return ONLY the complete test file contents`, targetLang),
		Messages: []llm.Message{
			{Role: llm.RoleUser, Content: fmt.Sprintf("Generate tests for:\n\n%s", context_.String())},
		},
	}

	resp, err := provider.Complete(ctx, prompt, opts)
	if err != nil {
		return ""
	}

	return strings.TrimSpace(resp.Content)
}

// generateStubTests creates template test files when no LLM is available.
func generateStubTests(graph *ir.SemanticGraph, targetLang string) []plugins.GeneratedFile {
	var files []plugins.GeneratedFile

	for _, mod := range graph.Modules {
		content := generateStubTestContent(mod, targetLang)
		path := testFilePath(mod.Name, "", targetLang)
		files = append(files, plugins.GeneratedFile{
			Path:    path,
			Content: []byte(content),
		})
	}

	return files
}

func generateStubTestContent(mod *ir.Module, targetLang string) string {
	switch targetLang {
	case "typescript":
		return generateTypeScriptStubTests(mod)
	case "python":
		return generatePythonStubTests(mod)
	case "java":
		return generateJavaStubTests(mod)
	case "go":
		return generateGoStubTests(mod)
	default:
		return generateTypeScriptStubTests(mod)
	}
}

func generateTypeScriptStubTests(mod *ir.Module) string {
	svcName := stringutil.ToPascalCase(mod.Name) + "Service"
	var b strings.Builder
	b.WriteString("/* Generated by Anvil TestGen. */\n")
	b.WriteString(fmt.Sprintf("import { %s } from \"../generated/%s\";\n\n", svcName, stringutil.ToKebabCase(mod.Name)))
	b.WriteString(fmt.Sprintf("describe(\"%s\", () => {\n", svcName))
	b.WriteString(fmt.Sprintf("  let service: %s;\n\n", svcName))
	b.WriteString("  beforeEach(() => {\n")
	b.WriteString(fmt.Sprintf("    service = new %s();\n", svcName))
	b.WriteString("  });\n\n")

	for _, fn := range mod.Functions {
		methodName := stringutil.ToCamelCase(fn.Name)
		b.WriteString(fmt.Sprintf("  describe(\"%s\", () => {\n", methodName))
		b.WriteString(fmt.Sprintf("    it(\"should execute without error\", () => {\n"))
		b.WriteString(fmt.Sprintf("      // TODO: Add test assertions for %s\n", fn.Name))
		b.WriteString(fmt.Sprintf("      expect(() => service.%s()).not.toThrow();\n", methodName))
		b.WriteString("    });\n")
		b.WriteString("  });\n\n")
	}

	b.WriteString("});\n")
	return b.String()
}

func generatePythonStubTests(mod *ir.Module) string {
	className := stringutil.ToPascalCase(mod.Name) + "Service"
	modFile := stringutil.ToSnakeCase(mod.Name)
	var b strings.Builder
	b.WriteString("# Generated by Anvil TestGen.\n")
	b.WriteString("import pytest\n")
	b.WriteString(fmt.Sprintf("from generated.%s import %s\n\n\n", modFile, className))
	b.WriteString(fmt.Sprintf("class Test%s:\n", className))
	b.WriteString("    def setup_method(self):\n")
	b.WriteString(fmt.Sprintf("        self.service = %s()\n\n", className))

	for _, fn := range mod.Functions {
		methodName := stringutil.ToSnakeCase(fn.Name)
		b.WriteString(fmt.Sprintf("    def test_%s(self):\n", methodName))
		b.WriteString(fmt.Sprintf("        # TODO: Add test assertions for %s\n", fn.Name))
		b.WriteString(fmt.Sprintf("        self.service.%s()\n\n", methodName))
	}

	return b.String()
}

func generateJavaStubTests(mod *ir.Module) string {
	className := stringutil.ToPascalCase(mod.Name) + "Service"
	var b strings.Builder
	b.WriteString("/* Generated by Anvil TestGen. */\n")
	b.WriteString("package generated;\n\n")
	b.WriteString("import org.junit.jupiter.api.BeforeEach;\n")
	b.WriteString("import org.junit.jupiter.api.Test;\n")
	b.WriteString("import static org.junit.jupiter.api.Assertions.*;\n\n")
	b.WriteString(fmt.Sprintf("class %sTest {\n\n", className))
	b.WriteString(fmt.Sprintf("    private %s service;\n\n", className))
	b.WriteString("    @BeforeEach\n")
	b.WriteString("    void setUp() {\n")
	b.WriteString(fmt.Sprintf("        service = new %s();\n", className))
	b.WriteString("    }\n\n")

	for _, fn := range mod.Functions {
		methodName := stringutil.ToCamelCase(fn.Name)
		b.WriteString("    @Test\n")
		b.WriteString(fmt.Sprintf("    void test%s() {\n", stringutil.ToPascalCase(fn.Name)))
		b.WriteString(fmt.Sprintf("        // TODO: Add test assertions for %s\n", fn.Name))
		b.WriteString(fmt.Sprintf("        assertDoesNotThrow(() -> service.%s());\n", methodName))
		b.WriteString("    }\n\n")
	}

	b.WriteString("}\n")
	return b.String()
}

func generateGoStubTests(mod *ir.Module) string {
	var b strings.Builder
	b.WriteString("// Generated by Anvil TestGen.\n")
	b.WriteString("package generated\n\n")
	b.WriteString("import \"testing\"\n\n")

	for _, fn := range mod.Functions {
		testName := "Test" + stringutil.ToPascalCase(fn.Name)
		b.WriteString(fmt.Sprintf("func %s(t *testing.T) {\n", testName))
		b.WriteString(fmt.Sprintf("\t// TODO: Add test assertions for %s\n", fn.Name))
		b.WriteString("\tt.Skip(\"not implemented\")\n")
		b.WriteString("}\n\n")
	}

	return b.String()
}

func testFilePath(modName, fnName, targetLang string) string {
	base := stringutil.ToKebabCase(modName)
	switch targetLang {
	case "typescript":
		return fmt.Sprintf("src/tests/%s.test.ts", base)
	case "python":
		return fmt.Sprintf("tests/test_%s.py", stringutil.ToSnakeCase(modName))
	case "java":
		return fmt.Sprintf("src/test/java/generated/%sServiceTest.java", stringutil.ToPascalCase(modName))
	case "go":
		return fmt.Sprintf("generated/%s_test.go", stringutil.ToSnakeCase(modName))
	default:
		return fmt.Sprintf("tests/%s.test.ts", base)
	}
}
