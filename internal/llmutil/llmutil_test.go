package llmutil_test

import (
	"testing"

	"github.com/efebarandurmaz/anvil/internal/llmutil"
)

func TestStripThinkingTags(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{
			name:  "no tags",
			input: "just code here",
			want:  "just code here",
		},
		{
			name:  "single think block",
			input: "<think>reasoning here</think>actual output",
			want:  "actual output",
		},
		{
			name:  "think block with whitespace",
			input: "<think>reasoning</think>\n\nactual output",
			want:  "actual output",
		},
		{
			name:  "multiple think blocks",
			input: "<think>first</think>middle<think>second</think>end",
			want:  "middleend",
		},
		{
			name:  "unclosed think tag",
			input: "before<think>unclosed reasoning",
			want:  "before",
		},
		{
			name:  "empty input",
			input: "",
			want:  "",
		},
		{
			name:  "only whitespace after tag removal",
			input: "<think>all reasoning</think>   ",
			want:  "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := llmutil.StripThinkingTags(tt.input)
			if got != tt.want {
				t.Errorf("StripThinkingTags(%q) = %q, want %q", tt.input, got, tt.want)
			}
		})
	}
}

func TestStripMarkdownFences(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{
			name:  "no fences",
			input: "plain code",
			want:  "plain code",
		},
		{
			name:  "typescript fence",
			input: "```typescript\nconst x = 1;\n```",
			want:  "const x = 1;",
		},
		{
			name:  "plain fence",
			input: "```\nsome code\n```",
			want:  "some code",
		},
		{
			name:  "fence with thinking tags",
			input: "<think>reasoning</think>\n```go\nfunc foo() {}\n```",
			want:  "func foo() {}",
		},
		{
			name:  "fence with leading text before opening fence",
			input: "```python\nx = 1\ny = 2\n```",
			want:  "x = 1\ny = 2",
		},
		{
			name:  "empty input",
			input: "",
			want:  "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := llmutil.StripMarkdownFences(tt.input)
			if got != tt.want {
				t.Errorf("StripMarkdownFences(%q) = %q, want %q", tt.input, got, tt.want)
			}
		})
	}
}

func TestSanitizeLLMOutput(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{
			name:  "strips import statement",
			input: "import Decimal from 'decimal.js';\nconst x = 1;",
			want:  "const x = 1;",
		},
		{
			name:  "strips export statement",
			input: "export function foo() {\n  return 1;\n}",
			want:  "function foo() {\n  return 1;\n}",
		},
		{
			name:  "strips require statement",
			input: "const fs = require('fs');\nconst x = 1;",
			want:  "const x = 1;",
		},
		{
			name:  "stops at trailing prose - This",
			input: "const x = 1;\nThis implements the logic.",
			want:  "const x = 1;",
		},
		{
			name:  "stops at trailing prose - Note:",
			input: "return result;\nNote: this handles edge cases",
			want:  "return result;",
		},
		{
			name:  "unwraps function wrapper",
			input: "function myFunc() {\n  const x = 1;\n  return x;\n}",
			want:  "const x = 1;\nreturn x;",
		},
		{
			name:  "unwraps async function wrapper",
			input: "async function myFunc() {\n  await something();\n}",
			want:  "await something();",
		},
		{
			name:  "unwraps class wrapper",
			input: "class MyClass {\n  doSomething() {}\n}",
			want:  "doSomething() {}",
		},
		{
			name:  "plain code no changes",
			input: "const x = new Decimal(this.value);\nreturn x.toNumber();",
			want:  "const x = new Decimal(this.value);\nreturn x.toNumber();",
		},
		{
			name:  "trims leading and trailing blank lines",
			input: "\n\nconst x = 1;\n\n",
			want:  "const x = 1;",
		},
		{
			name:  "empty input",
			input: "",
			want:  "",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := llmutil.SanitizeLLMOutput(tt.input)
			if got != tt.want {
				t.Errorf("SanitizeLLMOutput(%q) = %q, want %q", tt.input, got, tt.want)
			}
		})
	}
}

func TestToPascalCase(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"simple word", "hello", "Hello"},
		{"underscore separated", "hello_world", "HelloWorld"},
		{"dash separated", "hello-world", "HelloWorld"},
		{"space separated", "hello world", "HelloWorld"},
		{"dot separated", "hello.world", "HelloWorld"},
		{"all caps input", "HELLO_WORLD", "HelloWorld"},
		{"mixed separators", "hello_world-foo", "HelloWorldFoo"},
		{"empty string", "", "Generated"},
		{"single underscore", "_", "Generated"},
		{"already pascal", "HelloWorld", "Helloworld"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := llmutil.ToPascalCase(tt.input)
			if got != tt.want {
				t.Errorf("ToPascalCase(%q) = %q, want %q", tt.input, got, tt.want)
			}
		})
	}
}

func TestToCamelCase(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"simple word", "hello", "hello"},
		{"underscore separated", "hello_world", "helloWorld"},
		{"dash separated", "hello-world", "helloWorld"},
		{"space separated", "hello world", "helloWorld"},
		{"dot separated", "hello.world", "helloWorld"},
		{"all caps", "HELLO_WORLD", "helloWorld"},
		{"empty string", "", "unnamed"},
		{"single word uppercase", "HELLO", "hELLO"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := llmutil.ToCamelCase(tt.input)
			if got != tt.want {
				t.Errorf("ToCamelCase(%q) = %q, want %q", tt.input, got, tt.want)
			}
		})
	}
}
