package typescript

import "testing"

func TestStripMarkdownFences(t *testing.T) {
	tests := []struct {
		name     string
		input    string
		expected string
	}{
		{
			name:  "typescript fence",
			input: "```typescript\nfunction hello() {\n  return \"world\";\n}\n```",
			expected: `function hello() {
  return "world";
}`,
		},
		{
			name:     "ts fence",
			input:    "```ts\nconst x = 42;\n```",
			expected: "const x = 42;",
		},
		{
			name: "generic fence",
			input: "```\nsome code\nmore code\n```",
			expected: "some code\nmore code",
		},
		{
			name:     "no fence",
			input:    "plain code\nno fences here",
			expected: "plain code\nno fences here",
		},
		{
			name:     "empty string",
			input:    "",
			expected: "",
		},
		{
			name:     "fence with extra whitespace",
			input:    "   ```typescript\ncode here\n   ```   ",
			expected: "code here",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			result := stripMarkdownFences(tt.input)
			if result != tt.expected {
				t.Errorf("stripMarkdownFences() = %q, want %q", result, tt.expected)
			}
		})
	}
}
