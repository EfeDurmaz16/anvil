package stringutil

import (
	"testing"
)

func TestToPascalCase(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"empty", "", "Generated"},
		{"single word", "hello", "Hello"},
		{"hyphenated", "hello-world", "HelloWorld"},
		{"underscored", "hello_world", "HelloWorld"},
		{"spaced", "hello world", "HelloWorld"},
		{"dotted", "hello.world", "HelloWorld"},
		{"mixed separators", "foo-bar_baz.qux", "FooBarBazQux"},
		{"already pascal", "HelloWorld", "Helloworld"},
		{"uppercase word", "HELLO_WORLD", "HelloWorld"},
		{"multiple separators", "a--b__c", "ABC"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := ToPascalCase(tc.input)
			if got != tc.want {
				t.Errorf("ToPascalCase(%q) = %q, want %q", tc.input, got, tc.want)
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
		{"empty", "", "unnamed"},
		{"single word", "Hello", "hello"},
		{"hyphenated", "hello-world", "helloWorld"},
		{"underscored", "hello_world", "helloWorld"},
		{"spaced", "hello world", "helloWorld"},
		{"dotted", "hello.world", "helloWorld"},
		{"mixed separators", "foo-bar_baz", "fooBarBaz"},
		{"uppercase", "HELLO_WORLD", "helloWorld"},
		{"three words", "one-two-three", "oneTwoThree"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := ToCamelCase(tc.input)
			if got != tc.want {
				t.Errorf("ToCamelCase(%q) = %q, want %q", tc.input, got, tc.want)
			}
		})
	}
}

func TestToSnakeCase(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"empty", "", ""},
		{"single word", "hello", "hello"},
		{"hyphenated", "hello-world", "hello_world"},
		{"underscored", "hello_world", "hello_world"},
		{"spaced", "hello world", "hello_world"},
		{"dotted", "hello.world", "hello_world"},
		{"mixed separators", "foo-bar_baz", "foo_bar_baz"},
		{"uppercase", "HELLO_WORLD", "hello_world"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := ToSnakeCase(tc.input)
			if got != tc.want {
				t.Errorf("ToSnakeCase(%q) = %q, want %q", tc.input, got, tc.want)
			}
		})
	}
}

func TestToKebabCase(t *testing.T) {
	tests := []struct {
		name  string
		input string
		want  string
	}{
		{"empty", "", ""},
		{"single word", "hello", "hello"},
		{"hyphenated", "hello-world", "hello-world"},
		{"underscored", "hello_world", "hello-world"},
		{"spaced", "hello world", "hello-world"},
		{"dotted", "hello.world", "hello-world"},
		{"mixed separators", "foo-bar_baz", "foo-bar-baz"},
		{"uppercase", "HELLO_WORLD", "hello-world"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := ToKebabCase(tc.input)
			if got != tc.want {
				t.Errorf("ToKebabCase(%q) = %q, want %q", tc.input, got, tc.want)
			}
		})
	}
}

func TestDedup(t *testing.T) {
	tests := []struct {
		name  string
		input []string
		want  []string
	}{
		{"nil input", nil, nil},
		{"empty input", []string{}, nil},
		{"no duplicates", []string{"a", "b", "c"}, []string{"a", "b", "c"}},
		{"all duplicates", []string{"a", "a", "a"}, []string{"a"}},
		{"mixed", []string{"a", "b", "a", "c", "b"}, []string{"a", "b", "c"}},
		{"preserves order", []string{"c", "b", "a"}, []string{"c", "b", "a"}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			got := Dedup(tc.input)
			if len(got) != len(tc.want) {
				t.Errorf("Dedup(%v) = %v, want %v", tc.input, got, tc.want)
				return
			}
			for i := range got {
				if got[i] != tc.want[i] {
					t.Errorf("Dedup(%v)[%d] = %q, want %q", tc.input, i, got[i], tc.want[i])
				}
			}
		})
	}
}
