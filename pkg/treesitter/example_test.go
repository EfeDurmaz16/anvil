package treesitter_test

import (
	"fmt"

	"github.com/efebarandurmaz/anvil/pkg/treesitter"
	_ "github.com/efebarandurmaz/anvil/pkg/treesitter/languages/cobol" // Register COBOL language
)

// ExampleParser demonstrates how to use the tree-sitter parser
// with the abstract language registration system.
func ExampleParser() {
	// The COBOL language was registered via the blank import above.
	// This will work if built with CGO and -tags=treesitter.

	parser, err := treesitter.NewParser("cobol")
	if err != nil {
		// Without CGO/treesitter, this returns an error
		fmt.Println("Tree-sitter not available")
		return
	}
	defer parser.Close()

	source := []byte("IDENTIFICATION DIVISION.")
	root, err := parser.Parse(source)
	if err != nil {
		fmt.Println("Parse error:", err)
		return
	}

	fmt.Println("Root type:", root.Type())
	fmt.Println("Root text:", root.Text(source))
}

// ExampleTreeCursor demonstrates efficient tree traversal using a cursor.
func ExampleTreeCursor() {
	parser, err := treesitter.NewParser("cobol")
	if err != nil {
		fmt.Println("Tree-sitter not available")
		return
	}
	defer parser.Close()

	source := []byte("IDENTIFICATION DIVISION.")
	root, err := parser.Parse(source)
	if err != nil {
		fmt.Println("Parse error:", err)
		return
	}

	// Create a cursor for efficient traversal
	cursor := root.Walk()
	defer cursor.Close()

	// Traverse the tree
	if cursor.GotoFirstChild() {
		for {
			node := cursor.Node()
			fmt.Printf("Node: %s, Line: %d-%d\n",
				node.Type(), node.StartLine(), node.EndLine())

			if !cursor.GotoNextSibling() {
				break
			}
		}
	}
}

// ExampleLanguages demonstrates listing all registered languages.
func ExampleLanguages() {
	// List all registered languages
	langs := treesitter.Languages()
	fmt.Println("Registered languages:", langs)
}
