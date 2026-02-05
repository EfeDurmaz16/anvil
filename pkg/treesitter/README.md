# Tree-sitter Integration for Anvil

This package provides an abstract, language-agnostic tree-sitter integration for the Anvil legacy code modernization platform.

## Architecture

The integration consists of three layers:

### 1. Core API (`parser.go`, `stub.go`, `language.go`)

- **`language.go`**: Language registration system (no build tags, always compiled)
- **`parser.go`**: CGO implementation with `//go:build cgo && treesitter`
- **`stub.go`**: Stub implementation with `//go:build !cgo || !treesitter`

The stub allows the package to compile and be imported even without CGO or tree-sitter support.

### 2. Language Registry

Languages are registered via `Register()` function, typically called from language-specific packages' `init()` functions:

```go
func Register(name string, fn LanguageFunc)
```

This enables a plugin architecture where any source language can be added without modifying core code.

### 3. Language Packages (`languages/*/`)

Each language grammar lives in its own subpackage under `languages/`:

- **`languages/cobol/cobol.go`**: CGO implementation that registers COBOL
- **`languages/cobol/stub.go`**: Stub for non-CGO builds

## Usage

### Basic Parsing

```go
import (
    "github.com/efebarandurmaz/anvil/pkg/treesitter"
    _ "github.com/efebarandurmaz/anvil/pkg/treesitter/languages/cobol" // Register COBOL
)

parser, err := treesitter.NewParser("cobol")
if err != nil {
    // Handle error (e.g., tree-sitter not available)
}
defer parser.Close()

source := []byte("IDENTIFICATION DIVISION.")
root, err := parser.Parse(source)
if err != nil {
    // Handle parse error
}

// Access node information
fmt.Println(root.Type())
fmt.Println(root.StartLine(), root.EndLine())
fmt.Println(root.Text(source))
```

### Tree Traversal

The package provides two traversal methods:

**1. Direct node access (simple but allocates more):**

```go
for i := 0; i < root.ChildCount(); i++ {
    child := root.Child(i)
    fmt.Println(child.Type())
}
```

**2. Tree cursor (efficient for large trees):**

```go
cursor := root.Walk()
defer cursor.Close()

if cursor.GotoFirstChild() {
    for {
        node := cursor.Node()
        fmt.Printf("%s at line %d\n", node.Type(), node.StartLine())

        if !cursor.GotoNextSibling() {
            break
        }
    }
}
```

### Field-based Access

```go
// Get a specific child by field name
identifier := root.ChildByFieldName("name")
if !identifier.IsNull() {
    fmt.Println(identifier.Text(source))
}
```

## API Reference

### Parser

- `NewParser(language string) (*Parser, error)` - Create parser for registered language
- `Language() string` - Get parser's language name
- `Parse(source []byte) (*Node, error)` - Parse source code
- `Close()` - Release parser resources

### Node

- `Type() string` - Node type name
- `Text(source []byte) string` - Extract node text
- `IsNull() bool` - Check if node is null/invalid
- `ChildCount() int` - Number of children
- `Child(i int) *Node` - Get i-th child
- `NamedChildCount() int` - Number of named children
- `NamedChild(i int) *Node` - Get i-th named child
- `ChildByFieldName(name string) *Node` - Get child by field name
- `StartLine() int` - 0-based start line
- `EndLine() int` - 0-based end line
- `StartByte() int` - Start byte offset
- `EndByte() int` - End byte offset
- `Walk() *TreeCursor` - Create cursor for traversal

### TreeCursor

- `Close()` - Release cursor resources
- `Node() *Node` - Get current node
- `FieldName() string` - Get current field name
- `GotoFirstChild() bool` - Move to first child
- `GotoNextSibling() bool` - Move to next sibling
- `GotoParent() bool` - Move to parent

### Language Registry

- `Register(name string, fn LanguageFunc)` - Register a language grammar
- `GetLanguage(name string) (LanguageFunc, bool)` - Look up a language
- `Languages() []string` - List all registered languages

## Adding New Languages

To add support for a new language (e.g., Java):

1. Create `pkg/treesitter/languages/java/java.go`:

```go
//go:build cgo && treesitter
// +build cgo,treesitter

package java

/*
#cgo LDFLAGS: -ltree-sitter
#include <tree_sitter/api.h>
extern const TSLanguage *tree_sitter_java(void);
*/
import "C"
import (
    "unsafe"
    "github.com/efebarandurmaz/anvil/pkg/treesitter"
)

func init() {
    treesitter.Register("java", func() uintptr {
        return uintptr(unsafe.Pointer(C.tree_sitter_java()))
    })
}
```

2. Create `pkg/treesitter/languages/java/stub.go`:

```go
//go:build !cgo || !treesitter
// +build !cgo !treesitter

package java

// This file exists so the package compiles without CGO/tree-sitter.
```

3. Import the language package where needed:

```go
import _ "github.com/efebarandurmaz/anvil/pkg/treesitter/languages/java"
```

## Build Tags

- Build **with** tree-sitter: `go build -tags=treesitter`
- Build **without** tree-sitter: `go build` (default, uses stubs)

## Testing

The package includes tests that work in both modes:

```bash
# Test without tree-sitter (uses stubs)
go test ./pkg/treesitter/...

# Test with tree-sitter (requires CGO and tree-sitter library)
go test -tags=treesitter ./pkg/treesitter/...
```
