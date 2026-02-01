package treesitter

import (
	"fmt"
	"unsafe"
)

/*
#cgo LDFLAGS: -ltree-sitter
#include <tree_sitter/api.h>

extern const TSLanguage *tree_sitter_cobol(void);
*/
import "C"

// Parser wraps a tree-sitter parser for a given language.
type Parser struct {
	parser *C.TSParser
}

// Node wraps a tree-sitter node.
type Node struct {
	node C.TSNode
}

// NewParser creates a tree-sitter parser for the given language.
func NewParser(language string) (*Parser, error) {
	p := C.ts_parser_new()
	if p == nil {
		return nil, fmt.Errorf("failed to create parser")
	}

	var lang *C.TSLanguage
	switch language {
	case "cobol":
		lang = C.tree_sitter_cobol()
	default:
		C.ts_parser_delete(p)
		return nil, fmt.Errorf("unsupported language: %s", language)
	}

	if !C.ts_parser_set_language(p, lang) {
		C.ts_parser_delete(p)
		return nil, fmt.Errorf("incompatible language version")
	}

	return &Parser{parser: p}, nil
}

// Parse parses source code and returns the root node.
func (p *Parser) Parse(source []byte) (*Node, error) {
	tree := C.ts_parser_parse_string(p.parser, nil, (*C.char)(unsafe.Pointer(&source[0])), C.uint(len(source)))
	if tree == nil {
		return nil, fmt.Errorf("parse failed")
	}
	root := C.ts_tree_root_node(tree)
	return &Node{node: root}, nil
}

// Close releases parser resources.
func (p *Parser) Close() {
	if p.parser != nil {
		C.ts_parser_delete(p.parser)
		p.parser = nil
	}
}

// Type returns the node's type name.
func (n *Node) Type() string {
	return C.GoString(C.ts_node_type(n.node))
}

// Text extracts the node's text from source.
func (n *Node) Text(source []byte) string {
	start := C.ts_node_start_byte(n.node)
	end := C.ts_node_end_byte(n.node)
	return string(source[start:end])
}

// ChildCount returns the number of children.
func (n *Node) ChildCount() int {
	return int(C.ts_node_child_count(n.node))
}

// Child returns the i-th child node.
func (n *Node) Child(i int) *Node {
	child := C.ts_node_child(n.node, C.uint(i))
	return &Node{node: child}
}

// NamedChildCount returns the number of named children.
func (n *Node) NamedChildCount() int {
	return int(C.ts_node_named_child_count(n.node))
}

// NamedChild returns the i-th named child.
func (n *Node) NamedChild(i int) *Node {
	child := C.ts_node_named_child(n.node, C.uint(i))
	return &Node{node: child}
}
