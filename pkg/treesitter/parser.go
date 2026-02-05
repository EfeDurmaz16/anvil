//go:build cgo && treesitter
// +build cgo,treesitter

package treesitter

import (
	"fmt"
	"unsafe"
)

/*
#cgo LDFLAGS: -ltree-sitter
#include <tree_sitter/api.h>
*/
import "C"

// Parser wraps a tree-sitter parser for a given language.
type Parser struct {
	parser   *C.TSParser
	language string
}

// Node wraps a tree-sitter node.
type Node struct {
	node C.TSNode
}

// TreeCursor wraps a tree-sitter tree cursor for efficient traversal.
type TreeCursor struct {
	cursor C.TSTreeCursor
}

// NewParser creates a tree-sitter parser for the given language.
// The language must be registered via Register() before calling this.
func NewParser(language string) (*Parser, error) {
	langFn, ok := GetLanguage(language)
	if !ok {
		return nil, fmt.Errorf("unsupported language: %s (not registered)", language)
	}

	p := C.ts_parser_new()
	if p == nil {
		return nil, fmt.Errorf("failed to create parser")
	}

	lang := (*C.TSLanguage)(unsafe.Pointer(langFn()))
	if !C.ts_parser_set_language(p, lang) {
		C.ts_parser_delete(p)
		return nil, fmt.Errorf("incompatible language version for %s", language)
	}

	return &Parser{parser: p, language: language}, nil
}

// Language returns the parser's language name.
func (p *Parser) Language() string { return p.language }

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

// --- Node methods ---

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

// IsNull returns true if the node is null/invalid.
func (n *Node) IsNull() bool {
	return bool(C.ts_node_is_null(n.node))
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

// ChildByFieldName returns a child node by its field name.
// Returns a node where IsNull() is true if the field doesn't exist.
func (n *Node) ChildByFieldName(name string) *Node {
	cName := C.CString(name)
	defer C.free(unsafe.Pointer(cName))
	child := C.ts_node_child_by_field_name(n.node, cName, C.uint(len(name)))
	return &Node{node: child}
}

// StartLine returns the 0-based start line of the node.
func (n *Node) StartLine() int {
	point := C.ts_node_start_point(n.node)
	return int(point.row)
}

// EndLine returns the 0-based end line of the node.
func (n *Node) EndLine() int {
	point := C.ts_node_end_point(n.node)
	return int(point.row)
}

// StartByte returns the start byte offset of the node.
func (n *Node) StartByte() int {
	return int(C.ts_node_start_byte(n.node))
}

// EndByte returns the end byte offset of the node.
func (n *Node) EndByte() int {
	return int(C.ts_node_end_byte(n.node))
}

// --- TreeCursor for efficient traversal ---

// Walk creates a tree cursor starting at this node.
func (n *Node) Walk() *TreeCursor {
	cursor := C.ts_tree_cursor_new(n.node)
	return &TreeCursor{cursor: cursor}
}

// Close releases cursor resources.
func (tc *TreeCursor) Close() {
	C.ts_tree_cursor_delete(&tc.cursor)
}

// Node returns the current cursor node.
func (tc *TreeCursor) Node() *Node {
	node := C.ts_tree_cursor_current_node(&tc.cursor)
	return &Node{node: node}
}

// FieldName returns the field name of the current cursor position.
func (tc *TreeCursor) FieldName() string {
	name := C.ts_tree_cursor_current_field_name(&tc.cursor)
	if name == nil {
		return ""
	}
	return C.GoString(name)
}

// GotoFirstChild moves cursor to the first child. Returns true if successful.
func (tc *TreeCursor) GotoFirstChild() bool {
	return bool(C.ts_tree_cursor_goto_first_child(&tc.cursor))
}

// GotoNextSibling moves cursor to the next sibling. Returns true if successful.
func (tc *TreeCursor) GotoNextSibling() bool {
	return bool(C.ts_tree_cursor_goto_next_sibling(&tc.cursor))
}

// GotoParent moves cursor to the parent. Returns true if successful.
func (tc *TreeCursor) GotoParent() bool {
	return bool(C.ts_tree_cursor_goto_parent(&tc.cursor))
}
