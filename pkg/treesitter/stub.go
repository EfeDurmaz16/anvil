//go:build !cgo || !treesitter
// +build !cgo !treesitter

package treesitter

import "fmt"

// Parser is a stub when tree-sitter support is not enabled.
type Parser struct{}

// Node is a stub when tree-sitter support is not enabled.
type Node struct{}

// TreeCursor is a stub when tree-sitter support is not enabled.
type TreeCursor struct{}

// NewParser returns an error unless built with `-tags=treesitter` and CGO enabled.
func NewParser(language string) (*Parser, error) {
	return nil, fmt.Errorf("treesitter disabled (build with -tags=treesitter and CGO enabled); requested language: %s", language)
}

func (p *Parser) Language() string             { return "" }
func (p *Parser) Parse(_ []byte) (*Node, error) {
	return nil, fmt.Errorf("treesitter disabled")
}
func (p *Parser) Close() {}

func (n *Node) Type() string                    { return "" }
func (n *Node) Text(_ []byte) string            { return "" }
func (n *Node) IsNull() bool                    { return true }
func (n *Node) ChildCount() int                 { return 0 }
func (n *Node) Child(_ int) *Node               { return &Node{} }
func (n *Node) NamedChildCount() int            { return 0 }
func (n *Node) NamedChild(_ int) *Node          { return &Node{} }
func (n *Node) ChildByFieldName(_ string) *Node { return &Node{} }
func (n *Node) StartLine() int                  { return 0 }
func (n *Node) EndLine() int                    { return 0 }
func (n *Node) StartByte() int                  { return 0 }
func (n *Node) EndByte() int                    { return 0 }
func (n *Node) Walk() *TreeCursor               { return &TreeCursor{} }

func (tc *TreeCursor) Close()                  {}
func (tc *TreeCursor) Node() *Node             { return &Node{} }
func (tc *TreeCursor) FieldName() string       { return "" }
func (tc *TreeCursor) GotoFirstChild() bool    { return false }
func (tc *TreeCursor) GotoNextSibling() bool   { return false }
func (tc *TreeCursor) GotoParent() bool        { return false }
