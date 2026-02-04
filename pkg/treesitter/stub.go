//go:build !cgo || !treesitter

package treesitter

import "fmt"

// Parser is a stub when tree-sitter support is not enabled.
type Parser struct{}

// Node is a stub when tree-sitter support is not enabled.
type Node struct{}

// NewParser returns an error unless built with `-tags=treesitter` and CGO enabled.
func NewParser(language string) (*Parser, error) {
	return nil, fmt.Errorf("treesitter disabled (build with -tags=treesitter and CGO enabled); requested language: %s", language)
}

func (p *Parser) Parse(_ []byte) (*Node, error) {
	return nil, fmt.Errorf("treesitter disabled (build with -tags=treesitter and CGO enabled)")
}

func (p *Parser) Close() {}

func (n *Node) Type() string         { return "" }
func (n *Node) Text(_ []byte) string { return "" }
func (n *Node) ChildCount() int      { return 0 }
func (n *Node) Child(_ int) *Node    { return &Node{} }
func (n *Node) NamedChildCount() int { return 0 }
func (n *Node) NamedChild(_ int) *Node {
	return &Node{}
}
