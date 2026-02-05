//go:build !cgo || !treesitter
// +build !cgo !treesitter

package cobol

// This file exists so the package compiles without CGO/tree-sitter.
// The COBOL language will simply not be registered.
