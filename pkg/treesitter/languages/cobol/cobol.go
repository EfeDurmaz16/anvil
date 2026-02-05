//go:build cgo && treesitter
// +build cgo,treesitter

package cobol

/*
#cgo LDFLAGS: -ltree-sitter
#include <tree_sitter/api.h>
extern const TSLanguage *tree_sitter_cobol(void);
*/
import "C"
import (
	"unsafe"

	"github.com/efebarandurmaz/anvil/pkg/treesitter"
)

func init() {
	treesitter.Register("cobol", func() uintptr {
		return uintptr(unsafe.Pointer(C.tree_sitter_cobol()))
	})
}
