package cobol

import (
	"path/filepath"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

func parseCopybook(f plugins.SourceFile) []*ir.DataType {
	name := strings.TrimSuffix(filepath.Base(f.Path), filepath.Ext(f.Path))
	lines := strings.Split(string(f.Content), "\n")

	fields := parseDataDivision(lines)

	return []*ir.DataType{{
		Name:   strings.ToUpper(name),
		Kind:   ir.TypeStruct,
		Fields: fields,
		Metadata: map[string]string{
			"source": "copybook",
			"path":   f.Path,
		},
	}}
}
