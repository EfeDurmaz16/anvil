// Package defaults registers all built-in source and target plugins.
package defaults

import (
	"github.com/efebarandurmaz/anvil/internal/plugins"
	cobolplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/cobol"
	fortranplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/fortran"
	perlplugin "github.com/efebarandurmaz/anvil/internal/plugins/source/perl"
	goplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/golang"
	javaplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/java"
	pythonplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/python"
	tsplugin "github.com/efebarandurmaz/anvil/internal/plugins/target/typescript"
)

// RegisterAllDefaults registers all built-in source and target plugins into r.
func RegisterAllDefaults(r *plugins.Registry) {
	r.RegisterSource(cobolplugin.New())
	r.RegisterSource(perlplugin.New())
	r.RegisterSource(fortranplugin.New())
	r.RegisterTarget(javaplugin.New())
	r.RegisterTarget(pythonplugin.New())
	r.RegisterTarget(goplugin.New())
	r.RegisterTarget(tsplugin.New())
}
