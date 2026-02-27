package java

import (
	"fmt"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/stringutil"
)

func generateFromTemplate(mod *ir.Module, className string) string {
	var methods strings.Builder
	for _, fn := range mod.Functions {
		methods.WriteString(fmt.Sprintf(`
    public void %s() {
        // TODO: Migrated from source paragraph %s
        System.out.println("Executing %s");
    }
`, stringutil.ToCamelCase(fn.Name), fn.Name, fn.Name))
	}

	return fmt.Sprintf(`package com.anvil.generated;

import org.springframework.stereotype.Service;

@Service
public class %s {
%s}
`, className, methods.String())
}

func generateTypeClass(dt *ir.DataType) string {
	className := stringutil.ToPascalCase(dt.Name)
	var fields strings.Builder
	var gettersSetters strings.Builder

	for _, f := range dt.Fields {
		jType := mapType(f)
		fieldName := stringutil.ToCamelCase(f.Name)
		fields.WriteString(fmt.Sprintf("    private %s %s;\n", jType, fieldName))
		gettersSetters.WriteString(fmt.Sprintf(`
    public %s get%s() { return %s; }
    public void set%s(%s %s) { this.%s = %s; }
`, jType, stringutil.ToPascalCase(f.Name), fieldName, stringutil.ToPascalCase(f.Name), jType, fieldName, fieldName, fieldName))
	}

	return fmt.Sprintf(`package com.anvil.generated.model;

public class %s {
%s
%s}
`, className, fields.String(), gettersSetters.String())
}


