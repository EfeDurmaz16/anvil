package java

import (
	"fmt"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/ir"
)

func generateFromTemplate(mod *ir.Module, className string) string {
	var methods strings.Builder
	for _, fn := range mod.Functions {
		methods.WriteString(fmt.Sprintf(`
    public void %s() {
        // TODO: Migrated from source paragraph %s
        System.out.println("Executing %s");
    }
`, toCamelCase(fn.Name), fn.Name, fn.Name))
	}

	return fmt.Sprintf(`package com.anvil.generated;

import org.springframework.stereotype.Service;

@Service
public class %s {
%s}
`, className, methods.String())
}

func generateTypeClass(dt *ir.DataType) string {
	className := toClassName(dt.Name)
	var fields strings.Builder
	var gettersSetters strings.Builder

	for _, f := range dt.Fields {
		jType := mapType(f)
		fieldName := toCamelCase(f.Name)
		fields.WriteString(fmt.Sprintf("    private %s %s;\n", jType, fieldName))
		gettersSetters.WriteString(fmt.Sprintf(`
    public %s get%s() { return %s; }
    public void set%s(%s %s) { this.%s = %s; }
`, jType, toClassName(f.Name), fieldName, toClassName(f.Name), jType, fieldName, fieldName, fieldName))
	}

	return fmt.Sprintf(`package com.anvil.generated.model;

public class %s {
%s
%s}
`, className, fields.String(), gettersSetters.String())
}

func toClassName(name string) string {
	parts := strings.FieldsFunc(name, func(r rune) bool {
		return r == '-' || r == '_' || r == ' '
	})
	var result string
	for _, p := range parts {
		if len(p) > 0 {
			result += strings.ToUpper(p[:1]) + strings.ToLower(p[1:])
		}
	}
	if result == "" {
		return "Generated"
	}
	return result
}

func toCamelCase(name string) string {
	cls := toClassName(name)
	if len(cls) == 0 {
		return "unnamed"
	}
	return strings.ToLower(cls[:1]) + cls[1:]
}
