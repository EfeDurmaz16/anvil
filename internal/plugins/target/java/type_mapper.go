package java

import "github.com/efebarandurmaz/anvil/internal/ir"

func mapType(dt *ir.DataType) string {
	switch dt.Kind {
	case ir.TypeString:
		return "String"
	case ir.TypeInteger:
		if dt.Size > 9 {
			return "long"
		}
		return "int"
	case ir.TypeDecimal:
		return "java.math.BigDecimal"
	case ir.TypeBoolean:
		return "boolean"
	case ir.TypeArray:
		if dt.ElementType != nil {
			return "java.util.List<" + mapType(dt.ElementType) + ">"
		}
		return "java.util.List<Object>"
	case ir.TypeStruct:
		return toClassName(dt.Name)
	default:
		return "Object"
	}
}
