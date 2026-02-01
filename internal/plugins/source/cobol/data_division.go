package cobol

import (
	"regexp"
	"strconv"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/ir"
)

var picPattern = regexp.MustCompile(`PIC\s+(S?9+(?:\((\d+)\))?(?:V9+(?:\((\d+)\))?)?)`)

func parseDataDivision(lines []string) []*ir.DataType {
	var types []*ir.DataType
	inData := false

	for _, line := range lines {
		upper := strings.ToUpper(strings.TrimSpace(line))

		if strings.Contains(upper, "DATA DIVISION") {
			inData = true
			continue
		}
		if strings.Contains(upper, "PROCEDURE DIVISION") {
			break
		}
		if !inData {
			continue
		}

		trimmed := strings.TrimSpace(line)
		if trimmed == "" || strings.HasPrefix(trimmed, "*") {
			continue
		}

		// Parse level-number + name + PIC clause
		parts := strings.Fields(trimmed)
		if len(parts) < 2 {
			continue
		}

		level, err := strconv.Atoi(parts[0])
		if err != nil {
			continue
		}

		name := strings.TrimSuffix(parts[1], ".")

		dt := &ir.DataType{
			Name:     name,
			Metadata: map[string]string{"level": strconv.Itoa(level)},
		}

		if matches := picPattern.FindStringSubmatch(upper); len(matches) > 0 {
			dt.Kind, dt.Size, dt.Scale = picToType(matches[0])
		} else if level == 1 || level == 5 {
			dt.Kind = ir.TypeStruct
		} else {
			dt.Kind = ir.TypeUnknown
		}

		types = append(types, dt)
	}

	return types
}

func picToType(pic string) (ir.TypeKind, int, int) {
	upper := strings.ToUpper(pic)

	if strings.Contains(upper, "X") {
		size := countPicDigits(upper, "X")
		return ir.TypeString, size, 0
	}
	if strings.Contains(upper, "V") {
		intPart := countPicDigits(upper[:strings.Index(upper, "V")], "9")
		decPart := countPicDigits(upper[strings.Index(upper, "V"):], "9")
		return ir.TypeDecimal, intPart + decPart, decPart
	}
	if strings.Contains(upper, "9") {
		size := countPicDigits(upper, "9")
		return ir.TypeInteger, size, 0
	}
	return ir.TypeUnknown, 0, 0
}

func countPicDigits(pic, char string) int {
	// Handle both repeated chars and (n) notation
	re := regexp.MustCompile(char + `\((\d+)\)`)
	if m := re.FindStringSubmatch(pic); len(m) > 1 {
		n, _ := strconv.Atoi(m[1])
		return n
	}
	return strings.Count(pic, char)
}
