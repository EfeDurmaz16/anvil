package cobol

import (
	"regexp"
	"strconv"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/ir"
)

var (
	picClause   = regexp.MustCompile(`(?i)\bPIC(?:TURE)?\s+IS\s+|(?i)\bPIC(?:TURE)?\s+`)
	picValue    = regexp.MustCompile(`[SV9AX()\-.]+`)
	compClause  = regexp.MustCompile(`(?i)\b(COMP(?:-[1-5])?|BINARY|PACKED-DECIMAL)\b`)
	occursClause = regexp.MustCompile(`(?i)\bOCCURS\s+(\d+)\b`)
	valueClause = regexp.MustCompile(`(?i)\bVALUE\s+(.+?)\.?\s*$`)
	copyStmt    = regexp.MustCompile(`(?i)\bCOPY\s+([\w-]+)`)
	redefines   = regexp.MustCompile(`(?i)\bREDEFINES\s+([\w-]+)`)
)

func parseDataDivision(lines []string) []*ir.DataType {
	var types []*ir.DataType
	var copyRefs []string
	inData := false

	for _, line := range lines {
		// Handle fixed-format comment
		if len(line) >= 7 && (line[6] == '*' || line[6] == '/') {
			continue
		}

		upper := strings.ToUpper(strings.TrimSpace(line))

		if strings.Contains(upper, "DATA DIVISION") ||
			strings.Contains(upper, "WORKING-STORAGE SECTION") ||
			strings.Contains(upper, "FILE SECTION") ||
			strings.Contains(upper, "LINKAGE SECTION") ||
			strings.Contains(upper, "LOCAL-STORAGE SECTION") {
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
		if trimmed == "" {
			continue
		}

		// Detect COPY statements
		if m := copyStmt.FindStringSubmatch(upper); len(m) > 1 {
			copyRefs = append(copyRefs, m[1])
			continue
		}

		// Parse level-number + name
		parts := strings.Fields(trimmed)
		if len(parts) < 2 {
			continue
		}

		level, err := strconv.Atoi(strings.TrimSuffix(parts[0], "."))
		if err != nil {
			continue
		}

		name := strings.TrimSuffix(parts[1], ".")

		// Skip FILLER
		if strings.EqualFold(name, "FILLER") {
			name = "FILLER"
		}

		dt := &ir.DataType{
			Name:     name,
			Metadata: map[string]string{"level": strconv.Itoa(level)},
		}

		// 88-level = boolean condition
		if level == 88 {
			dt.Kind = ir.TypeBoolean
			if m := valueClause.FindStringSubmatch(upper); len(m) > 1 {
				dt.Metadata["values"] = strings.TrimSpace(m[1])
			}
			types = append(types, dt)
			continue
		}

		// 66-level RENAMES — track but type unknown
		if level == 66 {
			dt.Kind = ir.TypeUnknown
			dt.Metadata["renames"] = "true"
			types = append(types, dt)
			continue
		}

		// Check for PIC clause
		if loc := picClause.FindStringIndex(upper); loc != nil {
			rest := upper[loc[1]:]
			if m := picValue.FindString(rest); m != "" {
				dt.Kind, dt.Size, dt.Scale = picToType(m)
			}
		}

		// Check for COMP/BINARY/PACKED-DECIMAL
		if m := compClause.FindStringSubmatch(upper); len(m) > 1 {
			comp := strings.ToUpper(m[1])
			dt.Metadata["usage"] = comp
			// COMP-3/PACKED-DECIMAL with PIC 9 → typically decimal
			if (comp == "COMP-3" || comp == "PACKED-DECIMAL") && dt.Kind == ir.TypeInteger {
				// keep as integer unless it has V (scale > 0)
			}
			// COMP/BINARY → always integer/long
			if comp == "COMP" || comp == "BINARY" || comp == "COMP-4" || comp == "COMP-5" {
				if dt.Kind == "" || dt.Kind == ir.TypeUnknown {
					dt.Kind = ir.TypeInteger
				}
			}
		}

		// Check for OCCURS (array)
		if m := occursClause.FindStringSubmatch(upper); len(m) > 1 {
			n, _ := strconv.Atoi(m[1])
			dt.Metadata["occurs"] = strconv.Itoa(n)
			// Wrap the type as an array
			if dt.Kind != "" && dt.Kind != ir.TypeUnknown {
				dt.ElementType = &ir.DataType{
					Name: name + "-element",
					Kind: dt.Kind,
					Size: dt.Size,
				}
				dt.Kind = ir.TypeArray
			}
		}

		// Check for REDEFINES
		if m := redefines.FindStringSubmatch(upper); len(m) > 1 {
			dt.Metadata["redefines"] = m[1]
		}

		// Check for VALUE clause
		if m := valueClause.FindStringSubmatch(upper); len(m) > 1 {
			dt.Metadata["value"] = strings.TrimSpace(m[1])
		}

		// Group items (no PIC) at common levels
		if dt.Kind == "" || dt.Kind == ir.TypeUnknown {
			if level == 1 || level == 5 || level == 01 || level == 05 {
				dt.Kind = ir.TypeStruct
			} else if dt.Kind == "" {
				dt.Kind = ir.TypeStruct // group items without PIC are structs
			}
		}

		types = append(types, dt)
	}

	// Record COPY references in metadata
	if len(copyRefs) > 0 {
		for _, dt := range types {
			if dt.Metadata["level"] == "1" || dt.Metadata["level"] == "01" {
				dt.Metadata["copy_refs"] = strings.Join(copyRefs, ",")
				break
			}
		}
	}

	return types
}

func picToType(pic string) (ir.TypeKind, int, int) {
	upper := strings.ToUpper(pic)

	if strings.Contains(upper, "X") {
		size := countPicChars(upper, 'X')
		return ir.TypeString, size, 0
	}
	if strings.Contains(upper, "A") {
		size := countPicChars(upper, 'A')
		return ir.TypeString, size, 0
	}
	if strings.Contains(upper, "V") {
		idx := strings.Index(upper, "V")
		intPart := countPicChars(upper[:idx], '9')
		decPart := countPicChars(upper[idx:], '9')
		return ir.TypeDecimal, intPart + decPart, decPart
	}
	if strings.Contains(upper, "9") {
		size := countPicChars(upper, '9')
		return ir.TypeInteger, size, 0
	}
	return ir.TypeUnknown, 0, 0
}

// countPicChars counts the effective number of characters in a PIC clause,
// handling both repeated chars (999) and parenthesized notation (9(5)).
func countPicChars(pic string, ch byte) int {
	total := 0
	i := 0
	for i < len(pic) {
		if pic[i] == ch {
			// Check if followed by (n)
			if i+1 < len(pic) && pic[i+1] == '(' {
				end := strings.Index(pic[i+1:], ")")
				if end > 1 {
					n, err := strconv.Atoi(pic[i+2 : i+1+end])
					if err == nil {
						total += n
						i = i + 1 + end + 1
						continue
					}
				}
			}
			total++
		}
		i++
	}
	return total
}
