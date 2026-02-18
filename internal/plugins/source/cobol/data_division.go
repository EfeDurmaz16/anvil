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
	redefines   = regexp.MustCompile(`(?i)\bREDEFINES\s+([\w-]+)`)
)

// stackEntry holds a level number and its associated DataType for the hierarchy stack.
type stackEntry struct {
	level int
	dt    *ir.DataType
}

func parseDataDivision(lines []string) []*ir.DataType {
	var topLevel []*ir.DataType
	inData := false

	// stack holds (level, *DataType) pairs for building the hierarchy
	var stack []stackEntry

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

		// 88-level = boolean condition — always child of the immediately preceding item
		if level == 88 {
			dt.Kind = ir.TypeBoolean
			if m := valueClause.FindStringSubmatch(upper); len(m) > 1 {
				dt.Metadata["values"] = strings.TrimSpace(m[1])
			}
			if len(stack) > 0 {
				parent := stack[len(stack)-1].dt
				parent.Fields = append(parent.Fields, dt)
			} else {
				topLevel = append(topLevel, dt)
			}
			// 88-level items are not pushed onto the stack
			continue
		}

		// 66-level RENAMES — track but type unknown; treat as top-level sibling
		if level == 66 {
			dt.Kind = ir.TypeUnknown
			dt.Metadata["renames"] = "true"
			// Pop stack fully and add as top-level (or sibling of last 01)
			stack = nil
			topLevel = append(topLevel, dt)
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

		// Group items (no PIC) are structs
		if dt.Kind == "" || dt.Kind == ir.TypeUnknown {
			dt.Kind = ir.TypeStruct
		}

		// Stack-based hierarchy:
		// Pop all entries with level >= current level
		for len(stack) > 0 && stack[len(stack)-1].level >= level {
			stack = stack[:len(stack)-1]
		}

		if level == 1 || level == 77 {
			// 01-level and 77-level items are always top-level standalone roots
			stack = nil
			topLevel = append(topLevel, dt)
			stack = append(stack, stackEntry{level: level, dt: dt})
		} else {
			if len(stack) > 0 {
				// Attach as child of the current top of stack
				parent := stack[len(stack)-1].dt
				parent.Fields = append(parent.Fields, dt)
				// Upgrade parent to struct if it had a concrete type (group item with children)
				if parent.Kind != ir.TypeStruct && parent.Kind != ir.TypeArray {
					parent.Kind = ir.TypeStruct
				}
			} else {
				// No parent found; treat as top-level
				topLevel = append(topLevel, dt)
			}
			stack = append(stack, stackEntry{level: level, dt: dt})
		}
	}

	return topLevel
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
