package cobol

import (
	"regexp"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/ir"
)

var (
	paragraphPattern = regexp.MustCompile(`^(\s{0,6}\S[\w-]+)\.\s*$`)
	performPattern   = regexp.MustCompile(`(?i)PERFORM\s+([\w-]+)`)
	displayPattern   = regexp.MustCompile(`(?i)DISPLAY\s+(.+)`)
)

func parseProcedureDivision(lines []string) ([]*ir.Function, []*ir.IOContract) {
	var functions []*ir.Function
	var ioContracts []*ir.IOContract
	inProc := false

	var currentFn *ir.Function
	var bodyLines []string

	flush := func() {
		if currentFn != nil {
			currentFn.Body = strings.Join(bodyLines, "\n")
			functions = append(functions, currentFn)
		}
	}

	for _, line := range lines {
		upper := strings.ToUpper(strings.TrimSpace(line))

		if strings.Contains(upper, "PROCEDURE DIVISION") {
			inProc = true
			continue
		}
		if !inProc {
			continue
		}

		trimmed := strings.TrimSpace(line)

		// Detect paragraph/section headers
		if m := paragraphPattern.FindStringSubmatch(line); len(m) > 1 {
			flush()
			name := strings.TrimSpace(m[1])
			currentFn = &ir.Function{
				Name:     name,
				Metadata: map[string]string{},
			}
			bodyLines = nil
			continue
		}

		if currentFn == nil {
			// Lines before the first paragraph go into a main function
			if trimmed != "" && !strings.HasPrefix(trimmed, "*") {
				if currentFn == nil {
					currentFn = &ir.Function{Name: "MAIN", Metadata: map[string]string{}}
				}
			} else {
				continue
			}
		}

		bodyLines = append(bodyLines, line)

		// Extract PERFORM calls
		if m := performPattern.FindStringSubmatch(upper); len(m) > 1 {
			currentFn.Calls = append(currentFn.Calls, m[1])
		}

		// Detect DISPLAY → screen I/O
		if displayPattern.MatchString(upper) {
			ioContracts = append(ioContracts, &ir.IOContract{
				Name:      currentFn.Name + "_display",
				Kind:      ir.IOScreen,
				Direction: ir.IOWrite,
			})
		}

		// Detect file I/O
		if strings.Contains(upper, "READ ") || strings.Contains(upper, "WRITE ") || strings.Contains(upper, "OPEN ") {
			dir := ir.IORead
			if strings.Contains(upper, "WRITE ") {
				dir = ir.IOWrite
			}
			ioContracts = append(ioContracts, &ir.IOContract{
				Name:      currentFn.Name + "_file",
				Kind:      ir.IOFile,
				Direction: dir,
			})
		}
	}

	flush()
	return functions, ioContracts
}
