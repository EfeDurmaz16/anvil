package cobol

import (
	"regexp"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/ir"
)

var (
	// Paragraph: starts in area A (col 8-11), name followed by period
	// Matches: "       0000-MAIN.", "       ADD-NUMBERS.", "       YYYY-STORE-PFKEY-EXIT."
	paragraphPattern = regexp.MustCompile(`^.{6}\s{1,4}([\w][\w-]*)\.\s*$`)

	// Section: name followed by SECTION.
	sectionPattern = regexp.MustCompile(`(?i)^.{6}\s{1,4}([\w][\w-]*)\s+SECTION\.\s*$`)

	// PERFORM variants
	performSimple = regexp.MustCompile(`(?i)\bPERFORM\s+([\w-]+)`)
	performThru   = regexp.MustCompile(`(?i)\bPERFORM\s+([\w-]+)\s+THRU\s+([\w-]+)`)
	performUntil  = regexp.MustCompile(`(?i)\bPERFORM\s+([\w-]+)\s+UNTIL\b`)
	performVarying = regexp.MustCompile(`(?i)\bPERFORM\s+([\w-]+)\s+VARYING\b`)

	// CALL 'program-name'
	callPattern = regexp.MustCompile(`(?i)\bCALL\s+['"]?([\w-]+)['"]?`)

	// GO TO
	gotoPattern = regexp.MustCompile(`(?i)\bGO\s+TO\s+([\w-]+)`)

	// EXEC blocks
	execCICSStart = regexp.MustCompile(`(?i)\bEXEC\s+CICS\b`)
	execSQLStart  = regexp.MustCompile(`(?i)\bEXEC\s+SQL\b`)
	execEnd       = regexp.MustCompile(`(?i)\bEND-EXEC\b`)

	// EVALUATE (COBOL's switch/case)
	evaluateStart = regexp.MustCompile(`(?i)\bEVALUATE\b`)
	evaluateEnd   = regexp.MustCompile(`(?i)\bEND-EVALUATE\b`)
	whenPattern   = regexp.MustCompile(`(?i)\bWHEN\b`)

	// I/O patterns
	displayPattern = regexp.MustCompile(`(?i)\bDISPLAY\b`)
	readPattern    = regexp.MustCompile(`(?i)\bREAD\s+([\w-]+)`)
	writePattern   = regexp.MustCompile(`(?i)\bWRITE\s+([\w-]+)`)
	openPattern    = regexp.MustCompile(`(?i)\bOPEN\s+(INPUT|OUTPUT|I-O|EXTEND)\s+([\w-]+)`)
	closePattern   = regexp.MustCompile(`(?i)\bCLOSE\s+([\w-]+)`)
)

func parseProcedureDivision(lines []string) ([]*ir.Function, []*ir.IOContract) {
	var functions []*ir.Function
	var ioContracts []*ir.IOContract
	inProc := false

	var currentFn *ir.Function
	var bodyLines []string
	inExecBlock := false
	execType := ""

	flush := func() {
		if currentFn != nil {
			currentFn.Body = strings.Join(bodyLines, "\n")
			// Deduplicate calls
			currentFn.Calls = dedup(currentFn.Calls)
			functions = append(functions, currentFn)
		}
	}

	for _, line := range lines {
		// Handle fixed-format COBOL: column 7 is indicator area
		if len(line) >= 7 && (line[6] == '*' || line[6] == '/') {
			continue // comment line
		}

		upper := strings.ToUpper(strings.TrimSpace(line))
		if upper == "" {
			continue
		}

		if strings.Contains(upper, "PROCEDURE DIVISION") {
			inProc = true
			continue
		}
		if !inProc {
			continue
		}

		// Track EXEC blocks
		if execCICSStart.MatchString(upper) {
			inExecBlock = true
			execType = "CICS"
		} else if execSQLStart.MatchString(upper) {
			inExecBlock = true
			execType = "SQL"
		}
		if execEnd.MatchString(upper) && inExecBlock {
			// Record the EXEC block as metadata on the current function
			if currentFn != nil {
				count := currentFn.Metadata["exec_"+strings.ToLower(execType)+"_count"]
				if count == "" {
					count = "0"
				}
				n := atoi(count) + 1
				currentFn.Metadata["exec_"+strings.ToLower(execType)+"_count"] = itoa(n)
			}
			inExecBlock = false
			execType = ""
		}

		// Detect section headers (before paragraph check)
		if m := sectionPattern.FindStringSubmatch(line); len(m) > 1 {
			flush()
			name := strings.TrimSpace(m[1])
			currentFn = &ir.Function{
				Name:     name,
				Metadata: map[string]string{"kind": "section"},
			}
			bodyLines = nil
			continue
		}

		// Detect paragraph headers
		if m := paragraphPattern.FindStringSubmatch(line); len(m) > 1 {
			name := strings.TrimSpace(m[1])
			// Skip if it looks like a COBOL keyword
			if isKeyword(name) {
				goto collectBody
			}
			flush()
			currentFn = &ir.Function{
				Name:     name,
				Metadata: map[string]string{"kind": "paragraph"},
			}
			bodyLines = nil
			continue
		}

	collectBody:
		if currentFn == nil {
			// Lines before first paragraph → synthetic MAIN
			currentFn = &ir.Function{Name: "MAIN", Metadata: map[string]string{"kind": "paragraph"}}
		}

		bodyLines = append(bodyLines, line)

		// Extract calls — PERFORM THRU first (more specific)
		if m := performThru.FindStringSubmatch(upper); len(m) > 2 {
			currentFn.Calls = append(currentFn.Calls, m[1])
			currentFn.Metadata["perform_thru_"+m[1]] = m[2]
		} else if m := performUntil.FindStringSubmatch(upper); len(m) > 1 {
			currentFn.Calls = append(currentFn.Calls, m[1])
		} else if m := performVarying.FindStringSubmatch(upper); len(m) > 1 {
			currentFn.Calls = append(currentFn.Calls, m[1])
		} else if m := performSimple.FindStringSubmatch(upper); len(m) > 1 {
			target := m[1]
			// Filter out inline PERFORM (PERFORM UNTIL, PERFORM VARYING without target)
			if target != "UNTIL" && target != "VARYING" && target != "WITH" {
				currentFn.Calls = append(currentFn.Calls, target)
			}
		}

		// CALL 'external-program'
		if m := callPattern.FindStringSubmatch(upper); len(m) > 1 {
			currentFn.Calls = append(currentFn.Calls, "CALL:"+m[1])
		}

		// GO TO
		if m := gotoPattern.FindStringSubmatch(upper); len(m) > 1 {
			currentFn.Calls = append(currentFn.Calls, "GOTO:"+m[1])
			if currentFn.Metadata["has_goto"] == "" {
				currentFn.Metadata["has_goto"] = "true"
			}
		}

		// EVALUATE/WHEN — track complexity
		if evaluateStart.MatchString(upper) {
			n := atoi(currentFn.Metadata["evaluate_count"]) + 1
			currentFn.Metadata["evaluate_count"] = itoa(n)
		}
		if whenPattern.MatchString(upper) && !strings.Contains(upper, "WHEN OTHER") {
			n := atoi(currentFn.Metadata["when_count"]) + 1
			currentFn.Metadata["when_count"] = itoa(n)
		}

		// I/O detection
		extractIO(upper, currentFn, &ioContracts)
	}

	flush()
	return functions, ioContracts
}

func extractIO(upper string, fn *ir.Function, ioContracts *[]*ir.IOContract) {
	if fn == nil {
		return
	}

	// DISPLAY → screen output
	if displayPattern.MatchString(upper) {
		*ioContracts = append(*ioContracts, &ir.IOContract{
			Name:      fn.Name + "_display",
			Kind:      ir.IOScreen,
			Direction: ir.IOWrite,
		})
	}

	// READ file-name
	if m := readPattern.FindStringSubmatch(upper); len(m) > 1 {
		*ioContracts = append(*ioContracts, &ir.IOContract{
			Name:      m[1],
			Kind:      ir.IOFile,
			Direction: ir.IORead,
			Metadata:  map[string]string{"function": fn.Name},
		})
	}

	// WRITE record-name
	if m := writePattern.FindStringSubmatch(upper); len(m) > 1 {
		*ioContracts = append(*ioContracts, &ir.IOContract{
			Name:      m[1],
			Kind:      ir.IOFile,
			Direction: ir.IOWrite,
			Metadata:  map[string]string{"function": fn.Name},
		})
	}

	// OPEN INPUT/OUTPUT file
	if m := openPattern.FindStringSubmatch(upper); len(m) > 2 {
		dir := ir.IORead
		if strings.Contains(m[1], "OUTPUT") {
			dir = ir.IOWrite
		} else if strings.Contains(m[1], "I-O") {
			dir = ir.IOReadWrite
		}
		*ioContracts = append(*ioContracts, &ir.IOContract{
			Name:      m[2],
			Kind:      ir.IOFile,
			Direction: dir,
			Metadata:  map[string]string{"function": fn.Name},
		})
	}

	// CLOSE
	if m := closePattern.FindStringSubmatch(upper); len(m) > 1 {
		*ioContracts = append(*ioContracts, &ir.IOContract{
			Name:      m[1],
			Kind:      ir.IOFile,
			Direction: ir.IOReadWrite,
			Metadata:  map[string]string{"function": fn.Name, "operation": "close"},
		})
	}

	// EXEC CICS SEND/RECEIVE → screen I/O (CICS terminal)
	if strings.Contains(upper, "EXEC CICS") {
		if strings.Contains(upper, "SEND") {
			*ioContracts = append(*ioContracts, &ir.IOContract{
				Name:      fn.Name + "_cics_send",
				Kind:      ir.IOScreen,
				Direction: ir.IOWrite,
				Metadata:  map[string]string{"cics": "true"},
			})
		}
		if strings.Contains(upper, "RECEIVE") {
			*ioContracts = append(*ioContracts, &ir.IOContract{
				Name:      fn.Name + "_cics_receive",
				Kind:      ir.IOScreen,
				Direction: ir.IORead,
				Metadata:  map[string]string{"cics": "true"},
			})
		}
	}

	// EXEC SQL → database I/O
	if strings.Contains(upper, "EXEC SQL") {
		dir := ir.IORead
		if strings.Contains(upper, "INSERT") || strings.Contains(upper, "UPDATE") || strings.Contains(upper, "DELETE") {
			dir = ir.IOWrite
		}
		*ioContracts = append(*ioContracts, &ir.IOContract{
			Name:      fn.Name + "_sql",
			Kind:      ir.IODB,
			Direction: dir,
			Metadata:  map[string]string{"sql": "true"},
		})
	}
}

func dedup(ss []string) []string {
	seen := make(map[string]bool)
	var out []string
	for _, s := range ss {
		if !seen[s] {
			seen[s] = true
			out = append(out, s)
		}
	}
	return out
}

var cobolKeywords = map[string]bool{
	"IF": true, "ELSE": true, "END-IF": true, "PERFORM": true,
	"MOVE": true, "ADD": true, "SUBTRACT": true, "MULTIPLY": true,
	"DIVIDE": true, "COMPUTE": true, "DISPLAY": true, "ACCEPT": true,
	"READ": true, "WRITE": true, "OPEN": true, "CLOSE": true,
	"EVALUATE": true, "END-EVALUATE": true, "WHEN": true,
	"SEARCH": true, "SET": true, "GO": true, "STOP": true,
	"EXIT": true, "INITIALIZE": true, "STRING": true, "UNSTRING": true,
	"INSPECT": true, "CALL": true, "EXEC": true, "END-EXEC": true,
	"COPY": true, "REPLACING": true, "WORKING-STORAGE": true,
	"FILE": true, "LINKAGE": true, "LOCAL-STORAGE": true,
}

func isKeyword(name string) bool {
	return cobolKeywords[strings.ToUpper(name)]
}

func atoi(s string) int {
	n := 0
	for _, c := range s {
		if c >= '0' && c <= '9' {
			n = n*10 + int(c-'0')
		}
	}
	return n
}

func itoa(n int) string {
	if n == 0 {
		return "0"
	}
	s := ""
	for n > 0 {
		s = string(rune('0'+n%10)) + s
		n /= 10
	}
	return s
}
