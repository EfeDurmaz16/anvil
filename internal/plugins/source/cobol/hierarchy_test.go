package cobol

import (
	"fmt"
	"testing"

	"github.com/efebarandurmaz/anvil/internal/ir"
)

func TestHierarchyScenario(t *testing.T) {
	lines := []string{
		"       DATA DIVISION.",
		"       WORKING-STORAGE SECTION.",
		"       01 CUSTOMER-RECORD.",
		"          05 CUSTOMER-ID        PIC 9(8).",
		"          05 CUSTOMER-NAME.",
		"             10 FIRST-NAME      PIC X(20).",
		"             10 LAST-NAME       PIC X(30).",
		"          05 ACCOUNT-INFO.",
		"             10 ACCOUNT-NUMBER  PIC 9(10).",
		"             10 BALANCE          PIC S9(9)V99.",
	}
	types := parseDataDivision(lines)
	fmt.Printf("Top-level count: %d\n", len(types))
	if len(types) != 1 {
		t.Fatalf("expected 1 top-level type (CUSTOMER-RECORD), got %d: %v", len(types), typeNames(types))
	}
	cr := types[0]
	fmt.Printf("CUSTOMER-RECORD fields: %d\n", len(cr.Fields))
	for _, f := range cr.Fields {
		fmt.Printf("  field: %s kind=%s fields=%d\n", f.Name, f.Kind, len(f.Fields))
		for _, ff := range f.Fields {
			fmt.Printf("    subfield: %s kind=%s\n", ff.Name, ff.Kind)
		}
	}
	if len(cr.Fields) != 3 {
		t.Errorf("expected 3 fields on CUSTOMER-RECORD (CUSTOMER-ID, CUSTOMER-NAME, ACCOUNT-INFO), got %d", len(cr.Fields))
	}
	// CUSTOMER-NAME should be a struct with 2 children
	custName := findDT(cr.Fields, "CUSTOMER-NAME")
	if custName == nil {
		t.Fatal("expected CUSTOMER-NAME field")
	}
	if custName.Kind != ir.TypeStruct {
		t.Errorf("expected CUSTOMER-NAME to be struct, got %s", custName.Kind)
	}
	if len(custName.Fields) != 2 {
		t.Errorf("expected 2 children of CUSTOMER-NAME, got %d", len(custName.Fields))
	}
	// ACCOUNT-INFO should be a struct with 2 children
	acctInfo := findDT(cr.Fields, "ACCOUNT-INFO")
	if acctInfo == nil {
		t.Fatal("expected ACCOUNT-INFO field")
	}
	if len(acctInfo.Fields) != 2 {
		t.Errorf("expected 2 children of ACCOUNT-INFO, got %d", len(acctInfo.Fields))
	}
}

func typeNames(types []*ir.DataType) []string {
	var names []string
	for _, dt := range types {
		names = append(names, dt.Name)
	}
	return names
}

func findDT(types []*ir.DataType, name string) *ir.DataType {
	for _, dt := range types {
		if dt.Name == name {
			return dt
		}
	}
	return nil
}

func TestLevel77Standalone(t *testing.T) {
	lines := []string{
		"       DATA DIVISION.",
		"       WORKING-STORAGE SECTION.",
		"       01 WS-RECORD.",
		"          05 WS-FIELD PIC X(10).",
		"       77 WS-STANDALONE PIC 9(5).",
		"       01 WS-RECORD2.",
		"          05 WS-FIELD2 PIC X(5).",
	}
	types := parseDataDivision(lines)
	fmt.Printf("Types: %d\n", len(types))
	for _, t2 := range types {
		fmt.Printf("  type: %s level=%s\n", t2.Name, t2.Metadata["level"])
	}
	// 77-level is standalone top-level
	found77 := false
	for _, dt := range types {
		if dt.Name == "WS-STANDALONE" {
			found77 = true
		}
	}
	if !found77 {
		t.Error("expected WS-STANDALONE (level 77) as top-level")
	}
}
