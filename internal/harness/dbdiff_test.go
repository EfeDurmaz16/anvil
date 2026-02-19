package harness

import (
	"testing"
)

func TestIsValidIdentifier(t *testing.T) {
	valid := []string{"users", "my_table", "Table1", "_private"}
	invalid := []string{"", "1abc", "my-table", "table; DROP", "name space", "table.name"}

	for _, name := range valid {
		if !isValidIdentifier(name) {
			t.Errorf("expected %q to be valid identifier", name)
		}
	}
	for _, name := range invalid {
		if isValidIdentifier(name) {
			t.Errorf("expected %q to be invalid identifier", name)
		}
	}
}

func TestQuoteIdentifier(t *testing.T) {
	tests := []struct {
		input string
		want  string
	}{
		{"users", `"users"`},
		{"my_table", `"my_table"`},
		{`has"quote`, `"has""quote"`},
	}
	for _, tt := range tests {
		got := quoteIdentifier(tt.input)
		if got != tt.want {
			t.Errorf("quoteIdentifier(%q) = %q, want %q", tt.input, got, tt.want)
		}
	}
}

func TestDBDiffCompare_NoChanges(t *testing.T) {
	config := &DBDiffConfig{CriticalTables: []string{"test"}}
	differ := NewDBDiffer(config)

	snapshot := &DBSnapshot{
		Tables: map[string]*TableSnapshot{
			"test": {
				Name:     "test",
				RowCount: 2,
				Rows: map[string]map[string]interface{}{
					"1": {"id": 1, "name": "Alice"},
					"2": {"id": 2, "name": "Bob"},
				},
			},
		},
	}

	report := differ.Compare(snapshot, snapshot)
	if report.TotalViolations != 0 {
		t.Errorf("expected 0 violations, got %d", report.TotalViolations)
	}
}

func TestDBDiffCompare_InsertedRow(t *testing.T) {
	config := &DBDiffConfig{CriticalTables: []string{"test"}}
	differ := NewDBDiffer(config)

	before := &DBSnapshot{
		Tables: map[string]*TableSnapshot{
			"test": {
				Name:     "test",
				RowCount: 1,
				Rows: map[string]map[string]interface{}{
					"1": {"id": 1, "name": "Alice"},
				},
			},
		},
	}

	after := &DBSnapshot{
		Tables: map[string]*TableSnapshot{
			"test": {
				Name:     "test",
				RowCount: 2,
				Rows: map[string]map[string]interface{}{
					"1": {"id": 1, "name": "Alice"},
					"2": {"id": 2, "name": "Bob"},
				},
			},
		},
	}

	report := differ.Compare(before, after)
	tableDiff := report.Tables["test"]
	if len(tableDiff.InsertedRows) != 1 {
		t.Errorf("expected 1 inserted row, got %d", len(tableDiff.InsertedRows))
	}
}

func TestDBDiffCompare_ModifiedRow(t *testing.T) {
	config := &DBDiffConfig{CriticalTables: []string{"test"}}
	differ := NewDBDiffer(config)

	before := &DBSnapshot{
		Tables: map[string]*TableSnapshot{
			"test": {
				Name:     "test",
				RowCount: 1,
				Rows: map[string]map[string]interface{}{
					"1": {"id": 1, "name": "Alice"},
				},
			},
		},
	}

	after := &DBSnapshot{
		Tables: map[string]*TableSnapshot{
			"test": {
				Name:     "test",
				RowCount: 1,
				Rows: map[string]map[string]interface{}{
					"1": {"id": 1, "name": "Alicia"},
				},
			},
		},
	}

	report := differ.Compare(before, after)
	tableDiff := report.Tables["test"]
	if len(tableDiff.ModifiedRows) != 1 {
		t.Errorf("expected 1 modified row, got %d", len(tableDiff.ModifiedRows))
	}
	if report.TotalViolations != 1 {
		t.Errorf("expected 1 violation, got %d", report.TotalViolations)
	}
}

func TestDBDiffCompare_NormalizedColumn(t *testing.T) {
	config := &DBDiffConfig{
		CriticalTables:   []string{"test"},
		NormalizeColumns: []string{"*.updated_at"},
	}
	differ := NewDBDiffer(config)

	before := &DBSnapshot{
		Tables: map[string]*TableSnapshot{
			"test": {
				Name:     "test",
				RowCount: 1,
				Rows: map[string]map[string]interface{}{
					"1": {"id": 1, "updated_at": "2024-01-01"},
				},
			},
		},
	}

	after := &DBSnapshot{
		Tables: map[string]*TableSnapshot{
			"test": {
				Name:     "test",
				RowCount: 1,
				Rows: map[string]map[string]interface{}{
					"1": {"id": 1, "updated_at": "2024-01-02"},
				},
			},
		},
	}

	report := differ.Compare(before, after)
	// Normalized columns should not count as violations
	if report.TotalViolations != 0 {
		t.Errorf("expected 0 violations for normalized column, got %d", report.TotalViolations)
	}
}
