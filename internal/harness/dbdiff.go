package harness

import (
	"context"
	"crypto/sha256"
	"database/sql"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"regexp"
	"sort"
	"strings"
	"time"
)

// DBDiffConfig specifies how to compare database state.
type DBDiffConfig struct {
	// CriticalTables lists tables to compare.
	CriticalTables []string `json:"critical_tables"`

	// NormalizeColumns lists columns to normalize during comparison.
	// Format: "table.column" or "*.column" for all tables.
	NormalizeColumns []string `json:"normalize_columns,omitempty"`

	// ConnectionString is the database connection string.
	ConnectionString string `json:"connection_string,omitempty"`
}

// DBSnapshot captures the state of critical tables at a point in time.
type DBSnapshot struct {
	Timestamp time.Time                 `json:"timestamp"`
	Tables    map[string]*TableSnapshot `json:"tables"`
	Hash      string                    `json:"hash"`
}

// TableSnapshot contains the state of a single table.
type TableSnapshot struct {
	Name     string                            `json:"name"`
	RowCount int64                             `json:"row_count"`
	Rows     map[string]map[string]interface{} `json:"rows"`
	Columns  []string                          `json:"columns"`
}

// DBDiffReport contains the result of comparing two snapshots.
type DBDiffReport struct {
	Before          *DBSnapshot           `json:"before"`
	After           *DBSnapshot           `json:"after"`
	Tables          map[string]*TableDiff `json:"tables"`
	TotalViolations int                   `json:"total_violations"`
	Summary         string                `json:"summary"`
}

// TableDiff contains the diff for a single table.
type TableDiff struct {
	Name           string                   `json:"name"`
	RowCountBefore int64                    `json:"row_count_before"`
	RowCountAfter  int64                    `json:"row_count_after"`
	InsertedRows   []map[string]interface{} `json:"inserted_rows,omitempty"`
	DeletedRows    []map[string]interface{} `json:"deleted_rows,omitempty"`
	ModifiedRows   []RowDiff                `json:"modified_rows,omitempty"`
	Violations     []string                 `json:"violations,omitempty"`
}

// RowDiff contains the diff for a single row.
type RowDiff struct {
	Key            string                 `json:"key"`
	Before         map[string]interface{} `json:"before"`
	After          map[string]interface{} `json:"after"`
	ChangedColumns []ColumnDiff           `json:"changed_columns"`
}

// ColumnDiff contains the diff for a single column.
type ColumnDiff struct {
	Name       string      `json:"name"`
	Before     interface{} `json:"before"`
	After      interface{} `json:"after"`
	Normalized bool        `json:"normalized,omitempty"`
}

// DBDiffer compares database state before and after a test run.
type DBDiffer struct {
	config           *DBDiffConfig
	db               *sql.DB
	normalizeColumns map[string]bool
}

var identifierRegex = regexp.MustCompile(`^[a-zA-Z_][a-zA-Z0-9_]*$`)

// isValidIdentifier validates that a string is a safe SQL identifier.
func isValidIdentifier(s string) bool {
	return identifierRegex.MatchString(s)
}

// quoteIdentifier wraps a SQL identifier in double quotes and escapes any
// embedded double quotes, providing defense-in-depth against SQL injection.
func quoteIdentifier(name string) string {
	return `"` + strings.ReplaceAll(name, `"`, `""`) + `"`
}

// NewDBDiffer creates a new database differ.
func NewDBDiffer(config *DBDiffConfig) *DBDiffer {
	d := &DBDiffer{
		config:           config,
		normalizeColumns: make(map[string]bool),
	}

	// Build normalize lookup
	for _, col := range config.NormalizeColumns {
		d.normalizeColumns[strings.ToLower(col)] = true
	}

	return d
}

// Connect establishes a database connection.
func (d *DBDiffer) Connect(ctx context.Context, connectionString string) error {
	// Default to postgres driver - can be extended for other databases
	db, err := sql.Open("postgres", connectionString)
	if err != nil {
		return fmt.Errorf("connect to database: %w", err)
	}

	if err := db.PingContext(ctx); err != nil {
		db.Close()
		return fmt.Errorf("ping database: %w", err)
	}

	d.db = db
	return nil
}

// Close closes the database connection.
func (d *DBDiffer) Close() error {
	if d.db != nil {
		return d.db.Close()
	}
	return nil
}

// TakeSnapshot captures the current state of critical tables.
func (d *DBDiffer) TakeSnapshot(ctx context.Context) (*DBSnapshot, error) {
	snapshot := &DBSnapshot{
		Timestamp: time.Now(),
		Tables:    make(map[string]*TableSnapshot),
	}

	for _, table := range d.config.CriticalTables {
		tableSnapshot, err := d.snapshotTable(ctx, table)
		if err != nil {
			return nil, fmt.Errorf("snapshot table %s: %w", table, err)
		}
		snapshot.Tables[table] = tableSnapshot
	}

	// Calculate content hash
	data, _ := json.Marshal(snapshot.Tables)
	hash := sha256.Sum256(data)
	snapshot.Hash = hex.EncodeToString(hash[:])

	return snapshot, nil
}

func (d *DBDiffer) snapshotTable(ctx context.Context, table string) (*TableSnapshot, error) {
	// Validate table name to prevent SQL injection
	if !isValidIdentifier(table) {
		return nil, fmt.Errorf("invalid table name: %s", table)
	}

	snapshot := &TableSnapshot{
		Name: table,
		Rows: make(map[string]map[string]interface{}),
	}

	// Get columns - use parameterized query for WHERE clause
	colQuery := `
		SELECT column_name
		FROM information_schema.columns
		WHERE table_name = $1
		ORDER BY ordinal_position
	`

	rows, err := d.db.QueryContext(ctx, colQuery, table)
	if err != nil {
		return nil, fmt.Errorf("get columns: %w", err)
	}
	defer rows.Close()

	for rows.Next() {
		var col string
		if err := rows.Scan(&col); err != nil {
			return nil, fmt.Errorf("scan column: %w", err)
		}
		snapshot.Columns = append(snapshot.Columns, col)
	}

	// Get row count
	countQuery := fmt.Sprintf("SELECT COUNT(*) FROM %s", quoteIdentifier(table))
	if err := d.db.QueryRowContext(ctx, countQuery).Scan(&snapshot.RowCount); err != nil {
		return nil, fmt.Errorf("count rows: %w", err)
	}

	// Get rows (limit for safety)
	dataQuery := fmt.Sprintf("SELECT * FROM %s LIMIT 10000", quoteIdentifier(table))
	dataRows, err := d.db.QueryContext(ctx, dataQuery)
	if err != nil {
		return nil, fmt.Errorf("query rows: %w", err)
	}
	defer dataRows.Close()

	cols, _ := dataRows.Columns()
	for dataRows.Next() {
		values := make([]interface{}, len(cols))
		valuePtrs := make([]interface{}, len(cols))
		for i := range values {
			valuePtrs[i] = &values[i]
		}

		if err := dataRows.Scan(valuePtrs...); err != nil {
			return nil, fmt.Errorf("scan row: %w", err)
		}

		row := make(map[string]interface{})
		var keyParts []string
		for i, col := range cols {
			row[col] = values[i]
			// Use first column as key (usually primary key)
			if i == 0 {
				keyParts = append(keyParts, fmt.Sprintf("%v", values[i]))
			}
		}

		key := strings.Join(keyParts, ":")
		snapshot.Rows[key] = row
	}

	return snapshot, nil
}

// Compare compares two snapshots and returns a diff report.
func (d *DBDiffer) Compare(before, after *DBSnapshot) *DBDiffReport {
	report := &DBDiffReport{
		Before: before,
		After:  after,
		Tables: make(map[string]*TableDiff),
	}

	for table := range before.Tables {
		tableDiff := d.compareTable(before.Tables[table], after.Tables[table])
		report.Tables[table] = tableDiff
		report.TotalViolations += len(tableDiff.Violations)
	}

	// Generate summary
	var summaryParts []string
	for tableName, diff := range report.Tables {
		changes := len(diff.InsertedRows) + len(diff.DeletedRows) + len(diff.ModifiedRows)
		if changes > 0 {
			summaryParts = append(summaryParts, fmt.Sprintf("%s: %d changes", tableName, changes))
		}
	}
	if len(summaryParts) == 0 {
		report.Summary = "No changes detected"
	} else {
		report.Summary = strings.Join(summaryParts, "; ")
	}

	return report
}

func (d *DBDiffer) compareTable(before, after *TableSnapshot) *TableDiff {
	diff := &TableDiff{
		Name:           before.Name,
		RowCountBefore: before.RowCount,
		RowCountAfter:  after.RowCount,
	}

	// Find inserted rows
	for key, row := range after.Rows {
		if _, exists := before.Rows[key]; !exists {
			diff.InsertedRows = append(diff.InsertedRows, row)
		}
	}

	// Find deleted rows
	for key, row := range before.Rows {
		if _, exists := after.Rows[key]; !exists {
			diff.DeletedRows = append(diff.DeletedRows, row)
		}
	}

	// Find modified rows
	for key, beforeRow := range before.Rows {
		if afterRow, exists := after.Rows[key]; exists {
			if rowDiff := d.compareRow(before.Name, key, beforeRow, afterRow); rowDiff != nil {
				diff.ModifiedRows = append(diff.ModifiedRows, *rowDiff)
			}
		}
	}

	// Check for unexpected modifications (excluding normalized columns)
	for _, mod := range diff.ModifiedRows {
		for _, col := range mod.ChangedColumns {
			if !col.Normalized {
				diff.Violations = append(diff.Violations, fmt.Sprintf(
					"unexpected change in row %s, column %s: %v -> %v",
					mod.Key, col.Name, col.Before, col.After,
				))
			}
		}
	}

	return diff
}

func (d *DBDiffer) compareRow(table, key string, before, after map[string]interface{}) *RowDiff {
	var changedColumns []ColumnDiff

	// Get all columns
	columns := make(map[string]bool)
	for col := range before {
		columns[col] = true
	}
	for col := range after {
		columns[col] = true
	}

	sortedCols := make([]string, 0, len(columns))
	for col := range columns {
		sortedCols = append(sortedCols, col)
	}
	sort.Strings(sortedCols)

	for _, col := range sortedCols {
		beforeVal := before[col]
		afterVal := after[col]

		if fmt.Sprintf("%v", beforeVal) != fmt.Sprintf("%v", afterVal) {
			normalized := d.shouldNormalize(table, col)
			changedColumns = append(changedColumns, ColumnDiff{
				Name:       col,
				Before:     beforeVal,
				After:      afterVal,
				Normalized: normalized,
			})
		}
	}

	if len(changedColumns) == 0 {
		return nil
	}

	return &RowDiff{
		Key:            key,
		Before:         before,
		After:          after,
		ChangedColumns: changedColumns,
	}
}

func (d *DBDiffer) shouldNormalize(table, column string) bool {
	// Check table.column
	if d.normalizeColumns[strings.ToLower(table+"."+column)] {
		return true
	}
	// Check *.column (wildcard)
	if d.normalizeColumns[strings.ToLower("*."+column)] {
		return true
	}
	return false
}
