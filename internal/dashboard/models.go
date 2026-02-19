package dashboard

import "time"

// MigrationStatus represents the state of a migration run.
type MigrationStatus string

const (
	StatusPending   MigrationStatus = "pending"
	StatusRunning   MigrationStatus = "running"
	StatusCompleted MigrationStatus = "completed"
	StatusFailed    MigrationStatus = "failed"
)

// StageKind identifies a pipeline stage.
type StageKind string

const (
	StageCartographer StageKind = "cartographer"
	StageSpecular     StageKind = "specular"
	StageArchitect    StageKind = "architect"
	StageJudge        StageKind = "judge"
	StageTestGen      StageKind = "testgen"
)

// MigrationRun represents a single end-to-end migration.
type MigrationRun struct {
	ID           string          `json:"id"`
	Name         string          `json:"name"`
	SourceLang   string          `json:"source_lang"`
	TargetLang   string          `json:"target_lang"`
	Status       MigrationStatus `json:"status"`
	Stages       []StageResult   `json:"stages"`
	StartedAt    time.Time       `json:"started_at"`
	CompletedAt  *time.Time      `json:"completed_at,omitempty"`
	Error        string          `json:"error,omitempty"`
	InputModules int             `json:"input_modules"`
	OutputFiles  int             `json:"output_files"`
	TestsPassed  int             `json:"tests_passed"`
	TestsFailed  int             `json:"tests_failed"`
	LLMCalls     int             `json:"llm_calls"`
	TotalTokens  int             `json:"total_tokens"`
}

// StageResult represents a single pipeline stage execution.
type StageResult struct {
	Stage       StageKind       `json:"stage"`
	Status      MigrationStatus `json:"status"`
	StartedAt   time.Time       `json:"started_at"`
	CompletedAt *time.Time      `json:"completed_at,omitempty"`
	Duration    time.Duration   `json:"duration_ms"`
	Error       string          `json:"error,omitempty"`
	Metrics     StageMetrics    `json:"metrics"`
}

// StageMetrics holds per-stage metrics.
type StageMetrics struct {
	InputItems  int `json:"input_items"`
	OutputItems int `json:"output_items"`
	LLMCalls    int `json:"llm_calls"`
	Tokens      int `json:"tokens"`
	Retries     int `json:"retries"`
}

// DashboardStats holds aggregate statistics.
type DashboardStats struct {
	TotalMigrations     int     `json:"total_migrations"`
	ActiveMigrations    int     `json:"active_migrations"`
	CompletedMigrations int     `json:"completed_migrations"`
	FailedMigrations    int     `json:"failed_migrations"`
	TotalLLMCalls       int     `json:"total_llm_calls"`
	TotalTokens         int     `json:"total_tokens"`
	AvgDuration         float64 `json:"avg_duration_seconds"`
	SuccessRate         float64 `json:"success_rate"`
}

// Event represents a real-time dashboard event.
type Event struct {
	Type      string      `json:"type"`
	Timestamp time.Time   `json:"timestamp"`
	RunID     string      `json:"run_id,omitempty"`
	Stage     StageKind   `json:"stage,omitempty"`
	Data      interface{} `json:"data,omitempty"`
}

// LogEntry represents a log line for a migration.
type LogEntry struct {
	Timestamp time.Time `json:"timestamp"`
	Level     string    `json:"level"`
	Message   string    `json:"message"`
	RunID     string    `json:"run_id"`
	Stage     string    `json:"stage,omitempty"`
}
