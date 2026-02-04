// Package observability provides audit logging for compliance tracking.
package observability

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"sync"
	"time"
)

// AuditEventType categorizes audit events.
type AuditEventType string

const (
	AuditEventAgentStart    AuditEventType = "agent.start"
	AuditEventAgentComplete AuditEventType = "agent.complete"
	AuditEventAgentError    AuditEventType = "agent.error"
	AuditEventLLMRequest    AuditEventType = "llm.request"
	AuditEventLLMResponse   AuditEventType = "llm.response"
	AuditEventLLMError      AuditEventType = "llm.error"
	AuditEventFileGenerate  AuditEventType = "file.generate"
	AuditEventFileRead      AuditEventType = "file.read"
	AuditEventDBConnect     AuditEventType = "db.connect"
	AuditEventDBQuery       AuditEventType = "db.query"
	AuditEventDBSnapshot    AuditEventType = "db.snapshot"
	AuditEventCompile       AuditEventType = "compile"
	AuditEventFixtureRun    AuditEventType = "fixture.run"
	AuditEventProofPack     AuditEventType = "proofpack.generate"
	AuditEventWorkflowStart AuditEventType = "workflow.start"
	AuditEventWorkflowEnd   AuditEventType = "workflow.end"
)

// AuditEvent represents a single audit log entry.
type AuditEvent struct {
	Timestamp   time.Time              `json:"timestamp"`
	EventType   AuditEventType         `json:"event_type"`
	SessionID   string                 `json:"session_id"`
	WorkflowID  string                 `json:"workflow_id,omitempty"`
	AgentName   string                 `json:"agent_name,omitempty"`
	UserID      string                 `json:"user_id,omitempty"`
	Success     bool                   `json:"success"`
	Duration    time.Duration          `json:"duration_ms,omitempty"`
	Message     string                 `json:"message,omitempty"`
	Details     map[string]interface{} `json:"details,omitempty"`
	ErrorCode   string                 `json:"error_code,omitempty"`
	ErrorDetail string                 `json:"error_detail,omitempty"`
}

// AuditLogger handles audit event logging.
type AuditLogger struct {
	mu        sync.Mutex
	writer    io.Writer
	sessionID string
	userID    string
	enabled   bool
}

// AuditConfig configures the audit logger.
type AuditConfig struct {
	Enabled   bool
	OutputPath string // File path or "stdout"/"stderr"
	SessionID  string
	UserID     string
}

// DefaultAuditConfig returns default audit configuration.
func DefaultAuditConfig() *AuditConfig {
	return &AuditConfig{
		Enabled:   true,
		OutputPath: "stdout",
	}
}

// NewAuditLogger creates a new audit logger.
func NewAuditLogger(config *AuditConfig) (*AuditLogger, error) {
	if config == nil {
		config = DefaultAuditConfig()
	}

	var writer io.Writer
	switch config.OutputPath {
	case "stdout", "":
		writer = os.Stdout
	case "stderr":
		writer = os.Stderr
	default:
		f, err := os.OpenFile(config.OutputPath, os.O_CREATE|os.O_APPEND|os.O_WRONLY, 0644)
		if err != nil {
			return nil, fmt.Errorf("open audit log: %w", err)
		}
		writer = f
	}

	sessionID := config.SessionID
	if sessionID == "" {
		sessionID = fmt.Sprintf("session-%d", time.Now().UnixNano())
	}

	return &AuditLogger{
		writer:    writer,
		sessionID: sessionID,
		userID:    config.UserID,
		enabled:   config.Enabled,
	}, nil
}

// Log writes an audit event.
func (l *AuditLogger) Log(event *AuditEvent) error {
	if !l.enabled {
		return nil
	}

	l.mu.Lock()
	defer l.mu.Unlock()

	// Fill in defaults
	if event.Timestamp.IsZero() {
		event.Timestamp = time.Now().UTC()
	}
	if event.SessionID == "" {
		event.SessionID = l.sessionID
	}
	if event.UserID == "" {
		event.UserID = l.userID
	}

	data, err := json.Marshal(event)
	if err != nil {
		return fmt.Errorf("marshal audit event: %w", err)
	}

	_, err = fmt.Fprintf(l.writer, "%s\n", data)
	return err
}

// LogAgentStart logs an agent start event.
func (l *AuditLogger) LogAgentStart(ctx context.Context, agentName, workflowID string, params map[string]string) {
	l.Log(&AuditEvent{
		EventType:  AuditEventAgentStart,
		AgentName:  agentName,
		WorkflowID: workflowID,
		Success:    true,
		Message:    fmt.Sprintf("Agent %s started", agentName),
		Details: map[string]interface{}{
			"params": params,
		},
	})
}

// LogAgentComplete logs an agent completion event.
func (l *AuditLogger) LogAgentComplete(ctx context.Context, agentName, workflowID string, duration time.Duration, score float64, errCount int) {
	l.Log(&AuditEvent{
		EventType:  AuditEventAgentComplete,
		AgentName:  agentName,
		WorkflowID: workflowID,
		Success:    errCount == 0,
		Duration:   duration,
		Message:    fmt.Sprintf("Agent %s completed", agentName),
		Details: map[string]interface{}{
			"score":       score,
			"error_count": errCount,
		},
	})
}

// LogAgentError logs an agent error event.
func (l *AuditLogger) LogAgentError(ctx context.Context, agentName, workflowID string, err error) {
	l.Log(&AuditEvent{
		EventType:   AuditEventAgentError,
		AgentName:   agentName,
		WorkflowID:  workflowID,
		Success:     false,
		Message:     fmt.Sprintf("Agent %s failed", agentName),
		ErrorDetail: err.Error(),
	})
}

// LogLLMRequest logs an LLM request event.
func (l *AuditLogger) LogLLMRequest(ctx context.Context, provider, model string, promptTokens int) {
	l.Log(&AuditEvent{
		EventType: AuditEventLLMRequest,
		Success:   true,
		Message:   fmt.Sprintf("LLM request to %s/%s", provider, model),
		Details: map[string]interface{}{
			"provider":      provider,
			"model":         model,
			"prompt_tokens": promptTokens,
		},
	})
}

// LogLLMResponse logs an LLM response event.
func (l *AuditLogger) LogLLMResponse(ctx context.Context, provider, model string, duration time.Duration, inputTokens, outputTokens int) {
	l.Log(&AuditEvent{
		EventType: AuditEventLLMResponse,
		Success:   true,
		Duration:  duration,
		Message:   fmt.Sprintf("LLM response from %s/%s", provider, model),
		Details: map[string]interface{}{
			"provider":      provider,
			"model":         model,
			"input_tokens":  inputTokens,
			"output_tokens": outputTokens,
			"total_tokens":  inputTokens + outputTokens,
		},
	})
}

// LogLLMError logs an LLM error event.
func (l *AuditLogger) LogLLMError(ctx context.Context, provider, model string, err error) {
	l.Log(&AuditEvent{
		EventType:   AuditEventLLMError,
		Success:     false,
		Message:     fmt.Sprintf("LLM error from %s/%s", provider, model),
		ErrorDetail: err.Error(),
		Details: map[string]interface{}{
			"provider": provider,
			"model":    model,
		},
	})
}

// LogFileGenerate logs a file generation event.
func (l *AuditLogger) LogFileGenerate(ctx context.Context, path string, size int, language string) {
	l.Log(&AuditEvent{
		EventType: AuditEventFileGenerate,
		Success:   true,
		Message:   fmt.Sprintf("Generated file: %s", path),
		Details: map[string]interface{}{
			"path":     path,
			"size":     size,
			"language": language,
		},
	})
}

// LogDBSnapshot logs a database snapshot event.
func (l *AuditLogger) LogDBSnapshot(ctx context.Context, tables []string, rowCount int, duration time.Duration) {
	l.Log(&AuditEvent{
		EventType: AuditEventDBSnapshot,
		Success:   true,
		Duration:  duration,
		Message:   fmt.Sprintf("Database snapshot: %d tables, %d rows", len(tables), rowCount),
		Details: map[string]interface{}{
			"tables":    tables,
			"row_count": rowCount,
		},
	})
}

// LogCompile logs a compilation event.
func (l *AuditLogger) LogCompile(ctx context.Context, language string, fileCount int, success bool, duration time.Duration, errors []string) {
	l.Log(&AuditEvent{
		EventType: AuditEventCompile,
		Success:   success,
		Duration:  duration,
		Message:   fmt.Sprintf("Compiled %d %s files", fileCount, language),
		Details: map[string]interface{}{
			"language":   language,
			"file_count": fileCount,
			"errors":     errors,
		},
	})
}

// LogFixtureRun logs a fixture execution event.
func (l *AuditLogger) LogFixtureRun(ctx context.Context, fixtureName string, passed bool, duration time.Duration, errorMsg string) {
	event := &AuditEvent{
		EventType: AuditEventFixtureRun,
		Success:   passed,
		Duration:  duration,
		Message:   fmt.Sprintf("Fixture %s: %v", fixtureName, passed),
		Details: map[string]interface{}{
			"fixture_name": fixtureName,
		},
	}
	if errorMsg != "" {
		event.ErrorDetail = errorMsg
	}
	l.Log(event)
}

// LogProofPack logs a proof pack generation event.
func (l *AuditLogger) LogProofPack(ctx context.Context, outputPath string, fixtureCount, passCount int) {
	l.Log(&AuditEvent{
		EventType: AuditEventProofPack,
		Success:   true,
		Message:   fmt.Sprintf("Generated proof pack: %d/%d passed", passCount, fixtureCount),
		Details: map[string]interface{}{
			"output_path":   outputPath,
			"fixture_count": fixtureCount,
			"pass_count":    passCount,
			"fail_count":    fixtureCount - passCount,
		},
	})
}

// LogWorkflowStart logs a workflow start event.
func (l *AuditLogger) LogWorkflowStart(ctx context.Context, workflowID, sourceLang, targetLang, inputPath string) {
	l.Log(&AuditEvent{
		EventType:  AuditEventWorkflowStart,
		WorkflowID: workflowID,
		Success:    true,
		Message:    fmt.Sprintf("Workflow started: %s -> %s", sourceLang, targetLang),
		Details: map[string]interface{}{
			"source_lang": sourceLang,
			"target_lang": targetLang,
			"input_path":  inputPath,
		},
	})
}

// LogWorkflowEnd logs a workflow completion event.
func (l *AuditLogger) LogWorkflowEnd(ctx context.Context, workflowID string, success bool, duration time.Duration, score float64, outputPath string) {
	l.Log(&AuditEvent{
		EventType:  AuditEventWorkflowEnd,
		WorkflowID: workflowID,
		Success:    success,
		Duration:   duration,
		Message:    fmt.Sprintf("Workflow completed: score=%.2f", score),
		Details: map[string]interface{}{
			"score":       score,
			"output_path": outputPath,
		},
	})
}

// Close closes the audit logger (if using a file).
func (l *AuditLogger) Close() error {
	if closer, ok := l.writer.(io.Closer); ok {
		if closer != os.Stdout && closer != os.Stderr {
			return closer.Close()
		}
	}
	return nil
}

// Global audit logger instance
var globalAuditLogger *AuditLogger
var auditOnce sync.Once

// InitGlobalAuditLogger initializes the global audit logger.
func InitGlobalAuditLogger(config *AuditConfig) error {
	var err error
	auditOnce.Do(func() {
		globalAuditLogger, err = NewAuditLogger(config)
	})
	return err
}

// Audit returns the global audit logger.
func Audit() *AuditLogger {
	if globalAuditLogger == nil {
		// Return a disabled logger if not initialized
		return &AuditLogger{enabled: false}
	}
	return globalAuditLogger
}
