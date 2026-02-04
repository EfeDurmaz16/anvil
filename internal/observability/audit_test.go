package observability

import (
	"bytes"
	"context"
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// ==================== AuditConfig Tests ====================

func TestDefaultAuditConfig(t *testing.T) {
	cfg := DefaultAuditConfig()
	if !cfg.Enabled {
		t.Fatal("expected enabled by default")
	}
	if cfg.OutputPath != "stdout" {
		t.Fatalf("expected stdout, got %s", cfg.OutputPath)
	}
}

// ==================== AuditLogger Tests ====================

func TestAuditLogger_New_Stdout(t *testing.T) {
	l, err := NewAuditLogger(&AuditConfig{
		Enabled:    true,
		OutputPath: "stdout",
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if l == nil {
		t.Fatal("expected non-nil logger")
	}
}

func TestAuditLogger_New_Stderr(t *testing.T) {
	l, err := NewAuditLogger(&AuditConfig{
		Enabled:    true,
		OutputPath: "stderr",
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if l == nil {
		t.Fatal("expected non-nil logger")
	}
}

func TestAuditLogger_New_File(t *testing.T) {
	tmpDir := t.TempDir()
	logPath := filepath.Join(tmpDir, "audit.log")

	l, err := NewAuditLogger(&AuditConfig{
		Enabled:    true,
		OutputPath: logPath,
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	defer l.Close()

	if _, err := os.Stat(logPath); os.IsNotExist(err) {
		t.Fatal("expected log file to be created")
	}
}

func TestAuditLogger_New_NilConfig(t *testing.T) {
	l, err := NewAuditLogger(nil)
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if l == nil {
		t.Fatal("expected non-nil logger with default config")
	}
}

func TestAuditLogger_Log_Disabled(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{
		writer:  &buf,
		enabled: false,
	}

	err := l.Log(&AuditEvent{EventType: AuditEventAgentStart})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
	if buf.Len() > 0 {
		t.Fatal("expected no output when disabled")
	}
}

func TestAuditLogger_Log_WritesJSON(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{
		writer:    &buf,
		sessionID: "test-session",
		userID:    "test-user",
		enabled:   true,
	}

	err := l.Log(&AuditEvent{
		EventType: AuditEventAgentStart,
		AgentName: "cartographer",
		Success:   true,
		Message:   "test message",
	})
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Parse output
	var event AuditEvent
	if err := json.Unmarshal(buf.Bytes(), &event); err != nil {
		t.Fatalf("failed to parse output: %v", err)
	}

	if event.EventType != AuditEventAgentStart {
		t.Fatalf("expected agent.start, got %s", event.EventType)
	}
	if event.AgentName != "cartographer" {
		t.Fatalf("expected cartographer, got %s", event.AgentName)
	}
	if event.SessionID != "test-session" {
		t.Fatalf("expected test-session, got %s", event.SessionID)
	}
	if event.UserID != "test-user" {
		t.Fatalf("expected test-user, got %s", event.UserID)
	}
}

func TestAuditLogger_Log_FillsTimestamp(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{
		writer:  &buf,
		enabled: true,
	}

	before := time.Now().UTC()
	l.Log(&AuditEvent{EventType: AuditEventAgentStart})
	after := time.Now().UTC()

	var event AuditEvent
	json.Unmarshal(buf.Bytes(), &event)

	if event.Timestamp.Before(before) || event.Timestamp.After(after) {
		t.Fatal("timestamp should be set automatically")
	}
}

func TestAuditLogger_SessionID_Generated(t *testing.T) {
	l, _ := NewAuditLogger(&AuditConfig{
		Enabled:    true,
		OutputPath: "stdout",
	})

	if l.sessionID == "" {
		t.Fatal("expected auto-generated session ID")
	}
	if !strings.HasPrefix(l.sessionID, "session-") {
		t.Fatalf("expected session- prefix, got %s", l.sessionID)
	}
}

// ==================== Convenience Methods Tests ====================

func TestAuditLogger_LogAgentStart(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{writer: &buf, enabled: true}

	l.LogAgentStart(context.Background(), "cartographer", "wf-123", map[string]string{"source": "cobol"})

	var event AuditEvent
	json.Unmarshal(buf.Bytes(), &event)

	if event.EventType != AuditEventAgentStart {
		t.Fatalf("expected agent.start, got %s", event.EventType)
	}
	if event.AgentName != "cartographer" {
		t.Fatalf("expected cartographer, got %s", event.AgentName)
	}
	if event.WorkflowID != "wf-123" {
		t.Fatalf("expected wf-123, got %s", event.WorkflowID)
	}
}

func TestAuditLogger_LogAgentComplete(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{writer: &buf, enabled: true}

	l.LogAgentComplete(context.Background(), "judge", "wf-123", 5*time.Second, 0.95, 0)

	var event AuditEvent
	json.Unmarshal(buf.Bytes(), &event)

	if event.EventType != AuditEventAgentComplete {
		t.Fatalf("expected agent.complete, got %s", event.EventType)
	}
	if !event.Success {
		t.Fatal("expected success=true when errCount=0")
	}
	if event.Details["score"].(float64) != 0.95 {
		t.Fatalf("expected score 0.95, got %v", event.Details["score"])
	}
}

func TestAuditLogger_LogAgentError(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{writer: &buf, enabled: true}

	l.LogAgentError(context.Background(), "architect", "wf-123",
		&testError{msg: "compilation failed"})

	var event AuditEvent
	json.Unmarshal(buf.Bytes(), &event)

	if event.EventType != AuditEventAgentError {
		t.Fatalf("expected agent.error, got %s", event.EventType)
	}
	if event.Success {
		t.Fatal("expected success=false for error")
	}
	if event.ErrorDetail != "compilation failed" {
		t.Fatalf("expected error detail, got %s", event.ErrorDetail)
	}
}

func TestAuditLogger_LogLLMRequest(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{writer: &buf, enabled: true}

	l.LogLLMRequest(context.Background(), "anthropic", "claude-3", 1000)

	var event AuditEvent
	json.Unmarshal(buf.Bytes(), &event)

	if event.EventType != AuditEventLLMRequest {
		t.Fatalf("expected llm.request, got %s", event.EventType)
	}
	if event.Details["provider"] != "anthropic" {
		t.Fatalf("expected anthropic, got %v", event.Details["provider"])
	}
}

func TestAuditLogger_LogLLMResponse(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{writer: &buf, enabled: true}

	l.LogLLMResponse(context.Background(), "openai", "gpt-4", 2*time.Second, 500, 200)

	var event AuditEvent
	json.Unmarshal(buf.Bytes(), &event)

	if event.EventType != AuditEventLLMResponse {
		t.Fatalf("expected llm.response, got %s", event.EventType)
	}
	if event.Details["total_tokens"].(float64) != 700 {
		t.Fatalf("expected 700 total tokens, got %v", event.Details["total_tokens"])
	}
}

func TestAuditLogger_LogLLMError(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{writer: &buf, enabled: true}

	l.LogLLMError(context.Background(), "anthropic", "claude-3",
		&testError{msg: "rate limited"})

	var event AuditEvent
	json.Unmarshal(buf.Bytes(), &event)

	if event.EventType != AuditEventLLMError {
		t.Fatalf("expected llm.error, got %s", event.EventType)
	}
	if event.Success {
		t.Fatal("expected success=false")
	}
}

func TestAuditLogger_LogFileGenerate(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{writer: &buf, enabled: true}

	l.LogFileGenerate(context.Background(), "src/main.ts", 1500, "typescript")

	var event AuditEvent
	json.Unmarshal(buf.Bytes(), &event)

	if event.EventType != AuditEventFileGenerate {
		t.Fatalf("expected file.generate, got %s", event.EventType)
	}
	if event.Details["path"] != "src/main.ts" {
		t.Fatalf("expected path, got %v", event.Details["path"])
	}
}

func TestAuditLogger_LogDBSnapshot(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{writer: &buf, enabled: true}

	l.LogDBSnapshot(context.Background(), []string{"users", "accounts"}, 100, time.Second)

	var event AuditEvent
	json.Unmarshal(buf.Bytes(), &event)

	if event.EventType != AuditEventDBSnapshot {
		t.Fatalf("expected db.snapshot, got %s", event.EventType)
	}
}

func TestAuditLogger_LogCompile(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{writer: &buf, enabled: true}

	l.LogCompile(context.Background(), "typescript", 5, true, 2*time.Second, nil)

	var event AuditEvent
	json.Unmarshal(buf.Bytes(), &event)

	if event.EventType != AuditEventCompile {
		t.Fatalf("expected compile, got %s", event.EventType)
	}
	if !event.Success {
		t.Fatal("expected success=true")
	}
}

func TestAuditLogger_LogFixtureRun(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{writer: &buf, enabled: true}

	l.LogFixtureRun(context.Background(), "test_login", true, 100*time.Millisecond, "")

	var event AuditEvent
	json.Unmarshal(buf.Bytes(), &event)

	if event.EventType != AuditEventFixtureRun {
		t.Fatalf("expected fixture.run, got %s", event.EventType)
	}
	if !event.Success {
		t.Fatal("expected success=true")
	}
}

func TestAuditLogger_LogFixtureRun_WithError(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{writer: &buf, enabled: true}

	l.LogFixtureRun(context.Background(), "test_fail", false, 50*time.Millisecond, "assertion failed")

	var event AuditEvent
	json.Unmarshal(buf.Bytes(), &event)

	if event.Success {
		t.Fatal("expected success=false")
	}
	if event.ErrorDetail != "assertion failed" {
		t.Fatalf("expected error detail, got %s", event.ErrorDetail)
	}
}

func TestAuditLogger_LogProofPack(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{writer: &buf, enabled: true}

	l.LogProofPack(context.Background(), "/output/proof.zip", 10, 8)

	var event AuditEvent
	json.Unmarshal(buf.Bytes(), &event)

	if event.EventType != AuditEventProofPack {
		t.Fatalf("expected proofpack.generate, got %s", event.EventType)
	}
	if event.Details["fail_count"].(float64) != 2 {
		t.Fatalf("expected 2 failures, got %v", event.Details["fail_count"])
	}
}

func TestAuditLogger_LogWorkflowStart(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{writer: &buf, enabled: true}

	l.LogWorkflowStart(context.Background(), "wf-456", "cobol", "typescript", "/input")

	var event AuditEvent
	json.Unmarshal(buf.Bytes(), &event)

	if event.EventType != AuditEventWorkflowStart {
		t.Fatalf("expected workflow.start, got %s", event.EventType)
	}
	if event.WorkflowID != "wf-456" {
		t.Fatalf("expected wf-456, got %s", event.WorkflowID)
	}
}

func TestAuditLogger_LogWorkflowEnd(t *testing.T) {
	var buf bytes.Buffer
	l := &AuditLogger{writer: &buf, enabled: true}

	l.LogWorkflowEnd(context.Background(), "wf-456", true, 10*time.Minute, 0.92, "/output")

	var event AuditEvent
	json.Unmarshal(buf.Bytes(), &event)

	if event.EventType != AuditEventWorkflowEnd {
		t.Fatalf("expected workflow.end, got %s", event.EventType)
	}
	if !event.Success {
		t.Fatal("expected success=true")
	}
}

func TestAuditLogger_Close_File(t *testing.T) {
	tmpDir := t.TempDir()
	logPath := filepath.Join(tmpDir, "audit.log")

	l, _ := NewAuditLogger(&AuditConfig{
		Enabled:    true,
		OutputPath: logPath,
	})

	l.Log(&AuditEvent{EventType: AuditEventAgentStart})
	err := l.Close()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}

	// Verify file exists and has content
	data, err := os.ReadFile(logPath)
	if err != nil {
		t.Fatalf("failed to read log file: %v", err)
	}
	if len(data) == 0 {
		t.Fatal("expected log content")
	}
}

func TestAuditLogger_Close_Stdout(t *testing.T) {
	l, _ := NewAuditLogger(&AuditConfig{
		Enabled:    true,
		OutputPath: "stdout",
	})

	// Should not error when closing stdout
	err := l.Close()
	if err != nil {
		t.Fatalf("unexpected error: %v", err)
	}
}

// ==================== Global Logger Tests ====================

func TestAudit_DisabledByDefault(t *testing.T) {
	// Reset global state
	globalAuditLogger = nil

	l := Audit()
	if l.enabled {
		t.Fatal("expected disabled logger when not initialized")
	}
}

// ==================== Event Type Constants ====================

func TestAuditEventTypes(t *testing.T) {
	types := []AuditEventType{
		AuditEventAgentStart,
		AuditEventAgentComplete,
		AuditEventAgentError,
		AuditEventLLMRequest,
		AuditEventLLMResponse,
		AuditEventLLMError,
		AuditEventFileGenerate,
		AuditEventFileRead,
		AuditEventDBConnect,
		AuditEventDBQuery,
		AuditEventDBSnapshot,
		AuditEventCompile,
		AuditEventFixtureRun,
		AuditEventProofPack,
		AuditEventWorkflowStart,
		AuditEventWorkflowEnd,
	}

	for _, et := range types {
		if et == "" {
			t.Fatal("event type should not be empty")
		}
	}
}

// Helper error type for testing
type testError struct {
	msg string
}

func (e *testError) Error() string {
	return e.msg
}
