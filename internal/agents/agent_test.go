package agents

import (
	"testing"
	"time"
)

func TestNewAgentResult_Defaults(t *testing.T) {
	result := NewAgentResult()

	if result.Version != "1.0.0" {
		t.Errorf("expected version 1.0.0, got %s", result.Version)
	}
	if result.Status != StatusSuccess {
		t.Errorf("expected status success, got %s", result.Status)
	}
	if result.Score != 1.0 {
		t.Errorf("expected score 1.0, got %f", result.Score)
	}
	if result.Errors == nil {
		t.Error("expected Errors to be initialized")
	}
	if len(result.Errors) != 0 {
		t.Errorf("expected empty Errors slice, got %d items", len(result.Errors))
	}
	if result.Warnings == nil {
		t.Error("expected Warnings to be initialized")
	}
	if len(result.Warnings) != 0 {
		t.Errorf("expected empty Warnings slice, got %d items", len(result.Warnings))
	}
	if result.Metrics == nil {
		t.Fatal("expected Metrics to be non-nil")
	}
	if result.Metrics.StartTime.IsZero() {
		t.Error("expected StartTime to be set")
	}
	if result.Metadata == nil {
		t.Error("expected Metadata map to be initialized")
	}
}

func TestFinalize_SetsEndTimeAndDuration(t *testing.T) {
	result := NewAgentResult()

	// Wait a bit to ensure duration is measurable
	time.Sleep(10 * time.Millisecond)

	result.Finalize()

	if result.Metrics.EndTime.IsZero() {
		t.Error("expected EndTime to be set after Finalize")
	}
	if result.Metrics.Duration == 0 {
		t.Error("expected Duration to be calculated")
	}
	if result.Metrics.Duration < 0 {
		t.Errorf("expected positive duration, got %v", result.Metrics.Duration)
	}
}

func TestFinalize_SetsStatusPartialWhenErrorsExist(t *testing.T) {
	result := NewAgentResult()
	result.Errors = append(result.Errors, "test error")

	result.Finalize()

	if result.Status != StatusPartial {
		t.Errorf("expected status partial when errors exist, got %s", result.Status)
	}
}

func TestFinalize_DoesNotChangeStatusIfAlreadyFailed(t *testing.T) {
	result := NewAgentResult()
	result.Status = StatusFailed
	result.Errors = append(result.Errors, "test error")

	result.Finalize()

	if result.Status != StatusFailed {
		t.Errorf("expected status to remain failed, got %s", result.Status)
	}
}

func TestAddError_AppendsError(t *testing.T) {
	result := NewAgentResult()

	result.AddError("first error")
	result.AddError("second error")

	if len(result.Errors) != 2 {
		t.Errorf("expected 2 errors, got %d", len(result.Errors))
	}
	if result.Errors[0] != "first error" {
		t.Errorf("expected first error, got %s", result.Errors[0])
	}
	if result.Errors[1] != "second error" {
		t.Errorf("expected second error, got %s", result.Errors[1])
	}
}

func TestAddError_ChangesStatusFromSuccessToPartial(t *testing.T) {
	result := NewAgentResult()

	if result.Status != StatusSuccess {
		t.Fatalf("expected initial status success, got %s", result.Status)
	}

	result.AddError("test error")

	if result.Status != StatusPartial {
		t.Errorf("expected status partial after adding error, got %s", result.Status)
	}
}

func TestAddError_KeepsStatusPartialOnMultipleErrors(t *testing.T) {
	result := NewAgentResult()

	result.AddError("first error")
	if result.Status != StatusPartial {
		t.Fatalf("expected status partial after first error, got %s", result.Status)
	}

	result.AddError("second error")
	if result.Status != StatusPartial {
		t.Errorf("expected status to remain partial, got %s", result.Status)
	}

	result.AddError("third error")
	if result.Status != StatusPartial {
		t.Errorf("expected status to remain partial, got %s", result.Status)
	}
}

func TestAddError_DoesNotChangeNonSuccessStatus(t *testing.T) {
	result := NewAgentResult()
	result.Status = StatusFailed

	result.AddError("test error")

	if result.Status != StatusFailed {
		t.Errorf("expected status to remain failed, got %s", result.Status)
	}
}

func TestAddWarning_AppendsWarning(t *testing.T) {
	result := NewAgentResult()

	result.AddWarning("first warning")
	result.AddWarning("second warning")

	if len(result.Warnings) != 2 {
		t.Errorf("expected 2 warnings, got %d", len(result.Warnings))
	}
	if result.Warnings[0] != "first warning" {
		t.Errorf("expected first warning, got %s", result.Warnings[0])
	}
	if result.Warnings[1] != "second warning" {
		t.Errorf("expected second warning, got %s", result.Warnings[1])
	}
}

func TestAddWarning_DoesNotChangeStatus(t *testing.T) {
	result := NewAgentResult()

	if result.Status != StatusSuccess {
		t.Fatalf("expected initial status success, got %s", result.Status)
	}

	result.AddWarning("test warning")

	if result.Status != StatusSuccess {
		t.Errorf("expected status to remain success after warning, got %s", result.Status)
	}
}

func TestSetPassthrough_SetsStatusAndMetadata(t *testing.T) {
	result := NewAgentResult()

	result.SetPassthrough("LLM unavailable")

	if result.Status != StatusPassthrough {
		t.Errorf("expected status passthrough, got %s", result.Status)
	}
	if result.Metadata["mode"] != "passthrough" {
		t.Errorf("expected metadata mode=passthrough, got %s", result.Metadata["mode"])
	}
	if result.Metadata["passthrough_reason"] != "LLM unavailable" {
		t.Errorf("expected passthrough_reason to be set, got %s", result.Metadata["passthrough_reason"])
	}
}

func TestRecordLLMCall_IncrementsMetrics(t *testing.T) {
	result := NewAgentResult()

	result.RecordLLMCall(100*time.Millisecond, 500, 300)

	if result.Metrics.LLMCalls != 1 {
		t.Errorf("expected 1 LLM call, got %d", result.Metrics.LLMCalls)
	}
	if result.Metrics.LLMDuration != 100*time.Millisecond {
		t.Errorf("expected LLM duration 100ms, got %v", result.Metrics.LLMDuration)
	}
	if result.Metrics.PromptTokens != 500 {
		t.Errorf("expected 500 prompt tokens, got %d", result.Metrics.PromptTokens)
	}
	if result.Metrics.CompletionTokens != 300 {
		t.Errorf("expected 300 completion tokens, got %d", result.Metrics.CompletionTokens)
	}
}

func TestRecordLLMCall_AccumulatesMultipleCalls(t *testing.T) {
	result := NewAgentResult()

	result.RecordLLMCall(100*time.Millisecond, 500, 300)
	result.RecordLLMCall(200*time.Millisecond, 700, 400)
	result.RecordLLMCall(50*time.Millisecond, 100, 50)

	if result.Metrics.LLMCalls != 3 {
		t.Errorf("expected 3 LLM calls, got %d", result.Metrics.LLMCalls)
	}
	if result.Metrics.LLMDuration != 350*time.Millisecond {
		t.Errorf("expected LLM duration 350ms, got %v", result.Metrics.LLMDuration)
	}
	if result.Metrics.PromptTokens != 1300 {
		t.Errorf("expected 1300 prompt tokens, got %d", result.Metrics.PromptTokens)
	}
	if result.Metrics.CompletionTokens != 750 {
		t.Errorf("expected 750 completion tokens, got %d", result.Metrics.CompletionTokens)
	}
}

func TestAgentStatus_Constants(t *testing.T) {
	tests := []struct {
		name   string
		status AgentStatus
		want   string
	}{
		{"success status", StatusSuccess, "success"},
		{"partial status", StatusPartial, "partial"},
		{"failed status", StatusFailed, "failed"},
		{"passthrough status", StatusPassthrough, "passthrough"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			if string(tt.status) != tt.want {
				t.Errorf("expected status %s, got %s", tt.want, tt.status)
			}
		})
	}
}

func TestResultVersion_Constant(t *testing.T) {
	if ResultVersion != "1.0.0" {
		t.Errorf("expected ResultVersion 1.0.0, got %s", ResultVersion)
	}
}

func TestAgentResult_CompleteWorkflow(t *testing.T) {
	// Test a complete workflow simulating an agent execution
	result := NewAgentResult()

	// Simulate processing
	time.Sleep(5 * time.Millisecond)

	// Record some LLM calls
	result.RecordLLMCall(50*time.Millisecond, 1000, 500)
	result.RecordLLMCall(30*time.Millisecond, 800, 400)

	// Add some warnings
	result.AddWarning("deprecated syntax detected")
	result.AddWarning("performance could be improved")

	// Set some metrics
	result.Metrics.InputItems = 10
	result.Metrics.OutputItems = 9
	result.Metrics.SkippedItems = 1

	// Add an error
	result.AddError("failed to process one item")

	// Finalize
	result.Finalize()

	// Verify final state
	if result.Status != StatusPartial {
		t.Errorf("expected status partial (has errors), got %s", result.Status)
	}
	if len(result.Errors) != 1 {
		t.Errorf("expected 1 error, got %d", len(result.Errors))
	}
	if len(result.Warnings) != 2 {
		t.Errorf("expected 2 warnings, got %d", len(result.Warnings))
	}
	if result.Metrics.LLMCalls != 2 {
		t.Errorf("expected 2 LLM calls, got %d", result.Metrics.LLMCalls)
	}
	if result.Metrics.LLMDuration != 80*time.Millisecond {
		t.Errorf("expected LLM duration 80ms, got %v", result.Metrics.LLMDuration)
	}
	if result.Metrics.PromptTokens != 1800 {
		t.Errorf("expected 1800 prompt tokens, got %d", result.Metrics.PromptTokens)
	}
	if result.Metrics.CompletionTokens != 900 {
		t.Errorf("expected 900 completion tokens, got %d", result.Metrics.CompletionTokens)
	}
	if result.Metrics.Duration == 0 {
		t.Error("expected duration to be calculated")
	}
	if result.Metrics.EndTime.IsZero() {
		t.Error("expected EndTime to be set")
	}
}
