package dashboard

import (
	"errors"
	"testing"
	"time"
)

func TestStore_CreateAndGetRun(t *testing.T) {
	store := NewStore()

	run := &MigrationRun{
		ID:           "test-1",
		Name:         "Test Migration",
		SourceLang:   "cobol",
		TargetLang:   "java",
		Status:       StatusRunning,
		StartedAt:    time.Now(),
		InputModules: 10,
	}

	store.CreateRun(run)

	retrieved, ok := store.GetRun("test-1")
	if !ok {
		t.Fatal("Expected to retrieve run, got not found")
	}

	if retrieved.ID != run.ID {
		t.Errorf("Expected ID %s, got %s", run.ID, retrieved.ID)
	}
	if retrieved.Name != run.Name {
		t.Errorf("Expected Name %s, got %s", run.Name, retrieved.Name)
	}
	if retrieved.Status != run.Status {
		t.Errorf("Expected Status %s, got %s", run.Status, retrieved.Status)
	}
}

func TestStore_ListRuns(t *testing.T) {
	store := NewStore()

	now := time.Now()

	// Create runs with different start times
	run1 := &MigrationRun{
		ID:        "test-1",
		Name:      "First",
		Status:    StatusCompleted,
		StartedAt: now.Add(-2 * time.Hour),
	}
	run2 := &MigrationRun{
		ID:        "test-2",
		Name:      "Second",
		Status:    StatusRunning,
		StartedAt: now.Add(-1 * time.Hour),
	}
	run3 := &MigrationRun{
		ID:        "test-3",
		Name:      "Third",
		Status:    StatusPending,
		StartedAt: now,
	}

	store.CreateRun(run1)
	store.CreateRun(run2)
	store.CreateRun(run3)

	runs := store.ListRuns()

	if len(runs) != 3 {
		t.Fatalf("Expected 3 runs, got %d", len(runs))
	}

	// Verify sorted by StartedAt descending (most recent first)
	if runs[0].ID != "test-3" {
		t.Errorf("Expected first run to be test-3, got %s", runs[0].ID)
	}
	if runs[1].ID != "test-2" {
		t.Errorf("Expected second run to be test-2, got %s", runs[1].ID)
	}
	if runs[2].ID != "test-1" {
		t.Errorf("Expected third run to be test-1, got %s", runs[2].ID)
	}
}

func TestStore_UpdateRun(t *testing.T) {
	store := NewStore()

	run := &MigrationRun{
		ID:     "test-1",
		Name:   "Test",
		Status: StatusPending,
	}

	store.CreateRun(run)

	// Update the run's status
	store.UpdateRun("test-1", func(r *MigrationRun) {
		r.Status = StatusRunning
		r.LLMCalls = 42
	})

	updated, _ := store.GetRun("test-1")
	if updated.Status != StatusRunning {
		t.Errorf("Expected status Running, got %s", updated.Status)
	}
	if updated.LLMCalls != 42 {
		t.Errorf("Expected LLMCalls 42, got %d", updated.LLMCalls)
	}

	// Update non-existent run should be safe (no-op)
	store.UpdateRun("non-existent", func(r *MigrationRun) {
		r.Status = StatusFailed
	})
}

func TestStore_GetStats(t *testing.T) {
	store := NewStore()

	now := time.Now()

	// Create runs with various statuses
	completed1 := now.Add(-30 * time.Minute)
	run1 := &MigrationRun{
		ID:          "test-1",
		Status:      StatusCompleted,
		StartedAt:   now.Add(-1 * time.Hour),
		CompletedAt: &completed1,
		LLMCalls:    100,
		TotalTokens: 50000,
	}

	completed2 := now.Add(-15 * time.Minute)
	run2 := &MigrationRun{
		ID:          "test-2",
		Status:      StatusCompleted,
		StartedAt:   now.Add(-45 * time.Minute),
		CompletedAt: &completed2,
		LLMCalls:    150,
		TotalTokens: 75000,
	}

	run3 := &MigrationRun{
		ID:          "test-3",
		Status:      StatusRunning,
		StartedAt:   now.Add(-10 * time.Minute),
		LLMCalls:    50,
		TotalTokens: 25000,
	}

	run4 := &MigrationRun{
		ID:          "test-4",
		Status:      StatusFailed,
		StartedAt:   now.Add(-2 * time.Hour),
		CompletedAt: func() *time.Time { t := now.Add(-90 * time.Minute); return &t }(),
		LLMCalls:    80,
		TotalTokens: 40000,
	}

	store.CreateRun(run1)
	store.CreateRun(run2)
	store.CreateRun(run3)
	store.CreateRun(run4)

	stats := store.GetStats()

	if stats.TotalMigrations != 4 {
		t.Errorf("Expected TotalMigrations 4, got %d", stats.TotalMigrations)
	}
	if stats.CompletedMigrations != 2 {
		t.Errorf("Expected CompletedMigrations 2, got %d", stats.CompletedMigrations)
	}
	if stats.ActiveMigrations != 1 {
		t.Errorf("Expected ActiveMigrations 1, got %d", stats.ActiveMigrations)
	}
	if stats.FailedMigrations != 1 {
		t.Errorf("Expected FailedMigrations 1, got %d", stats.FailedMigrations)
	}

	expectedTokens := 50000 + 75000 + 25000 + 40000
	if stats.TotalTokens != expectedTokens {
		t.Errorf("Expected TotalTokens %d, got %d", expectedTokens, stats.TotalTokens)
	}

	expectedLLMCalls := 100 + 150 + 50 + 80
	if stats.TotalLLMCalls != expectedLLMCalls {
		t.Errorf("Expected TotalLLMCalls %d, got %d", expectedLLMCalls, stats.TotalLLMCalls)
	}

	// Success rate should be 2 completed / 4 total = 0.5
	if stats.SuccessRate != 0.5 {
		t.Errorf("Expected SuccessRate 0.5, got %f", stats.SuccessRate)
	}

	// Average duration should be (30 + 30) / 2 = 30 minutes = 1800 seconds
	expectedAvgDuration := 1800.0
	if stats.AvgDuration != expectedAvgDuration {
		t.Errorf("Expected AvgDuration %f seconds, got %f", expectedAvgDuration, stats.AvgDuration)
	}
}

func TestStore_AddAndGetLogs(t *testing.T) {
	store := NewStore()

	now := time.Now()

	// Add logs for a run
	store.AddLog(LogEntry{
		Timestamp: now.Add(-3 * time.Minute),
		Level:     "info",
		Message:   "First log",
		RunID:     "test-1",
	})
	store.AddLog(LogEntry{
		Timestamp: now.Add(-2 * time.Minute),
		Level:     "warn",
		Message:   "Second log",
		RunID:     "test-1",
	})
	store.AddLog(LogEntry{
		Timestamp: now.Add(-1 * time.Minute),
		Level:     "error",
		Message:   "Third log",
		RunID:     "test-1",
	})

	// Add a log for a different run
	store.AddLog(LogEntry{
		Timestamp: now,
		Level:     "info",
		Message:   "Different run",
		RunID:     "test-2",
	})

	// Get logs for test-1
	logs := store.GetLogs("test-1", 0)
	if len(logs) != 3 {
		t.Fatalf("Expected 3 logs for test-1, got %d", len(logs))
	}

	// Verify logs are returned most recent first
	if logs[0].Message != "Third log" {
		t.Errorf("Expected first log to be 'Third log', got %s", logs[0].Message)
	}
	if logs[2].Message != "First log" {
		t.Errorf("Expected last log to be 'First log', got %s", logs[2].Message)
	}

	// Test limit
	limitedLogs := store.GetLogs("test-1", 2)
	if len(limitedLogs) != 2 {
		t.Fatalf("Expected 2 logs with limit, got %d", len(limitedLogs))
	}
	if limitedLogs[0].Message != "Third log" {
		t.Errorf("Expected first limited log to be 'Third log', got %s", limitedLogs[0].Message)
	}

	// Get logs for different run
	logs2 := store.GetLogs("test-2", 0)
	if len(logs2) != 1 {
		t.Fatalf("Expected 1 log for test-2, got %d", len(logs2))
	}
	if logs2[0].Message != "Different run" {
		t.Errorf("Expected message 'Different run', got %s", logs2[0].Message)
	}
}

func TestStore_Eviction(t *testing.T) {
	store := NewStore()

	now := time.Now()

	// Create more than maxRuns (100) completed runs
	for i := 0; i < 110; i++ {
		completed := now.Add(time.Duration(-i) * time.Minute)
		run := &MigrationRun{
			ID:          string(rune('a' + i)),
			Status:      StatusCompleted,
			StartedAt:   now.Add(time.Duration(-i-1) * time.Minute),
			CompletedAt: &completed,
		}
		store.CreateRun(run)
	}

	// Verify that we have exactly maxRuns (100) runs
	runs := store.ListRuns()
	if len(runs) != maxRuns {
		t.Errorf("Expected %d runs after eviction, got %d", maxRuns, len(runs))
	}

	// Verify that the oldest completed runs were evicted
	// The most recent 100 should remain (indices 0-99 in creation order)
	// Since we created them in reverse chronological order by completion time,
	// the oldest 10 by completion time should be evicted
	for i := 0; i < 10; i++ {
		id := string(rune('a' + 100 + i))
		if _, ok := store.GetRun(id); ok {
			t.Errorf("Expected old run %s to be evicted, but it still exists", id)
		}
	}

	// Verify that recent runs are still present
	for i := 0; i < 10; i++ {
		id := string(rune('a' + i))
		if _, ok := store.GetRun(id); !ok {
			t.Errorf("Expected recent run %s to exist, but it was evicted", id)
		}
	}
}

func TestEmitter_MigrationLifecycle(t *testing.T) {
	store := NewStore()
	hub := NewHub()
	emitter := NewEmitter(store, hub)

	now := time.Now()

	// Start a migration
	emitter.MigrationStarted("test-1", "Test Migration", "cobol", "java", 10)

	run, ok := store.GetRun("test-1")
	if !ok {
		t.Fatal("Expected migration run to be created")
	}
	if run.Status != StatusRunning {
		t.Errorf("Expected status Running, got %s", run.Status)
	}
	if run.InputModules != 10 {
		t.Errorf("Expected InputModules 10, got %d", run.InputModules)
	}

	// Start cartographer stage
	emitter.StageStarted("test-1", StageCartographer)
	run, _ = store.GetRun("test-1")
	if len(run.Stages) != 1 {
		t.Fatalf("Expected 1 stage, got %d", len(run.Stages))
	}
	if run.Stages[0].Stage != StageCartographer {
		t.Errorf("Expected stage Cartographer, got %s", run.Stages[0].Stage)
	}
	if run.Stages[0].Status != StatusRunning {
		t.Errorf("Expected stage status Running, got %s", run.Stages[0].Status)
	}

	// Complete cartographer stage
	time.Sleep(10 * time.Millisecond) // Small delay to ensure duration > 0
	emitter.StageCompleted("test-1", StageCartographer, StageMetrics{
		InputItems:  10,
		OutputItems: 10,
	})
	run, _ = store.GetRun("test-1")
	if run.Stages[0].Status != StatusCompleted {
		t.Errorf("Expected stage status Completed, got %s", run.Stages[0].Status)
	}
	if run.Stages[0].CompletedAt == nil {
		t.Error("Expected CompletedAt to be set")
	}
	if run.Stages[0].Duration == 0 {
		t.Error("Expected Duration to be > 0")
	}

	// Start and complete specular stage
	emitter.StageStarted("test-1", StageSpecular)
	time.Sleep(10 * time.Millisecond)
	emitter.StageCompleted("test-1", StageSpecular, StageMetrics{
		InputItems:  10,
		OutputItems: 30,
		LLMCalls:    10,
		Tokens:      5000,
	})

	// Start and complete architect stage
	emitter.StageStarted("test-1", StageArchitect)
	time.Sleep(10 * time.Millisecond)
	emitter.StageCompleted("test-1", StageArchitect, StageMetrics{
		InputItems:  10,
		OutputItems: 8,
		LLMCalls:    20,
		Tokens:      10000,
	})

	// Start and complete judge stage
	emitter.StageStarted("test-1", StageJudge)
	time.Sleep(10 * time.Millisecond)
	emitter.StageCompleted("test-1", StageJudge, StageMetrics{
		InputItems:  8,
		OutputItems: 8,
		LLMCalls:    8,
		Tokens:      3000,
	})

	// Complete the migration
	totalLLMCalls := 10 + 20 + 8
	totalTokens := 5000 + 10000 + 3000
	emitter.MigrationCompleted("test-1", 8, 7, 1, totalLLMCalls, totalTokens)

	run, _ = store.GetRun("test-1")
	if run.Status != StatusCompleted {
		t.Errorf("Expected status Completed, got %s", run.Status)
	}
	if run.CompletedAt == nil {
		t.Error("Expected CompletedAt to be set")
	}
	if run.CompletedAt.Before(now) {
		t.Error("Expected CompletedAt to be after test start")
	}
	if run.OutputFiles != 8 {
		t.Errorf("Expected OutputFiles 8, got %d", run.OutputFiles)
	}
	if run.TestsPassed != 7 {
		t.Errorf("Expected TestsPassed 7, got %d", run.TestsPassed)
	}
	if run.TestsFailed != 1 {
		t.Errorf("Expected TestsFailed 1, got %d", run.TestsFailed)
	}

	// Verify total LLM calls and tokens
	if run.LLMCalls != totalLLMCalls {
		t.Errorf("Expected LLMCalls %d, got %d", totalLLMCalls, run.LLMCalls)
	}
	if run.TotalTokens != totalTokens {
		t.Errorf("Expected TotalTokens %d, got %d", totalTokens, run.TotalTokens)
	}
}

func TestEmitter_MigrationFailed(t *testing.T) {
	store := NewStore()
	hub := NewHub()
	emitter := NewEmitter(store, hub)

	// Start a migration
	emitter.MigrationStarted("test-1", "Test Migration", "cobol", "java", 10)

	// Start and complete cartographer
	emitter.StageStarted("test-1", StageCartographer)
	time.Sleep(10 * time.Millisecond)
	emitter.StageCompleted("test-1", StageCartographer, StageMetrics{
		InputItems:  10,
		OutputItems: 10,
	})

	// Start and complete specular
	emitter.StageStarted("test-1", StageSpecular)
	time.Sleep(10 * time.Millisecond)
	emitter.StageCompleted("test-1", StageSpecular, StageMetrics{
		InputItems:  10,
		OutputItems: 30,
		LLMCalls:    10,
		Tokens:      5000,
	})

	// Start architect and fail
	emitter.StageStarted("test-1", StageArchitect)
	time.Sleep(10 * time.Millisecond)

	stageErr := errors.New("LLM provider timeout")
	emitter.StageFailed("test-1", StageArchitect, stageErr)

	run, _ := store.GetRun("test-1")
	if len(run.Stages) != 3 {
		t.Fatalf("Expected 3 stages, got %d", len(run.Stages))
	}

	// Verify architect stage failed
	architectStage := run.Stages[2]
	if architectStage.Status != StatusFailed {
		t.Errorf("Expected stage status Failed, got %s", architectStage.Status)
	}
	if architectStage.Error != "LLM provider timeout" {
		t.Errorf("Expected error 'LLM provider timeout', got %s", architectStage.Error)
	}
	if architectStage.CompletedAt == nil {
		t.Error("Expected CompletedAt to be set on failed stage")
	}

	// Fail the migration
	migrationErr := errors.New("Pipeline failed at architect stage: LLM provider timeout")
	emitter.MigrationFailed("test-1", migrationErr)

	run, _ = store.GetRun("test-1")
	if run.Status != StatusFailed {
		t.Errorf("Expected status Failed, got %s", run.Status)
	}
	if run.Error != "Pipeline failed at architect stage: LLM provider timeout" {
		t.Errorf("Expected error message to be set, got %s", run.Error)
	}
	if run.CompletedAt == nil {
		t.Error("Expected CompletedAt to be set on failed migration")
	}
}
