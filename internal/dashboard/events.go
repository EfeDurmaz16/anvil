package dashboard

import (
	"sync"
	"time"
)

// Emitter collects pipeline events and forwards them to the dashboard.
// It is safe to use from multiple goroutines.
type Emitter struct {
	mu    sync.Mutex
	store *Store
	hub   *Hub
}

// NewEmitter creates a new event emitter.
func NewEmitter(store *Store, hub *Hub) *Emitter {
	return &Emitter{store: store, hub: hub}
}

// MigrationStarted creates a new MigrationRun in the store with StatusRunning,
// broadcasts "migration.started" event.
func (e *Emitter) MigrationStarted(id, name, sourceLang, targetLang string, inputModules int) {
	run := &MigrationRun{
		ID:           id,
		Name:         name,
		SourceLang:   sourceLang,
		TargetLang:   targetLang,
		Status:       StatusRunning,
		Stages:       make([]StageResult, 0),
		StartedAt:    time.Now(),
		InputModules: inputModules,
	}

	e.store.CreateRun(run)

	e.hub.Broadcast(&Event{
		Type:      "migration.started",
		Timestamp: time.Now(),
		RunID:     id,
		Data:      run,
	})
}

// StageStarted adds a new StageResult with StatusRunning to the run's Stages slice,
// broadcasts "stage.started" event.
func (e *Emitter) StageStarted(runID string, stage StageKind) {
	stageResult := StageResult{
		Stage:     stage,
		Status:    StatusRunning,
		StartedAt: time.Now(),
		Metrics:   StageMetrics{},
	}

	e.store.UpdateRun(runID, func(run *MigrationRun) {
		run.Stages = append(run.Stages, stageResult)
	})

	e.hub.Broadcast(&Event{
		Type:      "stage.started",
		Timestamp: time.Now(),
		RunID:     runID,
		Stage:     stage,
		Data:      stageResult,
	})
}

// StageCompleted updates the stage to StatusCompleted, sets duration,
// broadcasts "stage.completed" event.
func (e *Emitter) StageCompleted(runID string, stage StageKind, metrics StageMetrics) {
	var stageResult StageResult

	e.store.UpdateRun(runID, func(run *MigrationRun) {
		// Find the stage in the run's stages
		for i := range run.Stages {
			if run.Stages[i].Stage == stage {
				now := time.Now()
				run.Stages[i].Status = StatusCompleted
				run.Stages[i].CompletedAt = &now
				run.Stages[i].Duration = now.Sub(run.Stages[i].StartedAt)
				run.Stages[i].Metrics = metrics
				stageResult = run.Stages[i]
				break
			}
		}
	})

	e.hub.Broadcast(&Event{
		Type:      "stage.completed",
		Timestamp: time.Now(),
		RunID:     runID,
		Stage:     stage,
		Data:      stageResult,
	})
}

// StageFailed updates the stage to StatusFailed, sets error,
// broadcasts "stage.failed" event.
func (e *Emitter) StageFailed(runID string, stage StageKind, err error) {
	var stageResult StageResult
	errorMsg := ""
	if err != nil {
		errorMsg = err.Error()
	}

	e.store.UpdateRun(runID, func(run *MigrationRun) {
		// Find the stage in the run's stages
		for i := range run.Stages {
			if run.Stages[i].Stage == stage {
				now := time.Now()
				run.Stages[i].Status = StatusFailed
				run.Stages[i].CompletedAt = &now
				run.Stages[i].Duration = now.Sub(run.Stages[i].StartedAt)
				run.Stages[i].Error = errorMsg
				stageResult = run.Stages[i]
				break
			}
		}
	})

	e.hub.Broadcast(&Event{
		Type:      "stage.failed",
		Timestamp: time.Now(),
		RunID:     runID,
		Stage:     stage,
		Data:      stageResult,
	})
}

// MigrationCompleted updates run to StatusCompleted, sets metrics,
// broadcasts "migration.completed" event.
func (e *Emitter) MigrationCompleted(runID string, outputFiles, testsPassed, testsFailed, llmCalls, totalTokens int) {
	var completedRun *MigrationRun

	e.store.UpdateRun(runID, func(run *MigrationRun) {
		now := time.Now()
		run.Status = StatusCompleted
		run.CompletedAt = &now
		run.OutputFiles = outputFiles
		run.TestsPassed = testsPassed
		run.TestsFailed = testsFailed
		run.LLMCalls = llmCalls
		run.TotalTokens = totalTokens
		completedRun = run
	})

	e.hub.Broadcast(&Event{
		Type:      "migration.completed",
		Timestamp: time.Now(),
		RunID:     runID,
		Data:      completedRun,
	})
}

// MigrationFailed updates run to StatusFailed, broadcasts "migration.failed" event.
func (e *Emitter) MigrationFailed(runID string, err error) {
	var failedRun *MigrationRun
	errorMsg := ""
	if err != nil {
		errorMsg = err.Error()
	}

	e.store.UpdateRun(runID, func(run *MigrationRun) {
		now := time.Now()
		run.Status = StatusFailed
		run.CompletedAt = &now
		run.Error = errorMsg
		failedRun = run
	})

	e.hub.Broadcast(&Event{
		Type:      "migration.failed",
		Timestamp: time.Now(),
		RunID:     runID,
		Data:      failedRun,
	})
}

// Log adds a LogEntry to the store, broadcasts "log" event.
func (e *Emitter) Log(runID, stage, level, message string) {
	entry := LogEntry{
		Timestamp: time.Now(),
		Level:     level,
		Message:   message,
		RunID:     runID,
		Stage:     stage,
	}

	e.store.AddLog(entry)

	e.hub.Broadcast(&Event{
		Type:      "log",
		Timestamp: time.Now(),
		RunID:     runID,
		Data:      entry,
	})
}
