package dashboard

import (
	"sort"
	"sync"
	"time"
)

const (
	maxRuns       = 100
	maxLogsPerRun = 1000
	maxTotalLogs  = 10000
)

// Store provides thread-safe in-memory storage for migration runs and logs.
type Store struct {
	mu   sync.RWMutex
	runs map[string]*MigrationRun
	logs []LogEntry
}

// NewStore creates a new Store instance.
func NewStore() *Store {
	return &Store{
		runs: make(map[string]*MigrationRun),
		logs: make([]LogEntry, 0, maxTotalLogs),
	}
}

// CreateRun adds a new migration run to the store.
func (s *Store) CreateRun(run *MigrationRun) {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.runs[run.ID] = run
	s.evictOldRuns()
}

// GetRun retrieves a migration run by ID.
func (s *Store) GetRun(id string) (*MigrationRun, bool) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	run, ok := s.runs[id]
	return run, ok
}

// ListRuns returns all migration runs sorted by StartedAt descending.
func (s *Store) ListRuns() []*MigrationRun {
	s.mu.RLock()
	defer s.mu.RUnlock()

	runs := make([]*MigrationRun, 0, len(s.runs))
	for _, run := range s.runs {
		runs = append(runs, run)
	}

	sort.Slice(runs, func(i, j int) bool {
		return runs[i].StartedAt.After(runs[j].StartedAt)
	})

	return runs
}

// UpdateRun performs a thread-safe update on a migration run.
func (s *Store) UpdateRun(id string, fn func(*MigrationRun)) {
	s.mu.Lock()
	defer s.mu.Unlock()

	if run, ok := s.runs[id]; ok {
		fn(run)
	}
}

// DeleteRun removes a migration run from the store.
func (s *Store) DeleteRun(id string) {
	s.mu.Lock()
	defer s.mu.Unlock()

	delete(s.runs, id)
}

// GetStats computes and returns aggregate statistics.
func (s *Store) GetStats() *DashboardStats {
	s.mu.RLock()
	defer s.mu.RUnlock()

	stats := &DashboardStats{
		TotalMigrations: len(s.runs),
	}

	var totalDuration time.Duration
	var completedCount int

	for _, run := range s.runs {
		switch run.Status {
		case StatusRunning, StatusPending:
			stats.ActiveMigrations++
		case StatusCompleted:
			stats.CompletedMigrations++
			completedCount++
			if run.CompletedAt != nil {
				totalDuration += run.CompletedAt.Sub(run.StartedAt)
			}
		case StatusFailed:
			stats.FailedMigrations++
		}

		stats.TotalLLMCalls += run.LLMCalls
		stats.TotalTokens += run.TotalTokens
	}

	if completedCount > 0 {
		stats.AvgDuration = totalDuration.Seconds() / float64(completedCount)
	}

	if stats.TotalMigrations > 0 {
		stats.SuccessRate = float64(stats.CompletedMigrations) / float64(stats.TotalMigrations)
	}

	return stats
}

// AddLog adds a log entry to the store.
func (s *Store) AddLog(entry LogEntry) {
	s.mu.Lock()
	defer s.mu.Unlock()

	s.logs = append(s.logs, entry)

	// Evict old logs if we exceed the max
	if len(s.logs) > maxTotalLogs {
		s.logs = s.logs[len(s.logs)-maxTotalLogs:]
	}
}

// GetLogs retrieves logs for a specific run, most recent first.
func (s *Store) GetLogs(runID string, limit int) []LogEntry {
	s.mu.RLock()
	defer s.mu.RUnlock()

	var filtered []LogEntry
	for i := len(s.logs) - 1; i >= 0; i-- {
		if s.logs[i].RunID == runID {
			filtered = append(filtered, s.logs[i])
			if limit > 0 && len(filtered) >= limit {
				break
			}
		}
	}

	return filtered
}

// evictOldRuns removes the oldest completed runs if we exceed maxRuns.
// Must be called with lock held.
func (s *Store) evictOldRuns() {
	if len(s.runs) <= maxRuns {
		return
	}

	// Collect completed runs sorted by completion time
	type runTime struct {
		id   string
		time time.Time
	}

	var completed []runTime
	for id, run := range s.runs {
		if run.Status == StatusCompleted || run.Status == StatusFailed {
			t := run.StartedAt
			if run.CompletedAt != nil {
				t = *run.CompletedAt
			}
			completed = append(completed, runTime{id: id, time: t})
		}
	}

	if len(completed) == 0 {
		return
	}

	// Sort oldest first
	sort.Slice(completed, func(i, j int) bool {
		return completed[i].time.Before(completed[j].time)
	})

	// Delete oldest until we're under the limit
	toDelete := len(s.runs) - maxRuns
	for i := 0; i < toDelete && i < len(completed); i++ {
		delete(s.runs, completed[i].id)
	}
}
