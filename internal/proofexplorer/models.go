package proofexplorer

import "time"

// ProofPackSummary is a lightweight listing of available proof packs.
type ProofPackSummary struct {
	ID           string    `json:"id"`
	Dir          string    `json:"dir"`
	Pass         bool      `json:"pass"`
	FixtureCount int       `json:"fixture_count"`
	PassCount    int       `json:"pass_count"`
	FailCount    int       `json:"fail_count"`
	StartedAt    time.Time `json:"started_at"`
	EndedAt      time.Time `json:"ended_at"`
	Duration     string    `json:"duration"`
}

// ProofPackDetail is the full proof pack with all fixture results.
type ProofPackDetail struct {
	ProofPackSummary
	Results []FixtureDetail `json:"results"`
}

// FixtureDetail is a single fixture result for display.
type FixtureDetail struct {
	Name          string `json:"name"`
	Kind          string `json:"kind"`
	CorrelationID string `json:"correlation_id,omitempty"`
	Pass          bool   `json:"pass"`
	DiffKind      string `json:"diff_kind"`
	DiffReason    string `json:"diff_reason,omitempty"`
}

// ExplorerStats holds aggregate stats across all loaded proof packs.
type ExplorerStats struct {
	TotalPacks         int     `json:"total_packs"`
	TotalFixtures      int     `json:"total_fixtures"`
	TotalPassed        int     `json:"total_passed"`
	TotalFailed        int     `json:"total_failed"`
	OverallPassRate    float64 `json:"overall_pass_rate"`
	AvgFixturesPerPack float64 `json:"avg_fixtures_per_pack"`
}
