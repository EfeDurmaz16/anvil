package snapshot

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"time"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// Snapshot represents a point-in-time capture of a migration run's outputs.
type Snapshot struct {
	ID          string            `json:"id"`
	ParentID    string            `json:"parent_id,omitempty"`
	Tag         string            `json:"tag,omitempty"`
	Description string            `json:"description,omitempty"`
	CreatedAt   time.Time         `json:"created_at"`
	SourceLang  string            `json:"source_lang"`
	TargetLang  string            `json:"target_lang"`
	InputPath   string            `json:"input_path"`
	ContentHash string            `json:"content_hash"`
	Score       float64           `json:"score"`
	Status      string            `json:"status"` // success, partial, failed
	Provenance  *Provenance       `json:"provenance"`
	AgentStages []AgentStageInfo  `json:"agent_stages"`
	FileManifest []FileEntry      `json:"file_manifest"`
	Metadata    map[string]string `json:"metadata,omitempty"`
}

// AgentStageInfo captures per-agent metadata for provenance tracking.
type AgentStageInfo struct {
	Name          string        `json:"name"` // cartographer, specular, architect, judge, testgen
	Status        string        `json:"status"`
	Score         float64       `json:"score"`
	Duration      time.Duration `json:"duration"`
	LLMCalls      int           `json:"llm_calls"`
	PromptTokens  int           `json:"prompt_tokens"`
	CompletionTokens int       `json:"completion_tokens"`
	Model         string        `json:"model,omitempty"`
	ErrorCount    int           `json:"error_count"`
}

// Provenance tracks which LLM configuration produced this output.
type Provenance struct {
	Provider       string            `json:"provider"`
	Model          string            `json:"model"`
	TotalLLMCalls  int               `json:"total_llm_calls"`
	TotalTokens    int               `json:"total_tokens"`
	TotalDuration  time.Duration     `json:"total_duration"`
	ConfigHash     string            `json:"config_hash,omitempty"`
	AgentOverrides map[string]string `json:"agent_overrides,omitempty"`
}

// FileEntry records a generated file with its content hash.
type FileEntry struct {
	Path        string `json:"path"`
	ContentHash string `json:"content_hash"`
	Size        int    `json:"size"`
}

// SnapshotIndex is a lightweight listing of all snapshots for fast lookup.
type SnapshotIndex struct {
	Snapshots []SnapshotSummary `json:"snapshots"`
	UpdatedAt time.Time         `json:"updated_at"`
}

// SnapshotSummary is the minimal info for listing snapshots.
type SnapshotSummary struct {
	ID         string    `json:"id"`
	ParentID   string    `json:"parent_id,omitempty"`
	Tag        string    `json:"tag,omitempty"`
	CreatedAt  time.Time `json:"created_at"`
	SourceLang string    `json:"source_lang"`
	TargetLang string    `json:"target_lang"`
	Score      float64   `json:"score"`
	Status     string    `json:"status"`
	FileCount  int       `json:"file_count"`
}

// NewSnapshotFromResults creates a Snapshot from pipeline agent results.
func NewSnapshotFromResults(
	sourceLang, targetLang, inputPath string,
	graph *ir.SemanticGraph,
	files []plugins.GeneratedFile,
	agentResults map[string]*agents.AgentResult,
) *Snapshot {
	now := time.Now()
	snap := &Snapshot{
		CreatedAt:  now,
		SourceLang: sourceLang,
		TargetLang: targetLang,
		InputPath:  inputPath,
		Status:     "success",
		Metadata:   make(map[string]string),
	}

	// Build file manifest
	for _, f := range files {
		hash := ContentHash(f.Content)
		snap.FileManifest = append(snap.FileManifest, FileEntry{
			Path:        f.Path,
			ContentHash: hash,
			Size:        len(f.Content),
		})
	}

	// Build agent stage info + provenance
	var totalLLMCalls, totalTokens int
	var totalDuration time.Duration
	var bestScore float64

	for name, result := range agentResults {
		stage := AgentStageInfo{
			Name:       name,
			Status:     string(result.Status),
			Score:      result.Score,
			ErrorCount: len(result.Errors),
		}
		if result.Metrics != nil {
			stage.Duration = result.Metrics.Duration
			stage.LLMCalls = result.Metrics.LLMCalls
			stage.PromptTokens = result.Metrics.PromptTokens
			stage.CompletionTokens = result.Metrics.CompletionTokens
			totalLLMCalls += result.Metrics.LLMCalls
			totalTokens += result.Metrics.PromptTokens + result.Metrics.CompletionTokens
			totalDuration += result.Metrics.LLMDuration
		}
		if result.Score > bestScore {
			bestScore = result.Score
		}
		snap.AgentStages = append(snap.AgentStages, stage)
	}

	snap.Score = bestScore
	snap.Provenance = &Provenance{
		TotalLLMCalls: totalLLMCalls,
		TotalTokens:   totalTokens,
		TotalDuration: totalDuration,
	}

	// Compute content hash from all files
	snap.ContentHash = computeManifestHash(snap.FileManifest)
	// Generate ID from content + time
	snap.ID = generateSnapshotID(snap)

	return snap
}

// ContentHash computes SHA-256 of content.
func ContentHash(content []byte) string {
	h := sha256.Sum256(content)
	return hex.EncodeToString(h[:])
}

func computeManifestHash(entries []FileEntry) string {
	h := sha256.New()
	for _, e := range entries {
		h.Write([]byte(e.Path))
		h.Write([]byte(e.ContentHash))
	}
	return hex.EncodeToString(h.Sum(nil))
}

func generateSnapshotID(snap *Snapshot) string {
	data, _ := json.Marshal(struct {
		Time    int64  `json:"t"`
		Content string `json:"c"`
	}{
		Time:    snap.CreatedAt.UnixNano(),
		Content: snap.ContentHash,
	})
	h := sha256.Sum256(data)
	return hex.EncodeToString(h[:8]) // Short 16-char hex ID
}

// Summary returns a lightweight summary of this snapshot.
func (s *Snapshot) Summary() SnapshotSummary {
	return SnapshotSummary{
		ID:         s.ID,
		ParentID:   s.ParentID,
		Tag:        s.Tag,
		CreatedAt:  s.CreatedAt,
		SourceLang: s.SourceLang,
		TargetLang: s.TargetLang,
		Score:      s.Score,
		Status:     s.Status,
		FileCount:  len(s.FileManifest),
	}
}
