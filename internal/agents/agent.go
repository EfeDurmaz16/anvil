package agents

import (
	"context"
	"time"

	"github.com/efebarandurmaz/anvil/internal/graph"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/llm"
	"github.com/efebarandurmaz/anvil/internal/plugins"
	"github.com/efebarandurmaz/anvil/internal/vector"
)

// ResultVersion is the current version of the AgentResult schema.
const ResultVersion = "1.0.0"

// AgentStatus represents the execution status of an agent.
type AgentStatus string

const (
	StatusSuccess     AgentStatus = "success"
	StatusPartial     AgentStatus = "partial"    // Completed with some errors
	StatusFailed      AgentStatus = "failed"
	StatusPassthrough AgentStatus = "passthrough" // LLM unavailable, passed through
)

// Agent is the interface for all pipeline agents.
type Agent interface {
	// Name returns the agent identifier.
	Name() string
	// Run executes the agent's task.
	Run(ctx context.Context, ac *AgentContext) (*AgentResult, error)
}

// AgentContext provides shared resources to agents.
type AgentContext struct {
	Graph    *ir.SemanticGraph
	LLM      llm.Provider
	GraphDB  graph.Repository
	VectorDB vector.Repository
	Registry *plugins.Registry
	Params   map[string]string
}

// AgentMetrics captures performance and quality metrics.
type AgentMetrics struct {
	// Timing metrics
	StartTime   time.Time     `json:"start_time"`
	EndTime     time.Time     `json:"end_time"`
	Duration    time.Duration `json:"duration_ms"`
	LLMCalls    int           `json:"llm_calls"`
	LLMDuration time.Duration `json:"llm_duration_ms"`

	// Processing metrics
	InputItems    int `json:"input_items"`    // Functions/files processed
	OutputItems   int `json:"output_items"`   // Items produced
	SkippedItems  int `json:"skipped_items"`  // Items skipped due to errors

	// Quality metrics (used by Judge)
	CompilationScore float64 `json:"compilation_score,omitempty"` // 0-1
	FunctionalScore  float64 `json:"functional_score,omitempty"`  // 0-1
	DBDiffScore      float64 `json:"db_diff_score,omitempty"`     // 0-1
	OverallScore     float64 `json:"overall_score,omitempty"`     // 0-1

	// Token usage (for cost tracking)
	PromptTokens     int `json:"prompt_tokens,omitempty"`
	CompletionTokens int `json:"completion_tokens,omitempty"`
}

// AgentResult captures agent output.
type AgentResult struct {
	// Version for schema compatibility
	Version string `json:"version"`

	// Status indicates execution outcome
	Status AgentStatus `json:"status"`

	// Core outputs
	Graph          *ir.SemanticGraph      `json:"graph,omitempty"`
	GeneratedFiles []plugins.GeneratedFile `json:"generated_files,omitempty"`

	// Quality score (0-1, used by Judge)
	Score float64 `json:"score"`

	// Errors and warnings
	Errors   []string `json:"errors,omitempty"`
	Warnings []string `json:"warnings,omitempty"`

	// Structured metrics
	Metrics *AgentMetrics `json:"metrics,omitempty"`

	// Arbitrary metadata (backwards compatible)
	Metadata map[string]string `json:"metadata,omitempty"`
}

// NewAgentResult creates a new AgentResult with defaults.
func NewAgentResult() *AgentResult {
	return &AgentResult{
		Version:  ResultVersion,
		Status:   StatusSuccess,
		Score:    1.0,
		Errors:   []string{},
		Warnings: []string{},
		Metrics:  &AgentMetrics{StartTime: time.Now()},
		Metadata: make(map[string]string),
	}
}

// Finalize sets end time and calculates duration.
func (r *AgentResult) Finalize() {
	if r.Metrics != nil {
		r.Metrics.EndTime = time.Now()
		r.Metrics.Duration = r.Metrics.EndTime.Sub(r.Metrics.StartTime)
	}
	// Set status based on errors
	if len(r.Errors) > 0 {
		if r.Status == StatusSuccess {
			r.Status = StatusPartial
		}
	}
}

// AddError adds an error and updates status.
func (r *AgentResult) AddError(err string) {
	r.Errors = append(r.Errors, err)
	if r.Status == StatusSuccess {
		r.Status = StatusPartial
	}
}

// AddWarning adds a warning without affecting status.
func (r *AgentResult) AddWarning(warn string) {
	r.Warnings = append(r.Warnings, warn)
}

// SetPassthrough marks result as passthrough mode (no LLM).
func (r *AgentResult) SetPassthrough(reason string) {
	r.Status = StatusPassthrough
	// Backwards compatibility: older callers check Metadata["mode"].
	r.Metadata["mode"] = "passthrough"
	r.Metadata["passthrough_reason"] = reason
}

// RecordLLMCall tracks an LLM call for metrics.
func (r *AgentResult) RecordLLMCall(duration time.Duration, promptTokens, completionTokens int) {
	if r.Metrics != nil {
		r.Metrics.LLMCalls++
		r.Metrics.LLMDuration += duration
		r.Metrics.PromptTokens += promptTokens
		r.Metrics.CompletionTokens += completionTokens
	}
}
