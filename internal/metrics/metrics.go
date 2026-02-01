package metrics

import (
	"encoding/json"
	"fmt"
	"io"
	"time"

	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// PipelineMetrics collects statistics for a full pipeline run.
type PipelineMetrics struct {
	StartedAt  time.Time       `json:"started_at"`
	FinishedAt time.Time       `json:"finished_at,omitempty"`
	Duration   time.Duration   `json:"duration_ms,omitempty"`
	Source     SourceMetrics   `json:"source"`
	Target     TargetMetrics   `json:"target"`
	Agents     []AgentMetrics  `json:"agents"`
	LLMMode    string          `json:"llm_mode"` // "llm" or "passthrough"
	Score      float64         `json:"score"`
	Errors     []string        `json:"errors,omitempty"`
}

type SourceMetrics struct {
	Language      string `json:"language"`
	FileCount     int    `json:"file_count"`
	ModuleCount   int    `json:"module_count"`
	FunctionCount int    `json:"function_count"`
	DataTypeCount int    `json:"data_type_count"`
	IOContractCount int  `json:"io_contract_count"`
	CallEdgeCount int    `json:"call_edge_count"`
	RuleCount     int    `json:"rule_count"`
	TotalLines    int    `json:"total_lines"`
}

type TargetMetrics struct {
	Language       string `json:"language"`
	FilesGenerated int    `json:"files_generated"`
	TotalBytes     int    `json:"total_bytes"`
}

type AgentMetrics struct {
	Name     string        `json:"name"`
	Duration time.Duration `json:"duration_ms"`
	Mode     string        `json:"mode"`
	Errors   int           `json:"errors"`
}

// New starts tracking a pipeline run.
func New() *PipelineMetrics {
	return &PipelineMetrics{StartedAt: time.Now()}
}

// CollectSource computes source-side metrics from the IR.
func (m *PipelineMetrics) CollectSource(lang string, fileCount int, graph *ir.SemanticGraph) {
	m.Source.Language = lang
	m.Source.FileCount = fileCount
	m.Source.ModuleCount = len(graph.Modules)
	m.Source.DataTypeCount = len(graph.DataTypes)
	m.Source.RuleCount = len(graph.BusinessRules)

	if graph.CallGraph != nil {
		m.Source.CallEdgeCount = len(graph.CallGraph.Edges)
	}

	for _, mod := range graph.Modules {
		m.Source.FunctionCount += len(mod.Functions)
		m.Source.DataTypeCount += len(mod.DataTypes)
		m.Source.IOContractCount += len(mod.IOContracts)
	}
}

// CollectTarget computes target-side metrics from generated files.
func (m *PipelineMetrics) CollectTarget(lang string, files []plugins.GeneratedFile) {
	m.Target.Language = lang
	m.Target.FilesGenerated = len(files)
	for _, f := range files {
		m.Target.TotalBytes += len(f.Content)
	}
}

// AddAgent records a single agent's timing and status.
func (m *PipelineMetrics) AddAgent(name string, d time.Duration, mode string, errCount int) {
	m.Agents = append(m.Agents, AgentMetrics{
		Name:     name,
		Duration: d,
		Mode:     mode,
		Errors:   errCount,
	})
}

// Finish marks the pipeline as complete.
func (m *PipelineMetrics) Finish(score float64, errs []string) {
	m.FinishedAt = time.Now()
	m.Duration = m.FinishedAt.Sub(m.StartedAt)
	m.Score = score
	m.Errors = errs
}

// PrintSummary writes a human-readable summary.
func (m *PipelineMetrics) PrintSummary(w io.Writer) {
	fmt.Fprintf(w, "\n╔══════════════════════════════════════╗\n")
	fmt.Fprintf(w, "║        ANVIL PIPELINE REPORT         ║\n")
	fmt.Fprintf(w, "╠══════════════════════════════════════╣\n")
	fmt.Fprintf(w, "║ Duration:    %-23s║\n", m.Duration.Round(time.Millisecond))
	fmt.Fprintf(w, "║ LLM Mode:    %-23s║\n", m.LLMMode)
	fmt.Fprintf(w, "║ Score:       %-23.2f║\n", m.Score)
	fmt.Fprintf(w, "╠══════════════════════════════════════╣\n")
	fmt.Fprintf(w, "║ SOURCE (%s)\n", m.Source.Language)
	fmt.Fprintf(w, "║   Files:       %d\n", m.Source.FileCount)
	fmt.Fprintf(w, "║   Modules:     %d\n", m.Source.ModuleCount)
	fmt.Fprintf(w, "║   Functions:   %d\n", m.Source.FunctionCount)
	fmt.Fprintf(w, "║   Data Types:  %d\n", m.Source.DataTypeCount)
	fmt.Fprintf(w, "║   I/O:         %d\n", m.Source.IOContractCount)
	fmt.Fprintf(w, "║   Call Edges:  %d\n", m.Source.CallEdgeCount)
	fmt.Fprintf(w, "║   Biz Rules:   %d\n", m.Source.RuleCount)
	fmt.Fprintf(w, "╠══════════════════════════════════════╣\n")
	fmt.Fprintf(w, "║ TARGET (%s)\n", m.Target.Language)
	fmt.Fprintf(w, "║   Files:       %d\n", m.Target.FilesGenerated)
	fmt.Fprintf(w, "║   Total Size:  %s\n", formatBytes(m.Target.TotalBytes))
	fmt.Fprintf(w, "╠══════════════════════════════════════╣\n")
	fmt.Fprintf(w, "║ AGENTS\n")
	for _, a := range m.Agents {
		status := "OK"
		if a.Errors > 0 {
			status = fmt.Sprintf("%d errors", a.Errors)
		}
		fmt.Fprintf(w, "║   %-14s %8s  [%s] %s\n", a.Name, a.Duration.Round(time.Millisecond), a.Mode, status)
	}
	if len(m.Errors) > 0 {
		fmt.Fprintf(w, "╠══════════════════════════════════════╣\n")
		fmt.Fprintf(w, "║ ERRORS\n")
		for _, e := range m.Errors {
			fmt.Fprintf(w, "║   • %s\n", e)
		}
	}
	fmt.Fprintf(w, "╚══════════════════════════════════════╝\n")
}

// JSON returns the metrics as formatted JSON.
func (m *PipelineMetrics) JSON() ([]byte, error) {
	return json.MarshalIndent(m, "", "  ")
}

func formatBytes(b int) string {
	switch {
	case b >= 1<<20:
		return fmt.Sprintf("%.1f MB", float64(b)/float64(1<<20))
	case b >= 1<<10:
		return fmt.Sprintf("%.1f KB", float64(b)/float64(1<<10))
	default:
		return fmt.Sprintf("%d B", b)
	}
}
