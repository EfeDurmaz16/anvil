package qualitygate

import (
	"fmt"
	"time"
)

// GateStatus represents the result of a quality gate check.
type GateStatus string

const (
	GatePassed  GateStatus = "passed"
	GateFailed  GateStatus = "failed"
	GateSkipped GateStatus = "skipped"
	GateWarning GateStatus = "warning"
)

// GateSeverity indicates how critical a gate failure is.
type GateSeverity string

const (
	SeverityCritical GateSeverity = "critical" // Pipeline must abort
	SeverityRequired GateSeverity = "required" // Must pass but allows retry
	SeverityAdvisory GateSeverity = "advisory" // Warning only, does not block
)

// GateResult captures the outcome of a single gate evaluation.
type GateResult struct {
	Name        string       `json:"name"`
	Status      GateStatus   `json:"status"`
	Severity    GateSeverity `json:"severity"`
	Score       float64      `json:"score"`       // 0.0-1.0 normalized
	Threshold   float64      `json:"threshold"`   // Required minimum
	Message     string       `json:"message"`
	Details     []string     `json:"details,omitempty"`
	Duration    time.Duration `json:"duration"`
	EvaluatedAt time.Time    `json:"evaluated_at"`
}

// Gate is the interface all quality gates must implement.
type Gate interface {
	Name() string
	Severity() GateSeverity
	Evaluate(ctx *EvalContext) (*GateResult, error)
}

// EvalContext provides data for gate evaluation.
type EvalContext struct {
	Score            float64            // Judge score (0-1)
	CompilationOK    bool               // Did code compile?
	CompileErrors    []string           // Compilation errors
	FixturesPassed   int                // Number of fixtures passed
	FixturesTotal    int                // Total fixtures
	FunctionsTotal   int                // Total functions in graph
	FunctionsMatched int                // Functions verified by judge
	LLMCalls         int                // Total LLM calls made
	TotalTokens      int                // Total tokens used
	GeneratedFiles   int                // Number of files generated
	Warnings         []string           // Pipeline warnings
	Errors           []string           // Pipeline errors
	Metadata         map[string]string  // Additional context
}

// PipelineResult captures the complete gate pipeline evaluation.
type PipelineResult struct {
	Status      GateStatus    `json:"status"`      // Overall: passed if all required gates pass
	Gates       []GateResult  `json:"gates"`
	PassedCount int           `json:"passed_count"`
	FailedCount int           `json:"failed_count"`
	SkippedCount int          `json:"skipped_count"`
	WarningCount int          `json:"warning_count"`
	Duration    time.Duration `json:"duration"`
	EvaluatedAt time.Time     `json:"evaluated_at"`
	Summary     string        `json:"summary"`
}

// Pipeline orchestrates multiple quality gates in sequence.
type Pipeline struct {
	gates []Gate
}

// NewPipeline creates a new quality gate pipeline.
func NewPipeline(gates ...Gate) *Pipeline {
	return &Pipeline{gates: gates}
}

// AddGate appends a gate to the pipeline.
func (p *Pipeline) AddGate(g Gate) {
	p.gates = append(p.gates, g)
}

// Run evaluates all gates against the provided context.
func (p *Pipeline) Run(ctx *EvalContext) *PipelineResult {
	start := time.Now()
	result := &PipelineResult{
		Status:      GatePassed,
		EvaluatedAt: start,
	}

	aborted := false

	for _, gate := range p.gates {
		if aborted {
			result.Gates = append(result.Gates, GateResult{
				Name:        gate.Name(),
				Status:      GateSkipped,
				Severity:    gate.Severity(),
				Message:     "Skipped due to critical gate failure",
				EvaluatedAt: time.Now(),
			})
			result.SkippedCount++
			continue
		}

		gateStart := time.Now()
		gr, err := gate.Evaluate(ctx)
		if err != nil {
			gr = &GateResult{
				Name:     gate.Name(),
				Status:   GateFailed,
				Severity: gate.Severity(),
				Message:  fmt.Sprintf("Gate evaluation error: %v", err),
			}
		}
		gr.Duration = time.Since(gateStart)
		gr.EvaluatedAt = gateStart

		result.Gates = append(result.Gates, *gr)

		switch gr.Status {
		case GatePassed:
			result.PassedCount++
		case GateFailed:
			result.FailedCount++
			if gr.Severity == SeverityCritical {
				aborted = true
				result.Status = GateFailed
			} else if gr.Severity == SeverityRequired {
				result.Status = GateFailed
			}
		case GateWarning:
			result.WarningCount++
		case GateSkipped:
			result.SkippedCount++
		}
	}

	result.Duration = time.Since(start)
	result.Summary = formatSummary(result)

	return result
}

func formatSummary(r *PipelineResult) string {
	return fmt.Sprintf("Quality Gates: %d passed, %d failed, %d warnings, %d skipped [%s]",
		r.PassedCount, r.FailedCount, r.WarningCount, r.SkippedCount, r.Status)
}
