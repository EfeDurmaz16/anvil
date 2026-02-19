package qualitygate

import "fmt"

// GateConfig defines the configuration for quality gates.
type GateConfig struct {
	Enabled        bool    `mapstructure:"enabled" json:"enabled"`
	ScoreThreshold float64 `mapstructure:"score_threshold" json:"score_threshold"`
	ScoreSeverity  string  `mapstructure:"score_severity" json:"score_severity"`

	CompilationRequired bool `mapstructure:"compilation_required" json:"compilation_required"`

	FixturePassRate     float64 `mapstructure:"fixture_pass_rate" json:"fixture_pass_rate"`
	FixtureSeverity     string  `mapstructure:"fixture_severity" json:"fixture_severity"`

	CoverageThreshold   float64 `mapstructure:"coverage_threshold" json:"coverage_threshold"`
	CoverageSeverity    string  `mapstructure:"coverage_severity" json:"coverage_severity"`

	MaxTokens           int    `mapstructure:"max_tokens" json:"max_tokens"`
	TokenSeverity       string `mapstructure:"token_severity" json:"token_severity"`

	MaxErrors           int    `mapstructure:"max_errors" json:"max_errors"`
	ErrorSeverity       string `mapstructure:"error_severity" json:"error_severity"`
}

// DefaultConfig returns sensible default gate configuration.
func DefaultConfig() *GateConfig {
	return &GateConfig{
		Enabled:             true,
		ScoreThreshold:      0.8,
		ScoreSeverity:       "required",
		CompilationRequired: true,
		FixturePassRate:     0.95,
		FixtureSeverity:     "required",
		CoverageThreshold:   0.9,
		CoverageSeverity:    "advisory",
		MaxTokens:           0, // disabled by default
		TokenSeverity:       "advisory",
		MaxErrors:           0,
		ErrorSeverity:       "critical",
	}
}

// parseSeverity converts a string to GateSeverity.
func parseSeverity(s string) GateSeverity {
	switch s {
	case "critical":
		return SeverityCritical
	case "required":
		return SeverityRequired
	case "advisory":
		return SeverityAdvisory
	default:
		return SeverityRequired
	}
}

// BuildPipeline constructs a gate pipeline from configuration.
func BuildPipeline(cfg *GateConfig) *Pipeline {
	if cfg == nil {
		cfg = DefaultConfig()
	}

	p := NewPipeline()

	if cfg.CompilationRequired {
		p.AddGate(NewCompilationGate(SeverityCritical))
	}

	if cfg.ScoreThreshold > 0 {
		p.AddGate(NewScoreGate(cfg.ScoreThreshold, parseSeverity(cfg.ScoreSeverity)))
	}

	if cfg.FixturePassRate > 0 {
		p.AddGate(NewFixtureGate(cfg.FixturePassRate, parseSeverity(cfg.FixtureSeverity)))
	}

	if cfg.CoverageThreshold > 0 {
		p.AddGate(NewCoverageGate(cfg.CoverageThreshold, parseSeverity(cfg.CoverageSeverity)))
	}

	if cfg.MaxTokens > 0 {
		p.AddGate(NewTokenBudgetGate(cfg.MaxTokens, parseSeverity(cfg.TokenSeverity)))
	}

	if cfg.MaxErrors >= 0 {
		p.AddGate(NewErrorGate(cfg.MaxErrors, parseSeverity(cfg.ErrorSeverity)))
	}

	return p
}

// FormatReport returns a human-readable quality gate report.
func FormatReport(result *PipelineResult) string {
	var s string
	s += "╔══════════════════════════════════════════╗\n"
	s += "║        Quality Gate Report               ║\n"
	s += "╠══════════════════════════════════════════╣\n"

	for _, gr := range result.Gates {
		icon := "✓"
		switch gr.Status {
		case GateFailed:
			icon = "✗"
		case GateSkipped:
			icon = "○"
		case GateWarning:
			icon = "⚠"
		}

		severity := ""
		switch gr.Severity {
		case SeverityCritical:
			severity = "[CRITICAL]"
		case SeverityRequired:
			severity = "[REQUIRED]"
		case SeverityAdvisory:
			severity = "[ADVISORY]"
		}

		s += fmt.Sprintf("║ %s %-14s %-10s %s\n", icon, gr.Name, severity, gr.Message)
		for _, d := range gr.Details {
			s += fmt.Sprintf("║   → %s\n", d)
		}
	}

	s += "╠══════════════════════════════════════════╣\n"
	status := "PASSED"
	if result.Status == GateFailed {
		status = "FAILED"
	}
	s += fmt.Sprintf("║ Result: %s (%s)\n", status, result.Summary)
	s += "╚══════════════════════════════════════════╝\n"

	return s
}
