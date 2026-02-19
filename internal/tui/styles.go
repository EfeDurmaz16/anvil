package tui

import "github.com/charmbracelet/lipgloss"

// Color constants matching the dark dashboard theme
const (
	ColorBg          = "#0d1117"
	ColorCard        = "#161b22"
	ColorBorder      = "#30363d"
	ColorBlue        = "#58a6ff"
	ColorGreen       = "#3fb950"
	ColorRed         = "#f85149"
	ColorYellow      = "#d29922"
	ColorGray        = "#8b949e"
	ColorText        = "#c9d1d9"
	ColorBright      = "#f0f6fc"
)

// Styles holds all lipgloss styles for the TUI
type Styles struct {
	// Text styles
	Title    lipgloss.Style
	Subtitle lipgloss.Style
	Help     lipgloss.Style

	// Status badges
	StatusSuccess lipgloss.Style
	StatusFailed  lipgloss.Style
	StatusPartial lipgloss.Style
	StatusPending lipgloss.Style

	// Code display
	CodeBlock lipgloss.Style

	// Diff highlighting
	DiffAdded   lipgloss.Style
	DiffRemoved lipgloss.Style
	DiffContext lipgloss.Style

	// Score badge
	ScoreBadge lipgloss.Style

	// Borders
	Border       lipgloss.Style
	ActiveBorder lipgloss.Style

	// Tabs
	Tab       lipgloss.Style
	ActiveTab lipgloss.Style

	// Spinner
	Spinner lipgloss.Style
}

// DefaultStyles creates the default style set
func DefaultStyles() *Styles {
	return &Styles{
		Title: lipgloss.NewStyle().
			Bold(true).
			Foreground(lipgloss.Color(ColorBright)).
			MarginBottom(1),

		Subtitle: lipgloss.NewStyle().
			Foreground(lipgloss.Color(ColorText)).
			MarginBottom(1),

		Help: lipgloss.NewStyle().
			Foreground(lipgloss.Color(ColorGray)).
			Italic(true),

		StatusSuccess: lipgloss.NewStyle().
			Background(lipgloss.Color(ColorGreen)).
			Foreground(lipgloss.Color(ColorBg)).
			Padding(0, 1).
			Bold(true),

		StatusFailed: lipgloss.NewStyle().
			Background(lipgloss.Color(ColorRed)).
			Foreground(lipgloss.Color(ColorBg)).
			Padding(0, 1).
			Bold(true),

		StatusPartial: lipgloss.NewStyle().
			Background(lipgloss.Color(ColorYellow)).
			Foreground(lipgloss.Color(ColorBg)).
			Padding(0, 1).
			Bold(true),

		StatusPending: lipgloss.NewStyle().
			Background(lipgloss.Color(ColorGray)).
			Foreground(lipgloss.Color(ColorBg)).
			Padding(0, 1).
			Bold(true),

		CodeBlock: lipgloss.NewStyle().
			Background(lipgloss.Color(ColorCard)).
			Foreground(lipgloss.Color(ColorText)).
			Padding(1, 2).
			BorderStyle(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color(ColorBorder)),

		DiffAdded: lipgloss.NewStyle().
			Background(lipgloss.Color(ColorGreen)).
			Foreground(lipgloss.Color(ColorBg)),

		DiffRemoved: lipgloss.NewStyle().
			Background(lipgloss.Color(ColorRed)).
			Foreground(lipgloss.Color(ColorBg)),

		DiffContext: lipgloss.NewStyle().
			Foreground(lipgloss.Color(ColorGray)),

		ScoreBadge: lipgloss.NewStyle().
			Padding(0, 1).
			Bold(true),

		Border: lipgloss.NewStyle().
			BorderStyle(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color(ColorBorder)).
			Padding(1, 2),

		ActiveBorder: lipgloss.NewStyle().
			BorderStyle(lipgloss.RoundedBorder()).
			BorderForeground(lipgloss.Color(ColorBlue)).
			Padding(1, 2),

		Tab: lipgloss.NewStyle().
			Foreground(lipgloss.Color(ColorGray)).
			Padding(0, 2),

		ActiveTab: lipgloss.NewStyle().
			Foreground(lipgloss.Color(ColorBlue)).
			Bold(true).
			Padding(0, 2).
			BorderStyle(lipgloss.Border{Bottom: "─"}).
			BorderBottom(true).
			BorderForeground(lipgloss.Color(ColorBlue)),

		Spinner: lipgloss.NewStyle().
			Foreground(lipgloss.Color(ColorBlue)),
	}
}

// ScoreColor returns a styled badge based on the score value
// Green for >=0.8, yellow for >=0.5, red for <0.5
func ScoreColor(score float64) lipgloss.Style {
	style := lipgloss.NewStyle().
		Padding(0, 1).
		Bold(true)

	if score >= 0.8 {
		return style.
			Background(lipgloss.Color(ColorGreen)).
			Foreground(lipgloss.Color(ColorBg))
	} else if score >= 0.5 {
		return style.
			Background(lipgloss.Color(ColorYellow)).
			Foreground(lipgloss.Color(ColorBg))
	} else {
		return style.
			Background(lipgloss.Color(ColorRed)).
			Foreground(lipgloss.Color(ColorBg))
	}
}
