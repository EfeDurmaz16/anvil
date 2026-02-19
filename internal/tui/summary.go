package tui

import (
	"fmt"
	"strings"

	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

// SummaryModel displays the final review summary after review is complete
type SummaryModel struct {
	session  *ReviewSession
	styles   *Styles
	width    int
	height   int
	quitting bool
}

// NewSummaryModel creates a new summary screen
func NewSummaryModel(session *ReviewSession) SummaryModel {
	return SummaryModel{
		session: session,
		styles:  DefaultStyles(),
	}
}

// Init implements tea.Model
func (m SummaryModel) Init() tea.Cmd {
	return nil
}

// Update implements tea.Model
func (m SummaryModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		return m, nil

	case tea.KeyMsg:
		switch msg.String() {
		case "q", "esc", "enter":
			m.quitting = true
			return m, tea.Quit
		}
	}

	return m, nil
}

// View implements tea.Model
func (m SummaryModel) View() string {
	if m.quitting {
		return ""
	}

	var b strings.Builder

	// Title
	title := m.styles.Title.Render("Review Summary")
	b.WriteString(title)
	b.WriteString("\n\n")

	// Calculate stats
	total := len(m.session.Items)
	approved := 0
	rejected := 0
	regenerated := 0
	pending := 0

	for _, item := range m.session.Items {
		switch item.Status {
		case ReviewApproved:
			approved++
		case ReviewRejected:
			rejected++
		case ReviewRegenerated:
			regenerated++
		case ReviewPending:
			pending++
		}
	}

	// Stats table
	statsTable := m.renderStatsTable(total, approved, rejected, regenerated, pending)
	b.WriteString(statsTable)
	b.WriteString("\n\n")

	// Overall score badge
	scoreLabel := fmt.Sprintf("Overall Score: %.1f%%", m.session.OverallScore*100)
	scoreBadge := ScoreColor(m.session.OverallScore).Render(scoreLabel)
	b.WriteString(scoreBadge)
	b.WriteString("\n\n")

	// List rejected/regeneration items
	if rejected > 0 || regenerated > 0 {
		b.WriteString(m.styles.Subtitle.Render("Items Requiring Attention:"))
		b.WriteString("\n\n")

		for _, item := range m.session.Items {
			if item.Status == ReviewRejected || item.Status == ReviewRegenerated {
				b.WriteString(m.renderItemDetail(item))
				b.WriteString("\n")
			}
		}
		b.WriteString("\n")
	}

	// Help text
	help := m.styles.Help.Render("Press enter to save and exit")
	b.WriteString(help)

	return b.String()
}

// renderStatsTable creates a formatted stats table
func (m SummaryModel) renderStatsTable(total, approved, rejected, regenerated, pending int) string {
	var b strings.Builder

	// Table header
	b.WriteString(m.styles.Subtitle.Render("Statistics"))
	b.WriteString("\n\n")

	// Rows
	b.WriteString(fmt.Sprintf("  Total functions:       %d\n", total))

	approvedStyle := lipgloss.NewStyle().Foreground(lipgloss.Color(ColorGreen)).Bold(true)
	b.WriteString(fmt.Sprintf("  Approved:              %s\n", approvedStyle.Render(fmt.Sprintf("%d", approved))))

	rejectedStyle := lipgloss.NewStyle().Foreground(lipgloss.Color(ColorRed)).Bold(true)
	b.WriteString(fmt.Sprintf("  Rejected:              %s\n", rejectedStyle.Render(fmt.Sprintf("%d", rejected))))

	regeneratedStyle := lipgloss.NewStyle().Foreground(lipgloss.Color(ColorYellow)).Bold(true)
	b.WriteString(fmt.Sprintf("  Needs regeneration:    %s\n", regeneratedStyle.Render(fmt.Sprintf("%d", regenerated))))

	pendingStyle := lipgloss.NewStyle().Foreground(lipgloss.Color(ColorGray))
	b.WriteString(fmt.Sprintf("  Pending:               %s\n", pendingStyle.Render(fmt.Sprintf("%d", pending))))

	return b.String()
}

// renderItemDetail renders a single item with status and details
func (m SummaryModel) renderItemDetail(item *ReviewItem) string {
	var b strings.Builder

	// Item header
	itemHeader := fmt.Sprintf("%s.%s", item.ModuleName, item.FunctionName)
	b.WriteString(m.styles.Subtitle.Render(itemHeader))
	b.WriteString(" ")

	// Status badge
	var statusBadge string
	switch item.Status {
	case ReviewRejected:
		statusBadge = m.styles.StatusFailed.Render("REJECTED")
	case ReviewRegenerated:
		statusBadge = m.styles.StatusPartial.Render("REGENERATED")
	}
	b.WriteString(statusBadge)
	b.WriteString("\n")

	// Score
	scoreLabel := fmt.Sprintf("Score: %.1f%%", item.JudgeScore*100)
	b.WriteString("  ")
	b.WriteString(scoreLabel)
	b.WriteString("\n")

	return b.String()
}
