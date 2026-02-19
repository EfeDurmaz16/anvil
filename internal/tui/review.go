package tui

import (
	"fmt"
	"strings"

	"github.com/charmbracelet/bubbles/help"
	"github.com/charmbracelet/bubbles/key"
	"github.com/charmbracelet/bubbles/textinput"
	"github.com/charmbracelet/bubbles/viewport"
	tea "github.com/charmbracelet/bubbletea"
	"github.com/charmbracelet/lipgloss"
)

type Pane int

const (
	PaneLeft Pane = iota
	PaneRight
)

type ReviewModel struct {
	session    *ReviewSession
	styles     *Styles
	cursor     int // current item index
	viewport   viewport.Model
	activePane Pane
	width      int
	height     int
	quitting   bool
	feedback   string // user feedback for re-generation
	inputMode  bool   // true when typing feedback
	textInput  textinput.Model
	help       help.Model
	keys       keyMap
}

type keyMap struct {
	Up       key.Binding
	Down     key.Binding
	Tab      key.Binding
	Approve  key.Binding
	Reject   key.Binding
	Feedback key.Binding
	Enter    key.Binding
	Quit     key.Binding
	Escape   key.Binding
}

func (km keyMap) ShortHelp() []key.Binding {
	return []key.Binding{
		km.Up,
		km.Down,
		km.Tab,
		km.Approve,
		km.Reject,
		km.Feedback,
		km.Quit,
	}
}

func (km keyMap) FullHelp() [][]key.Binding {
	return [][]key.Binding{
		{km.Up, km.Down, km.Tab},
		{km.Approve, km.Reject, km.Feedback},
		{km.Enter, km.Escape, km.Quit},
	}
}

func newKeyMap() keyMap {
	return keyMap{
		Up: key.NewBinding(
			key.WithKeys("k", "up"),
			key.WithHelp("k/↑", "prev item"),
		),
		Down: key.NewBinding(
			key.WithKeys("j", "down"),
			key.WithHelp("j/↓", "next item"),
		),
		Tab: key.NewBinding(
			key.WithKeys("tab"),
			key.WithHelp("tab", "switch pane"),
		),
		Approve: key.NewBinding(
			key.WithKeys("a"),
			key.WithHelp("a", "approve"),
		),
		Reject: key.NewBinding(
			key.WithKeys("r"),
			key.WithHelp("r", "reject"),
		),
		Feedback: key.NewBinding(
			key.WithKeys("f"),
			key.WithHelp("f", "feedback"),
		),
		Enter: key.NewBinding(
			key.WithKeys("enter"),
			key.WithHelp("enter", "submit"),
		),
		Quit: key.NewBinding(
			key.WithKeys("q"),
			key.WithHelp("q", "quit"),
		),
		Escape: key.NewBinding(
			key.WithKeys("esc"),
			key.WithHelp("esc", "cancel"),
		),
	}
}

func NewReviewModel(session *ReviewSession) ReviewModel {
	ti := textinput.New()
	ti.Placeholder = "Enter feedback for re-generation..."
	ti.Width = 50

	vp := viewport.New(0, 0)
	vp.Style = lipgloss.NewStyle()

	return ReviewModel{
		session:    session,
		styles:     DefaultStyles(),
		cursor:     0,
		viewport:   vp,
		activePane: PaneLeft,
		width:      80,
		height:     24,
		quitting:   false,
		feedback:   "",
		inputMode:  false,
		textInput:  ti,
		help:       help.New(),
		keys:       newKeyMap(),
	}
}

func (m ReviewModel) Init() tea.Cmd {
	return nil
}

func (m ReviewModel) Update(msg tea.Msg) (tea.Model, tea.Cmd) {
	var cmd tea.Cmd

	switch msg := msg.(type) {
	case tea.WindowSizeMsg:
		m.width = msg.Width
		m.height = msg.Height
		m.viewport.Width = msg.Width/2 - 4
		m.viewport.Height = msg.Height - 10
		return m, nil

	case tea.KeyMsg:
		if m.inputMode {
			switch msg.String() {
			case "enter":
				m.feedback = m.textInput.Value()
				if m.cursor < len(m.session.Items) {
					m.session.Items[m.cursor].Status = ReviewRegenerated
				}
				m.inputMode = false
				m.textInput.SetValue("")
				return m, nil
			case "esc":
				m.inputMode = false
				m.textInput.SetValue("")
				return m, nil
			default:
				m.textInput, cmd = m.textInput.Update(msg)
				return m, cmd
			}
		}

		switch msg.String() {
		case "j", "down":
			if m.cursor < len(m.session.Items)-1 {
				m.cursor++
			}
			return m, nil

		case "k", "up":
			if m.cursor > 0 {
				m.cursor--
			}
			return m, nil

		case "tab":
			if m.activePane == PaneLeft {
				m.activePane = PaneRight
			} else {
				m.activePane = PaneLeft
			}
			return m, nil

		case "a":
			if m.cursor < len(m.session.Items) {
				m.session.Items[m.cursor].Status = ReviewApproved
			}
			return m, nil

		case "r":
			if m.cursor < len(m.session.Items) {
				m.session.Items[m.cursor].Status = ReviewRejected
			}
			return m, nil

		case "f":
			m.inputMode = true
			m.textInput.Focus()
			return m, nil

		case "q", "esc":
			m.quitting = true
			return m, tea.Quit

		}
	}

	return m, nil
}

func (m ReviewModel) View() string {
	if m.quitting {
		return ""
	}

	if len(m.session.Items) == 0 {
		return m.styles.StatusFailed.Render("No items to review")
	}

	var sections []string

	// Top bar
	topBar := m.renderTopBar()
	sections = append(sections, topBar)

	// Item navigator
	navigator := m.renderNavigator()
	sections = append(sections, navigator)

	// Side-by-side panels
	panels := m.renderPanels()
	sections = append(sections, panels)

	// Bottom bar
	bottom := m.renderBottom()
	sections = append(sections, bottom)

	return lipgloss.JoinVertical(lipgloss.Left, sections...)
}

func (m ReviewModel) renderTopBar() string {
	title := fmt.Sprintf("Anvil Review - %s → %s", m.session.SourceLang, m.session.TargetLang)
	scoreText := fmt.Sprintf("%.2f", m.session.OverallScore)
	scoreBadge := ScoreColor(m.session.OverallScore).Render(scoreText)

	titleStyled := m.styles.Title.Render(title)
	return lipgloss.JoinHorizontal(lipgloss.Top, titleStyled, "  ", scoreBadge)
}

func (m ReviewModel) renderNavigator() string {
	if m.cursor >= len(m.session.Items) {
		return ""
	}

	item := m.session.Items[m.cursor]
	position := fmt.Sprintf("[%d/%d]", m.cursor+1, len(m.session.Items))
	itemName := fmt.Sprintf("%s.%s", item.ModuleName, item.FunctionName)
	score := fmt.Sprintf("Score: %.2f", item.JudgeScore)
	status := m.formatStatus(item.Status)

	parts := []string{position, itemName, score, status}
	return m.styles.Subtitle.Render(strings.Join(parts, "  "))
}

func (m ReviewModel) formatStatus(status ReviewStatus) string {
	switch status {
	case ReviewApproved:
		return m.styles.StatusSuccess.Render("[Approved]")
	case ReviewRejected:
		return m.styles.StatusFailed.Render("[Rejected]")
	case ReviewRegenerated:
		return m.styles.StatusPartial.Render("[Regenerated]")
	case ReviewPending:
		return m.styles.StatusPending.Render("[Pending]")
	default:
		return m.styles.StatusPending.Render("[Pending]")
	}
}

func (m ReviewModel) renderPanels() string {
	if m.cursor >= len(m.session.Items) {
		return ""
	}

	item := m.session.Items[m.cursor]

	leftPanel := m.renderCodePanel(
		fmt.Sprintf("Original (%s)", item.SourceLang),
		item.OriginalBody,
		m.activePane == PaneLeft,
	)

	rightPanel := m.renderCodePanel(
		fmt.Sprintf("Generated (%s)", item.TargetLang),
		item.GeneratedCode,
		m.activePane == PaneRight,
	)

	panelWidth := (m.width - 6) / 2
	leftPanel = lipgloss.NewStyle().Width(panelWidth).Render(leftPanel)
	rightPanel = lipgloss.NewStyle().Width(panelWidth).Render(rightPanel)

	separator := m.styles.Border.Render("│\n│\n│\n│\n│\n│\n│\n│\n│\n│")

	return lipgloss.JoinHorizontal(lipgloss.Top, leftPanel, separator, rightPanel)
}

func (m ReviewModel) renderCodePanel(title string, code string, active bool) string {
	style := m.styles.Border
	if active {
		style = m.styles.ActiveBorder
	}

	titleStyled := m.styles.Tab.Render(title)
	if active {
		titleStyled = m.styles.ActiveTab.Render(title)
	}

	codeLines := strings.Split(code, "\n")
	maxLines := m.height - 12
	if maxLines < 1 {
		maxLines = 1
	}

	var numberedLines []string
	for i, line := range codeLines {
		if i >= maxLines {
			break
		}
		lineNum := fmt.Sprintf("%3d", i+1)
		truncated := m.truncateLine(line, (m.width/2)-10)
		numberedLines = append(numberedLines, fmt.Sprintf("%s │ %s", lineNum, truncated))
	}

	codeContent := strings.Join(numberedLines, "\n")
	codeStyled := m.styles.CodeBlock.Render(codeContent)

	panel := lipgloss.JoinVertical(lipgloss.Left, titleStyled, codeStyled)
	return style.Render(panel)
}

func (m ReviewModel) truncateLine(line string, maxWidth int) string {
	if len(line) <= maxWidth {
		return line
	}
	if maxWidth < 3 {
		return "..."
	}
	return line[:maxWidth-3] + "..."
}

func (m ReviewModel) renderBottom() string {
	if m.inputMode {
		return m.styles.Help.Render("Feedback: " + m.textInput.View())
	}

	helpView := m.help.ShortHelpView([]key.Binding{
		m.keys.Up,
		m.keys.Down,
		m.keys.Tab,
		m.keys.Approve,
		m.keys.Reject,
		m.keys.Feedback,
		m.keys.Quit,
	})

	return m.styles.Help.Render(helpView)
}
