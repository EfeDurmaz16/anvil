package tui

import (
	"encoding/json"
	"fmt"
	"os"
	"time"

	tea "github.com/charmbracelet/bubbletea"
)

// RunReview starts the interactive TUI review program.
// It shows the review screen, then transitions to summary.
// Returns the final ReviewSession with user decisions.
func RunReview(session *ReviewSession) (*ReviewSession, error) {
	// Create and run the review model
	reviewModel := NewReviewModel(session)
	p := tea.NewProgram(reviewModel, tea.WithAltScreen())
	finalModel, err := p.Run()
	if err != nil {
		return nil, fmt.Errorf("TUI error: %w", err)
	}

	// Extract session from final model
	final := finalModel.(ReviewModel)

	// Show summary
	summaryModel := NewSummaryModel(final.session)
	sp := tea.NewProgram(summaryModel, tea.WithAltScreen())
	_, err = sp.Run()
	if err != nil {
		return nil, fmt.Errorf("summary error: %w", err)
	}

	return final.session, nil
}

// ReviewReport represents the JSON structure for the review report
type ReviewReport struct {
	Timestamp    string              `json:"timestamp"`
	SourceLang   string              `json:"source_lang"`
	TargetLang   string              `json:"target_lang"`
	OverallScore float64             `json:"overall_score"`
	Items        []ReviewReportItem  `json:"items"`
	Summary      ReviewReportSummary `json:"summary"`
}

// ReviewReportItem represents a single review item in the report
type ReviewReportItem struct {
	Module   string  `json:"module"`
	Function string  `json:"function"`
	Score    float64 `json:"score"`
	Status   string  `json:"status"`
	Feedback string  `json:"feedback,omitempty"`
}

// ReviewReportSummary represents the summary statistics
type ReviewReportSummary struct {
	Total       int `json:"total"`
	Approved    int `json:"approved"`
	Rejected    int `json:"rejected"`
	Regenerated int `json:"regenerated"`
}

// SaveReviewReport writes a JSON report of the review decisions.
func SaveReviewReport(session *ReviewSession, outputPath string) error {
	// Calculate summary stats
	total := len(session.Items)
	approved := 0
	rejected := 0
	regenerated := 0

	items := make([]ReviewReportItem, 0, total)
	for _, item := range session.Items {
		// Count by status
		switch item.Status {
		case ReviewApproved:
			approved++
		case ReviewRejected:
			rejected++
		case ReviewRegenerated:
			regenerated++
		}

		// Convert status to string
		var statusStr string
		switch item.Status {
		case ReviewPending:
			statusStr = "pending"
		case ReviewApproved:
			statusStr = "approved"
		case ReviewRejected:
			statusStr = "rejected"
		case ReviewRegenerated:
			statusStr = "regenerated"
		default:
			statusStr = "unknown"
		}

		items = append(items, ReviewReportItem{
			Module:   item.ModuleName,
			Function: item.FunctionName,
			Score:    item.JudgeScore,
			Status:   statusStr,
			Feedback: "", // Add feedback field when available in ReviewItem
		})
	}

	// Build report
	report := ReviewReport{
		Timestamp:    time.Now().UTC().Format(time.RFC3339),
		SourceLang:   session.SourceLang,
		TargetLang:   session.TargetLang,
		OverallScore: session.OverallScore,
		Items:        items,
		Summary: ReviewReportSummary{
			Total:       total,
			Approved:    approved,
			Rejected:    rejected,
			Regenerated: regenerated,
		},
	}

	// Marshal to JSON
	data, err := json.MarshalIndent(report, "", "  ")
	if err != nil {
		return fmt.Errorf("failed to marshal report: %w", err)
	}

	// Write to file
	if err := os.WriteFile(outputPath, data, 0644); err != nil {
		return fmt.Errorf("failed to write report: %w", err)
	}

	return nil
}
