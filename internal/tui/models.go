package tui

import (
	"fmt"
	"strings"
	"time"

	"github.com/efebarandurmaz/anvil/internal/agents"
	"github.com/efebarandurmaz/anvil/internal/ir"
	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// ReviewStatus represents the review state of a migration item
type ReviewStatus int

const (
	ReviewPending ReviewStatus = iota
	ReviewApproved
	ReviewRejected
	ReviewRegenerated
)

// String returns the string representation of ReviewStatus
func (s ReviewStatus) String() string {
	switch s {
	case ReviewPending:
		return "pending"
	case ReviewApproved:
		return "approved"
	case ReviewRejected:
		return "rejected"
	case ReviewRegenerated:
		return "regenerated"
	default:
		return "unknown"
	}
}

// ReviewItem represents a single function migration to review
type ReviewItem struct {
	ModuleName    string
	FunctionName  string
	OriginalBody  string  // Source language body
	GeneratedCode string  // Target language code
	SourceLang    string
	TargetLang    string
	JudgeScore    float64 // 0-1
	JudgeReason   string
	Status        ReviewStatus // pending, approved, rejected, regenerated
	FilePath      string       // generated file path
}

// ReviewSession holds all items for a review
type ReviewSession struct {
	Items        []*ReviewItem
	SourceLang   string
	TargetLang   string
	OverallScore float64
	CreatedAt    time.Time
}

// NewReviewSession creates a session from agent results
func NewReviewSession(graph *ir.SemanticGraph, generatedFiles []plugins.GeneratedFile, judgeResult *agents.AgentResult) *ReviewSession {
	session := &ReviewSession{
		Items:        make([]*ReviewItem, 0),
		CreatedAt:    time.Now(),
		OverallScore: 1.0,
	}

	// Extract source and target languages from graph metadata
	if graph.Metadata != nil {
		session.SourceLang = graph.Metadata["source_language"]
		session.TargetLang = graph.Metadata["target_language"]
	}

	// Extract overall score from judge result if available
	if judgeResult != nil {
		session.OverallScore = judgeResult.Score
	}

	// Build a map of generated file content by path for quick lookup
	fileContentMap := make(map[string]string)
	for _, gf := range generatedFiles {
		fileContentMap[gf.Path] = string(gf.Content)
	}

	// Iterate over modules and functions to create review items
	for _, module := range graph.Modules {
		moduleName := module.Name
		if session.SourceLang == "" && module.Language != "" {
			session.SourceLang = module.Language
		}

		for _, fn := range module.Functions {
			item := &ReviewItem{
				ModuleName:   moduleName,
				FunctionName: fn.Name,
				OriginalBody: fn.Body,
				SourceLang:   session.SourceLang,
				TargetLang:   session.TargetLang,
				JudgeScore:   session.OverallScore, // default to overall score
				Status:       ReviewPending,
			}

			// Try to find the generated code for this function
			// We search through generated files for the function name
			item.GeneratedCode, item.FilePath = findGeneratedCodeForFunction(fn.Name, generatedFiles, fileContentMap)

			// Extract function-specific judge score if available
			if judgeResult != nil && judgeResult.Metadata != nil {
				// Look for function-specific score in metadata
				scoreKey := fmt.Sprintf("score_%s_%s", moduleName, fn.Name)
				if scoreStr, ok := judgeResult.Metadata[scoreKey]; ok {
					var funcScore float64
					if _, err := fmt.Sscanf(scoreStr, "%f", &funcScore); err == nil {
						item.JudgeScore = funcScore
					}
				}

				// Look for function-specific reason
				reasonKey := fmt.Sprintf("reason_%s_%s", moduleName, fn.Name)
				if reason, ok := judgeResult.Metadata[reasonKey]; ok {
					item.JudgeReason = reason
				}
			}

			session.Items = append(session.Items, item)
		}
	}

	return session
}

// findGeneratedCodeForFunction searches generated files for a function's code
// Returns the function code snippet and the file path where it was found
func findGeneratedCodeForFunction(functionName string, generatedFiles []plugins.GeneratedFile, fileContentMap map[string]string) (string, string) {
	// Search through all generated files
	for _, gf := range generatedFiles {
		content := string(gf.Content)

		// Simple heuristic: look for function name in the content
		// This could be improved with language-specific parsing
		if strings.Contains(content, functionName) {
			// Try to extract the function block
			// For now, return a snippet around the function name
			extracted := extractFunctionSnippet(content, functionName)
			return extracted, gf.Path
		}
	}

	// If not found, return empty
	return "", ""
}

// extractFunctionSnippet attempts to extract a function's code from file content
// This is a simple heuristic that can be improved with language-specific parsing
func extractFunctionSnippet(content, functionName string) string {
	lines := strings.Split(content, "\n")

	// Find the line containing the function name
	for i, line := range lines {
		if strings.Contains(line, functionName) {
			// Extract surrounding context (e.g., 20 lines before and after)
			start := i - 5
			if start < 0 {
				start = 0
			}
			end := i + 20
			if end > len(lines) {
				end = len(lines)
			}

			snippet := strings.Join(lines[start:end], "\n")
			return snippet
		}
	}

	// If we can't find it, return the whole content (truncated if too large)
	if len(content) > 1000 {
		return content[:1000] + "\n... (truncated)"
	}
	return content
}
