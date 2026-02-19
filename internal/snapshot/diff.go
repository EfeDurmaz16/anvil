package snapshot

import (
	"fmt"
	"sort"
	"strings"
)

// DiffType indicates the kind of change.
type DiffType string

const (
	DiffAdded    DiffType = "added"
	DiffRemoved  DiffType = "removed"
	DiffModified DiffType = "modified"
	DiffRenamed  DiffType = "renamed"
)

// SnapshotDiff represents the complete diff between two snapshots.
type SnapshotDiff struct {
	OldID       string          `json:"old_id"`
	NewID       string          `json:"new_id"`
	OldTag      string          `json:"old_tag,omitempty"`
	NewTag      string          `json:"new_tag,omitempty"`
	ScoreDelta  float64         `json:"score_delta"`
	FileDiffs   []FileDiff      `json:"file_diffs"`
	StageDiffs  []StageDiff     `json:"stage_diffs"`
	Summary     DiffSummary     `json:"summary"`
}

// FileDiff represents a change to a single generated file.
type FileDiff struct {
	Path        string   `json:"path"`
	Type        DiffType `json:"type"`
	OldHash     string   `json:"old_hash,omitempty"`
	NewHash     string   `json:"new_hash,omitempty"`
	OldSize     int      `json:"old_size,omitempty"`
	NewSize     int      `json:"new_size,omitempty"`
	SizeDelta   int      `json:"size_delta"`
	HunkCount   int      `json:"hunk_count,omitempty"`
	LinesAdded  int      `json:"lines_added,omitempty"`
	LinesRemoved int     `json:"lines_removed,omitempty"`
	Hunks       []DiffHunk `json:"hunks,omitempty"`
}

// DiffHunk represents a contiguous block of changes in a file.
type DiffHunk struct {
	OldStart int      `json:"old_start"`
	OldCount int      `json:"old_count"`
	NewStart int      `json:"new_start"`
	NewCount int      `json:"new_count"`
	Lines    []DiffLine `json:"lines"`
}

// DiffLine is a single line in a diff hunk.
type DiffLine struct {
	Type    string `json:"type"` // "context", "add", "remove"
	Content string `json:"content"`
	OldNum  int    `json:"old_num,omitempty"`
	NewNum  int    `json:"new_num,omitempty"`
}

// StageDiff captures changes in an agent stage between two snapshots.
type StageDiff struct {
	Name           string  `json:"name"`
	ScoreDelta     float64 `json:"score_delta"`
	LLMCallsDelta  int     `json:"llm_calls_delta"`
	TokensDelta    int     `json:"tokens_delta"`
	DurationDelta  int64   `json:"duration_delta_ms"`
	StatusChanged  bool    `json:"status_changed"`
	OldStatus      string  `json:"old_status,omitempty"`
	NewStatus      string  `json:"new_status,omitempty"`
}

// DiffSummary provides aggregate stats about the diff.
type DiffSummary struct {
	FilesAdded    int `json:"files_added"`
	FilesRemoved  int `json:"files_removed"`
	FilesModified int `json:"files_modified"`
	TotalAdded    int `json:"total_lines_added"`
	TotalRemoved  int `json:"total_lines_removed"`
	ScoreImproved bool `json:"score_improved"`
}

// Diff computes the differences between two snapshots.
// If store is provided, it will also compute line-level diffs.
func Diff(old, new *Snapshot, store *Store) (*SnapshotDiff, error) {
	d := &SnapshotDiff{
		OldID:      old.ID,
		NewID:      new.ID,
		OldTag:     old.Tag,
		NewTag:     new.Tag,
		ScoreDelta: new.Score - old.Score,
	}

	// File-level diffs
	d.FileDiffs = diffFiles(old.FileManifest, new.FileManifest)

	// If store is available, compute line-level diffs for modified files
	if store != nil {
		if err := enrichWithLineDiffs(d, old, new, store); err != nil {
			// Non-fatal: we still have file-level diffs
			_ = err
		}
	}

	// Stage diffs
	d.StageDiffs = diffStages(old.AgentStages, new.AgentStages)

	// Compute summary
	d.Summary = computeSummary(d)

	return d, nil
}

func diffFiles(oldFiles, newFiles []FileEntry) []FileDiff {
	oldMap := make(map[string]FileEntry, len(oldFiles))
	for _, f := range oldFiles {
		oldMap[f.Path] = f
	}
	newMap := make(map[string]FileEntry, len(newFiles))
	for _, f := range newFiles {
		newMap[f.Path] = f
	}

	var diffs []FileDiff

	// Check for modified and removed files
	for path, oldEntry := range oldMap {
		if newEntry, ok := newMap[path]; ok {
			if oldEntry.ContentHash != newEntry.ContentHash {
				diffs = append(diffs, FileDiff{
					Path:      path,
					Type:      DiffModified,
					OldHash:   oldEntry.ContentHash,
					NewHash:   newEntry.ContentHash,
					OldSize:   oldEntry.Size,
					NewSize:   newEntry.Size,
					SizeDelta: newEntry.Size - oldEntry.Size,
				})
			}
		} else {
			diffs = append(diffs, FileDiff{
				Path:    path,
				Type:    DiffRemoved,
				OldHash: oldEntry.ContentHash,
				OldSize: oldEntry.Size,
				SizeDelta: -oldEntry.Size,
			})
		}
	}

	// Check for added files
	for path, newEntry := range newMap {
		if _, ok := oldMap[path]; !ok {
			diffs = append(diffs, FileDiff{
				Path:      path,
				Type:      DiffAdded,
				NewHash:   newEntry.ContentHash,
				NewSize:   newEntry.Size,
				SizeDelta: newEntry.Size,
			})
		}
	}

	sort.Slice(diffs, func(i, j int) bool {
		return diffs[i].Path < diffs[j].Path
	})

	return diffs
}

func enrichWithLineDiffs(d *SnapshotDiff, old, new *Snapshot, store *Store) error {
	oldFiles, err := store.LoadFiles(old)
	if err != nil {
		return fmt.Errorf("load old files: %w", err)
	}
	newFiles, err := store.LoadFiles(new)
	if err != nil {
		return fmt.Errorf("load new files: %w", err)
	}

	oldContents := make(map[string]string, len(oldFiles))
	for _, f := range oldFiles {
		oldContents[f.Path] = string(f.Content)
	}
	newContents := make(map[string]string, len(newFiles))
	for _, f := range newFiles {
		newContents[f.Path] = string(f.Content)
	}

	for i, fd := range d.FileDiffs {
		switch fd.Type {
		case DiffModified:
			oldText := oldContents[fd.Path]
			newText := newContents[fd.Path]
			hunks := computeHunks(oldText, newText)
			d.FileDiffs[i].Hunks = hunks
			d.FileDiffs[i].HunkCount = len(hunks)
			for _, h := range hunks {
				for _, l := range h.Lines {
					switch l.Type {
					case "add":
						d.FileDiffs[i].LinesAdded++
					case "remove":
						d.FileDiffs[i].LinesRemoved++
					}
				}
			}
		case DiffAdded:
			text := newContents[fd.Path]
			lines := strings.Split(text, "\n")
			d.FileDiffs[i].LinesAdded = len(lines)
		case DiffRemoved:
			text := oldContents[fd.Path]
			lines := strings.Split(text, "\n")
			d.FileDiffs[i].LinesRemoved = len(lines)
		}
	}

	return nil
}

// computeHunks implements a simple Myers-like diff producing hunks.
func computeHunks(oldText, newText string) []DiffHunk {
	oldLines := strings.Split(oldText, "\n")
	newLines := strings.Split(newText, "\n")

	// Use LCS-based diff
	lcs := longestCommonSubsequence(oldLines, newLines)
	rawDiff := buildRawDiff(oldLines, newLines, lcs)

	// Group into hunks with context
	return groupIntoHunks(rawDiff, 3)
}

type rawDiffLine struct {
	typ     string // "context", "add", "remove"
	content string
	oldNum  int
	newNum  int
}

func longestCommonSubsequence(a, b []string) [][]int {
	m, n := len(a), len(b)
	dp := make([][]int, m+1)
	for i := range dp {
		dp[i] = make([]int, n+1)
	}
	for i := 1; i <= m; i++ {
		for j := 1; j <= n; j++ {
			if a[i-1] == b[j-1] {
				dp[i][j] = dp[i-1][j-1] + 1
			} else if dp[i-1][j] > dp[i][j-1] {
				dp[i][j] = dp[i-1][j]
			} else {
				dp[i][j] = dp[i][j-1]
			}
		}
	}
	return dp
}

func buildRawDiff(oldLines, newLines []string, dp [][]int) []rawDiffLine {
	var result []rawDiffLine
	i, j := len(oldLines), len(newLines)

	for i > 0 || j > 0 {
		if i > 0 && j > 0 && oldLines[i-1] == newLines[j-1] {
			result = append(result, rawDiffLine{
				typ: "context", content: oldLines[i-1],
				oldNum: i, newNum: j,
			})
			i--
			j--
		} else if j > 0 && (i == 0 || dp[i][j-1] >= dp[i-1][j]) {
			result = append(result, rawDiffLine{
				typ: "add", content: newLines[j-1],
				newNum: j,
			})
			j--
		} else {
			result = append(result, rawDiffLine{
				typ: "remove", content: oldLines[i-1],
				oldNum: i,
			})
			i--
		}
	}

	// Reverse (we built it backwards)
	for left, right := 0, len(result)-1; left < right; left, right = left+1, right-1 {
		result[left], result[right] = result[right], result[left]
	}

	return result
}

func groupIntoHunks(rawDiff []rawDiffLine, contextLines int) []DiffHunk {
	if len(rawDiff) == 0 {
		return nil
	}

	// Find change regions
	type region struct{ start, end int }
	var regions []region

	for i, line := range rawDiff {
		if line.typ != "context" {
			if len(regions) == 0 || i > regions[len(regions)-1].end+contextLines*2 {
				regions = append(regions, region{start: i, end: i})
			} else {
				regions[len(regions)-1].end = i
			}
		}
	}

	var hunks []DiffHunk
	for _, r := range regions {
		start := r.start - contextLines
		if start < 0 {
			start = 0
		}
		end := r.end + contextLines + 1
		if end > len(rawDiff) {
			end = len(rawDiff)
		}

		hunk := DiffHunk{}
		for k := start; k < end; k++ {
			line := rawDiff[k]
			dl := DiffLine{
				Type:    line.typ,
				Content: line.content,
				OldNum:  line.oldNum,
				NewNum:  line.newNum,
			}
			hunk.Lines = append(hunk.Lines, dl)
		}

		if len(hunk.Lines) > 0 {
			// Set hunk ranges
			for _, l := range hunk.Lines {
				if l.OldNum > 0 {
					if hunk.OldStart == 0 || l.OldNum < hunk.OldStart {
						hunk.OldStart = l.OldNum
					}
					hunk.OldCount++
				}
				if l.NewNum > 0 {
					if hunk.NewStart == 0 || l.NewNum < hunk.NewStart {
						hunk.NewStart = l.NewNum
					}
					hunk.NewCount++
				}
			}
			hunks = append(hunks, hunk)
		}
	}

	return hunks
}

func diffStages(oldStages, newStages []AgentStageInfo) []StageDiff {
	oldMap := make(map[string]AgentStageInfo, len(oldStages))
	for _, s := range oldStages {
		oldMap[s.Name] = s
	}

	var diffs []StageDiff
	for _, ns := range newStages {
		sd := StageDiff{Name: ns.Name}
		if os, ok := oldMap[ns.Name]; ok {
			sd.ScoreDelta = ns.Score - os.Score
			sd.LLMCallsDelta = ns.LLMCalls - os.LLMCalls
			sd.TokensDelta = (ns.PromptTokens + ns.CompletionTokens) - (os.PromptTokens + os.CompletionTokens)
			sd.DurationDelta = ns.Duration.Milliseconds() - os.Duration.Milliseconds()
			if ns.Status != os.Status {
				sd.StatusChanged = true
				sd.OldStatus = os.Status
				sd.NewStatus = ns.Status
			}
		} else {
			sd.ScoreDelta = ns.Score
			sd.LLMCallsDelta = ns.LLMCalls
			sd.TokensDelta = ns.PromptTokens + ns.CompletionTokens
			sd.StatusChanged = true
			sd.NewStatus = ns.Status
		}
		diffs = append(diffs, sd)
	}

	return diffs
}

func computeSummary(d *SnapshotDiff) DiffSummary {
	s := DiffSummary{
		ScoreImproved: d.ScoreDelta > 0,
	}
	for _, fd := range d.FileDiffs {
		switch fd.Type {
		case DiffAdded:
			s.FilesAdded++
		case DiffRemoved:
			s.FilesRemoved++
		case DiffModified:
			s.FilesModified++
		}
		s.TotalAdded += fd.LinesAdded
		s.TotalRemoved += fd.LinesRemoved
	}
	return s
}

// FormatDiff returns a human-readable string representation of the diff.
func FormatDiff(d *SnapshotDiff) string {
	var sb strings.Builder

	sb.WriteString(fmt.Sprintf("Diff: %s → %s\n", d.OldID, d.NewID))
	if d.OldTag != "" || d.NewTag != "" {
		sb.WriteString(fmt.Sprintf("Tags: %s → %s\n", d.OldTag, d.NewTag))
	}
	sb.WriteString(fmt.Sprintf("Score: %+.2f\n\n", d.ScoreDelta))

	sb.WriteString(fmt.Sprintf("Files: +%d -%d ~%d\n",
		d.Summary.FilesAdded, d.Summary.FilesRemoved, d.Summary.FilesModified))
	sb.WriteString(fmt.Sprintf("Lines: +%d -%d\n\n",
		d.Summary.TotalAdded, d.Summary.TotalRemoved))

	for _, fd := range d.FileDiffs {
		icon := "~"
		switch fd.Type {
		case DiffAdded:
			icon = "+"
		case DiffRemoved:
			icon = "-"
		}
		sb.WriteString(fmt.Sprintf("  %s %s", icon, fd.Path))
		if fd.Type == DiffModified && fd.HunkCount > 0 {
			sb.WriteString(fmt.Sprintf(" (+%d/-%d in %d hunks)", fd.LinesAdded, fd.LinesRemoved, fd.HunkCount))
		}
		sb.WriteString("\n")
	}

	if len(d.StageDiffs) > 0 {
		sb.WriteString("\nAgent Stages:\n")
		for _, sd := range d.StageDiffs {
			sb.WriteString(fmt.Sprintf("  %s: score %+.2f, llm_calls %+d, tokens %+d",
				sd.Name, sd.ScoreDelta, sd.LLMCallsDelta, sd.TokensDelta))
			if sd.StatusChanged {
				sb.WriteString(fmt.Sprintf(" [%s→%s]", sd.OldStatus, sd.NewStatus))
			}
			sb.WriteString("\n")
		}
	}

	return sb.String()
}
