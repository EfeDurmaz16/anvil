package migration

import (
	"encoding/json"
	"fmt"
	"log/slog"
	"os"
	"path/filepath"
	"sort"
	"time"

	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// IncrementalConfig configures the incremental migration runner.
type IncrementalConfig struct {
	OutputDir      string              // Directory for state and cache
	SourceLanguage string              // e.g. "cobol"
	TargetLanguage string              // e.g. "typescript"
	Dependencies   map[string][]string // File dependency map
	ForceAll       bool                // Force re-migration of all files
	DryRun         bool                // Only report what would change
}

// IncrementalResult captures the result of an incremental analysis.
type IncrementalResult struct {
	TotalFiles     int           `json:"total_files"`
	ChangedFiles   []string      `json:"changed_files"`
	UnchangedFiles []string      `json:"unchanged_files"`
	NewFiles       []string      `json:"new_files"`
	DeletedFiles   []string      `json:"deleted_files"`
	Skipped        int           `json:"skipped"`
	Duration       time.Duration `json:"duration"`
	IsFirstRun     bool          `json:"is_first_run"`
	ForcedFull     bool          `json:"forced_full"`
}

// ResultCache stores previously generated migration results for reuse.
type ResultCache struct {
	Version   string                  `json:"version"`
	Entries   map[string]*CacheEntry  `json:"entries"`
	CreatedAt time.Time               `json:"created_at"`
	UpdatedAt time.Time               `json:"updated_at"`
}

// CacheEntry represents a cached migration result for a single file.
type CacheEntry struct {
	SourcePath    string    `json:"source_path"`
	CompositeHash string    `json:"composite_hash"`
	OutputPath    string    `json:"output_path"`
	GeneratedAt   time.Time `json:"generated_at"`
	TokensUsed    int       `json:"tokens_used"`
	Score         float64   `json:"score"`
	Success       bool      `json:"success"`
}

const cacheFileName = ".anvil-cache.json"
const cacheVersion = "1.0.0"

// IncrementalRunner manages incremental migration with fingerprint-based change detection.
type IncrementalRunner struct {
	config *IncrementalConfig
	state  *MigrationState
	cache  *ResultCache
	logger *slog.Logger
}

// NewIncrementalRunner creates a new incremental runner.
func NewIncrementalRunner(cfg *IncrementalConfig) *IncrementalRunner {
	return &IncrementalRunner{
		config: cfg,
		logger: slog.Default(),
	}
}

// Analyze examines files to determine which need re-migration.
func (r *IncrementalRunner) Analyze(files []plugins.SourceFile) (*IncrementalResult, error) {
	start := time.Now()
	result := &IncrementalResult{
		TotalFiles: len(files),
	}

	// Load previous state
	prevState, err := LoadState(r.config.OutputDir)
	if err != nil {
		return nil, fmt.Errorf("load state: %w", err)
	}

	if prevState == nil {
		result.IsFirstRun = true
	}

	// Force full migration if requested
	if r.config.ForceAll {
		result.ForcedFull = true
		for _, f := range files {
			result.ChangedFiles = append(result.ChangedFiles, f.Path)
		}
		sort.Strings(result.ChangedFiles)
		result.Duration = time.Since(start)
		return result, nil
	}

	// Compute current fingerprints
	currentFPs := ComputeFingerprints(files, r.config.Dependencies)

	// Detect changed files
	changedPaths := ChangedFiles(currentFPs, prevState)
	changedSet := make(map[string]bool, len(changedPaths))
	for _, p := range changedPaths {
		changedSet[p] = true
	}

	// Categorize files
	currentPaths := make(map[string]bool, len(files))
	for _, f := range files {
		currentPaths[f.Path] = true
		if result.IsFirstRun {
			result.NewFiles = append(result.NewFiles, f.Path)
		} else if changedSet[f.Path] {
			if prevState != nil {
				if _, existed := prevState.Fingerprints[f.Path]; existed {
					result.ChangedFiles = append(result.ChangedFiles, f.Path)
				} else {
					result.NewFiles = append(result.NewFiles, f.Path)
				}
			} else {
				result.NewFiles = append(result.NewFiles, f.Path)
			}
		} else {
			result.UnchangedFiles = append(result.UnchangedFiles, f.Path)
			result.Skipped++
		}
	}

	// Detect deleted files
	if prevState != nil {
		for path := range prevState.Fingerprints {
			if !currentPaths[path] {
				result.DeletedFiles = append(result.DeletedFiles, path)
			}
		}
	}

	sort.Strings(result.ChangedFiles)
	sort.Strings(result.UnchangedFiles)
	sort.Strings(result.NewFiles)
	sort.Strings(result.DeletedFiles)

	result.Duration = time.Since(start)

	r.logger.Info("incremental analysis complete",
		"total", result.TotalFiles,
		"changed", len(result.ChangedFiles),
		"new", len(result.NewFiles),
		"unchanged", len(result.UnchangedFiles),
		"deleted", len(result.DeletedFiles),
		"first_run", result.IsFirstRun,
	)

	return result, nil
}

// FilesToMigrate returns the list of files that need migration (changed + new).
func (r *IncrementalRunner) FilesToMigrate(result *IncrementalResult) []string {
	all := make([]string, 0, len(result.ChangedFiles)+len(result.NewFiles))
	all = append(all, result.ChangedFiles...)
	all = append(all, result.NewFiles...)
	sort.Strings(all)
	return all
}

// FilterFiles filters source files to only those needing migration.
func (r *IncrementalRunner) FilterFiles(files []plugins.SourceFile, result *IncrementalResult) []plugins.SourceFile {
	needed := make(map[string]bool)
	for _, p := range r.FilesToMigrate(result) {
		needed[p] = true
	}

	filtered := make([]plugins.SourceFile, 0, len(needed))
	for _, f := range files {
		if needed[f.Path] {
			filtered = append(filtered, f)
		}
	}
	return filtered
}

// SaveState persists the current fingerprints as the new state.
func (r *IncrementalRunner) SaveState(files []plugins.SourceFile) error {
	state := NewMigrationState(r.config.SourceLanguage, r.config.TargetLanguage)
	fps := ComputeFingerprints(files, r.config.Dependencies)
	state.Fingerprints = fps
	return state.Save(r.config.OutputDir)
}

// LoadCache loads the result cache from disk.
func LoadCache(outputDir string) (*ResultCache, error) {
	path := filepath.Join(outputDir, cacheFileName)
	data, err := os.ReadFile(path)
	if err != nil {
		if os.IsNotExist(err) {
			return &ResultCache{
				Version:   cacheVersion,
				Entries:   make(map[string]*CacheEntry),
				CreatedAt: time.Now(),
				UpdatedAt: time.Now(),
			}, nil
		}
		return nil, err
	}

	var cache ResultCache
	if err := json.Unmarshal(data, &cache); err != nil {
		return nil, err
	}
	return &cache, nil
}

// Save persists the result cache to disk.
func (c *ResultCache) Save(outputDir string) error {
	c.UpdatedAt = time.Now()
	data, err := json.MarshalIndent(c, "", "  ")
	if err != nil {
		return err
	}
	path := filepath.Join(outputDir, cacheFileName)
	return os.WriteFile(path, data, 0o644)
}

// Get retrieves a cached entry for a file if its hash matches.
func (c *ResultCache) Get(path, compositeHash string) (*CacheEntry, bool) {
	entry, exists := c.Entries[path]
	if !exists {
		return nil, false
	}
	if entry.CompositeHash != compositeHash {
		return nil, false
	}
	return entry, true
}

// Put stores a migration result in the cache.
func (c *ResultCache) Put(entry *CacheEntry) {
	c.Entries[entry.SourcePath] = entry
}

// Remove deletes a cache entry.
func (c *ResultCache) Remove(path string) {
	delete(c.Entries, path)
}

// Prune removes entries for files that no longer exist.
func (c *ResultCache) Prune(currentFiles map[string]bool) int {
	pruned := 0
	for path := range c.Entries {
		if !currentFiles[path] {
			delete(c.Entries, path)
			pruned++
		}
	}
	return pruned
}

// Stats returns cache statistics.
func (c *ResultCache) Stats() CacheStats {
	stats := CacheStats{
		TotalEntries: len(c.Entries),
	}
	for _, e := range c.Entries {
		if e.Success {
			stats.SuccessEntries++
		} else {
			stats.FailedEntries++
		}
		stats.TotalTokens += e.TokensUsed
	}
	return stats
}

// CacheStats holds cache statistics.
type CacheStats struct {
	TotalEntries   int `json:"total_entries"`
	SuccessEntries int `json:"success_entries"`
	FailedEntries  int `json:"failed_entries"`
	TotalTokens    int `json:"total_tokens"`
}

// FormatIncrementalReport returns a human-readable report of incremental analysis.
func FormatIncrementalReport(result *IncrementalResult) string {
	var s string
	s += "╔══════════════════════════════════════════╗\n"
	s += "║     Incremental Migration Report         ║\n"
	s += "╠══════════════════════════════════════════╣\n"

	if result.IsFirstRun {
		s += "║ Mode: First Run (full migration)         \n"
	} else if result.ForcedFull {
		s += "║ Mode: Forced Full Re-migration           \n"
	} else {
		s += "║ Mode: Incremental                        \n"
	}

	s += fmt.Sprintf("║ Total Files:     %d\n", result.TotalFiles)
	s += fmt.Sprintf("║ Changed Files:   %d\n", len(result.ChangedFiles))
	s += fmt.Sprintf("║ New Files:       %d\n", len(result.NewFiles))
	s += fmt.Sprintf("║ Unchanged:       %d (skipped)\n", len(result.UnchangedFiles))
	s += fmt.Sprintf("║ Deleted:         %d\n", len(result.DeletedFiles))
	s += fmt.Sprintf("║ Analysis Time:   %s\n", result.Duration.Round(time.Millisecond))

	needMigration := len(result.ChangedFiles) + len(result.NewFiles)
	if result.TotalFiles > 0 {
		skipRate := float64(result.Skipped) / float64(result.TotalFiles) * 100
		s += fmt.Sprintf("║ Skip Rate:       %.1f%%\n", skipRate)
	}
	s += fmt.Sprintf("║ Files to Migrate: %d\n", needMigration)

	if len(result.ChangedFiles) > 0 {
		s += "╠══════════════════════════════════════════╣\n"
		s += "║ Changed Files:                           \n"
		for _, f := range result.ChangedFiles {
			s += fmt.Sprintf("║   ~ %s\n", f)
		}
	}

	if len(result.NewFiles) > 0 {
		s += "╠══════════════════════════════════════════╣\n"
		s += "║ New Files:                               \n"
		for _, f := range result.NewFiles {
			s += fmt.Sprintf("║   + %s\n", f)
		}
	}

	if len(result.DeletedFiles) > 0 {
		s += "╠══════════════════════════════════════════╣\n"
		s += "║ Deleted Files:                           \n"
		for _, f := range result.DeletedFiles {
			s += fmt.Sprintf("║   - %s\n", f)
		}
	}

	s += "╚══════════════════════════════════════════╝\n"
	return s
}
