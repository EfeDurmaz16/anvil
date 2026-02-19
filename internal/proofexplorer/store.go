package proofexplorer

import (
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"sync"
	"time"

	"github.com/efebarandurmaz/anvil/internal/harness"
)

// Store manages proof pack loading and caching.
type Store struct {
	mu       sync.RWMutex
	packs    map[string]*loadedPack // keyed by ID
	rootDirs []string               // directories to scan for proof packs
}

type loadedPack struct {
	summary ProofPackSummary
	detail  ProofPackDetail
	raw     *harness.ProofPack
}

// NewStore creates a store that scans the given directories for proof packs.
func NewStore(dirs ...string) *Store {
	return &Store{
		packs:    make(map[string]*loadedPack),
		rootDirs: dirs,
	}
}

// Scan walks all root directories looking for summary.json files and loads them.
func (s *Store) Scan() error {
	s.mu.Lock()
	defer s.mu.Unlock()

	for _, dir := range s.rootDirs {
		err := filepath.Walk(dir, func(path string, info os.FileInfo, err error) error {
			if err != nil {
				return nil // skip unreadable dirs
			}
			if info.IsDir() || info.Name() != "summary.json" {
				return nil
			}
			if loadErr := s.loadPack(path); loadErr != nil {
				// Log but don't fail on individual pack load errors
				return nil
			}
			return nil
		})
		if err != nil {
			return fmt.Errorf("scanning %s: %w", dir, err)
		}
	}
	return nil
}

func (s *Store) loadPack(summaryPath string) error {
	data, err := os.ReadFile(summaryPath)
	if err != nil {
		return err
	}

	var pack harness.ProofPack
	if err := json.Unmarshal(data, &pack); err != nil {
		return err
	}

	dir := filepath.Dir(summaryPath)
	id := generatePackID(dir)

	duration := pack.EndedAt.Sub(pack.StartedAt)

	summary := ProofPackSummary{
		ID:           id,
		Dir:          dir,
		Pass:         pack.Pass,
		FixtureCount: pack.FixtureCount,
		PassCount:    pack.PassCount,
		FailCount:    pack.FailCount,
		StartedAt:    pack.StartedAt,
		EndedAt:      pack.EndedAt,
		Duration:     duration.Round(time.Millisecond).String(),
	}

	fixtures := make([]FixtureDetail, 0, len(pack.Results))
	for _, r := range pack.Results {
		fixtures = append(fixtures, FixtureDetail{
			Name:          r.Name,
			Kind:          string(r.Kind),
			CorrelationID: r.CorrelationID,
			Pass:          r.Diff.Pass,
			DiffKind:      string(r.Diff.Kind),
			DiffReason:    r.Diff.Reason,
		})
	}

	detail := ProofPackDetail{
		ProofPackSummary: summary,
		Results:          fixtures,
	}

	s.packs[id] = &loadedPack{
		summary: summary,
		detail:  detail,
		raw:     &pack,
	}
	return nil
}

func generatePackID(dir string) string {
	h := sha256.Sum256([]byte(dir))
	return hex.EncodeToString(h[:8])
}

// List returns summaries of all loaded proof packs, sorted by start time (newest first).
func (s *Store) List() []ProofPackSummary {
	s.mu.RLock()
	defer s.mu.RUnlock()

	result := make([]ProofPackSummary, 0, len(s.packs))
	for _, p := range s.packs {
		result = append(result, p.summary)
	}
	sort.Slice(result, func(i, j int) bool {
		return result[i].StartedAt.After(result[j].StartedAt)
	})
	return result
}

// Get returns the full detail for a proof pack by ID.
func (s *Store) Get(id string) (ProofPackDetail, bool) {
	s.mu.RLock()
	defer s.mu.RUnlock()

	p, ok := s.packs[id]
	if !ok {
		return ProofPackDetail{}, false
	}
	return p.detail, true
}

// Stats returns aggregate statistics.
func (s *Store) Stats() ExplorerStats {
	s.mu.RLock()
	defer s.mu.RUnlock()

	stats := ExplorerStats{
		TotalPacks: len(s.packs),
	}
	for _, p := range s.packs {
		stats.TotalFixtures += p.summary.FixtureCount
		stats.TotalPassed += p.summary.PassCount
		stats.TotalFailed += p.summary.FailCount
	}
	if stats.TotalFixtures > 0 {
		stats.OverallPassRate = float64(stats.TotalPassed) / float64(stats.TotalFixtures)
	}
	if stats.TotalPacks > 0 {
		stats.AvgFixturesPerPack = float64(stats.TotalFixtures) / float64(stats.TotalPacks)
	}
	return stats
}

// PackCount returns the number of loaded packs.
func (s *Store) PackCount() int {
	s.mu.RLock()
	defer s.mu.RUnlock()
	return len(s.packs)
}
