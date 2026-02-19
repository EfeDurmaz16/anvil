package proofexplorer

import (
	"encoding/json"
	"fmt"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/efebarandurmaz/anvil/internal/harness"
)

// Helper to create test proof pack on disk
func createTestProofPack(t *testing.T, dir string, pass bool, fixtureCount int) {
	t.Helper()

	passCount := fixtureCount
	failCount := 0
	if !pass {
		passCount = fixtureCount / 2
		failCount = fixtureCount - passCount
	}

	results := make([]harness.FixtureResult, fixtureCount)
	for i := 0; i < fixtureCount; i++ {
		passed := i < passCount
		diffKind := harness.DiffKindHTTP
		diffReason := ""
		if !passed {
			diffReason = "output mismatch"
		}

		results[i] = harness.FixtureResult{
			Name: filepath.Join("test", "fixture", fmt.Sprintf("%d.http", i+1)),
			Kind: harness.FixtureHTTP,
			Diff: harness.DiffResult{
				Kind:   diffKind,
				Pass:   passed,
				Reason: diffReason,
			},
		}
	}

	pack := &harness.ProofPack{
		StartedAt:    time.Now().Add(-5 * time.Minute),
		EndedAt:      time.Now(),
		Pass:         pass,
		FixtureCount: fixtureCount,
		PassCount:    passCount,
		FailCount:    failCount,
		Results:      results,
	}

	data, err := json.MarshalIndent(pack, "", "  ")
	if err != nil {
		t.Fatalf("failed to marshal pack: %v", err)
	}

	if err := os.MkdirAll(dir, 0755); err != nil {
		t.Fatalf("failed to create dir: %v", err)
	}

	if err := os.WriteFile(filepath.Join(dir, "summary.json"), data, 0644); err != nil {
		t.Fatalf("failed to write summary.json: %v", err)
	}
}

// Helper to setup test server
func setupTestServer(t *testing.T, dirs ...string) (*Explorer, *httptest.Server) {
	t.Helper()

	cfg := &Config{
		ListenAddr: ":0",
		PackDirs:   dirs,
	}

	explorer, err := New(cfg)
	if err != nil {
		t.Fatalf("failed to create explorer: %v", err)
	}

	// Create test server using the explorer's handler
	mux := http.NewServeMux()
	mux.HandleFunc("/api/packs", explorer.handlePacks)
	mux.HandleFunc("/api/packs/", explorer.handlePackDetail)
	mux.HandleFunc("/api/stats", explorer.handleStats)
	mux.HandleFunc("/api/health", explorer.handleHealth)
	mux.HandleFunc("/api/rescan", explorer.handleRescan)

	handler := corsMiddleware(mux)
	server := httptest.NewServer(handler)

	return explorer, server
}

// ============================================================================
// Store Tests
// ============================================================================

func TestNewStore(t *testing.T) {
	dir1 := t.TempDir()
	dir2 := t.TempDir()

	store := NewStore(dir1, dir2)
	if store == nil {
		t.Fatal("expected non-nil store")
	}

	if len(store.rootDirs) != 2 {
		t.Errorf("expected 2 root dirs, got %d", len(store.rootDirs))
	}

	if store.packs == nil {
		t.Error("expected packs map to be initialized")
	}
}

func TestStoreScan_EmptyDir(t *testing.T) {
	dir := t.TempDir()
	store := NewStore(dir)

	if err := store.Scan(); err != nil {
		t.Fatalf("scan failed: %v", err)
	}

	if count := store.PackCount(); count != 0 {
		t.Errorf("expected 0 packs in empty dir, got %d", count)
	}
}

func TestStoreScan_ValidPacks(t *testing.T) {
	dir := t.TempDir()

	// Create two test packs
	pack1Dir := filepath.Join(dir, "pack1")
	pack2Dir := filepath.Join(dir, "pack2")

	createTestProofPack(t, pack1Dir, true, 10)
	createTestProofPack(t, pack2Dir, false, 20)

	store := NewStore(dir)
	if err := store.Scan(); err != nil {
		t.Fatalf("scan failed: %v", err)
	}

	if count := store.PackCount(); count != 2 {
		t.Errorf("expected 2 packs, got %d", count)
	}

	packs := store.List()
	if len(packs) != 2 {
		t.Errorf("expected 2 packs in list, got %d", len(packs))
	}

	// Verify pack details
	for _, p := range packs {
		if p.FixtureCount == 10 {
			if !p.Pass {
				t.Error("expected pack with 10 fixtures to pass")
			}
			if p.PassCount != 10 || p.FailCount != 0 {
				t.Errorf("expected 10/0 pass/fail, got %d/%d", p.PassCount, p.FailCount)
			}
		} else if p.FixtureCount == 20 {
			if p.Pass {
				t.Error("expected pack with 20 fixtures to fail")
			}
			if p.PassCount != 10 || p.FailCount != 10 {
				t.Errorf("expected 10/10 pass/fail, got %d/%d", p.PassCount, p.FailCount)
			}
		} else {
			t.Errorf("unexpected fixture count: %d", p.FixtureCount)
		}
	}
}

func TestStoreScan_InvalidJSON(t *testing.T) {
	dir := t.TempDir()

	// Create valid pack
	pack1Dir := filepath.Join(dir, "pack1")
	createTestProofPack(t, pack1Dir, true, 5)

	// Create invalid pack with malformed JSON
	pack2Dir := filepath.Join(dir, "pack2")
	if err := os.MkdirAll(pack2Dir, 0755); err != nil {
		t.Fatalf("failed to create dir: %v", err)
	}
	if err := os.WriteFile(filepath.Join(pack2Dir, "summary.json"), []byte("{invalid json}"), 0644); err != nil {
		t.Fatalf("failed to write invalid json: %v", err)
	}

	store := NewStore(dir)
	if err := store.Scan(); err != nil {
		t.Fatalf("scan should not fail on invalid packs: %v", err)
	}

	// Should only load the valid pack
	if count := store.PackCount(); count != 1 {
		t.Errorf("expected 1 valid pack, got %d", count)
	}
}

func TestStoreScan_MultipleDirs(t *testing.T) {
	dir1 := t.TempDir()
	dir2 := t.TempDir()

	createTestProofPack(t, filepath.Join(dir1, "pack1"), true, 5)
	createTestProofPack(t, filepath.Join(dir2, "pack2"), true, 10)

	store := NewStore(dir1, dir2)
	if err := store.Scan(); err != nil {
		t.Fatalf("scan failed: %v", err)
	}

	if count := store.PackCount(); count != 2 {
		t.Errorf("expected 2 packs from multiple dirs, got %d", count)
	}
}

func TestStoreList_SortedByTime(t *testing.T) {
	dir := t.TempDir()

	// Create packs with different timestamps
	for i := 0; i < 3; i++ {
		packDir := filepath.Join(dir, fmt.Sprintf("pack%d", i))
		if err := os.MkdirAll(packDir, 0755); err != nil {
			t.Fatalf("failed to create dir: %v", err)
		}

		now := time.Now().Add(time.Duration(i) * time.Hour)
		pack := &harness.ProofPack{
			StartedAt:    now.Add(-5 * time.Minute),
			EndedAt:      now,
			Pass:         true,
			FixtureCount: 1,
			PassCount:    1,
			FailCount:    0,
			Results:      []harness.FixtureResult{},
		}

		data, _ := json.MarshalIndent(pack, "", "  ")
		os.WriteFile(filepath.Join(packDir, "summary.json"), data, 0644)

		// Sleep to ensure different file modification times
		time.Sleep(10 * time.Millisecond)
	}

	store := NewStore(dir)
	if err := store.Scan(); err != nil {
		t.Fatalf("scan failed: %v", err)
	}

	packs := store.List()
	if len(packs) != 3 {
		t.Fatalf("expected 3 packs, got %d", len(packs))
	}

	// Verify newest first ordering
	for i := 0; i < len(packs)-1; i++ {
		if packs[i].StartedAt.Before(packs[i+1].StartedAt) {
			t.Errorf("packs not sorted by time (newest first): pack[%d] is older than pack[%d]", i, i+1)
		}
	}
}

func TestStoreGet_Found(t *testing.T) {
	dir := t.TempDir()
	packDir := filepath.Join(dir, "pack1")
	createTestProofPack(t, packDir, true, 5)

	store := NewStore(dir)
	if err := store.Scan(); err != nil {
		t.Fatalf("scan failed: %v", err)
	}

	packs := store.List()
	if len(packs) == 0 {
		t.Fatal("expected at least one pack")
	}

	id := packs[0].ID
	detail, ok := store.Get(id)
	if !ok {
		t.Fatal("expected to find pack")
	}

	if detail.ID != id {
		t.Errorf("expected ID %s, got %s", id, detail.ID)
	}

	if len(detail.Results) != 5 {
		t.Errorf("expected 5 results, got %d", len(detail.Results))
	}
}

func TestStoreGet_NotFound(t *testing.T) {
	dir := t.TempDir()
	store := NewStore(dir)

	if err := store.Scan(); err != nil {
		t.Fatalf("scan failed: %v", err)
	}

	_, ok := store.Get("nonexistent-id")
	if ok {
		t.Error("expected not to find pack with invalid ID")
	}
}

func TestStoreStats(t *testing.T) {
	dir := t.TempDir()

	createTestProofPack(t, filepath.Join(dir, "pack1"), true, 10)   // 10 pass, 0 fail
	createTestProofPack(t, filepath.Join(dir, "pack2"), false, 20)  // 10 pass, 10 fail

	store := NewStore(dir)
	if err := store.Scan(); err != nil {
		t.Fatalf("scan failed: %v", err)
	}

	stats := store.Stats()

	if stats.TotalPacks != 2 {
		t.Errorf("expected 2 total packs, got %d", stats.TotalPacks)
	}

	if stats.TotalFixtures != 30 {
		t.Errorf("expected 30 total fixtures, got %d", stats.TotalFixtures)
	}

	if stats.TotalPassed != 20 {
		t.Errorf("expected 20 passed, got %d", stats.TotalPassed)
	}

	if stats.TotalFailed != 10 {
		t.Errorf("expected 10 failed, got %d", stats.TotalFailed)
	}

	expectedPassRate := 20.0 / 30.0
	if stats.OverallPassRate != expectedPassRate {
		t.Errorf("expected pass rate %.2f, got %.2f", expectedPassRate, stats.OverallPassRate)
	}

	expectedAvg := 30.0 / 2.0
	if stats.AvgFixturesPerPack != expectedAvg {
		t.Errorf("expected avg %.2f fixtures per pack, got %.2f", expectedAvg, stats.AvgFixturesPerPack)
	}
}

func TestStoreStats_EmptyStore(t *testing.T) {
	dir := t.TempDir()
	store := NewStore(dir)

	if err := store.Scan(); err != nil {
		t.Fatalf("scan failed: %v", err)
	}

	stats := store.Stats()

	if stats.TotalPacks != 0 {
		t.Errorf("expected 0 packs, got %d", stats.TotalPacks)
	}

	if stats.OverallPassRate != 0 {
		t.Errorf("expected 0 pass rate, got %.2f", stats.OverallPassRate)
	}

	if stats.AvgFixturesPerPack != 0 {
		t.Errorf("expected 0 avg fixtures, got %.2f", stats.AvgFixturesPerPack)
	}
}

func TestStoreRescan(t *testing.T) {
	dir := t.TempDir()

	// Initial scan with one pack
	pack1Dir := filepath.Join(dir, "pack1")
	createTestProofPack(t, pack1Dir, true, 5)

	store := NewStore(dir)
	if err := store.Scan(); err != nil {
		t.Fatalf("initial scan failed: %v", err)
	}

	if count := store.PackCount(); count != 1 {
		t.Errorf("expected 1 pack initially, got %d", count)
	}

	// Add another pack
	pack2Dir := filepath.Join(dir, "pack2")
	createTestProofPack(t, pack2Dir, true, 10)

	// Rescan
	if err := store.Scan(); err != nil {
		t.Fatalf("rescan failed: %v", err)
	}

	if count := store.PackCount(); count != 2 {
		t.Errorf("expected 2 packs after rescan, got %d", count)
	}
}

// ============================================================================
// Server/API Tests
// ============================================================================

func TestHandlePacks(t *testing.T) {
	dir := t.TempDir()
	createTestProofPack(t, filepath.Join(dir, "pack1"), true, 5)
	createTestProofPack(t, filepath.Join(dir, "pack2"), false, 10)

	_, server := setupTestServer(t, dir)
	defer server.Close()

	resp, err := http.Get(server.URL + "/api/packs")
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Errorf("expected status 200, got %d", resp.StatusCode)
	}

	var packs []ProofPackSummary
	if err := json.NewDecoder(resp.Body).Decode(&packs); err != nil {
		t.Fatalf("failed to decode response: %v", err)
	}

	if len(packs) != 2 {
		t.Errorf("expected 2 packs, got %d", len(packs))
	}
}

func TestHandlePackDetail(t *testing.T) {
	dir := t.TempDir()
	packDir := filepath.Join(dir, "pack1")
	createTestProofPack(t, packDir, true, 5)

	explorer, server := setupTestServer(t, dir)
	defer server.Close()

	// Get the pack ID
	packs := explorer.store.List()
	if len(packs) == 0 {
		t.Fatal("expected at least one pack")
	}
	packID := packs[0].ID

	resp, err := http.Get(server.URL + "/api/packs/" + packID)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Errorf("expected status 200, got %d", resp.StatusCode)
	}

	var detail ProofPackDetail
	if err := json.NewDecoder(resp.Body).Decode(&detail); err != nil {
		t.Fatalf("failed to decode response: %v", err)
	}

	if detail.ID != packID {
		t.Errorf("expected ID %s, got %s", packID, detail.ID)
	}

	if len(detail.Results) != 5 {
		t.Errorf("expected 5 results, got %d", len(detail.Results))
	}
}

func TestHandlePackDetail_NotFound(t *testing.T) {
	dir := t.TempDir()
	_, server := setupTestServer(t, dir)
	defer server.Close()

	resp, err := http.Get(server.URL + "/api/packs/nonexistent")
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusNotFound {
		t.Errorf("expected status 404, got %d", resp.StatusCode)
	}
}

func TestHandlePackFixtures(t *testing.T) {
	dir := t.TempDir()
	packDir := filepath.Join(dir, "pack1")
	createTestProofPack(t, packDir, true, 5)

	explorer, server := setupTestServer(t, dir)
	defer server.Close()

	packs := explorer.store.List()
	if len(packs) == 0 {
		t.Fatal("expected at least one pack")
	}
	packID := packs[0].ID

	resp, err := http.Get(server.URL + "/api/packs/" + packID + "/fixtures")
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Errorf("expected status 200, got %d", resp.StatusCode)
	}

	var fixtures []FixtureDetail
	if err := json.NewDecoder(resp.Body).Decode(&fixtures); err != nil {
		t.Fatalf("failed to decode response: %v", err)
	}

	if len(fixtures) != 5 {
		t.Errorf("expected 5 fixtures, got %d", len(fixtures))
	}
}

func TestHandleStats(t *testing.T) {
	dir := t.TempDir()
	createTestProofPack(t, filepath.Join(dir, "pack1"), true, 10)
	createTestProofPack(t, filepath.Join(dir, "pack2"), false, 20)

	_, server := setupTestServer(t, dir)
	defer server.Close()

	resp, err := http.Get(server.URL + "/api/stats")
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Errorf("expected status 200, got %d", resp.StatusCode)
	}

	var stats ExplorerStats
	if err := json.NewDecoder(resp.Body).Decode(&stats); err != nil {
		t.Fatalf("failed to decode response: %v", err)
	}

	if stats.TotalPacks != 2 {
		t.Errorf("expected 2 packs, got %d", stats.TotalPacks)
	}

	if stats.TotalFixtures != 30 {
		t.Errorf("expected 30 fixtures, got %d", stats.TotalFixtures)
	}
}

func TestHandleHealth(t *testing.T) {
	dir := t.TempDir()
	_, server := setupTestServer(t, dir)
	defer server.Close()

	resp, err := http.Get(server.URL + "/api/health")
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Errorf("expected status 200, got %d", resp.StatusCode)
	}

	var health map[string]interface{}
	if err := json.NewDecoder(resp.Body).Decode(&health); err != nil {
		t.Fatalf("failed to decode response: %v", err)
	}

	if health["status"] != "ok" {
		t.Errorf("expected status ok, got %v", health["status"])
	}

	if _, ok := health["time"]; !ok {
		t.Error("expected time field in health response")
	}

	if _, ok := health["pack_count"]; !ok {
		t.Error("expected pack_count field in health response")
	}
}

func TestHandleRescan(t *testing.T) {
	dir := t.TempDir()
	createTestProofPack(t, filepath.Join(dir, "pack1"), true, 5)

	explorer, server := setupTestServer(t, dir)
	defer server.Close()

	initialCount := explorer.store.PackCount()

	// Add another pack
	createTestProofPack(t, filepath.Join(dir, "pack2"), true, 10)

	// Rescan via API
	resp, err := http.Post(server.URL+"/api/rescan", "application/json", nil)
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if resp.StatusCode != http.StatusOK {
		t.Errorf("expected status 200, got %d", resp.StatusCode)
	}

	var result map[string]interface{}
	if err := json.NewDecoder(resp.Body).Decode(&result); err != nil {
		t.Fatalf("failed to decode response: %v", err)
	}

	if result["status"] != "ok" {
		t.Errorf("expected status ok, got %v", result["status"])
	}

	newCount := explorer.store.PackCount()
	if newCount <= initialCount {
		t.Errorf("expected pack count to increase after rescan: initial=%d, new=%d", initialCount, newCount)
	}
}

func TestMethodNotAllowed(t *testing.T) {
	dir := t.TempDir()
	_, server := setupTestServer(t, dir)
	defer server.Close()

	tests := []struct {
		method string
		path   string
	}{
		{"PUT", "/api/packs"},
		{"DELETE", "/api/packs"},
		{"POST", "/api/packs"},
		{"PUT", "/api/stats"},
		{"DELETE", "/api/stats"},
		{"POST", "/api/stats"},
		{"PUT", "/api/health"},
		{"DELETE", "/api/health"},
		{"POST", "/api/health"},
		{"GET", "/api/rescan"},
		{"PUT", "/api/rescan"},
		{"DELETE", "/api/rescan"},
	}

	for _, tt := range tests {
		t.Run(tt.method+"_"+tt.path, func(t *testing.T) {
			req, err := http.NewRequest(tt.method, server.URL+tt.path, nil)
			if err != nil {
				t.Fatalf("failed to create request: %v", err)
			}

			resp, err := http.DefaultClient.Do(req)
			if err != nil {
				t.Fatalf("request failed: %v", err)
			}
			defer resp.Body.Close()

			if resp.StatusCode != http.StatusMethodNotAllowed {
				t.Errorf("expected status 405, got %d", resp.StatusCode)
			}
		})
	}
}

func TestCORSHeaders(t *testing.T) {
	dir := t.TempDir()
	_, server := setupTestServer(t, dir)
	defer server.Close()

	resp, err := http.Get(server.URL + "/api/health")
	if err != nil {
		t.Fatalf("request failed: %v", err)
	}
	defer resp.Body.Close()

	if origin := resp.Header.Get("Access-Control-Allow-Origin"); origin != "*" {
		t.Errorf("expected CORS origin *, got %s", origin)
	}

	if methods := resp.Header.Get("Access-Control-Allow-Methods"); methods == "" {
		t.Error("expected CORS methods header to be set")
	}

	if headers := resp.Header.Get("Access-Control-Allow-Headers"); headers == "" {
		t.Error("expected CORS headers header to be set")
	}
}
