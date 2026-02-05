package migration

import (
	"crypto/sha256"
	"encoding/hex"
	"sort"
	"strings"

	"github.com/efebarandurmaz/anvil/internal/plugins"
)

// Fingerprint represents a content-addressable hash of a source file
// and its transitive dependencies (copybooks, includes, etc.).
type Fingerprint struct {
	// FileHash is the SHA-256 of the raw file content.
	FileHash string `json:"file_hash"`
	// DependencyHashes are sorted hashes of files this file depends on.
	DependencyHashes []string `json:"dependency_hashes,omitempty"`
	// CompositeHash combines FileHash + DependencyHashes into a single hash.
	// If this hasn't changed since last run, the file can be skipped.
	CompositeHash string `json:"composite_hash"`
}

// ComputeFingerprints computes fingerprints for all source files.
// The deps map provides dependency relationships: file path → list of dependency paths.
func ComputeFingerprints(files []plugins.SourceFile, deps map[string][]string) map[string]*Fingerprint {
	// Step 1: Compute individual file hashes
	fileHashes := make(map[string]string, len(files))
	for _, f := range files {
		fileHashes[f.Path] = hashBytes(f.Content)
	}

	// Step 2: Build fingerprints with dependency hashes
	result := make(map[string]*Fingerprint, len(files))
	for _, f := range files {
		fp := &Fingerprint{
			FileHash: fileHashes[f.Path],
		}

		// Collect dependency hashes
		if depPaths, ok := deps[f.Path]; ok && len(depPaths) > 0 {
			for _, depPath := range depPaths {
				if h, ok := fileHashes[depPath]; ok {
					fp.DependencyHashes = append(fp.DependencyHashes, h)
				}
			}
			sort.Strings(fp.DependencyHashes)
		}

		// Compute composite hash
		fp.CompositeHash = computeComposite(fp.FileHash, fp.DependencyHashes)
		result[f.Path] = fp
	}

	return result
}

// hashBytes computes SHA-256 of raw bytes.
func hashBytes(data []byte) string {
	h := sha256.Sum256(data)
	return hex.EncodeToString(h[:])
}

// computeComposite creates a single hash from file hash + sorted dependency hashes.
func computeComposite(fileHash string, depHashes []string) string {
	parts := make([]string, 0, 1+len(depHashes))
	parts = append(parts, fileHash)
	parts = append(parts, depHashes...)
	combined := strings.Join(parts, "|")
	h := sha256.Sum256([]byte(combined))
	return hex.EncodeToString(h[:])
}
