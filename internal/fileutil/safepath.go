package fileutil

import (
	"fmt"
	"path/filepath"
	"strings"
)

// SafeJoin safely joins baseDir and userPath, ensuring the result stays within baseDir.
// It prevents path traversal attacks via ".." components.
func SafeJoin(baseDir, userPath string) (string, error) {
	// Reject any path containing ".." components (defense in depth)
	cleaned := filepath.Clean(userPath)
	for _, part := range strings.Split(cleaned, string(filepath.Separator)) {
		if part == ".." {
			return "", fmt.Errorf("path %q escapes base directory %q", userPath, baseDir)
		}
	}

	absBase, err := filepath.Abs(baseDir)
	if err != nil {
		return "", fmt.Errorf("resolve base dir: %w", err)
	}

	// Join and clean the full path
	joined := filepath.Join(absBase, cleaned)

	// Double-check the result is within the base directory
	if !strings.HasPrefix(joined, absBase+string(filepath.Separator)) && joined != absBase {
		return "", fmt.Errorf("path %q escapes base directory %q", userPath, baseDir)
	}

	return joined, nil
}
