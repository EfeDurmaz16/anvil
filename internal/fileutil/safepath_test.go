package fileutil

import (
	"strings"
	"testing"
)

func TestSafeJoin(t *testing.T) {
	tests := []struct {
		name        string
		baseDir     string
		userPath    string
		wantSuffix  string // expected suffix of result path
		wantErr     bool
		errContains string
	}{
		{
			name:       "normal path",
			baseDir:    "/tmp/base",
			userPath:   "foo/bar.ts",
			wantSuffix: "/tmp/base/foo/bar.ts",
		},
		{
			name:        "path traversal blocked",
			baseDir:     "/tmp/base",
			userPath:    "../../../etc/passwd",
			wantErr:     true,
			errContains: "escapes base directory",
		},
		{
			name:        "double dot in middle",
			baseDir:     "/tmp/base",
			userPath:    "foo/../../bar",
			wantErr:     true,
			errContains: "escapes base directory",
		},
		{
			name:       "absolute path input treated as relative",
			baseDir:    "/tmp/base",
			userPath:   "/etc/passwd",
			wantSuffix: "/tmp/base/etc/passwd",
		},
		{
			name:       "clean path stays inside",
			baseDir:    "/tmp/base",
			userPath:   "foo/../foo/bar.ts",
			wantSuffix: "/tmp/base/foo/bar.ts",
		},
		{
			name:       "empty userPath returns base",
			baseDir:    "/tmp/base",
			userPath:   "",
			wantSuffix: "/tmp/base",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got, err := SafeJoin(tt.baseDir, tt.userPath)
			if tt.wantErr {
				if err == nil {
					t.Errorf("SafeJoin(%q, %q) expected error, got nil (result: %q)", tt.baseDir, tt.userPath, got)
					return
				}
				if tt.errContains != "" && !strings.Contains(err.Error(), tt.errContains) {
					t.Errorf("SafeJoin(%q, %q) error %q does not contain %q", tt.baseDir, tt.userPath, err.Error(), tt.errContains)
				}
				return
			}
			if err != nil {
				t.Errorf("SafeJoin(%q, %q) unexpected error: %v", tt.baseDir, tt.userPath, err)
				return
			}
			if got != tt.wantSuffix {
				t.Errorf("SafeJoin(%q, %q) = %q, want %q", tt.baseDir, tt.userPath, got, tt.wantSuffix)
			}
		})
	}
}
