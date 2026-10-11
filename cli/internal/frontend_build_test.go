package internal

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestBuildFrontendIfNeeded_BuildsWheneverFrontendPackageJSONExists(t *testing.T) {
	// With no docker on PATH, a build attempt fails fast with the Docker
	// error, which tells a build from a skip without running a container.
	t.Setenv("PATH", t.TempDir())

	tests := []struct {
		name      string
		dirs      []string
		files     map[string]string
		wantBuild bool
	}{
		{
			name: "no frontend directory",
		},
		{
			name: "frontend directory without package.json",
			dirs: []string{"frontend"},
		},
		{
			name:  "package.json at the project root only",
			files: map[string]string{"package.json": `{"name": "root"}`},
		},
		{
			name:      "package.json in the frontend directory",
			files:     map[string]string{"frontend/package.json": `{"name": "frontend"}`},
			wantBuild: true,
		},
		{
			name: "existing bundle is rebuilt",
			files: map[string]string{
				"frontend/package.json":   `{"name": "frontend"}`,
				"frontend/dist/bundle.js": "// old bundle",
			},
			wantBuild: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			projectPath := t.TempDir()
			for _, dir := range tt.dirs {
				if err := os.MkdirAll(filepath.Join(projectPath, dir), 0755); err != nil {
					t.Fatalf("Failed to create %s: %v", dir, err)
				}
			}
			for name, content := range tt.files {
				path := filepath.Join(projectPath, name)
				if err := os.MkdirAll(filepath.Dir(path), 0755); err != nil {
					t.Fatalf("Failed to create the directory for %s: %v", name, err)
				}
				if err := os.WriteFile(path, []byte(content), 0644); err != nil {
					t.Fatalf("Failed to create %s: %v", name, err)
				}
			}

			err := BuildFrontendIfNeeded(projectPath)

			if !tt.wantBuild {
				if err != nil {
					t.Errorf("Expected the build to be skipped, got: %v", err)
				}
				return
			}
			if err == nil || !strings.Contains(err.Error(), "docker unavailable for frontend build") {
				t.Errorf("Expected a build attempt, got: %v", err)
			}
		})
	}
}
