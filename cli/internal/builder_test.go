package internal

import (
	"bytes"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/fatih/color"
)

func TestBuildCompiledLanguagesIfNeeded_WithoutDockerSaysDockerIsNeeded(t *testing.T) {
	t.Setenv("PATH", t.TempDir())
	var out bytes.Buffer
	previous := color.Output
	color.Output = &out
	t.Cleanup(func() { color.Output = previous })

	projectPath := t.TempDir()
	if err := os.WriteFile(filepath.Join(projectPath, "Cargo.toml"), []byte("[package]\nname = \"bot\"\n"), 0644); err != nil {
		t.Fatal(err)
	}

	if err := BuildCompiledLanguagesIfNeeded(projectPath); err == nil {
		t.Fatal("BuildCompiledLanguagesIfNeeded() error = nil, want Docker reported missing")
	}

	// Every deploy rebuilds a detected project, so building it beforehand changes nothing.
	if strings.Contains(out.String(), "pre-build") {
		t.Errorf("hint = %q, want no advice to pre-build the project", out.String())
	}
	if !strings.Contains(out.String(), "Solution: Install Docker") {
		t.Errorf("hint = %q, want it to say to install Docker", out.String())
	}
}
