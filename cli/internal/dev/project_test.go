package dev

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func touch(t *testing.T, dir, name string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(dir, name), []byte{}, 0644); err != nil {
		t.Fatalf("touch %s: %v", name, err)
	}
}

func TestDetectRuntime_Supported(t *testing.T) {
	tests := []struct {
		name  string
		files []string
		want  Runtime
	}{
		{"python main.py", []string{"main.py"}, RuntimePython},
		{"python requirements", []string{"requirements.txt"}, RuntimePython},
		{"python pyproject", []string{"pyproject.toml"}, RuntimePython},
		{"node package.json", []string{"package.json"}, RuntimeNode},
		{"node main.js", []string{"main.js"}, RuntimeNode},
		// When both package.json and main.py are present, the strongest
		// manifest wins: package.json (node) over main.py (python), matching
		// deploy-side detection (manifest-first).
		{"node over python", []string{"package.json", "main.py"}, RuntimeNode},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			dir := t.TempDir()
			for _, f := range tc.files {
				touch(t, dir, f)
			}
			got, err := DetectRuntime(dir)
			if err != nil {
				t.Fatalf("DetectRuntime: %v", err)
			}
			if got != tc.want {
				t.Errorf("got %q, want %q", got, tc.want)
			}
		})
	}
}

// TestDetectRuntime_CompiledLanguages_Phase2Error asserts that compiled
// languages (Rust, C++, .NET, Scala, Haskell) return a friendly error
// pointing users to phase 2. These runtimes need a builder-image or
// pre-built artifact strategy which is out of scope for v1.
func TestDetectRuntime_CompiledLanguages_Phase2Error(t *testing.T) {
	cases := []struct {
		name  string
		files []string
	}{
		{"rust Cargo.toml", []string{"Cargo.toml"}},
		{"dotnet csproj", []string{"bot.csproj"}},
		{"scala sbt", []string{"build.sbt"}},
		{"cpp CMake", []string{"CMakeLists.txt"}},
		{"cpp Makefile", []string{"Makefile"}},
		{"haskell cabal", []string{"bot.cabal"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			dir := t.TempDir()
			for _, f := range tc.files {
				touch(t, dir, f)
			}
			_, err := DetectRuntime(dir)
			if err == nil {
				t.Fatalf("expected v1 error for %s, got nil", tc.name)
			}
			if !strings.Contains(err.Error(), "phase 2") {
				t.Errorf("error %q should mention phase 2", err)
			}
		})
	}
}

func TestDetectRuntime_EmptyDirErrors(t *testing.T) {
	_, err := DetectRuntime(t.TempDir())
	if err == nil {
		t.Fatal("expected error for empty dir")
	}
}
