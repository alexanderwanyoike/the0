package dev

import (
	"os"
	"path/filepath"
	"testing"
)

func touch(t *testing.T, dir, name string) {
	t.Helper()
	if err := os.WriteFile(filepath.Join(dir, name), []byte{}, 0644); err != nil {
		t.Fatalf("touch %s: %v", name, err)
	}
}

func TestDetectRuntime(t *testing.T) {
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
		{"rust Cargo.toml", []string{"Cargo.toml"}, RuntimeRust},
		{"dotnet csproj", []string{"bot.csproj"}, RuntimeDotnet},
		{"scala sbt", []string{"build.sbt"}, RuntimeScala},
		{"cpp CMake", []string{"CMakeLists.txt"}, RuntimeCpp},
		{"cpp Makefile", []string{"Makefile"}, RuntimeCpp},
		{"haskell cabal", []string{"bot.cabal"}, RuntimeHaskell},
		// When both package.json and main.py are present, the strongest
		// manifest wins: package.json (node) over main.py (python) because
		// deploy-side detection is manifest-first.
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

func TestDetectRuntime_EmptyDirErrors(t *testing.T) {
	_, err := DetectRuntime(t.TempDir())
	if err == nil {
		t.Fatal("expected error for empty dir")
	}
}
