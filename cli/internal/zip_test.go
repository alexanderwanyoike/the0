package internal

import (
	"archive/zip"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestCreateBotZipUsesForwardSlashes guards the path separator used in archive
// entry names.
//
// The ZIP spec (APPNOTE 4.4.17.1) requires forward slashes. filepath.Rel
// returns OS-native separators, so on Windows this previously produced entries
// named `vendor\the0\__init__.py`. Backslash is a legal filename character on
// Linux, so the runtime extracted that as a single flat file, `vendor/` never
// existed inside the bot container, and every dependency import failed at
// runtime with ModuleNotFoundError - long after the CLI had reported a
// successful deploy.
//
// Reading the archive back with a library is not enough to catch this on
// Windows: Go's archive/zip preserves the raw name, but several tools and
// libraries (Python's zipfile, for one) silently normalise os.sep to '/' when
// reading, which hides the defect on the very platform that produces it. This
// asserts on the stored name directly.
func TestCreateBotZipUsesForwardSlashes(t *testing.T) {
	sourceDir := t.TempDir()

	nested := filepath.Join(sourceDir, "vendor", "the0", "inner")
	if err := os.MkdirAll(nested, 0755); err != nil {
		t.Fatalf("failed to create nested dirs: %v", err)
	}
	files := []string{
		filepath.Join(sourceDir, "main.py"),
		filepath.Join(sourceDir, "vendor", "the0", "__init__.py"),
		filepath.Join(nested, "deep.py"),
	}
	for _, path := range files {
		if err := os.WriteFile(path, []byte("# test\n"), 0644); err != nil {
			t.Fatalf("failed to write %s: %v", path, err)
		}
	}

	zipPath, err := CreateBotZipFromDir(sourceDir)
	if err != nil {
		t.Fatalf("CreateBotZipFromDir failed: %v", err)
	}
	defer os.Remove(zipPath)

	reader, err := zip.OpenReader(zipPath)
	if err != nil {
		t.Fatalf("failed to open zip: %v", err)
	}
	defer reader.Close()

	found := map[string]bool{}
	for _, file := range reader.File {
		if strings.Contains(file.Name, `\`) {
			t.Errorf("zip entry %q contains a backslash; entry names must use forward slashes", file.Name)
		}
		found[file.Name] = true
	}

	for _, want := range []string{
		"main.py",
		"vendor/the0/__init__.py",
		"vendor/the0/inner/deep.py",
	} {
		if !found[want] {
			t.Errorf("expected zip entry %q, got entries: %v", want, keys(found))
		}
	}
}

func keys(m map[string]bool) []string {
	out := make([]string, 0, len(m))
	for k := range m {
		out = append(out, k)
	}
	return out
}
