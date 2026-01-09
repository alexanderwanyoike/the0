package internal

import (
	"os"
	"path/filepath"
	"testing"
)

func TestHaskellBuilder_Detect(t *testing.T) {
	builder := &HaskellBuilder{}

	t.Run("detects project with cabal file", func(t *testing.T) {
		tmpDir := t.TempDir()
		cabalFile := filepath.Join(tmpDir, "my-project.cabal")
		if err := os.WriteFile(cabalFile, []byte("cabal-version: 3.0\nname: my-project"), 0644); err != nil {
			t.Fatal(err)
		}

		if !builder.Detect(tmpDir) {
			t.Error("expected Detect to return true for directory with .cabal file")
		}
	})

	t.Run("does not detect project without cabal file", func(t *testing.T) {
		tmpDir := t.TempDir()

		if builder.Detect(tmpDir) {
			t.Error("expected Detect to return false for directory without .cabal file")
		}
	})

	t.Run("does not detect non-existent directory", func(t *testing.T) {
		if builder.Detect("/non/existent/path") {
			t.Error("expected Detect to return false for non-existent directory")
		}
	})
}

func TestHaskellBuilder_ShouldBuild(t *testing.T) {
	builder := &HaskellBuilder{}

	t.Run("should build with cabal file", func(t *testing.T) {
		tmpDir := t.TempDir()
		cabalFile := filepath.Join(tmpDir, "test.cabal")
		if err := os.WriteFile(cabalFile, []byte("cabal-version: 3.0"), 0644); err != nil {
			t.Fatal(err)
		}

		shouldBuild, err := builder.ShouldBuild(tmpDir)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if !shouldBuild {
			t.Error("expected ShouldBuild to return true")
		}
	})

	t.Run("should not build without cabal file", func(t *testing.T) {
		tmpDir := t.TempDir()

		shouldBuild, err := builder.ShouldBuild(tmpDir)
		if err != nil {
			t.Fatalf("unexpected error: %v", err)
		}
		if shouldBuild {
			t.Error("expected ShouldBuild to return false")
		}
	})
}

func TestHaskellBuilder_FindBinary(t *testing.T) {
	builder := &HaskellBuilder{}

	t.Run("finds binary in bin directory", func(t *testing.T) {
		tmpDir := t.TempDir()
		binDir := filepath.Join(tmpDir, "bin")
		if err := os.MkdirAll(binDir, 0755); err != nil {
			t.Fatal(err)
		}

		// Create executable file
		binaryPath := filepath.Join(binDir, "my-bot")
		if err := os.WriteFile(binaryPath, []byte("#!/bin/bash\necho hello"), 0755); err != nil {
			t.Fatal(err)
		}

		result := builder.FindBinary(tmpDir)
		if result == "" {
			t.Error("expected FindBinary to return path to binary")
		}
		if filepath.Base(result) != "my-bot" {
			t.Errorf("expected binary name 'my-bot', got '%s'", filepath.Base(result))
		}
	})

	t.Run("returns empty for missing bin directory", func(t *testing.T) {
		tmpDir := t.TempDir()

		result := builder.FindBinary(tmpDir)
		if result != "" {
			t.Errorf("expected empty string, got '%s'", result)
		}
	})

	t.Run("returns empty for empty bin directory", func(t *testing.T) {
		tmpDir := t.TempDir()
		binDir := filepath.Join(tmpDir, "bin")
		if err := os.MkdirAll(binDir, 0755); err != nil {
			t.Fatal(err)
		}

		result := builder.FindBinary(tmpDir)
		if result != "" {
			t.Errorf("expected empty string, got '%s'", result)
		}
	})
}

func TestHaskellBuilder_FindAllBinaries(t *testing.T) {
	builder := &HaskellBuilder{}

	t.Run("finds multiple binaries", func(t *testing.T) {
		tmpDir := t.TempDir()
		binDir := filepath.Join(tmpDir, "bin")
		if err := os.MkdirAll(binDir, 0755); err != nil {
			t.Fatal(err)
		}

		// Create multiple executable files
		binaries := []string{"bot-main", "bot-query", "bot-worker"}
		for _, name := range binaries {
			path := filepath.Join(binDir, name)
			if err := os.WriteFile(path, []byte("#!/bin/bash"), 0755); err != nil {
				t.Fatal(err)
			}
		}

		result := builder.FindAllBinaries(tmpDir)
		if len(result) != 3 {
			t.Errorf("expected 3 binaries, got %d", len(result))
		}
	})

	t.Run("ignores non-executable files", func(t *testing.T) {
		tmpDir := t.TempDir()
		binDir := filepath.Join(tmpDir, "bin")
		if err := os.MkdirAll(binDir, 0755); err != nil {
			t.Fatal(err)
		}

		// Create one executable and one non-executable
		execPath := filepath.Join(binDir, "my-bot")
		if err := os.WriteFile(execPath, []byte("#!/bin/bash"), 0755); err != nil {
			t.Fatal(err)
		}
		nonExecPath := filepath.Join(binDir, "readme.txt")
		if err := os.WriteFile(nonExecPath, []byte("readme"), 0644); err != nil {
			t.Fatal(err)
		}

		result := builder.FindAllBinaries(tmpDir)
		if len(result) != 1 {
			t.Errorf("expected 1 binary, got %d", len(result))
		}
	})

	t.Run("ignores directories", func(t *testing.T) {
		tmpDir := t.TempDir()
		binDir := filepath.Join(tmpDir, "bin")
		if err := os.MkdirAll(binDir, 0755); err != nil {
			t.Fatal(err)
		}

		// Create executable file and a subdirectory
		execPath := filepath.Join(binDir, "my-bot")
		if err := os.WriteFile(execPath, []byte("#!/bin/bash"), 0755); err != nil {
			t.Fatal(err)
		}
		subDir := filepath.Join(binDir, "subdir")
		if err := os.MkdirAll(subDir, 0755); err != nil {
			t.Fatal(err)
		}

		result := builder.FindAllBinaries(tmpDir)
		if len(result) != 1 {
			t.Errorf("expected 1 binary, got %d", len(result))
		}
	})
}

func TestHaskellBuilder_Name(t *testing.T) {
	builder := &HaskellBuilder{}
	if builder.Name() != "Haskell" {
		t.Errorf("expected name 'Haskell', got '%s'", builder.Name())
	}
}

func TestHaskellBuilder_DockerImage(t *testing.T) {
	builder := &HaskellBuilder{}
	if builder.DockerImage() != "haskell:9.6" {
		t.Errorf("expected image 'haskell:9.6', got '%s'", builder.DockerImage())
	}
}
