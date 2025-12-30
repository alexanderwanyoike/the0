package internal

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

func TestPythonVendor_Name(t *testing.T) {
	v := &PythonVendor{}
	if v.Name() != "Python" {
		t.Errorf("Expected 'Python', got '%s'", v.Name())
	}
}

func TestPythonVendor_DockerImage(t *testing.T) {
	v := &PythonVendor{}
	if v.DockerImage() != "python:3.11-slim" {
		t.Errorf("Expected 'python:3.11-slim', got '%s'", v.DockerImage())
	}
}

func TestPythonVendor_Detect_WithRequirements(t *testing.T) {
	tmpDir := t.TempDir()
	requirementsPath := filepath.Join(tmpDir, "requirements.txt")
	if err := os.WriteFile(requirementsPath, []byte("requests\n"), 0644); err != nil {
		t.Fatalf("Failed to create requirements.txt: %v", err)
	}

	v := &PythonVendor{}
	if !v.Detect(tmpDir) {
		t.Error("Expected Detect to return true when requirements.txt exists")
	}
}

func TestPythonVendor_Detect_WithoutRequirements(t *testing.T) {
	tmpDir := t.TempDir()

	v := &PythonVendor{}
	if v.Detect(tmpDir) {
		t.Error("Expected Detect to return false when requirements.txt doesn't exist")
	}
}

func TestPythonVendor_ShouldVendor_NoRequirements(t *testing.T) {
	tmpDir := t.TempDir()

	v := &PythonVendor{}
	shouldVendor, err := v.ShouldVendor(tmpDir)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if shouldVendor {
		t.Error("Expected ShouldVendor to return false when no requirements.txt")
	}
}

func TestPythonVendor_ShouldVendor_WithRequirements(t *testing.T) {
	tmpDir := t.TempDir()
	requirementsPath := filepath.Join(tmpDir, "requirements.txt")
	if err := os.WriteFile(requirementsPath, []byte("requests\n"), 0644); err != nil {
		t.Fatalf("Failed to create requirements.txt: %v", err)
	}

	v := &PythonVendor{}
	shouldVendor, err := v.ShouldVendor(tmpDir)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if !shouldVendor {
		t.Error("Expected ShouldVendor to return true when requirements.txt exists")
	}
}

func TestPythonVendor_ShouldVendor_VendorNewer(t *testing.T) {
	tmpDir := t.TempDir()

	// Create requirements.txt with older timestamp
	requirementsPath := filepath.Join(tmpDir, "requirements.txt")
	if err := os.WriteFile(requirementsPath, []byte("requests\n"), 0644); err != nil {
		t.Fatalf("Failed to create requirements.txt: %v", err)
	}
	// Set requirements.txt to be older
	oldTime := time.Now().Add(-time.Hour)
	if err := os.Chtimes(requirementsPath, oldTime, oldTime); err != nil {
		t.Fatalf("Failed to set requirements.txt time: %v", err)
	}

	// Create vendor directory with newer timestamp
	vendorPath := filepath.Join(tmpDir, "vendor")
	if err := os.MkdirAll(vendorPath, 0755); err != nil {
		t.Fatalf("Failed to create vendor directory: %v", err)
	}

	v := &PythonVendor{}
	shouldVendor, err := v.ShouldVendor(tmpDir)
	if err != nil {
		t.Fatalf("Unexpected error: %v", err)
	}
	if shouldVendor {
		t.Error("Expected ShouldVendor to return false when vendor is newer than requirements.txt")
	}
}

func TestPythonVendor_GetInstallCommand_ContainsGitConfig(t *testing.T) {
	v := &PythonVendor{}
	cmd := v.getInstallCommand()

	// Should contain git config for GITHUB_TOKEN
	if !strings.Contains(cmd, "GITHUB_TOKEN") {
		t.Error("Expected command to reference GITHUB_TOKEN")
	}

	if !strings.Contains(cmd, "git config --global") {
		t.Error("Expected command to contain git config")
	}

	// Should contain the URL rewrite for github.com
	if !strings.Contains(cmd, "github.com") {
		t.Error("Expected command to contain github.com URL rewrite")
	}

	// Should still contain pip install
	if !strings.Contains(cmd, "pip install") {
		t.Error("Expected command to contain pip install")
	}

	// Should contain chown for proper ownership
	if !strings.Contains(cmd, "chown") {
		t.Error("Expected command to contain chown")
	}
}

func TestPythonVendor_GetInstallCommand_ConditionalGitConfig(t *testing.T) {
	v := &PythonVendor{}
	cmd := v.getInstallCommand()

	// Git config should be conditional on GITHUB_TOKEN being set
	if !strings.Contains(cmd, `if [ -n "$GITHUB_TOKEN" ]`) {
		t.Error("Expected git config to be conditional on GITHUB_TOKEN")
	}
}

func TestPythonVendor_GetInstallCommand_WithPipIndex(t *testing.T) {
	v := &PythonVendor{}
	cmd := v.getInstallCommand()

	// Should contain pip extra index setup
	if !strings.Contains(cmd, "PIP_EXTRA_INDEX_URL") {
		t.Error("Expected command to reference PIP_EXTRA_INDEX_URL")
	}

	if !strings.Contains(cmd, "PIP_EXTRA_INDEX_FLAG") {
		t.Error("Expected command to use PIP_EXTRA_INDEX_FLAG")
	}

	if !strings.Contains(cmd, "--extra-index-url") {
		t.Error("Expected command to contain --extra-index-url")
	}
}
