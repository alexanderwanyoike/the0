package internal

import (
	"os"
	"strings"
	"testing"
)

func TestGetBuildEnvVars_NoSecrets(t *testing.T) {
	// Use a temp home directory with no secrets
	tmpDir := t.TempDir()
	originalHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", originalHome)

	envVars := getBuildEnvVars()
	if envVars != nil && len(envVars) > 0 {
		t.Errorf("Expected nil or empty env vars when no secrets, got: %v", envVars)
	}
}

func TestGetBuildEnvVars_WithGitHubToken(t *testing.T) {
	tmpDir := t.TempDir()
	originalHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", originalHome)

	// Save a GitHub token
	secrets := &BuildSecrets{GitHubToken: "ghp_testtoken123"}
	if err := SaveBuildSecrets(secrets); err != nil {
		t.Fatalf("Failed to save secrets: %v", err)
	}

	envVars := getBuildEnvVars()
	if len(envVars) != 1 {
		t.Fatalf("Expected 1 env var, got: %d", len(envVars))
	}

	expected := "GITHUB_TOKEN=ghp_testtoken123"
	if envVars[0] != expected {
		t.Errorf("Expected env var '%s', got: '%s'", expected, envVars[0])
	}
}

func TestGetBuildEnvVars_EmptyToken(t *testing.T) {
	tmpDir := t.TempDir()
	originalHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", originalHome)

	// Save empty secrets
	secrets := &BuildSecrets{GitHubToken: ""}
	if err := SaveBuildSecrets(secrets); err != nil {
		t.Fatalf("Failed to save secrets: %v", err)
	}

	envVars := getBuildEnvVars()
	if envVars != nil && len(envVars) > 0 {
		t.Errorf("Expected nil or empty env vars for empty token, got: %v", envVars)
	}
}

func TestGetPythonInstallCommand_ContainsGitConfig(t *testing.T) {
	vm := &VendorManager{projectPath: "/tmp/test"}
	cmd := vm.getPythonInstallCommand()

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

func TestGetPythonInstallCommand_ConditionalGitConfig(t *testing.T) {
	vm := &VendorManager{projectPath: "/tmp/test"}
	cmd := vm.getPythonInstallCommand()

	// Git config should be conditional on GITHUB_TOKEN being set
	if !strings.Contains(cmd, `if [ -n "$GITHUB_TOKEN" ]`) {
		t.Error("Expected git config to be conditional on GITHUB_TOKEN")
	}
}

func TestGetBuildEnvVars_WithPipIndexURL(t *testing.T) {
	tmpDir := t.TempDir()
	originalHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", originalHome)

	// Save pip index URL
	secrets := &BuildSecrets{PipIndexURL: "https://user:pass@pypi.example.com/simple/"}
	if err := SaveBuildSecrets(secrets); err != nil {
		t.Fatalf("Failed to save secrets: %v", err)
	}

	envVars := getBuildEnvVars()
	if len(envVars) != 1 {
		t.Fatalf("Expected 1 env var, got: %d", len(envVars))
	}

	expected := "PIP_EXTRA_INDEX_URL=https://user:pass@pypi.example.com/simple/"
	if envVars[0] != expected {
		t.Errorf("Expected env var '%s', got: '%s'", expected, envVars[0])
	}
}

func TestGetBuildEnvVars_WithBothSecrets(t *testing.T) {
	tmpDir := t.TempDir()
	originalHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", originalHome)

	// Save both secrets
	secrets := &BuildSecrets{
		GitHubToken: "ghp_token123",
		PipIndexURL: "https://pypi.example.com/simple/",
	}
	if err := SaveBuildSecrets(secrets); err != nil {
		t.Fatalf("Failed to save secrets: %v", err)
	}

	envVars := getBuildEnvVars()
	if len(envVars) != 2 {
		t.Fatalf("Expected 2 env vars, got: %d", len(envVars))
	}

	// Check both are present
	hasGitHub := false
	hasPip := false
	for _, v := range envVars {
		if strings.HasPrefix(v, "GITHUB_TOKEN=") {
			hasGitHub = true
		}
		if strings.HasPrefix(v, "PIP_EXTRA_INDEX_URL=") {
			hasPip = true
		}
	}

	if !hasGitHub {
		t.Error("Expected GITHUB_TOKEN env var")
	}
	if !hasPip {
		t.Error("Expected PIP_EXTRA_INDEX_URL env var")
	}
}

func TestGetPythonInstallCommand_WithPipIndex(t *testing.T) {
	vm := &VendorManager{projectPath: "/tmp/test"}
	cmd := vm.getPythonInstallCommand()

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
