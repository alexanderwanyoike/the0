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
