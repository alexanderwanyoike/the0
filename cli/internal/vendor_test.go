package internal

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

func TestVerifyVendoredFiles(t *testing.T) {
	tests := []struct {
		name    string
		setup   func(vendorPath string) error
		wantErr string
	}{
		{
			name: "a vendor directory with packages passes",
			setup: func(vendorPath string) error {
				if err := os.MkdirAll(filepath.Join(vendorPath, "requests"), 0755); err != nil {
					return err
				}
				return os.WriteFile(filepath.Join(vendorPath, "requests", "__init__.py"), []byte("# requests"), 0644)
			},
		},
		{
			name:    "an empty vendor directory fails",
			setup:   func(vendorPath string) error { return os.MkdirAll(vendorPath, 0755) },
			wantErr: "no packages were vendored",
		},
		{
			name:    "a missing vendor directory fails",
			setup:   func(string) error { return nil },
			wantErr: "failed to read vendor directory",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			vendorPath := filepath.Join(t.TempDir(), "vendor")
			if err := tt.setup(vendorPath); err != nil {
				t.Fatal(err)
			}

			err := (&VendorManager{}).verifyVendoredFiles(vendorPath)

			if tt.wantErr == "" {
				if err != nil {
					t.Errorf("verifyVendoredFiles() error = %v", err)
				}
				return
			}
			if err == nil || !strings.Contains(err.Error(), tt.wantErr) {
				t.Errorf("verifyVendoredFiles() error = %v, want it to contain %q", err, tt.wantErr)
			}
		})
	}
}

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
