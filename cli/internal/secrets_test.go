package internal

import (
	"encoding/json"
	"os"
	"path/filepath"
	"testing"
)

func TestLoadBuildSecrets_FileNotExists(t *testing.T) {
	// Use a temp home directory
	tmpDir := t.TempDir()
	originalHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", originalHome)

	secrets, err := LoadBuildSecrets()
	if err != nil {
		t.Fatalf("Expected no error for missing file, got: %v", err)
	}

	if secrets == nil {
		t.Fatal("Expected empty secrets struct, got nil")
	}

	if secrets.GitHubToken != "" {
		t.Errorf("Expected empty GitHubToken, got: %s", secrets.GitHubToken)
	}
}

func TestLoadBuildSecrets_ValidFile(t *testing.T) {
	tmpDir := t.TempDir()
	originalHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", originalHome)

	// Create secrets file
	secretsDir := filepath.Join(tmpDir, ".the0")
	if err := os.MkdirAll(secretsDir, 0700); err != nil {
		t.Fatalf("Failed to create secrets dir: %v", err)
	}

	secretsData := BuildSecrets{GitHubToken: "ghp_testtoken123"}
	data, _ := json.Marshal(secretsData)
	secretsPath := filepath.Join(secretsDir, "secrets.json")
	if err := os.WriteFile(secretsPath, data, 0600); err != nil {
		t.Fatalf("Failed to write secrets file: %v", err)
	}

	secrets, err := LoadBuildSecrets()
	if err != nil {
		t.Fatalf("Expected no error, got: %v", err)
	}

	if secrets.GitHubToken != "ghp_testtoken123" {
		t.Errorf("Expected GitHubToken 'ghp_testtoken123', got: %s", secrets.GitHubToken)
	}
}

func TestLoadBuildSecrets_InvalidJSON(t *testing.T) {
	tmpDir := t.TempDir()
	originalHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", originalHome)

	// Create invalid JSON file
	secretsDir := filepath.Join(tmpDir, ".the0")
	if err := os.MkdirAll(secretsDir, 0700); err != nil {
		t.Fatalf("Failed to create secrets dir: %v", err)
	}

	secretsPath := filepath.Join(secretsDir, "secrets.json")
	if err := os.WriteFile(secretsPath, []byte("not valid json"), 0600); err != nil {
		t.Fatalf("Failed to write secrets file: %v", err)
	}

	_, err := LoadBuildSecrets()
	if err == nil {
		t.Fatal("Expected error for invalid JSON, got nil")
	}
}

func TestSaveBuildSecrets_CreatesFile(t *testing.T) {
	tmpDir := t.TempDir()
	originalHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", originalHome)

	secrets := &BuildSecrets{GitHubToken: "ghp_newtoken456"}
	if err := SaveBuildSecrets(secrets); err != nil {
		t.Fatalf("Failed to save secrets: %v", err)
	}

	// Verify file exists with correct permissions
	secretsPath := filepath.Join(tmpDir, ".the0", "secrets.json")
	info, err := os.Stat(secretsPath)
	if err != nil {
		t.Fatalf("Secrets file not created: %v", err)
	}

	// Check permissions (0600)
	if info.Mode().Perm() != 0600 {
		t.Errorf("Expected permissions 0600, got: %o", info.Mode().Perm())
	}

	// Verify content
	data, err := os.ReadFile(secretsPath)
	if err != nil {
		t.Fatalf("Failed to read secrets file: %v", err)
	}

	var loaded BuildSecrets
	if err := json.Unmarshal(data, &loaded); err != nil {
		t.Fatalf("Failed to parse saved secrets: %v", err)
	}

	if loaded.GitHubToken != "ghp_newtoken456" {
		t.Errorf("Expected GitHubToken 'ghp_newtoken456', got: %s", loaded.GitHubToken)
	}
}

func TestSaveBuildSecrets_OverwritesExisting(t *testing.T) {
	tmpDir := t.TempDir()
	originalHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", originalHome)

	// Save initial secrets
	secrets1 := &BuildSecrets{GitHubToken: "ghp_oldtoken"}
	if err := SaveBuildSecrets(secrets1); err != nil {
		t.Fatalf("Failed to save initial secrets: %v", err)
	}

	// Save new secrets
	secrets2 := &BuildSecrets{GitHubToken: "ghp_newtoken"}
	if err := SaveBuildSecrets(secrets2); err != nil {
		t.Fatalf("Failed to save updated secrets: %v", err)
	}

	// Verify new content
	loaded, err := LoadBuildSecrets()
	if err != nil {
		t.Fatalf("Failed to load secrets: %v", err)
	}

	if loaded.GitHubToken != "ghp_newtoken" {
		t.Errorf("Expected GitHubToken 'ghp_newtoken', got: %s", loaded.GitHubToken)
	}
}

func TestClearBuildSecrets(t *testing.T) {
	tmpDir := t.TempDir()
	originalHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", originalHome)

	// Save secrets first
	secrets := &BuildSecrets{GitHubToken: "ghp_token"}
	if err := SaveBuildSecrets(secrets); err != nil {
		t.Fatalf("Failed to save secrets: %v", err)
	}

	// Clear secrets
	if err := ClearBuildSecrets(); err != nil {
		t.Fatalf("Failed to clear secrets: %v", err)
	}

	// Verify file is gone
	secretsPath := filepath.Join(tmpDir, ".the0", "secrets.json")
	if _, err := os.Stat(secretsPath); !os.IsNotExist(err) {
		t.Error("Expected secrets file to be removed")
	}
}

func TestClearBuildSecrets_NoFile(t *testing.T) {
	tmpDir := t.TempDir()
	originalHome := os.Getenv("HOME")
	os.Setenv("HOME", tmpDir)
	defer os.Setenv("HOME", originalHome)

	// Should not error when file doesn't exist
	if err := ClearBuildSecrets(); err != nil {
		t.Fatalf("Expected no error when clearing non-existent file, got: %v", err)
	}
}

func TestMaskToken(t *testing.T) {
	tests := []struct {
		input    string
		expected string
	}{
		{"ghp_1234567890abcdef", "ghp_...cdef"},
		{"short", "****"},
		{"12345678", "****"},
		{"123456789", "1234...6789"},
		{"", "****"},
	}

	for _, tt := range tests {
		result := MaskToken(tt.input)
		if result != tt.expected {
			t.Errorf("MaskToken(%q) = %q, want %q", tt.input, result, tt.expected)
		}
	}
}
