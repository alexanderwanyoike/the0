package internal

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"the0/internal"
)

func TestValidateBotConfig(t *testing.T) {
	// Create temporary directory for test files
	tempDir, err := os.MkdirTemp("", "config-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Change to temp directory for file operations
	oldWd, _ := os.Getwd()
	defer os.Chdir(oldWd)
	os.Chdir(tempDir)

	// Helper function to create test files
	createTestFiles := func(files []string) {
		for _, file := range files {
			dir := filepath.Dir(file)
			if dir != "." {
				os.MkdirAll(dir, 0755)
			}
			os.WriteFile(file, []byte("test content"), 0644)
		}
	}

	tests := []struct {
		name      string
		config    *internal.BotConfig
		files     []string // Files to create for this test
		wantErr   bool
		errorType string // Type of error expected
	}{
		{
			name: "valid config with all files",
			config: &internal.BotConfig{
				Name:        "test-bot",
				Version:     "1.0.0",
				Type:        "scheduled",
				Author:      "test-author",
				Description: "Test bot description",
				Runtime:     "", // Optional for scheduled bots
				Entrypoints: struct {
					Bot   string `yaml:"bot" json:"bot"`
					Query string `yaml:"query,omitempty" json:"query,omitempty"`
				}{
					Bot: "main.py",
				},
				Schema: struct {
					Bot string `yaml:"bot" json:"bot"`
				}{
					Bot: "bot-schema.json",
				},
				Readme: "README.md",
			},
			files:   []string{"main.py", "bot-schema.json", "README.md"},
			wantErr: false,
		},
		{
			name: "invalid name with uppercase",
			config: &internal.BotConfig{
				Name:    "Test-Bot",
				Version: "1.0.0",
				Type:    "scheduled",
			},
			files:     []string{},
			wantErr:   true,
			errorType: "name validation",
		},
		{
			name: "invalid name with special characters",
			config: &internal.BotConfig{
				Name:    "test_bot@123",
				Version: "1.0.0",
				Type:    "scheduled",
			},
			files:     []string{},
			wantErr:   true,
			errorType: "name validation",
		},
		{
			name: "invalid version",
			config: &internal.BotConfig{
				Name:    "test-bot",
				Version: "invalid-version",
				Type:    "scheduled",
			},
			files:     []string{},
			wantErr:   true,
			errorType: "version validation",
		},
		{
			name: "missing required fields",
			config: &internal.BotConfig{
				Name: "test-bot",
			},
			files:     []string{},
			wantErr:   true,
			errorType: "missing fields",
		},
		{
			name: "missing files",
			config: &internal.BotConfig{
				Name:        "test-bot",
				Version:     "1.0.0",
				Type:        "scheduled",
				Author:      "test-author",
				Description: "Test bot description",
				Entrypoints: struct {
					Bot   string `yaml:"bot" json:"bot"`
					Query string `yaml:"query,omitempty" json:"query,omitempty"`
				}{
					Bot: "missing-main.py",
				},
				Schema: struct {
					Bot string `yaml:"bot" json:"bot"`
				}{
					Bot: "missing-bot-schema.json",
				},
				Readme: "missing-README.md",
			},
			files:     []string{}, // Don't create the files
			wantErr:   true,
			errorType: "missing files",
		},
		{
			name: "empty name",
			config: &internal.BotConfig{
				Name:    "",
				Version: "1.0.0",
				Type:    "scheduled",
			},
			files:     []string{},
			wantErr:   true,
			errorType: "empty name",
		},
		{
			name: "empty version",
			config: &internal.BotConfig{
				Name:    "test-bot",
				Version: "",
				Type:    "scheduled",
			},
			files:     []string{},
			wantErr:   true,
			errorType: "empty version",
		},
		{
			name: "empty type",
			config: &internal.BotConfig{
				Name:    "test-bot",
				Version: "1.0.0",
				Type:    "",
			},
			files:     []string{},
			wantErr:   true,
			errorType: "empty type",
		},
		{
			name: "valid realtime bot with runtime",
			config: &internal.BotConfig{
				Name:        "realtime-bot",
				Version:     "1.0.0",
				Type:        "realtime",
				Runtime:     "python3.11",
				Author:      "test-author",
				Description: "Test realtime bot",
				Entrypoints: struct {
					Bot   string `yaml:"bot" json:"bot"`
					Query string `yaml:"query,omitempty" json:"query,omitempty"`
				}{
					Bot: "main.py",
				},
				Schema: struct {
					Bot string `yaml:"bot" json:"bot"`
				}{
					Bot: "bot-schema.json",
				},
				Readme: "README.md",
			},
			files:   []string{"main.py", "bot-schema.json", "README.md"},
			wantErr: false,
		},
		{
			name: "invalid realtime bot without runtime",
			config: &internal.BotConfig{
				Name:        "realtime-bot",
				Version:     "1.0.0",
				Type:        "realtime",
				Runtime:     "", // Missing runtime for realtime bot
				Author:      "test-author",
				Description: "Test realtime bot",
				Entrypoints: struct {
					Bot   string `yaml:"bot" json:"bot"`
					Query string `yaml:"query,omitempty" json:"query,omitempty"`
				}{
					Bot: "main.py",
				},
				Schema: struct {
					Bot string `yaml:"bot" json:"bot"`
				}{
					Bot: "bot-schema.json",
				},
				Readme: "README.md",
			},
			files:     []string{"main.py", "bot-schema.json", "README.md"},
			wantErr:   true,
			errorType: "missing runtime",
		},
		{
			name: "valid scheduled bot without runtime",
			config: &internal.BotConfig{
				Name:        "scheduled-bot",
				Version:     "1.0.0",
				Type:        "scheduled",
				Runtime:     "", // Optional for scheduled bots
				Author:      "test-author",
				Description: "Test scheduled bot",
				Entrypoints: struct {
					Bot   string `yaml:"bot" json:"bot"`
					Query string `yaml:"query,omitempty" json:"query,omitempty"`
				}{
					Bot: "main.py",
				},
				Schema: struct {
					Bot string `yaml:"bot" json:"bot"`
				}{
					Bot: "bot-schema.json",
				},
				Readme: "README.md",
			},
			files:   []string{"main.py", "bot-schema.json", "README.md"},
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Clean up any existing files and create new ones
			files, _ := filepath.Glob("*")
			for _, file := range files {
				os.RemoveAll(file)
			}

			// Create required test files
			createTestFiles(tt.files)

			err := internal.ValidateBotConfig(tt.config)
			if (err != nil) != tt.wantErr {
				t.Errorf("ValidateBotConfig() error = %v, wantErr %v", err, tt.wantErr)
				return
			}

			// Additional checks for specific error types
			if tt.wantErr && err != nil {
				switch tt.errorType {
				case "name validation":
					if !strings.Contains(err.Error(), "name must contain only lowercase") {
						t.Errorf("Expected name validation error, got: %v", err)
					}
				case "version validation":
					if !strings.Contains(err.Error(), "invalid version format") {
						t.Errorf("Expected version validation error, got: %v", err)
					}
				case "missing files":
					if !strings.Contains(err.Error(), "required file not found") {
						t.Errorf("Expected missing file error, got: %v", err)
					}
				case "empty name":
					if !strings.Contains(err.Error(), "bot name is required") {
						t.Errorf("Expected empty name error, got: %v", err)
					}
				case "empty version":
					if !strings.Contains(err.Error(), "version is required") {
						t.Errorf("Expected empty version error, got: %v", err)
					}
				case "empty type":
					if !strings.Contains(err.Error(), "bot type is required") {
						t.Errorf("Expected empty type error, got: %v", err)
					}
				case "missing runtime":
					if !strings.Contains(err.Error(), "runtime is required for realtime bots") {
						t.Errorf("Expected missing runtime error, got: %v", err)
					}
				}
			}
		})
	}
}

func TestLoadBotConfig(t *testing.T) {
	// Create temporary directory for test
	tempDir, err := os.MkdirTemp("", "config-load-test")
	if err != nil {
		t.Fatalf("Failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tempDir)

	// Change to temp directory
	oldWd, _ := os.Getwd()
	defer os.Chdir(oldWd)
	os.Chdir(tempDir)

	t.Run("missing config file", func(t *testing.T) {
		_, err := internal.LoadBotConfig()
		if err == nil {
			t.Error("LoadBotConfig() expected error for missing file, got nil")
		}
		if !strings.Contains(err.Error(), "bot-config.yaml not found") {
			t.Errorf("Expected 'not found' error, got: %v", err)
		}
	})

	t.Run("invalid YAML", func(t *testing.T) {
		// Create invalid YAML file
		invalidYAML := `
name: test-bot
version: 1.0.0
invalid_yaml: [
  missing_close_bracket
`
		os.WriteFile("bot-config.yaml", []byte(invalidYAML), 0644)

		_, err := internal.LoadBotConfig()
		if err == nil {
			t.Error("LoadBotConfig() expected error for invalid YAML, got nil")
		}
		if !strings.Contains(err.Error(), "invalid YAML") {
			t.Errorf("Expected 'invalid YAML' error, got: %v", err)
		}

		// Clean up
		os.Remove("bot-config.yaml")
	})

	t.Run("valid config file", func(t *testing.T) {
		// Create valid YAML file
		validYAML := `
name: test-bot
description: A test bot
version: 1.0.0
author: test-author
type: scheduled
runtime: python3.11

entrypoints:
  bot: main.py

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  tags: ["test"]
  category: "testing"
`
		os.WriteFile("bot-config.yaml", []byte(validYAML), 0644)

		config, err := internal.LoadBotConfig()
		if err != nil {
			t.Errorf("LoadBotConfig() unexpected error: %v", err)
			return
		}

		// Verify loaded config
		if config.Name != "test-bot" {
			t.Errorf("Expected name 'test-bot', got '%s'", config.Name)
		}
		if config.Version != "1.0.0" {
			t.Errorf("Expected version '1.0.0', got '%s'", config.Version)
		}
		if config.Type != "scheduled" {
			t.Errorf("Expected type 'scheduled', got '%s'", config.Type)
		}
		if config.Entrypoints.Bot != "main.py" {
			t.Errorf("Expected bot entrypoint 'main.py', got '%s'", config.Entrypoints.Bot)
		}
		if config.Runtime != "python3.11" {
			t.Errorf("Expected runtime 'python3.11', got '%s'", config.Runtime)
		}

		// Clean up
		os.Remove("bot-config.yaml")
	})
}
