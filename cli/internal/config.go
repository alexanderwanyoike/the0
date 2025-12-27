package internal

import (
	"fmt"
	"os"
	"regexp"

	"github.com/Masterminds/semver/v3"
	"gopkg.in/yaml.v3"
)

// BotConfig represents the bot configuration structure
type BotConfig struct {
	Name        string `yaml:"name" json:"name"`
	Description string `yaml:"description" json:"description"`
	Version     string `yaml:"version" json:"version"`
	Author      string `yaml:"author" json:"author"`
	Runtime     string `yaml:"runtime,omitempty" json:"runtime,omitempty"` // e.g., "python3.11", "nodejs20", defaults to "none"
	Type        string `yaml:"type" json:"type"`                           // e.g., "scheduled", "event", "realtime"
	Entrypoints struct {
		Bot string `yaml:"bot" json:"bot"`
	} `yaml:"entrypoints" json:"entrypoints"`
	Schema struct {
		Bot string `yaml:"bot" json:"bot"`
	} `yaml:"schema" json:"schema"`
	Readme   string                 `yaml:"readme" json:"readme"`
	Metadata map[string]interface{} `yaml:"metadata,omitempty" json:"metadata,omitempty"`
}

// LoadBotConfig loads and parses the bot-config.yaml file
func LoadBotConfig() (*BotConfig, error) {
	if _, err := os.Stat("bot-config.yaml"); os.IsNotExist(err) {
		return nil, fmt.Errorf("bot-config.yaml not found in current directory")
	}

	data, err := os.ReadFile("bot-config.yaml")
	if err != nil {
		return nil, fmt.Errorf("failed to read bot-config.yaml: %v", err)
	}

	var config BotConfig
	if err := yaml.Unmarshal(data, &config); err != nil {
		return nil, fmt.Errorf("invalid YAML in bot-config.yaml: %v", err)
	}

	return &config, nil
}

// ValidateBotConfig validates the bot configuration structure (no file existence checks)
func ValidateBotConfig(config *BotConfig) error {
	if config.Name == "" {
		return fmt.Errorf("bot name is required")
	}

	// Validate name format (only dashes allowed)
	nameRegex := regexp.MustCompile(`^[a-z0-9-]+$`)
	if !nameRegex.MatchString(config.Name) {
		return fmt.Errorf("bot name must contain only lowercase letters, numbers, and dashes")
	}

	if config.Version == "" {
		return fmt.Errorf("version is required")
	}

	// Validate semver
	if _, err := semver.NewVersion(config.Version); err != nil {
		return fmt.Errorf("invalid version format. Use semantic versioning (e.g., 1.0.0): %v", err)
	}

	if config.Type == "" {
		return fmt.Errorf("bot type is required (e.g., 'scheduled', 'realtime', 'event')")
	}

	// Validate runtime requirement for realtime bots
	if config.Type == "realtime" && config.Runtime == "" {
		return fmt.Errorf("runtime is required for realtime bots (e.g., 'python3.11', 'nodejs20')")
	}

	if config.Entrypoints.Bot == "" {
		return fmt.Errorf("bot entrypoint is required")
	}

	if config.Schema.Bot == "" {
		return fmt.Errorf("bot schema is required")
	}

	if config.Readme == "" {
		return fmt.Errorf("readme file is required")
	}

	// Check if source files exist (schema and readme must exist before build)
	sourceFiles := []string{
		config.Schema.Bot,
		config.Readme,
	}

	for _, file := range sourceFiles {
		if _, err := os.Stat(file); os.IsNotExist(err) {
			return fmt.Errorf("required file not found: %s", file)
		}
	}

	return nil
}

// ValidateBotFiles validates that all required files exist after build
// This should be called after build steps complete (for compiled/transpiled languages)
func ValidateBotFiles(config *BotConfig) error {
	// Check entrypoint exists (this is a build output for compiled/transpiled languages)
	if _, err := os.Stat(config.Entrypoints.Bot); os.IsNotExist(err) {
		return fmt.Errorf("entrypoint file not found: %s (build may have failed)", config.Entrypoints.Bot)
	}
	return nil
}
