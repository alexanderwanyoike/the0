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
		Bot      string `yaml:"bot" json:"bot"`
		Backtest string `yaml:"backtest" json:"backtest"`
	} `yaml:"entrypoints" json:"entrypoints"`
	Schema struct {
		Backtest string `yaml:"backtest" json:"backtest"`
		Bot      string `yaml:"bot" json:"bot"`
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

// ValidateBotConfig validates the bot configuration
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

	// Backtest entrypoint and schema are now optional

	if config.Schema.Bot == "" {
		return fmt.Errorf("bot schema is required")
	}

	if config.Readme == "" {
		return fmt.Errorf("readme file is required")
	}

	// Check if required files exist
	requiredFiles := []string{
		config.Entrypoints.Bot,
		config.Schema.Bot,
		config.Readme,
	}

	// Add backtest files to validation if they are specified
	if config.Entrypoints.Backtest != "" {
		requiredFiles = append(requiredFiles, config.Entrypoints.Backtest)
	}
	if config.Schema.Backtest != "" {
		requiredFiles = append(requiredFiles, config.Schema.Backtest)
	}

	for _, file := range requiredFiles {
		if _, err := os.Stat(file); os.IsNotExist(err) {
			return fmt.Errorf("required file not found: %s", file)
		}
	}

	return nil
}
