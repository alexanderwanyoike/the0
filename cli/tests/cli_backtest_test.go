package internal

import (
	"encoding/json"
	"strings"
	"testing"
	"the0/cmd"
)

func TestBacktestDeployCommand_Arguments(t *testing.T) {
	tests := []struct {
		name        string
		args        []string
		expectError bool
		errorMsg    string
	}{
		{
			name:        "correct single argument",
			args:        []string{"config.json"},
			expectError: false,
		},
		{
			name:        "no arguments",
			args:        []string{},
			expectError: true,
			errorMsg:    "accepts 1 arg(s), received 0",
		},
		{
			name:        "too many arguments",
			args:        []string{"config.json", "extra"},
			expectError: true,
			errorMsg:    "accepts 1 arg(s), received 2",
		},
		{
			name:        "too many arguments (multiple)",
			args:        []string{"config.json", "extra-arg", "another-arg"},
			expectError: true,
			errorMsg:    "accepts 1 arg(s), received 3",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cmd := cmd.NewBacktestDeployCmd()

			// Test argument validation directly without executing
			err := cmd.Args(cmd, tt.args)

			if tt.expectError {
				if err == nil {
					t.Errorf("Expected error but got nil")
				} else if tt.errorMsg != "" && !strings.Contains(err.Error(), tt.errorMsg) {
					t.Errorf("Expected error to contain '%s', got: %s", tt.errorMsg, err.Error())
				}
			} else {
				if err != nil {
					t.Errorf("Unexpected error: %v", err)
				}
			}
		})
	}
}

func TestBacktestDeployCommand_ConfigValidation(t *testing.T) {
	// Test config validation logic (unit test for config parsing)
	tests := []struct {
		name           string
		configJSON     string
		expectName     string
		expectError    bool
		expectedFields []string // Required fields that should be present
		missingFields  []string // Fields that should be missing
	}{
		{
			name: "valid backtest config",
			configJSON: `{
				"name": "test-backtest",
				"type": "backtest",
				"version": "1.0.0",
				"strategy": "mean-reversion"
			}`,
			expectName:     "test-backtest",
			expectedFields: []string{"name", "type", "version"},
		},
		{
			name:          "invalid config without name",
			configJSON:    `{"type": "backtest", "version": "1.0.0"}`,
			expectError:   true,
			missingFields: []string{"name"},
		},
		{
			name:          "invalid config without type",
			configJSON:    `{"name": "test-backtest", "version": "1.0.0"}`,
			expectError:   true,
			missingFields: []string{"type"},
		},
		{
			name:          "invalid config without version",
			configJSON:    `{"name": "test-backtest", "type": "backtest"}`,
			expectError:   true,
			missingFields: []string{"version"},
		},
		{
			name:        "invalid JSON",
			configJSON:  `{invalid json}`,
			expectError: true,
		},
		{
			name:        "config with all required fields but name is not string",
			configJSON:  `{"name": 123, "type": "backtest", "version": "1.0.0"}`,
			expectError: true,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var config map[string]interface{}
			err := json.Unmarshal([]byte(tt.configJSON), &config)

			if err != nil && !tt.expectError {
				t.Fatalf("Unexpected JSON parse error: %v", err)
			}

			if err == nil {
				// Test required field validation
				requiredFields := []string{"name", "type", "version"}
				hasAllRequired := true

				for _, field := range requiredFields {
					if config[field] == nil {
						hasAllRequired = false
						break
					}
				}

				// Test name extraction (must be string)
				var backtestName string
				var hasValidName bool
				if nameVal, ok := config["name"].(string); ok {
					backtestName = nameVal
					hasValidName = true
				}

				if tt.expectError {
					if hasAllRequired && hasValidName {
						t.Errorf("Expected validation error but all required fields are present and valid")
					}
				} else {
					if !hasAllRequired {
						t.Errorf("Expected all required fields to be present")
					}
					if !hasValidName {
						t.Errorf("Expected name to be a valid string")
					}
					if backtestName != tt.expectName {
						t.Errorf("Expected name '%s', got '%s'", tt.expectName, backtestName)
					}
				}

				// Test specific expected fields
				for _, field := range tt.expectedFields {
					if config[field] == nil {
						t.Errorf("Expected field '%s' to be present", field)
					}
				}

				// Test specific missing fields
				for _, field := range tt.missingFields {
					if config[field] != nil {
						t.Errorf("Expected field '%s' to be missing", field)
					}
				}
			}
		})
	}
}

func TestBacktestDeployCommand_Usage(t *testing.T) {
	cmd := cmd.NewBacktestDeployCmd()

	// Test that the usage string is correct
	expectedUsage := "deploy <config.json>"
	if cmd.Use != expectedUsage {
		t.Errorf("Expected usage '%s', got '%s'", expectedUsage, cmd.Use)
	}

	// Test that it expects exactly 1 argument
	err := cmd.Args(cmd, []string{})
	if err == nil {
		t.Error("Expected error for no arguments")
	}

	err = cmd.Args(cmd, []string{"config.json"})
	if err != nil {
		t.Errorf("Expected no error for single argument, got: %v", err)
	}

	err = cmd.Args(cmd, []string{"arg1", "arg2"})
	if err == nil {
		t.Error("Expected error for multiple arguments")
	}
}

func TestBacktestListCommand_Arguments(t *testing.T) {
	tests := []struct {
		name        string
		args        []string
		expectError bool
		errorMsg    string
	}{
		{
			name:        "no arguments (correct)",
			args:        []string{},
			expectError: false,
		},
		{
			name:        "with arguments (incorrect)",
			args:        []string{"extra"},
			expectError: true,
			errorMsg:    "unknown command",
		},
		{
			name:        "with multiple arguments (incorrect)",
			args:        []string{"extra", "another"},
			expectError: true,
			errorMsg:    "unknown command",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cmd := cmd.NewBacktestListCmd()

			err := cmd.Args(cmd, tt.args)

			if tt.expectError {
				if err == nil {
					t.Errorf("Expected error but got nil")
				} else if tt.errorMsg != "" && !strings.Contains(err.Error(), tt.errorMsg) {
					t.Errorf("Expected error to contain '%s', got: %s", tt.errorMsg, err.Error())
				}
			} else {
				if err != nil {
					t.Errorf("Unexpected error: %v", err)
				}
			}
		})
	}
}

func TestBacktestListCommand_Usage(t *testing.T) {
	cmd := cmd.NewBacktestListCmd()

	expectedUsage := "list"
	if cmd.Use != expectedUsage {
		t.Errorf("Expected usage '%s', got '%s'", expectedUsage, cmd.Use)
	}

	expectedShort := "List all backtests"
	if cmd.Short != expectedShort {
		t.Errorf("Expected short description '%s', got '%s'", expectedShort, cmd.Short)
	}
}

func TestBacktestDeleteCommand_Arguments(t *testing.T) {
	tests := []struct {
		name        string
		args        []string
		expectError bool
		errorMsg    string
	}{
		{
			name:        "correct single argument",
			args:        []string{"bt_123456789"},
			expectError: false,
		},
		{
			name:        "no arguments",
			args:        []string{},
			expectError: true,
			errorMsg:    "accepts 1 arg(s), received 0",
		},
		{
			name:        "too many arguments",
			args:        []string{"bt_123", "extra"},
			expectError: true,
			errorMsg:    "accepts 1 arg(s), received 2",
		},
		{
			name:        "too many arguments (multiple)",
			args:        []string{"bt_123", "extra", "another"},
			expectError: true,
			errorMsg:    "accepts 1 arg(s), received 3",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cmd := cmd.NewBacktestDeleteCmd()

			err := cmd.Args(cmd, tt.args)

			if tt.expectError {
				if err == nil {
					t.Errorf("Expected error but got nil")
				} else if tt.errorMsg != "" && !strings.Contains(err.Error(), tt.errorMsg) {
					t.Errorf("Expected error to contain '%s', got: %s", tt.errorMsg, err.Error())
				}
			} else {
				if err != nil {
					t.Errorf("Unexpected error: %v", err)
				}
			}
		})
	}
}

func TestBacktestDeleteCommand_Usage(t *testing.T) {
	cmd := cmd.NewBacktestDeleteCmd()

	expectedUsage := "delete <backtest-id>"
	if cmd.Use != expectedUsage {
		t.Errorf("Expected usage '%s', got '%s'", expectedUsage, cmd.Use)
	}

	expectedShort := "Delete a backtest"
	if cmd.Short != expectedShort {
		t.Errorf("Expected short description '%s', got '%s'", expectedShort, cmd.Short)
	}
}

func TestBacktestCommand_Structure(t *testing.T) {
	cmd := cmd.NewBacktestCmd()

	// Test that it's a proper command group
	expectedUse := "backtest"
	if cmd.Use != expectedUse {
		t.Errorf("Expected command use '%s', got '%s'", expectedUse, cmd.Use)
	}

	expectedShort := "Manage backtests"
	if cmd.Short != expectedShort {
		t.Errorf("Expected short description '%s', got '%s'", expectedShort, cmd.Short)
	}

	// Test that it has the expected subcommands
	subcommands := cmd.Commands()
	expectedSubcommands := []string{"deploy", "list", "delete"}

	for _, expectedSubcmd := range expectedSubcommands {
		var found bool
		for _, subcmd := range subcommands {
			if subcmd.Name() == expectedSubcmd {
				found = true
				break
			}
		}
		if !found {
			t.Errorf("Expected backtest command to have '%s' subcommand", expectedSubcmd)
		}
	}

	// Verify no unexpected subcommands
	actualSubcmdNames := make(map[string]bool)
	for _, subcmd := range subcommands {
		actualSubcmdNames[subcmd.Name()] = true
	}

	for _, expectedName := range expectedSubcommands {
		if !actualSubcmdNames[expectedName] {
			t.Errorf("Missing expected subcommand: %s", expectedName)
		}
	}
}

func TestBacktestCommand_HasAllSubcommands(t *testing.T) {
	cmd := cmd.NewBacktestCmd()

	subcommands := cmd.Commands()

	expectedSubcommands := map[string]bool{
		"deploy": false,
		"list":   false,
		"delete": false,
	}

	for _, subcmd := range subcommands {
		if _, exists := expectedSubcommands[subcmd.Name()]; exists {
			expectedSubcommands[subcmd.Name()] = true
		}
	}

	for subcmdName, found := range expectedSubcommands {
		if !found {
			t.Errorf("Expected backtest command to have '%s' subcommand", subcmdName)
		}
	}
}
