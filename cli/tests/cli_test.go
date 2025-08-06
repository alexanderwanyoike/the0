package internal

import (
	"encoding/json"
	"strings"
	"testing"
	"the0/cmd"
)

func TestBotDeployCommand_Arguments(t *testing.T) {
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
			name:        "too many arguments (old format)",
			args:        []string{"bot-name", "config.json"},
			expectError: true,
			errorMsg:    "accepts 1 arg(s), received 2",
		},
		{
			name:        "too many arguments",
			args:        []string{"config.json", "extra-arg", "another-arg"},
			expectError: true,
			errorMsg:    "accepts 1 arg(s), received 3",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cmd := cmd.NewBotDeployCmd()

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

func TestBotDeployCommand_ConfigNameExtraction(t *testing.T) {
	// Test config name extraction logic (unit test for config parsing)
	tests := []struct {
		name        string
		configJSON  string
		expectName  string
		expectError bool
		errorMsg    string
	}{
		{
			name:       "valid config with name",
			configJSON: `{"name": "test-bot", "type": "trading", "version": "1.0.0"}`,
			expectName: "test-bot",
		},
		{
			name:        "invalid config without name",
			configJSON:  `{"type": "trading", "version": "1.0.0"}`,
			expectError: true,
			errorMsg:    "name not found",
		},
		{
			name:        "invalid JSON",
			configJSON:  `{invalid json}`,
			expectError: true,
		},
		{
			name:        "name is not a string",
			configJSON:  `{"name": 123, "type": "trading"}`,
			expectError: true,
			errorMsg:    "name not found",
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
				// Test name extraction logic
				var botName string
				nameVal, ok := config["name"].(string)
				if ok {
					botName = nameVal
				}

				if tt.expectError {
					if botName != "" {
						t.Errorf("Expected error but got name: %s", botName)
					}
				} else {
					if botName != tt.expectName {
						t.Errorf("Expected name '%s', got '%s'", tt.expectName, botName)
					}
				}
			}
		})
	}
}

func TestBotDeployCommand_Usage(t *testing.T) {
	cmd := cmd.NewBotDeployCmd()

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

func TestCustomBotVersionsCommand_Arguments(t *testing.T) {
	tests := []struct {
		name        string
		args        []string
		expectError bool
		errorMsg    string
	}{
		{
			name:        "correct single argument",
			args:        []string{"trading/my-bot"},
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
			args:        []string{"trading/my-bot", "extra"},
			expectError: true,
			errorMsg:    "accepts 1 arg(s), received 2",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cmd := cmd.NewCustomBotVersionsCmd()

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

func TestCustomBotVersionsCommand_Usage(t *testing.T) {
	cmd := cmd.NewCustomBotVersionsCmd()

	expectedUsage := "versions <type|name>"
	if cmd.Use != expectedUsage {
		t.Errorf("Expected usage '%s', got '%s'", expectedUsage, cmd.Use)
	}

	expectedShort := "List all versions of a custom bot"
	if cmd.Short != expectedShort {
		t.Errorf("Expected short description '%s', got '%s'", expectedShort, cmd.Short)
	}
}

func TestCustomBotCommand_HasVersionsSubcommand(t *testing.T) {
	cmd := cmd.NewCustomBotCmd()

	subcommands := cmd.Commands()

	var hasVersions bool
	for _, subcmd := range subcommands {
		if subcmd.Name() == "versions" {
			hasVersions = true
		}
	}

	if !hasVersions {
		t.Error("Expected custom-bot command to have 'versions' subcommand")
	}
}

func TestBotLogsCommand_Arguments(t *testing.T) {
	tests := []struct {
		name        string
		args        []string
		expectError bool
		errorMsg    string
	}{
		{
			name:        "correct single argument",
			args:        []string{"bot-123"},
			expectError: false,
		},
		{
			name:        "correct two arguments",
			args:        []string{"bot-123", "20241201"},
			expectError: false,
		},
		{
			name:        "no arguments",
			args:        []string{},
			expectError: true,
			errorMsg:    "accepts between 1 and 2 arg(s), received 0",
		},
		{
			name:        "too many arguments",
			args:        []string{"bot-123", "20241201", "extra"},
			expectError: true,
			errorMsg:    "accepts between 1 and 2 arg(s), received 3",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cmd := cmd.NewBotLogsCmd()

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

func TestBotLogsCommand_Usage(t *testing.T) {
	cmd := cmd.NewBotLogsCmd()

	expectedUsage := "logs <bot_id> [date|dateRange]"
	if cmd.Use != expectedUsage {
		t.Errorf("Expected usage '%s', got '%s'", expectedUsage, cmd.Use)
	}

	expectedShort := "View bot logs"
	if cmd.Short != expectedShort {
		t.Errorf("Expected short description '%s', got '%s'", expectedShort, cmd.Short)
	}
}

func TestBotLogsCommand_Flags(t *testing.T) {
	cmd := cmd.NewBotLogsCmd()

	// Test that watch flag exists
	watchFlag := cmd.Flag("watch")
	if watchFlag == nil {
		t.Error("Expected command to have 'watch' flag")
	} else {
		if watchFlag.Shorthand != "w" {
			t.Errorf("Expected watch flag shorthand 'w', got '%s'", watchFlag.Shorthand)
		}
	}

	// Test that limit flag exists
	limitFlag := cmd.Flag("limit")
	if limitFlag == nil {
		t.Error("Expected command to have 'limit' flag")
	} else {
		if limitFlag.DefValue != "100" {
			t.Errorf("Expected limit flag default value '100', got '%s'", limitFlag.DefValue)
		}
	}
}

func TestBotCommand_HasLogsSubcommand(t *testing.T) {
	cmd := cmd.NewBotCmd()

	subcommands := cmd.Commands()

	var hasLogs bool
	for _, subcmd := range subcommands {
		if subcmd.Name() == "logs" {
			hasLogs = true
		}
	}

	if !hasLogs {
		t.Error("Expected bot command to have 'logs' subcommand")
	}
}
