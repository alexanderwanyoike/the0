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

func TestBotDeleteCommand_Arguments(t *testing.T) {
	tests := []struct {
		name        string
		args        []string
		expectError bool
		errorMsg    string
	}{
		{
			name:        "correct single argument",
			args:        []string{"bot_123"},
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
			args:        []string{"bot_123", "extra"},
			expectError: true,
			errorMsg:    "accepts 1 arg(s), received 2",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cmd := cmd.NewBotDeleteCmd()

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

func TestBotDeleteCommand_Usage(t *testing.T) {
	cmd := cmd.NewBotDeleteCmd()

	expectedUsage := "delete <bot_id>"
	if cmd.Use != expectedUsage {
		t.Errorf("Expected usage '%s', got '%s'", expectedUsage, cmd.Use)
	}

	expectedShort := "Delete a bot instance"
	if cmd.Short != expectedShort {
		t.Errorf("Expected short description '%s', got '%s'", expectedShort, cmd.Short)
	}
}

func TestBotDeleteCommand_YesFlag(t *testing.T) {
	cmd := cmd.NewBotDeleteCmd()

	// Test that yes flag exists
	yesFlag := cmd.Flag("yes")
	if yesFlag == nil {
		t.Error("Expected command to have 'yes' flag")
	} else {
		if yesFlag.Shorthand != "y" {
			t.Errorf("Expected yes flag shorthand 'y', got '%s'", yesFlag.Shorthand)
		}
		if yesFlag.DefValue != "false" {
			t.Errorf("Expected yes flag default value 'false', got '%s'", yesFlag.DefValue)
		}
	}
}

func TestBotCommand_HasDeleteSubcommand(t *testing.T) {
	cmd := cmd.NewBotCmd()

	subcommands := cmd.Commands()

	var hasDelete bool
	for _, subcmd := range subcommands {
		if subcmd.Name() == "delete" {
			hasDelete = true
		}
	}

	if !hasDelete {
		t.Error("Expected bot command to have 'delete' subcommand")
	}
}

func TestBotQueryCommand_Arguments(t *testing.T) {
	tests := []struct {
		name        string
		args        []string
		expectError bool
		errorMsg    string
	}{
		{
			name:        "correct two arguments",
			args:        []string{"bot-123", "/portfolio"},
			expectError: false,
		},
		{
			name:        "no arguments",
			args:        []string{},
			expectError: true,
			errorMsg:    "accepts 2 arg(s), received 0",
		},
		{
			name:        "only one argument",
			args:        []string{"bot-123"},
			expectError: true,
			errorMsg:    "accepts 2 arg(s), received 1",
		},
		{
			name:        "too many arguments",
			args:        []string{"bot-123", "/portfolio", "extra"},
			expectError: true,
			errorMsg:    "accepts 2 arg(s), received 3",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cmd := cmd.NewBotQueryCmd()

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

func TestBotQueryCommand_Usage(t *testing.T) {
	cmd := cmd.NewBotQueryCmd()

	expectedUsage := "query <bot_id> <query_path>"
	if cmd.Use != expectedUsage {
		t.Errorf("Expected usage '%s', got '%s'", expectedUsage, cmd.Use)
	}

	expectedShort := "Execute a query against a bot"
	if cmd.Short != expectedShort {
		t.Errorf("Expected short description '%s', got '%s'", expectedShort, cmd.Short)
	}
}

func TestBotQueryCommand_Flags(t *testing.T) {
	cmd := cmd.NewBotQueryCmd()

	// Test that params flag exists
	paramsFlag := cmd.Flag("params")
	if paramsFlag == nil {
		t.Error("Expected command to have 'params' flag")
	} else {
		if paramsFlag.Shorthand != "p" {
			t.Errorf("Expected params flag shorthand 'p', got '%s'", paramsFlag.Shorthand)
		}
		if paramsFlag.DefValue != "{}" {
			t.Errorf("Expected params flag default value '{}', got '%s'", paramsFlag.DefValue)
		}
	}

	// Test that timeout flag exists
	timeoutFlag := cmd.Flag("timeout")
	if timeoutFlag == nil {
		t.Error("Expected command to have 'timeout' flag")
	} else {
		if timeoutFlag.Shorthand != "t" {
			t.Errorf("Expected timeout flag shorthand 't', got '%s'", timeoutFlag.Shorthand)
		}
		if timeoutFlag.DefValue != "30" {
			t.Errorf("Expected timeout flag default value '30', got '%s'", timeoutFlag.DefValue)
		}
	}

	// Test that raw flag exists
	rawFlag := cmd.Flag("raw")
	if rawFlag == nil {
		t.Error("Expected command to have 'raw' flag")
	} else {
		if rawFlag.Shorthand != "r" {
			t.Errorf("Expected raw flag shorthand 'r', got '%s'", rawFlag.Shorthand)
		}
		if rawFlag.DefValue != "false" {
			t.Errorf("Expected raw flag default value 'false', got '%s'", rawFlag.DefValue)
		}
	}
}

func TestBotCommand_HasQuerySubcommand(t *testing.T) {
	cmd := cmd.NewBotCmd()

	subcommands := cmd.Commands()

	var hasQuery bool
	for _, subcmd := range subcommands {
		if subcmd.Name() == "query" {
			hasQuery = true
		}
	}

	if !hasQuery {
		t.Error("Expected bot command to have 'query' subcommand")
	}
}
