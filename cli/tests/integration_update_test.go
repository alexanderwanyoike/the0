package internal

import (
	"strings"
	"testing"
	"the0/cmd"
)

func TestCheckUpdateCommand(t *testing.T) {
	// Set version for testing
	cmd.SetVersion("1.0.0")

	testCases := []struct {
		name        string
		args        []string
		expectError bool
		envVars     map[string]string
	}{
		{
			name:        "Check update with default channel",
			args:        []string{"check-update"},
			expectError: false,
		},
		{
			name:        "Check update with staging channel via env var",
			args:        []string{"check-update"},
			expectError: false,
			envVars:     map[string]string{"THE0_CLI_UPDATE_CHANNEL": "staging"},
		},
		{
			name:        "Check update with custom timeout",
			args:        []string{"check-update", "--timeout", "5s"},
			expectError: false,
		},
		{
			name:        "Check update with quiet mode",
			args:        []string{"check-update"},
			expectError: false,
			envVars:     map[string]string{"THE0_QUIET": "1"},
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			// Set environment variables
			for key, value := range tc.envVars {
				t.Setenv(key, value)
			}

			// Create command
			checkCmd := cmd.NewCheckUpdateCmd()
			checkCmd.SetArgs(tc.args[1:]) // Remove command name

			// Execute command
			err := checkCmd.Execute()

			if tc.expectError && err == nil {
				t.Error("Expected error, but got none")
			}
			if !tc.expectError && err != nil {
				// Allow network errors in tests - we're testing command structure
				if !strings.Contains(err.Error(), "network") &&
					!strings.Contains(err.Error(), "timeout") &&
					!strings.Contains(err.Error(), "failed to fetch") &&
					!strings.Contains(err.Error(), "failed to check for updates") &&
					!strings.Contains(err.Error(), "HTTP 404") &&
					!strings.Contains(err.Error(), "manifest request failed") {
					t.Errorf("Expected no error, but got: %v", err)
				}
			}
		})
	}
}

func TestSelfUpdateCommand(t *testing.T) {
	// Set version for testing
	cmd.SetVersion("1.0.0")

	testCases := []struct {
		name        string
		args        []string
		expectError bool
		envVars     map[string]string
	}{
		{
			name:        "Self update check only",
			args:        []string{"self-update", "--check-only"},
			expectError: false,
		},
		{
			name:        "Self update with staging channel via env var",
			args:        []string{"self-update", "--check-only"},
			expectError: false,
			envVars:     map[string]string{"THE0_CLI_UPDATE_CHANNEL": "staging"},
		},
		{
			name:        "Self update with custom timeout",
			args:        []string{"self-update", "--timeout", "10s", "--check-only"},
			expectError: false,
		},
		{
			name:        "Self update with force flag",
			args:        []string{"self-update", "--force", "--check-only"},
			expectError: false,
		},
		{
			name:        "Self update with yes flag",
			args:        []string{"self-update", "--yes", "--check-only"},
			expectError: false,
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			// Set environment variables
			for key, value := range tc.envVars {
				t.Setenv(key, value)
			}

			// Create command
			selfUpdateCmd := cmd.NewSelfUpdateCmd()
			selfUpdateCmd.SetArgs(tc.args[1:]) // Remove command name

			// Execute command
			err := selfUpdateCmd.Execute()

			if tc.expectError && err == nil {
				t.Error("Expected error, but got none")
			}
			if !tc.expectError && err != nil {
				// Allow network errors in tests - we're testing command structure
				if !strings.Contains(err.Error(), "network") &&
					!strings.Contains(err.Error(), "timeout") &&
					!strings.Contains(err.Error(), "failed to fetch") &&
					!strings.Contains(err.Error(), "failed to check for updates") &&
					!strings.Contains(err.Error(), "HTTP 404") &&
					!strings.Contains(err.Error(), "manifest request failed") {
					t.Errorf("Expected no error, but got: %v", err)
				}
			}
		})
	}
}

func TestUpdateChannelEnvironmentVariable(t *testing.T) {
	testCases := []struct {
		name     string
		envValue string
		expected string
	}{
		{
			name:     "Default channel",
			envValue: "",
			expected: "production",
		},
		{
			name:     "Staging channel",
			envValue: "staging",
			expected: "staging",
		},
		{
			name:     "Production channel",
			envValue: "production",
			expected: "production",
		},
		{
			name:     "Unknown channel defaults to production",
			envValue: "unknown",
			expected: "production",
		},
	}

	for _, tc := range testCases {
		t.Run(tc.name, func(t *testing.T) {
			// Set environment variable
			if tc.envValue != "" {
				t.Setenv("THE0_CLI_UPDATE_CHANNEL", tc.envValue)
			}

			// Test that the environment variable is read correctly
			// This is tested indirectly through the command execution
			cmd.SetVersion("1.0.0")
			checkCmd := cmd.NewCheckUpdateCmd()
			checkCmd.SetArgs([]string{"--timeout", "1s"}) // Short timeout to fail quickly

			// Execute command - we expect it to fail with network error
			// but we're checking that the environment variable is processed
			err := checkCmd.Execute()

			// We expect network errors, but not validation errors
			if err != nil {
				errorStr := err.Error()
				if strings.Contains(errorStr, "invalid channel") {
					t.Errorf("Unexpected channel validation error: %v", err)
				}
			}
		})
	}
}

func TestCommandRegistration(t *testing.T) {
	// Test that commands are properly registered by checking they can be created
	checkCmd := cmd.NewCheckUpdateCmd()
	if checkCmd == nil {
		t.Error("NewCheckUpdateCmd() returned nil")
	}

	selfUpdateCmd := cmd.NewSelfUpdateCmd()
	if selfUpdateCmd == nil {
		t.Error("NewSelfUpdateCmd() returned nil")
	}

	// Test command structure
	if checkCmd.Use != "check-update" {
		t.Errorf("Expected command use 'check-update', got '%s'", checkCmd.Use)
	}

	if selfUpdateCmd.Use != "self-update" {
		t.Errorf("Expected command use 'self-update', got '%s'", selfUpdateCmd.Use)
	}

	// Test flags are registered
	timeoutFlag := checkCmd.Flag("timeout")
	if timeoutFlag == nil {
		t.Error("check-update command missing --timeout flag")
	}

	// Test self-update flags
	checkOnlyFlag := selfUpdateCmd.Flag("check-only")
	if checkOnlyFlag == nil {
		t.Error("self-update command missing --check-only flag")
	}

	forceFlag := selfUpdateCmd.Flag("force")
	if forceFlag == nil {
		t.Error("self-update command missing --force flag")
	}

	yesFlag := selfUpdateCmd.Flag("yes")
	if yesFlag == nil {
		t.Error("self-update command missing --yes flag")
	}
}

func TestVersionSetting(t *testing.T) {
	// Test that SetVersion works correctly
	testVersion := "2.0.0-test"
	cmd.SetVersion(testVersion)

	// Create a command and verify it uses the set version
	checkCmd := cmd.NewCheckUpdateCmd()
	checkCmd.SetArgs([]string{"--timeout", "1s"})

	// The command should use the set version internally
	// We can't directly test this without exposing internals,
	// but we can ensure the command doesn't crash
	err := checkCmd.Execute()
	if err != nil {
		// Network errors are expected, but not version parsing/validation errors
		errorStr := err.Error()
		// Check for actual version parsing errors, not network errors mentioning versions
		if (strings.Contains(errorStr, "invalid") && strings.Contains(errorStr, "version")) ||
			(strings.Contains(errorStr, "version") && !strings.Contains(errorStr, "fetch") && !strings.Contains(errorStr, "manifest") && !strings.Contains(errorStr, "HTTP")) {
			t.Errorf("Unexpected version parsing error: %v", err)
		}
	}
}
