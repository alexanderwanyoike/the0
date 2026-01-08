package execute

import (
	"os"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/constants"
)

func TestBuildBotCommand_Python311(t *testing.T) {
	cmd := BuildBotCommand("python3.11", "main.py", constants.TestBotDir)

	assert.Equal(t, "python3", cmd.Args[0])
	assert.Contains(t, cmd.Args, constants.TestPythonWrapperPath)
	assert.Equal(t, constants.TestBotDir, cmd.Dir)
}

func TestBuildBotCommand_NodeJS20(t *testing.T) {
	cmd := BuildBotCommand("nodejs20", "index.js", constants.TestBotDir)

	assert.Equal(t, "node", cmd.Args[0])
	assert.Contains(t, cmd.Args, constants.TestNodeWrapperPath)
	assert.Equal(t, constants.TestBotDir, cmd.Dir)
}

func TestBuildBotCommand_AllRuntimes(t *testing.T) {
	tests := []struct {
		name         string
		runtime      string
		entrypoint   string
		workDir      string
		expectedArgs []string
		expectedDir  string
	}{
		{
			name:         "python3.11",
			runtime:      "python3.11",
			entrypoint:   "main.py",
			workDir:      constants.TestBotDir,
			expectedArgs: []string{"python3", constants.TestPythonWrapperPath},
			expectedDir:  constants.TestBotDir,
		},
		{
			name:         "nodejs20",
			runtime:      "nodejs20",
			entrypoint:   "index.js",
			workDir:      constants.TestBotDir,
			expectedArgs: []string{"node", constants.TestNodeWrapperPath},
			expectedDir:  constants.TestBotDir,
		},
		{
			name:         "dotnet8 with dll",
			runtime:      "dotnet8",
			entrypoint:   "app.dll",
			workDir:      constants.TestBotDir,
			expectedArgs: []string{"dotnet", "app.dll"},
			expectedDir:  constants.TestBotDir,
		},
		{
			name:         "dotnet8 with project",
			runtime:      "dotnet8",
			entrypoint:   "app.csproj",
			workDir:      constants.TestBotDir,
			expectedArgs: []string{"dotnet", "run", "--project", "app.csproj"},
			expectedDir:  constants.TestBotDir,
		},
		{
			name:         "rust-stable",
			runtime:      "rust-stable",
			entrypoint:   "bot",
			workDir:      constants.TestBotDir,
			expectedArgs: []string{"/bot/bot"},
			expectedDir:  constants.TestBotDir,
		},
		{
			name:         "gcc13",
			runtime:      "gcc13",
			entrypoint:   "bot",
			workDir:      constants.TestBotDir,
			expectedArgs: []string{"/bot/bot"},
			expectedDir:  constants.TestBotDir,
		},
		{
			name:         "cpp-gcc13",
			runtime:      "cpp-gcc13",
			entrypoint:   "bot",
			workDir:      constants.TestBotDir,
			expectedArgs: []string{"/bot/bot"},
			expectedDir:  constants.TestBotDir,
		},
		{
			name:         "ghc96 (Haskell)",
			runtime:      "ghc96",
			entrypoint:   "bot",
			workDir:      constants.TestBotDir,
			expectedArgs: []string{"/bot/bot"},
			expectedDir:  constants.TestBotDir,
		},
		{
			name:         "scala3",
			runtime:      "scala3",
			entrypoint:   "app.jar",
			workDir:      constants.TestBotDir,
			expectedArgs: []string{"java", "-jar", "app.jar"},
			expectedDir:  constants.TestBotDir,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cmd := BuildBotCommand(tt.runtime, tt.entrypoint, tt.workDir)

			assert.Equal(t, tt.expectedArgs, cmd.Args, "Args mismatch")
			assert.Equal(t, tt.expectedDir, cmd.Dir, "Dir mismatch")
		})
	}
}

func TestBuildQueryCommand_Python(t *testing.T) {
	cmd := BuildQueryCommand("python3.11", "query.py", constants.TestBotDir)

	assert.Equal(t, "python3", cmd.Args[0])
	assert.Contains(t, cmd.Args, "query.py")
	assert.Equal(t, constants.TestBotDir, cmd.Dir)
}

func TestBuildQueryCommand_Node(t *testing.T) {
	cmd := BuildQueryCommand("nodejs20", "query.js", constants.TestBotDir)

	assert.Equal(t, "node", cmd.Args[0])
	assert.Contains(t, cmd.Args, "query.js")
	assert.Equal(t, constants.TestBotDir, cmd.Dir)
}

func TestBuildBotEnv_BasicFields(t *testing.T) {
	cfg := &Config{
		BotID:      "test-bot",
		BotConfig:  `{"key":"value"}`,
		CodePath:   constants.TestBotDir,
		StatePath:  "/state",
		Entrypoint: "main.py",
	}

	env := BuildBotEnv(cfg)

	// Check that required environment variables are set
	assert.Contains(t, env, "BOT_ID=test-bot")
	assert.Contains(t, env, "BOT_CONFIG={\"key\":\"value\"}")
	assert.Contains(t, env, "CODE_MOUNT_DIR=/bot")
	assert.Contains(t, env, "SCRIPT_PATH=main.py")
	assert.Contains(t, env, "ENTRYPOINT_TYPE=bot")
}

func TestBuildBotEnv_StateDirFromEnv(t *testing.T) {
	// Set STATE_DIR environment variable
	os.Setenv("STATE_DIR", "/custom/state")
	defer os.Unsetenv("STATE_DIR")

	cfg := &Config{
		BotID:     "test-bot",
		StatePath: "/state",
	}

	env := BuildBotEnv(cfg)

	// Should use the environment variable value
	assert.Contains(t, env, "STATE_DIR=/custom/state")
}

func TestBuildBotEnv_StateDirDefault(t *testing.T) {
	// Clear STATE_DIR if set
	os.Unsetenv("STATE_DIR")

	cfg := &Config{
		BotID:     "test-bot",
		StatePath: "/state",
	}

	env := BuildBotEnv(cfg)

	// Should default to StatePath/.the0-state
	assert.Contains(t, env, "STATE_DIR=/state/.the0-state")
}

func TestBuildBotEnv_PythonPath(t *testing.T) {
	cfg := &Config{
		BotID:    "test-bot",
		CodePath: constants.TestBotDir,
	}

	env := BuildBotEnv(cfg)

	// Check PYTHONPATH includes vendor directory
	foundPythonPath := false
	for _, e := range env {
		if strings.HasPrefix(e, "PYTHONPATH=") {
			assert.Contains(t, e, "/bot/vendor")
			assert.Contains(t, e, constants.TestBotDir)
			foundPythonPath = true
			break
		}
	}
	assert.True(t, foundPythonPath, "PYTHONPATH not found in environment")
}

func TestBuildBotEnv_QueryPath(t *testing.T) {
	cfg := &Config{
		BotID:     "test-bot",
		QueryPath: "/status",
	}

	env := BuildBotEnv(cfg)

	assert.Contains(t, env, "QUERY_PATH=/status")
}

func TestBuildBotEnv_QueryParams(t *testing.T) {
	cfg := &Config{
		BotID:       "test-bot",
		QueryPath:   "/data",
		QueryParams: `{"limit":10,"offset":20}`,
	}

	env := BuildBotEnv(cfg)

	assert.Contains(t, env, "QUERY_PARAMS={\"limit\":10,\"offset\":20}")
	// Check that individual query params are flattened
	assert.Contains(t, env, "QUERY_PARAM_limit=10")
	assert.Contains(t, env, "QUERY_PARAM_offset=20")
}

func TestBuildBotEnv_BotType(t *testing.T) {
	cfg := &Config{
		BotID:   "test-bot",
		BotType: "realtime",
	}

	env := BuildBotEnv(cfg)

	assert.Contains(t, env, "BOT_TYPE=realtime")
}

func TestParseQueryParams_ValidJSON(t *testing.T) {
	jsonStr := `{"limit":10,"offset":20,"filter":"active"}`

	params, err := ParseQueryParams(jsonStr)

	require.NoError(t, err)
	assert.Equal(t, float64(10), params["limit"]) // JSON numbers are float64
	assert.Equal(t, float64(20), params["offset"])
	assert.Equal(t, "active", params["filter"])
}

func TestParseQueryParams_EmptyString(t *testing.T) {
	params, err := ParseQueryParams("")

	require.NoError(t, err)
	assert.Empty(t, params)
}

func TestParseQueryParams_InvalidJSON(t *testing.T) {
	jsonStr := `{invalid json}`

	params, err := ParseQueryParams(jsonStr)

	assert.Error(t, err)
	assert.Nil(t, params)
	assert.Contains(t, err.Error(), "failed to parse query params")
}

func TestParseQueryParams_ComplexTypes(t *testing.T) {
	jsonStr := `{"nested":{"key":"value"},"array":[1,2,3]}`

	params, err := ParseQueryParams(jsonStr)

	require.NoError(t, err)
	assert.NotNil(t, params["nested"])
	assert.NotNil(t, params["array"])
}

func TestEnsureExecutable_CompiledRuntimes(t *testing.T) {
	compiledRuntimes := []string{"gcc13", "cpp-gcc13", "rust-stable", "ghc96"}

	for _, runtime := range compiledRuntimes {
		t.Run(runtime, func(t *testing.T) {
			// Create temp directory and file
			tmpDir := t.TempDir()
			entrypoint := "bot"
			filePath := tmpDir + "/" + entrypoint

			// Create file without execute permission
			err := os.WriteFile(filePath, []byte("#!/bin/bash\necho hello"), 0644)
			require.NoError(t, err)

			cfg := &Config{
				Runtime:    runtime,
				CodePath:   tmpDir,
				Entrypoint: entrypoint,
			}

			// Ensure executable
			err = EnsureExecutable(cfg)
			require.NoError(t, err)

			// Verify permissions
			info, err := os.Stat(filePath)
			require.NoError(t, err)
			assert.Equal(t, os.FileMode(0755), info.Mode().Perm(), "Expected 0755 permissions")
		})
	}
}

func TestEnsureExecutable_NonCompiledRuntimes(t *testing.T) {
	nonCompiledRuntimes := []string{"python3.11", "nodejs20", "dotnet8", "scala3"}

	for _, runtime := range nonCompiledRuntimes {
		t.Run(runtime, func(t *testing.T) {
			// Create temp directory and file
			tmpDir := t.TempDir()
			entrypoint := "main.py"
			filePath := tmpDir + "/" + entrypoint

			// Create file without execute permission
			err := os.WriteFile(filePath, []byte("print('hello')"), 0644)
			require.NoError(t, err)

			cfg := &Config{
				Runtime:    runtime,
				CodePath:   tmpDir,
				Entrypoint: entrypoint,
			}

			// Ensure executable should do nothing
			err = EnsureExecutable(cfg)
			require.NoError(t, err)

			// Verify permissions unchanged
			info, err := os.Stat(filePath)
			require.NoError(t, err)
			assert.Equal(t, os.FileMode(0644), info.Mode().Perm(), "Permissions should not change for non-compiled runtimes")
		})
	}
}

func TestEnsureExecutable_WithQueryEntrypoint(t *testing.T) {
	tmpDir := t.TempDir()
	entrypoint := "bot"
	queryEntrypoint := "query_bot"

	// Create both files
	err := os.WriteFile(tmpDir+"/"+entrypoint, []byte("#!/bin/bash"), 0644)
	require.NoError(t, err)
	err = os.WriteFile(tmpDir+"/"+queryEntrypoint, []byte("#!/bin/bash"), 0644)
	require.NoError(t, err)

	cfg := &Config{
		Runtime:         "gcc13",
		CodePath:        tmpDir,
		Entrypoint:      entrypoint,
		QueryEntrypoint: queryEntrypoint,
	}

	err = EnsureExecutable(cfg)
	require.NoError(t, err)

	// Verify both files are executable
	info, err := os.Stat(tmpDir + "/" + entrypoint)
	require.NoError(t, err)
	assert.Equal(t, os.FileMode(0755), info.Mode().Perm())

	info, err = os.Stat(tmpDir + "/" + queryEntrypoint)
	require.NoError(t, err)
	assert.Equal(t, os.FileMode(0755), info.Mode().Perm())
}

func TestEnsureExecutable_MissingFile(t *testing.T) {
	tmpDir := t.TempDir()

	cfg := &Config{
		Runtime:    "gcc13",
		CodePath:   tmpDir,
		Entrypoint: "nonexistent",
	}

	err := EnsureExecutable(cfg)
	assert.Error(t, err)
	assert.Contains(t, err.Error(), "failed to chmod entrypoint")
}

func TestEnsureExecutable_MissingQueryEntrypoint(t *testing.T) {
	tmpDir := t.TempDir()
	entrypoint := "bot"

	// Create main entrypoint only
	err := os.WriteFile(tmpDir+"/"+entrypoint, []byte("#!/bin/bash"), 0644)
	require.NoError(t, err)

	cfg := &Config{
		Runtime:         "gcc13",
		CodePath:        tmpDir,
		Entrypoint:      entrypoint,
		QueryEntrypoint: "nonexistent_query",
	}

	// Should not fail even if query entrypoint doesn't exist
	err = EnsureExecutable(cfg)
	require.NoError(t, err)

	// Main entrypoint should still be executable
	info, err := os.Stat(tmpDir + "/" + entrypoint)
	require.NoError(t, err)
	assert.Equal(t, os.FileMode(0755), info.Mode().Perm())
}
