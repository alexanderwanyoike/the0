package execute

import (
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
)

// BuildBotCommand creates an exec.Cmd for the given runtime and entrypoint.
// For Python and Node.js, uses wrapper scripts that handle signal management,
// config parsing, and result writing.
func BuildBotCommand(runtime, entrypoint, workDir string) *exec.Cmd {
	var cmd *exec.Cmd

	switch runtime {
	case "python3.11":
		// Use the Python wrapper script which handles:
		// - Signal management (SIGTERM/SIGINT)
		// - Config parsing from BOT_CONFIG env var
		// - Result file writing
		// - Python path setup
		// The wrapper reads SCRIPT_PATH env var for the actual entrypoint
		cmd = exec.Command("python3", "/app/wrappers/python_bot.py")
	case "nodejs20":
		// Use the Node.js wrapper script which handles:
		// - AbortController for cancellation
		// - Signal handling
		// - Config parsing
		// - Result file writing
		// The wrapper reads SCRIPT_PATH env var for the actual entrypoint
		cmd = exec.Command("node", "/app/wrappers/node_bot.js")
	case "dotnet8":
		// For .NET, entrypoint is either a .dll or project path
		if filepath.Ext(entrypoint) == ".dll" {
			cmd = exec.Command("dotnet", entrypoint)
		} else {
			cmd = exec.Command("dotnet", "run", "--project", entrypoint)
		}
	case "rust-stable", "gcc13", "cpp-gcc13":
		// Compiled binaries - make sure it's executable
		binPath := filepath.Join(workDir, entrypoint)
		cmd = exec.Command(binPath)
	case "ghc96":
		// Haskell compiled binary
		binPath := filepath.Join(workDir, entrypoint)
		cmd = exec.Command(binPath)
	case "scala3":
		// Scala runs as JAR with Java
		// Support jar:MainClass syntax for specifying alternate main class
		if idx := strings.LastIndex(entrypoint, ":"); idx > 0 && !strings.HasPrefix(entrypoint[idx:], ":\\") {
			// Has main class override (e.g., "app.jar:com.example.Main")
			jarPath := entrypoint[:idx]
			mainClass := entrypoint[idx+1:]
			cmd = exec.Command("java", "-cp", jarPath, mainClass)
		} else {
			cmd = exec.Command("java", "-jar", entrypoint)
		}
	default:
		// Default: try to run as executable
		cmd = exec.Command(entrypoint)
	}

	cmd.Dir = workDir
	return cmd
}

// BuildQueryCommand creates an exec.Cmd for query execution.
// Queries run directly without wrappers - the SDK handles everything.
func BuildQueryCommand(runtime, entrypoint, workDir string) *exec.Cmd {
	var cmd *exec.Cmd

	switch runtime {
	case "python3.11":
		// Run query script directly - it uses the SDK's query.run()
		cmd = exec.Command("python3", entrypoint)
	case "nodejs20":
		// Run query script directly
		cmd = exec.Command("node", entrypoint)
	case "dotnet8":
		// For .NET, entrypoint is either a .dll or project path
		if filepath.Ext(entrypoint) == ".dll" {
			cmd = exec.Command("dotnet", entrypoint)
		} else {
			cmd = exec.Command("dotnet", "run", "--project", entrypoint)
		}
	case "rust-stable", "gcc13", "cpp-gcc13", "ghc96":
		// Compiled binaries
		binPath := filepath.Join(workDir, entrypoint)
		cmd = exec.Command(binPath)
	case "scala3":
		// Scala runs as JAR with Java
		// Support jar:MainClass syntax for specifying alternate main class
		if idx := strings.LastIndex(entrypoint, ":"); idx > 0 && !strings.HasPrefix(entrypoint[idx:], ":\\") {
			// Has main class override (e.g., "app.jar:com.example.Main")
			jarPath := entrypoint[:idx]
			mainClass := entrypoint[idx+1:]
			cmd = exec.Command("java", "-cp", jarPath, mainClass)
		} else {
			cmd = exec.Command("java", "-jar", entrypoint)
		}
	default:
		// Default: try to run as executable
		cmd = exec.Command(entrypoint)
	}

	cmd.Dir = workDir
	return cmd
}

// BuildBotEnv creates the environment for bot execution.
func BuildBotEnv(cfg *Config) []string {
	env := os.Environ()

	// Determine STATE_DIR: use existing env var if set, otherwise derive from StatePath
	stateDir := os.Getenv("STATE_DIR")
	if stateDir == "" {
		// Default to .the0-state subdirectory within StatePath
		stateDir = filepath.Join(cfg.StatePath, ".the0-state")
	}

	codeMountDir := strings.TrimPrefix(cfg.CodePath, "/")
	env = append(env,
		"BOT_ID="+cfg.BotID,
		"BOT_CONFIG="+cfg.BotConfig,
		"STATE_DIR="+stateDir,
		"CODE_MOUNT_DIR="+codeMountDir,
		"SCRIPT_PATH="+cfg.Entrypoint, // Used by Python/Node.js wrappers
		"ENTRYPOINT_TYPE=bot",         // Used by Node.js wrapper
	)

	// Add query-specific env vars if applicable
	if cfg.QueryPath != "" {
		env = append(env, "QUERY_PATH="+cfg.QueryPath)
		// Parse and add query params as individual env vars or as JSON
		if cfg.QueryParams != "" {
			env = append(env, "QUERY_PARAMS="+cfg.QueryParams)
			// Also try to parse and flatten query params
			params, err := ParseQueryParams(cfg.QueryParams)
			if err == nil {
				for k, v := range params {
					env = append(env, fmt.Sprintf("QUERY_PARAM_%s=%v", k, v))
				}
			}
		}
	}

	if cfg.BotType != "" {
		env = append(env, "BOT_TYPE="+cfg.BotType)
	}

	return env
}

// ParseQueryParams parses a JSON string into a map.
// Returns an error if the JSON is invalid.
func ParseQueryParams(queryParams string) (map[string]interface{}, error) {
	if queryParams == "" {
		return make(map[string]interface{}), nil
	}

	var params map[string]interface{}
	if err := json.Unmarshal([]byte(queryParams), &params); err != nil {
		return nil, fmt.Errorf("failed to parse query params: %w", err)
	}

	return params, nil
}

// EnsureExecutable sets execute permissions on binary files for compiled runtimes.
// ZIP files often don't preserve Unix execute permissions, so we need to set them manually.
func EnsureExecutable(cfg *Config) error {
	// Only needed for compiled runtimes
	compiledRuntimes := map[string]bool{
		"gcc13":       true,
		"cpp-gcc13":   true,
		"rust-stable": true,
		"ghc96":       true,
	}

	if !compiledRuntimes[cfg.Runtime] {
		return nil
	}

	// Make the main entrypoint executable
	entrypointPath := filepath.Join(cfg.CodePath, cfg.Entrypoint)
	if err := os.Chmod(entrypointPath, 0755); err != nil {
		return fmt.Errorf("failed to chmod entrypoint %s: %w", entrypointPath, err)
	}

	// Also make query entrypoint executable if present
	if cfg.QueryEntrypoint != "" {
		queryPath := filepath.Join(cfg.CodePath, cfg.QueryEntrypoint)
		if err := os.Chmod(queryPath, 0755); err != nil {
			// Query entrypoint may not exist, log but don't fail
			return nil
		}
	}

	return nil
}
