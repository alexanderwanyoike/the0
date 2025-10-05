/*
Script management for Docker containers
key responsibilites include:
- Factory orchestration (Manages the CodeEntrypointFactory and BashEntrypointFactory)
- Configuration management (Handles settings related to script execution)
- Filesystem Interaction: Handles writing the final entrypoint script
- Abstraction with a high-level interface (ScriptManager) for script generation
*/

package dockerrunner

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"runtime/internal/docker-runner/entrypoints"
	"runtime/internal/model"
	"runtime/internal/util"
)

type ScriptManager interface {
	Create(ctx context.Context, executable model.Executable, botDir string) (string, error)
}

type defaultScriptManager struct {
	logger util.Logger
}

func NewScriptManager(logger util.Logger) ScriptManager {
	return &defaultScriptManager{
		logger: logger,
	}
}

func (m *defaultScriptManager) Create(
	ctx context.Context,
	executable model.Executable,
	botDir string,
) (string, error) {
	// Add logging for debugging
	m.logger.Info("Script Manager: Creating entrypoint script", "bot_id", executable.ID)
	m.logger.Info("Script Manager: Entrypoint type", "entrypoint", executable.Entrypoint)
	m.logger.Info("Script Manager: Available EntrypointFiles", "files", executable.EntrypointFiles)

	// Get the script path from entrypoints
	scriptPath, exists := executable.EntrypointFiles[executable.Entrypoint]
	if !exists {
		m.logger.Error("Script Manager: Entrypoint not found", "entrypoint", executable.Entrypoint, "available", executable.EntrypointFiles)
		return "", fmt.Errorf("entrypoint '%s' not found in executable configuration", executable.Entrypoint)
	}

	m.logger.Info("Script Manager: Found script path", "script_path", scriptPath, "entrypoint", executable.Entrypoint)

	runtime := executable.Runtime
	var entrypointScript string

	// Convert config to JSON for passing to the script
	configJSON, err := json.Marshal(executable.Config)
	if err != nil {
		return "", fmt.Errorf("failed to marshal bot config: %v", err)
	}

	codeFactory := entrypoints.NewCodeEntrypointFactory(
		executable.Entrypoint,
		runtime,
	)
	entrypointScript, err = codeFactory.GetCode()
	if err != nil {
		return "", fmt.Errorf("failed to get entrypoint code: %v", err)
	}

	bashFactory := entrypoints.NewBashEntrypointFactory(
		executable.Entrypoint,
		entrypointScript,
		executable.ID,
		string(configJSON),
		scriptPath,
	)

	entrypointScript, err = bashFactory.BuildBashEntrypoint(runtime)
	if err != nil {
		return "", fmt.Errorf("failed to build bash entrypoint: %v", err)
	}

	// Ensure botDir exists before writing script (fixes race condition with cleanup)
	if _, err := os.Stat(botDir); os.IsNotExist(err) {
		if err := os.MkdirAll(botDir, 0755); err != nil {
			return "", fmt.Errorf("failed to create bot directory: %v", err)
		}
	}

	// Write entrypoint script to file
	scriptFile := filepath.Join(botDir, "entrypoint.sh")
	m.logger.Info("Script Manager: Writing entrypoint script", "script_file", scriptFile, "bot_dir", botDir)
	if err := os.WriteFile(scriptFile, []byte(entrypointScript), 0755); err != nil {
		m.logger.Error("Script Manager: Failed to write entrypoint script", "error", err, "script_file", scriptFile)
		return "", fmt.Errorf("failed to write entrypoint script: %v", err)
	}
	m.logger.Info("Script Manager: Successfully wrote entrypoint script", "script_file", scriptFile, "size", len(entrypointScript))

	return scriptFile, nil
}
