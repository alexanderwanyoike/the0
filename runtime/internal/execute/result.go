package execute

import (
	"encoding/json"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
)

// BotResultPath is the result file of a bot run, /{CODE_MOUNT_DIR}/result.json
// with CODE_MOUNT_DIR as BuildBotEnv sets it.
func BotResultPath(codePath string) string {
	return filepath.Join("/", strings.TrimPrefix(codePath, "/"), "result.json")
}

// ClearBotResult removes the result file at path, if there is one.
func ClearBotResult(path string) error {
	if err := os.Remove(path); err != nil && !errors.Is(err, fs.ErrNotExist) {
		return err
	}
	return nil
}

// RunExitCode applies the bot contract's exit code rules (see "Exit codes" in
// docs/custom-bot-development/bot-contract.md) once the bot process has exited.
func RunExitCode(processExitCode int, resultPath string) int {
	if processExitCode == 0 && isErrorResult(resultPath) {
		return 1
	}
	return processExitCode
}

func isErrorResult(path string) bool {
	content, err := os.ReadFile(path)
	if err != nil {
		return false
	}
	var result map[string]any
	if json.Unmarshal(content, &result) != nil {
		return false
	}
	return result["status"] == "error"
}
