package execute

import (
	"bytes"
	"encoding/json"
	"errors"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"time"
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

// RunOutcome is how a bot run ended once its result file is taken into account.
type RunOutcome struct {
	ExitCode int
	// Failure is why the run failed, and is empty when it succeeded.
	Failure string
}

// SettleRun applies the bot contract's exit code rules (see "Exit codes" in
// docs/custom-bot-development/bot-contract.md) once the bot process has exited.
func SettleRun(processExitCode int, resultPath string) RunOutcome {
	message, isError := readErrorResult(resultPath)
	switch {
	case isError && processExitCode == 0:
		return RunOutcome{ExitCode: 1, Failure: message}
	case isError:
		return RunOutcome{ExitCode: processExitCode, Failure: message}
	case processExitCode != 0:
		return RunOutcome{ExitCode: processExitCode, Failure: fmt.Sprintf("Bot exited with code %d", processExitCode)}
	default:
		return RunOutcome{}
	}
}

func readErrorResult(path string) (message string, isError bool) {
	content, err := os.ReadFile(path)
	if err != nil {
		return "", false
	}
	var result map[string]any
	if json.Unmarshal(content, &result) != nil || result["status"] != "error" {
		return "", false
	}
	switch m := result["message"].(type) {
	case string:
		return m, true
	case nil:
		return "Bot reported an error", true
	default:
		text, _ := json.Marshal(m)
		return string(text), true
	}
}

// RunFailedLine is the log line the runner prints after a failed run.
func RunFailedLine(failure string, now time.Time) []byte {
	var line bytes.Buffer
	encoder := json.NewEncoder(&line)
	encoder.SetEscapeHTML(false)
	_ = encoder.Encode(struct {
		Level     string `json:"level"`
		Message   string `json:"message"`
		Timestamp string `json:"timestamp"`
	}{"error", "Run failed: " + failure, now.UTC().Format(time.RFC3339Nano)})
	return line.Bytes()
}
