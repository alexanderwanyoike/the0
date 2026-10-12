package main

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/launch"
	"runtime/internal/util"
)

// compiledBot lays out a bot the way a compiled runtime (Rust, C++, Haskell)
// runs one: an executable entrypoint in the code directory with no wrapper
// around it. script is the shell the entrypoint runs.
func compiledBot(t *testing.T, script string) *launch.Settings {
	t.Helper()
	root := t.TempDir()
	cfg := &launch.Settings{
		BotID:      "compiled-test-bot",
		Runtime:    "rust-stable",
		Entrypoint: "bot",
		BotConfig:  "{}",
		CodePath:   filepath.Join(root, "bot"),
		StatePath:  filepath.Join(root, "state"),
		LogsPath:   filepath.Join(root, "logs"),
	}
	require.NoError(t, os.MkdirAll(cfg.CodePath, 0755))
	require.NoError(t, os.WriteFile(filepath.Join(cfg.CodePath, cfg.Entrypoint), []byte("#!/bin/sh\n"+script), 0755))
	return cfg
}

// writesResult is the shell for a bot that writes result as its result.json,
// at the path the SDKs build from CODE_MOUNT_DIR.
func writesResult(result string) string {
	return fmt.Sprintf("printf '%%s' '%s' > \"/$CODE_MOUNT_DIR/result.json\"\n", result)
}

func exits(code int) string {
	return fmt.Sprintf("exit %d\n", code)
}

func runCompiledBot(t *testing.T, cfg *launch.Settings) int {
	t.Helper()
	return executeBotProcess(context.Background(), cfg, &util.DefaultLogger{})
}

// botLogLines is every line of the run's bot.log, the file the sync runner
// uploads and publishes as the bot's logs, each parsed as JSON when it is.
func botLogLines(t *testing.T, cfg *launch.Settings) []map[string]any {
	t.Helper()
	content, err := os.ReadFile(filepath.Join(cfg.LogsPath, "bot.log"))
	require.NoError(t, err)
	var lines []map[string]any
	for _, line := range strings.Split(strings.TrimSpace(string(content)), "\n") {
		var entry map[string]any
		if json.Unmarshal([]byte(line), &entry) != nil {
			entry = map[string]any{"text": line}
		}
		lines = append(lines, entry)
	}
	return lines
}

func runSummaries(lines []map[string]any) []map[string]any {
	var summaries []map[string]any
	for _, line := range lines {
		if message, ok := line["message"].(string); ok && strings.HasPrefix(message, "Run failed: ") {
			summaries = append(summaries, line)
		}
	}
	return summaries
}

var isoTimestamp = regexp.MustCompile(`^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$`)

// logsFirst makes a bot print a log line of its own before script runs, so a
// test can tell the bot's output from what the runner adds after it.
func logsFirst(script string) string {
	return `echo '{"level":"info","message":"placing order"}' >&2` + "\n" + script
}

func TestBotRun_ExitCode(t *testing.T) {
	cases := []struct {
		name     string
		script   string
		exitCode int
	}{
		{
			name:     "a success result and exit 0 succeed",
			script:   writesResult(`{"status":"success","message":"order filled"}`) + exits(0),
			exitCode: 0,
		},
		{
			name:     "no result and exit 0 succeed",
			script:   exits(0),
			exitCode: 0,
		},
		{
			name:     "the bot's own non-zero exit wins over its error result",
			script:   writesResult(`{"status":"error","message":"position limit breached"}`) + exits(3),
			exitCode: 3,
		},
		{
			name:     "the bot's own non-zero exit wins over its success result",
			script:   writesResult(`{"status":"success","message":"order filled"}`) + exits(2),
			exitCode: 2,
		},
		{
			name:     "an error result fails a run that exits 0",
			script:   writesResult(`{"status":"error","message":"position limit breached"}`) + exits(0),
			exitCode: 1,
		},
		{
			name:     "an error result without a message fails a run that exits 0",
			script:   writesResult(`{"status":"error"}`) + exits(0),
			exitCode: 1,
		},
		{
			name:     "a status that is not exactly error is not an error result",
			script:   writesResult(`{"status":"ERROR","message":"position limit breached"}`) + exits(0),
			exitCode: 0,
		},
		{
			name:     "an error status below the top level is not an error result",
			script:   writesResult(`{"order":{"status":"error"}}`) + exits(0),
			exitCode: 0,
		},
		{
			name:     "a result that is not an object is not an error result",
			script:   writesResult(`["error"]`) + exits(0),
			exitCode: 0,
		},
		{
			name:     "a result that is not JSON is not an error result",
			script:   writesResult(`status: error`) + exits(0),
			exitCode: 0,
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			cfg := compiledBot(t, tc.script)

			assert.Equal(t, tc.exitCode, runCompiledBot(t, cfg))
		})
	}

	t.Run("a result left from an earlier run does not fail this one", func(t *testing.T) {
		cfg := compiledBot(t, exits(0))
		stale := `{"status":"error","message":"left from an earlier run"}`
		require.NoError(t, os.WriteFile(filepath.Join(cfg.CodePath, "result.json"), []byte(stale), 0644))

		assert.Equal(t, 0, runCompiledBot(t, cfg))
	})
}

func TestBotRun_FailedRunEndsWithOneSummaryLine(t *testing.T) {
	cases := []struct {
		name    string
		script  string
		message string
	}{
		{
			name:    "an error result and exit 0",
			script:  writesResult(`{"status":"error","message":"position limit breached"}`) + exits(0),
			message: "Run failed: position limit breached",
		},
		{
			name:    "an error result and the bot's own non-zero exit",
			script:  writesResult(`{"status":"error","message":"position limit breached"}`) + exits(3),
			message: "Run failed: position limit breached",
		},
		{
			name:    "an error result without a message",
			script:  writesResult(`{"status":"error"}`) + exits(0),
			message: "Run failed: Bot reported an error",
		},
		{
			name:    "an error result whose message is not a string",
			script:  writesResult(`{"status":"error","message":{"code":"E42"}}`) + exits(0),
			message: `Run failed: {"code":"E42"}`,
		},
		{
			name:    "a non-zero exit and no result",
			script:  exits(3),
			message: "Run failed: Bot exited with code 3",
		},
		{
			name:    "a non-zero exit and a success result",
			script:  writesResult(`{"status":"success","message":"order filled"}`) + exits(2),
			message: "Run failed: Bot exited with code 2",
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			cfg := compiledBot(t, logsFirst(tc.script))

			runCompiledBot(t, cfg)

			lines := botLogLines(t, cfg)
			summaries := runSummaries(lines)
			require.Len(t, summaries, 1, "bot.log: %v", lines)
			summary := summaries[0]
			assert.Equal(t, "error", summary["level"])
			assert.Equal(t, tc.message, summary["message"])
			assert.Regexp(t, isoTimestamp, summary["timestamp"])
			assert.Len(t, summary, 3, "a summary line has only level, message and timestamp")
			assert.Equal(t, summary, lines[len(lines)-1], "the summary comes after the bot's own output")
		})
	}
}

func TestBotRun_SuccessfulRunHasNoSummaryLine(t *testing.T) {
	cases := []struct {
		name   string
		script string
		stale  string
	}{
		{
			name:   "a success result",
			script: writesResult(`{"status":"success","message":"order filled"}`) + exits(0),
		},
		{
			name:   "no result",
			script: exits(0),
		},
		{
			name:   "an error result left from an earlier run",
			script: exits(0),
			stale:  `{"status":"error","message":"left from an earlier run"}`,
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			cfg := compiledBot(t, logsFirst(tc.script))
			if tc.stale != "" {
				require.NoError(t, os.WriteFile(filepath.Join(cfg.CodePath, "result.json"), []byte(tc.stale), 0644))
			}

			require.Equal(t, 0, runCompiledBot(t, cfg))

			assert.Equal(t, []map[string]any{{"level": "info", "message": "placing order"}}, botLogLines(t, cfg))
		})
	}
}
