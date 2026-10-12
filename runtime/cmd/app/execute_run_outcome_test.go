package main

import (
	"context"
	"fmt"
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/execute"
	"runtime/internal/util"
)

// compiledBot lays out a bot the way a compiled runtime (Rust, C++, Haskell)
// runs one: an executable entrypoint in the code directory with no wrapper
// around it. script is the shell the entrypoint runs.
func compiledBot(t *testing.T, script string) *execute.Config {
	t.Helper()
	root := t.TempDir()
	cfg := &execute.Config{
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

func runCompiledBot(t *testing.T, cfg *execute.Config) int {
	t.Helper()
	return executeBotProcess(context.Background(), cfg, &util.DefaultLogger{})
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
