package main

import (
	"os"
	"path/filepath"
	"testing"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"

	"runtime/internal/launch"
	"runtime/internal/util"
)

// compiledBotWithoutExecuteBits lays out a compiled bot (Rust, C++, Haskell)
// whose binaries came out of its code zip without execute bits, for a launch
// in mode. Each binary is a shell script that exits 0.
func compiledBotWithoutExecuteBits(t *testing.T, mode launch.Mode) *launch.Settings {
	t.Helper()
	root := t.TempDir()
	cfg := &launch.Settings{
		Mode:            mode,
		BotID:           "compiled-query-bot",
		Runtime:         "rust-stable",
		Entrypoint:      "bot",
		QueryEntrypoint: "query",
		BotConfig:       "{}",
		CodePath:        filepath.Join(root, "bot"),
		StatePath:       filepath.Join(root, "state"),
		LogsPath:        filepath.Join(root, "logs"),
		StateDir:        filepath.Join(root, "state", ".the0-state"),
	}
	if mode == launch.Query {
		cfg.Entrypoint = cfg.QueryEntrypoint
		cfg.QueryPath = "/status"
	}
	require.NoError(t, os.MkdirAll(cfg.CodePath, 0755))
	for _, binary := range []string{"bot", "query"} {
		require.NoError(t, os.WriteFile(filepath.Join(cfg.CodePath, binary), []byte("#!/bin/sh\nexit 0\n"), 0644))
	}
	// Without MinIO the state download fails at once, and a query goes on
	// without state.
	t.Setenv("MINIO_ENDPOINT", "")
	return cfg
}

func assertExecutable(t *testing.T, path string) {
	t.Helper()
	info, err := os.Stat(path)
	require.NoError(t, err)
	assert.NotZero(t, info.Mode().Perm()&0111, "%s is not executable (%v)", path, info.Mode().Perm())
}

func TestQueryLaunch_MakesTheCompiledQueryEntrypointExecutable(t *testing.T) {
	t.Run("query", func(t *testing.T) {
		cfg := compiledBotWithoutExecuteBits(t, launch.Query)

		runEphemeralQuery(cfg, &util.DefaultLogger{})

		assertExecutable(t, filepath.Join(cfg.CodePath, cfg.QueryEntrypoint))
	})

	t.Run("query server", func(t *testing.T) {
		cfg := compiledBotWithoutExecuteBits(t, launch.Server)

		exitCode := runQueryServerOnly(cfg, &util.DefaultLogger{})

		assert.Equal(t, 0, exitCode, "the query server did not run")
		assertExecutable(t, filepath.Join(cfg.CodePath, cfg.QueryEntrypoint))
	})
}
