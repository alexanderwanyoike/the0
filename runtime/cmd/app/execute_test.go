package main

import (
	"context"
	"testing"

	"github.com/stretchr/testify/assert"

	"runtime/internal/execute"
	"runtime/internal/util"
)

// TestInitPhase_SkipTrue_DoesNotCallDownloadsOrEnsureExecutable asserts that
// with skip=true, runInitPhase returns nil immediately and invokes neither
// downloadCode, downloadState, nor EnsureExecutable. This is the core
// contract of the --skip-init flag for local dev mode where code is
// already mounted and state is host-persistent.
func TestInitPhase_SkipTrue_DoesNotCallDownloadsOrEnsureExecutable(t *testing.T) {
	var codeCalls, stateCalls, ensureCalls int

	origCode := downloadCodeFn
	origState := downloadStateFn
	origEnsure := ensureExecutableFn
	defer func() {
		downloadCodeFn = origCode
		downloadStateFn = origState
		ensureExecutableFn = origEnsure
	}()

	downloadCodeFn = func(_ context.Context, _ *execute.Config, _ *util.DefaultLogger) error {
		codeCalls++
		return nil
	}
	downloadStateFn = func(_ context.Context, _ *execute.Config, _ *util.DefaultLogger) error {
		stateCalls++
		return nil
	}
	ensureExecutableFn = func(_ *execute.Config) error {
		ensureCalls++
		return nil
	}

	err := runInitPhase(context.Background(), &execute.Config{BotID: "t"}, &util.DefaultLogger{}, true)

	assert.NoError(t, err)
	assert.Zero(t, codeCalls, "downloadCode must not be called when skip=true")
	assert.Zero(t, stateCalls, "downloadState must not be called when skip=true")
	assert.Zero(t, ensureCalls, "ensureExecutable must not be called when skip=true")
}

// TestInitPhase_SkipFalse_CallsAllPhases asserts the default path still runs
// downloadCode, EnsureExecutable, and downloadState in order. Regression guard
// against accidentally gating production code paths behind the flag.
func TestInitPhase_SkipFalse_CallsAllPhases(t *testing.T) {
	var order []string

	origCode := downloadCodeFn
	origState := downloadStateFn
	origEnsure := ensureExecutableFn
	defer func() {
		downloadCodeFn = origCode
		downloadStateFn = origState
		ensureExecutableFn = origEnsure
	}()

	downloadCodeFn = func(_ context.Context, _ *execute.Config, _ *util.DefaultLogger) error {
		order = append(order, "code")
		return nil
	}
	downloadStateFn = func(_ context.Context, _ *execute.Config, _ *util.DefaultLogger) error {
		order = append(order, "state")
		return nil
	}
	ensureExecutableFn = func(_ *execute.Config) error {
		order = append(order, "ensure")
		return nil
	}

	err := runInitPhase(context.Background(), &execute.Config{BotID: "t"}, &util.DefaultLogger{}, false)

	assert.NoError(t, err)
	assert.Equal(t, []string{"code", "ensure", "state"}, order)
}

// TestExecuteSkipInitFlagRegistered asserts that cobra registers --skip-init
// with the expected default (false) so users can pass it via CLI.
func TestExecuteSkipInitFlagRegistered(t *testing.T) {
	f := executeCmd.Flags().Lookup("skip-init")
	if assert.NotNil(t, f, "--skip-init flag must be registered on executeCmd") {
		assert.Equal(t, "false", f.DefValue)
	}
}
