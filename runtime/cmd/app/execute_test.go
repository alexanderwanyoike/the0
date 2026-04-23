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

// TestBuildBotCommandForMode_DebugPortPositive_UsesDebugCommand asserts that
// when executeDebugPort is set, the runtime builds the wrapper-bypassing
// debug command (python -m debugpy ... / node --inspect ...) instead of the
// wrapper-based production command.
func TestBuildBotCommandForMode_DebugPortPositive_UsesDebugCommand(t *testing.T) {
	origPort := executeDebugPort
	origWait := executeDebugWait
	defer func() {
		executeDebugPort = origPort
		executeDebugWait = origWait
	}()
	executeDebugPort = 5678
	executeDebugWait = false

	cfg := &execute.Config{Runtime: "python3.11", CodePath: "/bot"}
	cmd := buildBotCommandForMode(cfg, "main.py")

	expected := execute.BuildBotDebugCommand("python3.11", "main.py", "/bot", 5678, false)
	assert.Equal(t, expected.Args, cmd.Args)
	assert.Equal(t, expected.Dir, cmd.Dir)
}

// TestBuildBotCommandForMode_DebugPortZero_UsesWrapperCommand is the
// regression guard: default (0) port must still route to the production
// wrapper invocation so real deployments are not silently debug-enabled.
func TestBuildBotCommandForMode_DebugPortZero_UsesWrapperCommand(t *testing.T) {
	origPort := executeDebugPort
	defer func() { executeDebugPort = origPort }()
	executeDebugPort = 0

	cfg := &execute.Config{Runtime: "python3.11", CodePath: "/bot"}
	cmd := buildBotCommandForMode(cfg, "main.py")

	expected := execute.BuildBotCommand("python3.11", "main.py", "/bot")
	assert.Equal(t, expected.Args, cmd.Args)
	assert.Equal(t, expected.Dir, cmd.Dir)
}

// TestExecuteDebugFlagsRegistered asserts both --debug-port and --debug-wait
// are registered with the expected defaults.
func TestExecuteDebugFlagsRegistered(t *testing.T) {
	port := executeCmd.Flags().Lookup("debug-port")
	if assert.NotNil(t, port, "--debug-port must be registered") {
		assert.Equal(t, "0", port.DefValue)
	}
	wait := executeCmd.Flags().Lookup("debug-wait")
	if assert.NotNil(t, wait, "--debug-wait must be registered") {
		assert.Equal(t, "false", wait.DefValue)
	}
}

// TestShouldStartQueryServer_RealtimeWithQueryEntrypoint is the baseline:
// realtime bot + query entrypoint + default flags => query server starts.
func TestShouldStartQueryServer_RealtimeWithQueryEntrypoint(t *testing.T) {
	origSkip := executeSkipQueryServer
	defer func() { executeSkipQueryServer = origSkip }()
	executeSkipQueryServer = false

	cfg := &execute.Config{BotType: "realtime", QueryEntrypoint: "query.py"}
	assert.True(t, shouldStartQueryServer(cfg))
}

// TestShouldStartQueryServer_DebugPortDoesNotInterfere locks in the
// invariant that debug mode preserves the query server decision. This is
// important for local dev where the user is debugging the bot/dashboard
// integration and needs queries to still work end-to-end.
func TestShouldStartQueryServer_DebugPortDoesNotInterfere(t *testing.T) {
	origPort := executeDebugPort
	origSkip := executeSkipQueryServer
	defer func() {
		executeDebugPort = origPort
		executeSkipQueryServer = origSkip
	}()
	executeDebugPort = 5678
	executeSkipQueryServer = false

	cfg := &execute.Config{BotType: "realtime", QueryEntrypoint: "query.py"}
	assert.True(t, shouldStartQueryServer(cfg), "debug mode must not disable query server")
}

// TestShouldStartQueryServer_SkipFlagDisables verifies the explicit skip
// flag still works (K8s sidecar mode).
func TestShouldStartQueryServer_SkipFlagDisables(t *testing.T) {
	origSkip := executeSkipQueryServer
	defer func() { executeSkipQueryServer = origSkip }()
	executeSkipQueryServer = true

	cfg := &execute.Config{BotType: "realtime", QueryEntrypoint: "query.py"}
	assert.False(t, shouldStartQueryServer(cfg))
}

// TestShouldStartQueryServer_ScheduledBotNoServer ensures scheduled bots
// (which don't expose a live query server) are correctly excluded.
func TestShouldStartQueryServer_ScheduledBotNoServer(t *testing.T) {
	cfg := &execute.Config{BotType: "scheduled", QueryEntrypoint: "query.py"}
	assert.False(t, shouldStartQueryServer(cfg))
}

// TestShouldStartQueryServer_NoQueryEntrypoint ensures realtime bots
// without a query entrypoint don't spin up an idle query server.
func TestShouldStartQueryServer_NoQueryEntrypoint(t *testing.T) {
	origSkip := executeSkipQueryServer
	defer func() { executeSkipQueryServer = origSkip }()
	executeSkipQueryServer = false

	cfg := &execute.Config{BotType: "realtime", QueryEntrypoint: ""}
	assert.False(t, shouldStartQueryServer(cfg))
}
