package cmd

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"regexp"
	"strings"
	"syscall"
	"time"

	"github.com/spf13/cobra"
	"the0/internal/dev"
	"the0/internal/dev/frontend"
	"the0/internal/logger"
)

type devFlags struct {
	configPath   string
	botID        string
	debug        bool
	debugWait    bool
	debugPort    int
	reset        bool
	frontend     bool
	frontendPort int
	watch        bool
}

func NewDevCmd() *cobra.Command {
	var f devFlags
	cmd := &cobra.Command{
		Use:   "dev",
		Short: "Run a bot locally with live event streaming",
		Long: `Run a bot locally against the same env-var contract production uses.

Events (metrics, logs, print) stream to the terminal in real time. State
persists between runs under .the0/dev/<bot-id>/state/ so re-running picks
up where the last one left off.

Delegates bot execution to the0/runtime:latest (same image production
uses). Python and Node bots are supported in v1; compiled-language
support (Rust, C++, Scala, .NET, Haskell) is coming in phase 2.

Examples:
  the0 dev                          # run once with ./config.json
  the0 dev --config dev.json        # custom config
  the0 dev --debug                  # enable debugger port forwarding
  the0 dev --watch                  # auto-restart on source changes
  the0 dev --frontend               # also serve the custom dashboard
  the0 dev --reset                  # wipe local dev state and exit`,
		RunE: func(cmd *cobra.Command, args []string) error { return runDev(cmd.Context(), f) },
	}
	cmd.Flags().StringVar(&f.configPath, "config", "", "Path to bot config (default: ./config.json)")
	cmd.Flags().StringVar(&f.botID, "bot-id", "", "Explicit bot ID (default: slug of config.name)")
	cmd.Flags().BoolVar(&f.debug, "debug", false, "Enable debugger port (per-runtime default: 5678 py / 9229 node)")
	cmd.Flags().BoolVar(&f.debugWait, "debug-wait", false, "Debugger must attach before main runs")
	cmd.Flags().IntVar(&f.debugPort, "debug-port", 0, "Override debugger port")
	cmd.Flags().BoolVar(&f.reset, "reset", false, "Wipe .the0/dev/<bot-id>/ and exit")
	cmd.Flags().BoolVar(&f.frontend, "frontend", false, "Also serve the custom dashboard (see docs/local-development/frontend.md)")
	cmd.Flags().IntVar(&f.frontendPort, "frontend-port", 0, "Port for the dev dashboard (0 = OS-assigned)")
	cmd.Flags().BoolVar(&f.watch, "watch", false, "Re-run the bot on source file changes")
	return cmd
}

func runDev(ctx context.Context, f devFlags) error {
	cwd, err := os.Getwd()
	if err != nil {
		return err
	}

	rt, err := dev.DetectRuntime(cwd)
	if err != nil {
		return err
	}
	logger.Info("Detected runtime: %s", rt)

	botConfig, botName, botType, err := loadBotConfig(f.configPath)
	if err != nil {
		return err
	}
	botID := f.botID
	if botID == "" {
		botID = slug(botName)
	}
	if botID == "" {
		botID = "local-dev"
	}

	devRoot := filepath.Join(cwd, ".the0", "dev", botID)
	stateDir := filepath.Join(devRoot, "state")

	if err := os.MkdirAll(stateDir, 0o755); err != nil {
		return err
	}

	// Acquire the per-bot lock BEFORE --reset so we don't wipe state out
	// from under an active session in another terminal.
	lock, err := dev.AcquireLock(filepath.Join(devRoot, ".lock"))
	if err != nil {
		return err
	}
	defer lock.Release()

	if f.reset {
		if err := os.RemoveAll(devRoot); err != nil {
			return err
		}
		logger.Success("Cleared %s", devRoot)
		return nil
	}

	queryEntrypoint := detectQueryEntrypoint(cwd, rt)

	var frontendSrv *frontend.Server
	if f.frontend {
		srv, err := frontend.New(cwd, "", botID, f.frontendPort)
		if err != nil {
			return fmt.Errorf("frontend server: %w", err)
		}
		logger.Info("Dashboard: http://%s", srv.Addr())
		go func() { _ = srv.Run(ctx) }()
		frontendSrv = srv
	}

	params := dev.RunParams{
		BotID:           botID,
		Runtime:         rt,
		Entrypoint:      rt.DefaultScript(),
		BotType:         botType,
		QueryEntrypoint: queryEntrypoint,
		BotConfig:       botConfig,
		CodeDir:         cwd,
		StateDir:        stateDir,
		Debug:           f.debug,
		DebugPort:       f.debugPort,
		DebugWait:       f.debugWait,
	}

	term := dev.NewTerminalSink(os.Stdout, terminalIsTTY())

	runCtx, cancel := context.WithCancel(ctx)
	defer cancel()
	go forwardSignals(cancel)

	logger.Info("Running %s bot %q", rt, botID)
	if botType == "realtime" && queryEntrypoint != "" {
		logger.Info("Query server: %s (proxied via frontend /query/*)", queryEntrypoint)
	}

	sinks := []dev.EventSink{term}
	if frontendSrv != nil {
		sinks = append(sinks, frontendSrv)
	}

	if f.watch {
		return runWatch(runCtx, params, frontendSrv, sinks...)
	}

	return runOnce(runCtx, params, sinks...)
}

// runOnce builds the docker argv, wraps it in an exec.Cmd, and runs the
// bot through the runner (pipe-scan + sink fan-out).
func runOnce(ctx context.Context, params dev.RunParams, sinks ...dev.EventSink) error {
	cmd, err := buildDockerCmd(params)
	if err != nil {
		return err
	}
	runner := dev.NewRunner(&dev.RunSpec{Cmd: cmd}, sinks...)
	exitCode, runErr := runner.Run(ctx)
	if runErr != nil {
		return runErr
	}
	if exitCode != 0 {
		return fmt.Errorf("bot exited with code %d", exitCode)
	}
	logger.Success("Bot exited cleanly")
	return nil
}

// runWatch loops: run the bot, watch for file changes, kill on change,
// emit a restart sentinel, repeat. Only returns when ctx is cancelled.
func runWatch(ctx context.Context, params dev.RunParams, feSrv *frontend.Server, sinks ...dev.EventSink) error {
	wcfg := dev.WatchConfigFor(params.Runtime)

	runBot := func(parent context.Context) {
		cmd, err := buildDockerCmd(params)
		if err != nil {
			logger.Error("docker argv: %v", err)
			return
		}
		runner := dev.NewRunner(&dev.RunSpec{Cmd: cmd}, sinks...)
		exitCode, runErr := runner.Run(parent)
		if runErr != nil {
			logger.Error("runner: %v", runErr)
		} else if exitCode != 0 && parent.Err() == nil {
			logger.Warning("bot exited with code %d", exitCode)
		}
		if feSrv != nil {
			if err := feSrv.RebuildBundle(); err != nil {
				logger.Warning("dashboard rebuild: %v", err)
			}
		}
	}

	restart := make(chan struct{}, 1)
	go func() {
		_ = dev.Watcher(ctx, params.CodeDir, wcfg, func() {
			select {
			case restart <- struct{}{}:
			default:
			}
		})
	}()

	// Each iteration runs the bot once under its own cancellable ctx and
	// waits for one of: user cancel, file change, bot exit on its own.
	// The loop itself guarantees botCancel is always called before we
	// create a new one, so no ctx leaks.
	for {
		botCtx, botCancel := context.WithCancel(ctx)
		done := make(chan struct{})
		go func() {
			runBot(botCtx)
			close(done)
		}()

		select {
		case <-ctx.Done():
			botCancel()
			<-done
			return nil

		case <-restart:
			for _, s := range sinks {
				s.Emit(dev.Event{Kind: dev.EventRestart, Timestamp: nowFn()})
			}
			botCancel()
			<-done

		case <-done:
			// Bot exited on its own; wait for either a restart or cancel.
			botCancel()
			for _, s := range sinks {
				s.Emit(dev.Event{Kind: dev.EventBotStopped, Timestamp: nowFn()})
			}
			select {
			case <-ctx.Done():
				return nil
			case <-restart:
				for _, s := range sinks {
					s.Emit(dev.Event{Kind: dev.EventRestart, Timestamp: nowFn()})
				}
			}
		}
	}
}

// buildDockerCmd translates RunParams into a ready-to-run *exec.Cmd.
func buildDockerCmd(params dev.RunParams) (*exec.Cmd, error) {
	args, err := dev.BuildRunArgs(params)
	if err != nil {
		return nil, err
	}
	return exec.Command("docker", args...), nil
}

// detectQueryEntrypoint looks for a conventional query script alongside
// the bot entrypoint. If present, the bot is wired as a realtime service
// with a query endpoint.
func detectQueryEntrypoint(cwd string, rt dev.Runtime) string {
	candidates := map[dev.Runtime][]string{
		dev.RuntimePython: {"query.py"},
		dev.RuntimeNode:   {"query.js", "query.mjs"},
	}
	for _, name := range candidates[rt] {
		if _, err := os.Stat(filepath.Join(cwd, name)); err == nil {
			return name
		}
	}
	return ""
}

// nowFn is a var so tests can freeze time; runtime callers use time.Now.
var nowFn = timeNow

func timeNow() time.Time { return time.Now() }

// loadBotConfig reads config.json and returns its raw contents, the bot
// name (for slugging), and the bot type category ("realtime" or "scheduled",
// parsed from the "type" field's "category/name" form — empty if absent).
func loadBotConfig(configPath string) (json.RawMessage, string, string, error) {
	if configPath == "" {
		configPath = "config.json"
	}
	raw, err := os.ReadFile(configPath)
	if err != nil {
		if os.IsNotExist(err) {
			logger.Warning("No %s found; using empty config", configPath)
			return json.RawMessage(`{}`), "", "", nil
		}
		return nil, "", "", err
	}
	var peek struct {
		Name string `json:"name"`
		Type string `json:"type"`
	}
	if err := json.Unmarshal(raw, &peek); err != nil {
		return nil, "", "", fmt.Errorf("%s: invalid JSON: %w", configPath, err)
	}
	// Type is "category/subtype", e.g. "scheduled/portfolio-tracker".
	category := peek.Type
	if slash := strings.Index(peek.Type, "/"); slash >= 0 {
		category = peek.Type[:slash]
	}
	return json.RawMessage(raw), peek.Name, category, nil
}

var slugRE = regexp.MustCompile(`[^a-z0-9-]+`)

func slug(s string) string {
	s = strings.ToLower(strings.TrimSpace(s))
	s = slugRE.ReplaceAllString(s, "-")
	return strings.Trim(s, "-")
}

func terminalIsTTY() bool {
	fi, err := os.Stdout.Stat()
	if err != nil {
		return false
	}
	return fi.Mode()&os.ModeCharDevice != 0
}

func forwardSignals(cancel context.CancelFunc) {
	sig := make(chan os.Signal, 1)
	signal.Notify(sig, os.Interrupt, syscall.SIGTERM)
	<-sig
	cancel()
}
