package cmd

import (
	"context"
	"encoding/json"
	"fmt"
	"os"
	"os/signal"
	"path/filepath"
	"regexp"
	"strings"
	"syscall"
	"time"

	"github.com/spf13/cobra"
	"the0/internal/dev"
	"the0/internal/dev/frontend"
	"the0/internal/dev/runtime"
	"the0/internal/logger"
)

type devFlags struct {
	configPath   string
	botID        string
	mode         string
	debug        bool
	debugWait    bool
	debugPort    int
	release      bool
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

Examples:
  the0 dev                          # run once with ./config.json
  the0 dev --config dev.json        # custom config
  the0 dev --docker                 # compiled langs: run in container
  the0 dev --debug                  # enable debug port forwarding
  the0 dev --reset                  # wipe local dev state and exit`,
		RunE: func(cmd *cobra.Command, args []string) error { return runDev(cmd.Context(), f) },
	}
	cmd.Flags().StringVar(&f.configPath, "config", "", "Path to bot config (default: ./config.json)")
	cmd.Flags().StringVar(&f.botID, "bot-id", "", "Explicit bot ID (default: slug of config.name)")
	cmd.Flags().StringVar(&f.mode, "mode", "", "Force runtime mode: native|docker")
	cmd.Flags().BoolVar(&f.debug, "debug", false, "Enable debugger port (per-runtime default)")
	cmd.Flags().BoolVar(&f.debugWait, "debug-wait", false, "Debugger must attach before main runs")
	cmd.Flags().IntVar(&f.debugPort, "debug-port", 0, "Override debugger port")
	cmd.Flags().BoolVar(&f.release, "release", false, "Compiled runtimes: build in release mode")
	cmd.Flags().BoolVar(&f.reset, "reset", false, "Wipe .the0/dev/<bot-id>/ and exit")
	cmd.Flags().BoolVar(&f.frontend, "frontend", false, "Also serve the custom dashboard (see docs/local-development/frontend.md)")
	cmd.Flags().IntVar(&f.frontendPort, "frontend-port", 0, "Port for the dev dashboard (0 = OS-assigned)")
	cmd.Flags().BoolVar(&f.watch, "watch", false, "Re-run the bot on source file changes")

	// Convenience alias booleans — wired via PreRunE so they override --mode.
	var native, docker bool
	cmd.Flags().BoolVar(&native, "native", false, "Alias for --mode=native")
	cmd.Flags().BoolVar(&docker, "docker", false, "Alias for --mode=docker")
	cmd.PreRunE = func(*cobra.Command, []string) error {
		if native && docker {
			return fmt.Errorf("--native and --docker are mutually exclusive")
		}
		if native {
			f.mode = "native"
		}
		if docker {
			f.mode = "docker"
		}
		return nil
	}
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

	botConfig, botName, err := loadBotConfig(f.configPath)
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
	codeDir := filepath.Join(devRoot, "code")
	stateDir := filepath.Join(devRoot, "state")

	if f.reset {
		if err := os.RemoveAll(devRoot); err != nil {
			return err
		}
		logger.Success("Cleared %s", devRoot)
		return nil
	}

	for _, d := range []string{codeDir, stateDir} {
		if err := os.MkdirAll(d, 0o755); err != nil {
			return err
		}
	}

	mode, err := resolveMode(f, rt)
	if err != nil {
		return err
	}
	var frontendSink dev.EventSink
	if f.frontend {
		srv, err := frontend.New(cwd, "", botID, f.frontendPort)
		if err != nil {
			return fmt.Errorf("frontend server: %w", err)
		}
		logger.Info("Dashboard: http://%s", srv.Addr())
		go func() { _ = srv.Run(ctx) }()
		frontendSink = srv
	}

	opts := runtime.Opts{
		Mode:      mode,
		Cwd:       cwd,
		Script:    rt.DefaultScript(),
		BotID:     botID,
		BotConfig: botConfig,
		CodeDir:   codeDir,
		StateDir:  stateDir,
		Debug:     f.debug,
		DebugPort: f.debugPort,
		DebugWait: f.debugWait,
		Release:   f.release,
	}

	spec, err := dispatch(rt, opts)
	if err != nil {
		return err
	}

	lock, err := dev.AcquireLock(filepath.Join(devRoot, ".lock"))
	if err != nil {
		return err
	}
	defer lock.Release()

	jsonl, err := dev.NewJSONLSink(filepath.Join(devRoot, "events.jsonl"))
	if err != nil {
		return err
	}
	defer jsonl.Close()

	term := dev.NewTerminalSink(os.Stdout, terminalIsTTY())

	runCtx, cancel := context.WithCancel(ctx)
	defer cancel()
	go forwardSignals(cancel)

	logger.Info("Running %s bot %q (mode=%s)", rt, botID, mode)

	sinks := []dev.EventSink{term, jsonl}
	if frontendSink != nil {
		sinks = append(sinks, frontendSink)
	}

	if f.watch {
		return runWatch(runCtx, cwd, rt, opts, sinks...)
	}

	runner := dev.NewRunner(spec, sinks...)
	exitCode, runErr := runner.Run(runCtx)
	if runErr != nil {
		return runErr
	}
	if exitCode != 0 {
		return fmt.Errorf("bot exited with code %d", exitCode)
	}
	logger.Success("Bot exited cleanly")
	return nil
}

// runWatch loops: run the bot, watch for file changes, kill the bot on change,
// emit a restart sentinel, repeat. Only returns when ctx is cancelled (the
// user pressed Ctrl-C).
func runWatch(ctx context.Context, cwd string, rt dev.Runtime, opts runtime.Opts, sinks ...dev.EventSink) error {
	wcfg := dev.WatchConfigFor(rt)

	// runOnce spawns a single bot invocation in its own cancellable ctx.
	runOnce := func(parent context.Context) {
		spec, err := dispatch(rt, opts)
		if err != nil {
			logger.Error("%v", err)
			return
		}
		runner := dev.NewRunner(spec, sinks...)
		_, _ = runner.Run(parent)
	}

	botCtx, botCancel := context.WithCancel(ctx)
	done := make(chan struct{})
	go func() {
		runOnce(botCtx)
		close(done)
	}()

	restart := make(chan struct{}, 1)
	go func() {
		_ = dev.Watcher(ctx, cwd, wcfg, func() {
			select {
			case restart <- struct{}{}:
			default:
			}
		})
	}()

	for {
		select {
		case <-ctx.Done():
			botCancel()
			<-done
			return nil
		case <-restart:
			// Broadcast restart sentinel, kill the running bot, start a new one.
			for _, s := range sinks {
				s.Emit(dev.Event{Kind: dev.EventRestart, Timestamp: nowFn()})
			}
			botCancel()
			<-done
			botCtx, botCancel = context.WithCancel(ctx)
			done = make(chan struct{})
			go func() {
				runOnce(botCtx)
				close(done)
			}()
		case <-done:
			// Bot exited on its own; wait for next source change or ctx cancel.
			for _, s := range sinks {
				s.Emit(dev.Event{Kind: dev.EventBotStopped, Timestamp: nowFn()})
			}
			// Rebuild done so the outer select blocks here until restart/ctx.
			done = make(chan struct{})
			select {
			case <-ctx.Done():
				return nil
			case <-restart:
				for _, s := range sinks {
					s.Emit(dev.Event{Kind: dev.EventRestart, Timestamp: nowFn()})
				}
				botCtx, botCancel = context.WithCancel(ctx)
				done = make(chan struct{})
				go func() {
					runOnce(botCtx)
					close(done)
				}()
			}
		}
	}
}

// nowFn is a var so tests can freeze time; runtime callers use time.Now.
var nowFn = timeNow

func timeNow() time.Time { return time.Now() }

func dispatch(rt dev.Runtime, opts runtime.Opts) (*dev.RunSpec, error) {
	switch rt {
	case dev.RuntimePython:
		return runtime.Python(opts)
	case dev.RuntimeNode:
		return runtime.Node(opts)
	case dev.RuntimeRust:
		return runtime.Rust(opts)
	case dev.RuntimeDotnet:
		return runtime.Dotnet(opts)
	case dev.RuntimeCpp:
		return runtime.Cpp(opts)
	case dev.RuntimeScala:
		return runtime.Scala(opts)
	case dev.RuntimeHaskell:
		return runtime.Haskell(opts)
	}
	return nil, fmt.Errorf("runtime %q is not supported", rt)
}

func resolveMode(f devFlags, rt dev.Runtime) (runtime.Mode, error) {
	raw := strings.ToLower(f.mode)
	if raw == "" {
		// Native by default when the toolchain is present; otherwise Docker.
		if ok, _ := detectNativeToolchain(rt); ok {
			return runtime.ModeNative, nil
		}
		return runtime.ModeDocker, nil
	}
	switch raw {
	case "native":
		return runtime.ModeNative, nil
	case "docker":
		return runtime.ModeDocker, nil
	}
	return "", fmt.Errorf("invalid --mode %q (use native or docker)", f.mode)
}

func detectNativeToolchain(rt dev.Runtime) (bool, string) {
	switch rt {
	case dev.RuntimePython:
		return runtime.DetectPythonToolchain()
	case dev.RuntimeNode:
		return runtime.DetectNodeToolchain()
	case dev.RuntimeRust:
		return runtime.DetectRustToolchain()
	case dev.RuntimeDotnet:
		return runtime.DetectDotnetToolchain()
	case dev.RuntimeCpp:
		return runtime.DetectCppToolchain()
	case dev.RuntimeScala:
		return runtime.DetectScalaToolchain()
	case dev.RuntimeHaskell:
		return runtime.DetectHaskellToolchain()
	}
	return false, ""
}

func loadBotConfig(configPath string) (json.RawMessage, string, error) {
	if configPath == "" {
		configPath = "config.json"
	}
	raw, err := os.ReadFile(configPath)
	if err != nil {
		if os.IsNotExist(err) {
			logger.Warning("No %s found; using empty config", configPath)
			return json.RawMessage(`{}`), "", nil
		}
		return nil, "", err
	}
	var peek struct {
		Name string `json:"name"`
	}
	_ = json.Unmarshal(raw, &peek)
	return json.RawMessage(raw), peek.Name, nil
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
