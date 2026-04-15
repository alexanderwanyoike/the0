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

	"github.com/spf13/cobra"
	"the0/internal/dev"
	"the0/internal/dev/runtime"
	"the0/internal/logger"
)

type devFlags struct {
	configPath string
	botID      string
	mode       string
	debug      bool
	debugWait  bool
	debugPort  int
	release    bool
	reset      bool
	frontend   bool
	// watch/frontendPort wired in later steps
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
	if f.frontend {
		logger.Warning("--frontend is not yet wired (coming in a follow-up commit); proceeding without it")
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
	}

	spec, err := dispatch(rt, opts)
	if err != nil {
		return err
	}

	jsonl, err := dev.NewJSONLSink(filepath.Join(devRoot, "events.jsonl"))
	if err != nil {
		return err
	}
	defer jsonl.Close()

	term := dev.NewTerminalSink(os.Stdout, terminalIsTTY())
	runner := dev.NewRunner(spec, term, jsonl)

	runCtx, cancel := context.WithCancel(ctx)
	defer cancel()
	go forwardSignals(cancel)

	logger.Info("Running %s bot %q (mode=%s)", rt, botID, mode)
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

func dispatch(rt dev.Runtime, opts runtime.Opts) (*dev.RunSpec, error) {
	switch rt {
	case dev.RuntimePython:
		return runtime.Python(opts)
	case dev.RuntimeNode:
		return runtime.Node(opts)
	default:
		return nil, fmt.Errorf("runtime %q not yet supported by `the0 dev` (coming in a follow-up)", rt)
	}
}

func resolveMode(f devFlags, rt dev.Runtime) (runtime.Mode, error) {
	raw := strings.ToLower(f.mode)
	if raw == "" {
		// Native by default when the toolchain is present.
		switch rt {
		case dev.RuntimePython:
			if ok, _ := runtime.DetectPythonToolchain(); ok {
				return runtime.ModeNative, nil
			}
			return runtime.ModeDocker, nil
		case dev.RuntimeNode:
			if ok, _ := runtime.DetectNodeToolchain(); ok {
				return runtime.ModeNative, nil
			}
			return runtime.ModeDocker, nil
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
