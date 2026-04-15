package runtime

import (
	"fmt"
	"os/exec"

	"the0/internal/dev"
)

const (
	haskellDockerImage = "haskell:9.6"
	haskellNativeExec  = "cabal"
)

// Haskell builds a RunSpec for a Haskell bot.
//
// Native: `cabal run`. --debug is not supported on haskell (no practical
// remote debugger for GHC-compiled binaries). If --debug is set, returns an
// explanatory error rather than silently ignoring.
//
// Docker: `cabal run` inside `haskell:9.6`.
func Haskell(opts Opts) (*dev.RunSpec, error) {
	if opts.Debug {
		return nil, fmt.Errorf("--debug is not supported for Haskell bots (no practical remote debugger for GHC binaries)")
	}

	var cmd *exec.Cmd
	switch opts.Mode {
	case ModeNative:
		cmd = haskellNativeCmd()
	case ModeDocker:
		cmd = haskellDockerCmd(opts)
	default:
		return nil, fmt.Errorf("unknown mode %q", opts.Mode)
	}
	cmd.Dir = opts.Cwd

	return &dev.RunSpec{
		Cmd:       cmd,
		BotID:     opts.BotID,
		BotConfig: opts.BotConfig,
		CodeDir:   opts.CodeDir,
		StateDir:  opts.StateDir,
	}, nil
}

func haskellNativeCmd() *exec.Cmd {
	return exec.Command(haskellNativeExec, "run")
}

func haskellDockerCmd(opts Opts) *exec.Cmd {
	args := []string{
		"run", "--rm", "--init",
		"-v", opts.Cwd + ":" + opts.Cwd,
	}
	if opts.StateDir != "" {
		args = append(args, "-v", opts.StateDir+":"+opts.StateDir)
	}
	if opts.CodeDir != "" && opts.CodeDir != opts.Cwd {
		args = append(args, "-v", opts.CodeDir+":"+opts.CodeDir)
	}
	args = append(args, "-w", opts.Cwd, haskellDockerImage, "cabal", "run")
	return exec.Command("docker", args...)
}

func DetectHaskellToolchain() (bool, string) {
	if p, err := exec.LookPath(haskellNativeExec); err == nil {
		return true, p
	}
	return false, ""
}
