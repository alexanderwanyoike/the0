package runtime

import (
	"fmt"
	"os/exec"

	"the0/internal/dev"
)

const (
	rustDebugPort   = 2345
	rustDockerImage = "rust:latest"
	rustNativeExec  = "cargo"
)

// Rust builds a RunSpec for a Rust bot.
//
// Native: `cargo run [--release]` in the project root. `--debug` is a no-op;
// the user's IDE attaches to the spawned process via its own "Attach by
// PID" flow (lldb/gdb).
//
// Docker: runs `cargo run` inside `rust:latest` with the project bind-mounted.
// `--debug` would require lldb-server preinstalled in a custom image — deferred
// to a follow-up; for now it surfaces a notice.
func Rust(opts Opts) (*dev.RunSpec, error) {
	var cmd *exec.Cmd
	switch opts.Mode {
	case ModeNative:
		cmd = rustNativeCmd(opts)
	case ModeDocker:
		cmd = rustDockerCmd(opts)
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

func rustNativeCmd(opts Opts) *exec.Cmd {
	args := []string{"run"}
	// Cargo's release profile matches what `custom-bot deploy` ships, but is
	// far slower to iterate on; default to debug builds for dev mode.
	if opts.Release {
		args = append(args, "--release")
	}
	return exec.Command(rustNativeExec, args...)
}

func rustDockerCmd(opts Opts) *exec.Cmd {
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
	if opts.Debug {
		port := opts.DebugPort
		if port == 0 {
			port = rustDebugPort
		}
		args = append(args, "-p", fmt.Sprintf("%d:%d", port, port))
	}
	args = append(args, "-w", opts.Cwd, rustDockerImage, "cargo", "run")
	if opts.Release {
		args = append(args, "--release")
	}
	return exec.Command("docker", args...)
}

// DetectRustToolchain reports whether `cargo` is on PATH.
func DetectRustToolchain() (bool, string) {
	if p, err := exec.LookPath(rustNativeExec); err == nil {
		return true, p
	}
	return false, ""
}
