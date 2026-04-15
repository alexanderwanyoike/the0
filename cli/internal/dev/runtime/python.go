package runtime

import (
	"fmt"
	"os/exec"
	"path/filepath"
	"strconv"

	"the0/internal/dev"
)

const (
	pythonDebugPort      = 5678
	pythonDockerImage    = "python:3.11-slim"
	pythonNativeExec     = "python3"
	pythonNativeFallback = "python"
)

// Python builds a RunSpec for a Python bot. Native mode shells out to the
// host `python` (or `python3`) interpreter; Docker mode runs a slim image with
// the bot's code bind-mounted. Debug mode wraps the script in `debugpy` on
// port 5678 (configurable).
func Python(opts Opts) (*dev.RunSpec, error) {
	script := opts.Script
	if script == "" {
		script = "main.py"
	}
	scriptPath := filepath.Join(opts.Cwd, script)

	var cmd *exec.Cmd
	switch opts.Mode {
	case ModeNative:
		cmd = pythonNativeCmd(scriptPath, opts)
	case ModeDocker:
		cmd = pythonDockerCmd(scriptPath, opts)
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
		ExtraEnv:  []string{"SCRIPT_PATH=" + script},
	}, nil
}

func pythonNativeCmd(scriptPath string, opts Opts) *exec.Cmd {
	interpreter := pythonNativeExec
	if _, err := exec.LookPath(interpreter); err != nil {
		interpreter = pythonNativeFallback
	}
	if !opts.Debug {
		return exec.Command(interpreter, scriptPath)
	}
	port := opts.DebugPort
	if port == 0 {
		port = pythonDebugPort
	}
	args := []string{"-m", "debugpy", "--listen", fmt.Sprintf("0.0.0.0:%d", port)}
	if opts.DebugWait {
		args = append(args, "--wait-for-client")
	}
	args = append(args, scriptPath)
	return exec.Command(interpreter, args...)
}

func pythonDockerCmd(scriptPath string, opts Opts) *exec.Cmd {
	// Bind-mount the project root and the dev state dir at the same absolute
	// paths they occupy on the host. The runner's applyEnv strips the leading
	// slash from CODE_MOUNT_DIR so the unchanged SDK reconstructs the correct
	// in-container path.
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
			port = pythonDebugPort
		}
		args = append(args,
			"-p", fmt.Sprintf("%d:%d", port, port),
		)
	}
	args = append(args, "-w", opts.Cwd, pythonDockerImage)
	if opts.Debug {
		port := opts.DebugPort
		if port == 0 {
			port = pythonDebugPort
		}
		inner := []string{
			"sh", "-c",
			fmt.Sprintf(
				"pip install --quiet debugpy && python -m debugpy --listen 0.0.0.0:%s %s",
				strconv.Itoa(port), scriptPath,
			),
		}
		args = append(args, inner...)
	} else {
		args = append(args, "python", scriptPath)
	}
	return exec.Command("docker", args...)
}

// DetectPythonToolchain returns (present, path). Used for the implicit
// --native default and for the "toolchain not found, try --docker" hint.
func DetectPythonToolchain() (bool, string) {
	if p, err := exec.LookPath(pythonNativeExec); err == nil {
		return true, p
	}
	if p, err := exec.LookPath(pythonNativeFallback); err == nil {
		return true, p
	}
	return false, ""
}
