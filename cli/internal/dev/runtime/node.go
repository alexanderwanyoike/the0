package runtime

import (
	"fmt"
	"os/exec"
	"path/filepath"

	"the0/internal/dev"
)

const (
	nodeDebugPort   = 9229
	nodeDockerImage = "node:20-slim"
	nodeNativeExec  = "node"
)

// Node builds a RunSpec for a Node.js bot. Native mode uses the host `node`;
// Docker mode runs a slim image. Debug mode adds `--inspect` (or
// `--inspect-brk` when DebugWait is set).
func Node(opts Opts) (*dev.RunSpec, error) {
	script := opts.Script
	if script == "" {
		script = "main.js"
	}
	scriptPath := filepath.Join(opts.Cwd, script)

	var cmd *exec.Cmd
	switch opts.Mode {
	case ModeNative:
		cmd = nodeNativeCmd(scriptPath, opts)
	case ModeDocker:
		cmd = nodeDockerCmd(scriptPath, opts)
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

func nodeNativeCmd(scriptPath string, opts Opts) *exec.Cmd {
	if !opts.Debug {
		return exec.Command(nodeNativeExec, scriptPath)
	}
	port := opts.DebugPort
	if port == 0 {
		port = nodeDebugPort
	}
	flag := "--inspect"
	if opts.DebugWait {
		flag = "--inspect-brk"
	}
	// Bind to localhost only; 0.0.0.0 would expose the inspector to the
	// network, letting any remote host execute arbitrary code.
	return exec.Command(nodeNativeExec, flag+"=127.0.0.1:"+fmt.Sprint(port), scriptPath)
}

func nodeDockerCmd(scriptPath string, opts Opts) *exec.Cmd {
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
			port = nodeDebugPort
		}
		args = append(args, "-p", fmt.Sprintf("%d:%d", port, port))
	}
	args = append(args, "-w", opts.Cwd, nodeDockerImage)
	if opts.Debug {
		port := opts.DebugPort
		if port == 0 {
			port = nodeDebugPort
		}
		flag := "--inspect"
		if opts.DebugWait {
			flag = "--inspect-brk"
		}
		args = append(args, "node", flag+"=0.0.0.0:"+fmt.Sprint(port), scriptPath)
	} else {
		args = append(args, "node", scriptPath)
	}
	return exec.Command("docker", args...)
}

// DetectNodeToolchain returns (present, path). Used for the implicit --native
// default and for the "toolchain not found, try --docker" hint.
func DetectNodeToolchain() (bool, string) {
	if p, err := exec.LookPath(nodeNativeExec); err == nil {
		return true, p
	}
	return false, ""
}
