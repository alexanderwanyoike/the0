package runtime

import (
	"fmt"
	"os/exec"

	"the0/internal/dev"
)

const (
	cppDebugPort   = 2346
	cppDockerImage = "mcr.microsoft.com/devcontainers/cpp:1-debian-12"
)

// Cpp builds a RunSpec for a C++ bot. C++ has no "one true" run command, so
// dev relies on a `make run` target in the project Makefile or CMake project.
// This keeps the CLI out of language-specific build graph discovery.
//
// Native: `make run` (project author supplies the target).
// Docker: same, inside the Microsoft C++ devcontainer image.
func Cpp(opts Opts) (*dev.RunSpec, error) {
	var cmd *exec.Cmd
	switch opts.Mode {
	case ModeNative:
		cmd = cppNativeCmd()
	case ModeDocker:
		cmd = cppDockerCmd(opts)
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

func cppNativeCmd() *exec.Cmd {
	return exec.Command("make", "run")
}

func cppDockerCmd(opts Opts) *exec.Cmd {
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
			port = cppDebugPort
		}
		args = append(args, "-p", fmt.Sprintf("%d:%d", port, port))
	}
	args = append(args, "-w", opts.Cwd, cppDockerImage, "make", "run")
	return exec.Command("docker", args...)
}

func DetectCppToolchain() (bool, string) {
	if p, err := exec.LookPath("make"); err == nil {
		return true, p
	}
	return false, ""
}
