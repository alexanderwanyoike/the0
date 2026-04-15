package runtime

import (
	"fmt"
	"os/exec"

	"the0/internal/dev"
)

const (
	dotnetDebugPort   = 4711
	dotnetDockerImage = "mcr.microsoft.com/dotnet/sdk:8.0"
	dotnetNativeExec  = "dotnet"
)

// Dotnet builds a RunSpec for a .NET bot.
//
// Native: `dotnet run`. --debug is a no-op; user attaches their IDE (Rider,
// VSCode C# Dev Kit) to the process.
//
// Docker: `dotnet run` inside `mcr.microsoft.com/dotnet/sdk:8.0`. Attaching
// a remote debugger requires `netcoredbg` in a custom image — deferred.
func Dotnet(opts Opts) (*dev.RunSpec, error) {
	var cmd *exec.Cmd
	switch opts.Mode {
	case ModeNative:
		cmd = dotnetNativeCmd(opts)
	case ModeDocker:
		cmd = dotnetDockerCmd(opts)
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

func dotnetNativeCmd(opts Opts) *exec.Cmd {
	args := []string{"run"}
	if opts.Release {
		args = append(args, "--configuration", "Release")
	}
	return exec.Command(dotnetNativeExec, args...)
}

func dotnetDockerCmd(opts Opts) *exec.Cmd {
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
			port = dotnetDebugPort
		}
		args = append(args, "-p", fmt.Sprintf("%d:%d", port, port))
	}
	args = append(args, "-w", opts.Cwd, dotnetDockerImage, "dotnet", "run")
	if opts.Release {
		args = append(args, "--configuration", "Release")
	}
	return exec.Command("docker", args...)
}

func DetectDotnetToolchain() (bool, string) {
	if p, err := exec.LookPath(dotnetNativeExec); err == nil {
		return true, p
	}
	return false, ""
}
