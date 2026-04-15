package runtime

import (
	"fmt"
	"os/exec"

	"the0/internal/dev"
)

const (
	scalaDebugPort   = 5005
	scalaDockerImage = "sbtscala/scala-sbt:eclipse-temurin-17.0.15_6_1.11.7_3.3.7"
	scalaNativeExec  = "sbt"
)

// Scala builds a RunSpec for a Scala bot.
//
// Native: `sbt run`. Expect a slow first invocation while sbt boots the JVM
// and resolves deps; subsequent ones are faster thanks to the sbt server.
//
// Docker: `sbt run` inside the official sbt image.
func Scala(opts Opts) (*dev.RunSpec, error) {
	var cmd *exec.Cmd
	switch opts.Mode {
	case ModeNative:
		cmd = scalaNativeCmd()
	case ModeDocker:
		cmd = scalaDockerCmd(opts)
	default:
		return nil, fmt.Errorf("unknown mode %q", opts.Mode)
	}
	cmd.Dir = opts.Cwd

	extra := []string(nil)
	if opts.Debug {
		port := opts.DebugPort
		if port == 0 {
			port = scalaDebugPort
		}
		// sbt consumes JAVA_TOOL_OPTIONS and forwards to the forked JVM.
		extra = append(extra, fmt.Sprintf(
			"JAVA_TOOL_OPTIONS=-agentlib:jdwp=transport=dt_socket,server=y,suspend=n,address=*:%d",
			port,
		))
	}
	return &dev.RunSpec{
		Cmd:       cmd,
		BotID:     opts.BotID,
		BotConfig: opts.BotConfig,
		CodeDir:   opts.CodeDir,
		StateDir:  opts.StateDir,
		ExtraEnv:  extra,
	}, nil
}

func scalaNativeCmd() *exec.Cmd {
	return exec.Command(scalaNativeExec, "run")
}

func scalaDockerCmd(opts Opts) *exec.Cmd {
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
			port = scalaDebugPort
		}
		args = append(args, "-p", fmt.Sprintf("%d:%d", port, port))
	}
	args = append(args, "-w", opts.Cwd, scalaDockerImage, "sbt", "run")
	return exec.Command("docker", args...)
}

func DetectScalaToolchain() (bool, string) {
	if p, err := exec.LookPath(scalaNativeExec); err == nil {
		return true, p
	}
	return false, ""
}
