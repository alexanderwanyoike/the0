package runtime

import (
	"strings"
	"testing"
)

func compiledOpts() Opts {
	return Opts{
		Cwd:      "/home/user/bot",
		BotID:    "abc",
		CodeDir:  "/home/user/.the0/dev/abc/code",
		StateDir: "/home/user/.the0/dev/abc/state",
	}
}

type dispatcher struct {
	name string
	fn   func(Opts) (interface{ _ignored() }, error)
}

func TestRust(t *testing.T) {
	t.Run("native debug build", func(t *testing.T) {
		opts := compiledOpts()
		opts.Mode = ModeNative
		spec, err := Rust(opts)
		if err != nil {
			t.Fatal(err)
		}
		if spec.Cmd.Args[0] != "cargo" || spec.Cmd.Args[1] != "run" {
			t.Errorf("args = %v", spec.Cmd.Args)
		}
		if strings.Contains(strings.Join(spec.Cmd.Args, " "), "--release") {
			t.Error("debug build should not include --release")
		}
	})
	t.Run("native release", func(t *testing.T) {
		opts := compiledOpts()
		opts.Mode = ModeNative
		opts.Release = true
		spec, _ := Rust(opts)
		if !strings.Contains(strings.Join(spec.Cmd.Args, " "), "--release") {
			t.Errorf("release build missing --release: %v", spec.Cmd.Args)
		}
	})
	t.Run("docker", func(t *testing.T) {
		opts := compiledOpts()
		opts.Mode = ModeDocker
		spec, _ := Rust(opts)
		args := strings.Join(spec.Cmd.Args, " ")
		if !strings.Contains(args, rustDockerImage) {
			t.Errorf("missing image: %v", spec.Cmd.Args)
		}
		if !strings.Contains(args, "-v /home/user/bot:/home/user/bot") {
			t.Errorf("missing mount: %v", spec.Cmd.Args)
		}
	})
	t.Run("docker debug forwards port", func(t *testing.T) {
		opts := compiledOpts()
		opts.Mode = ModeDocker
		opts.Debug = true
		spec, _ := Rust(opts)
		if !strings.Contains(strings.Join(spec.Cmd.Args, " "), "-p 2345:2345") {
			t.Errorf("port forward missing: %v", spec.Cmd.Args)
		}
	})
}

func TestDotnet(t *testing.T) {
	t.Run("native", func(t *testing.T) {
		opts := compiledOpts()
		opts.Mode = ModeNative
		spec, _ := Dotnet(opts)
		if spec.Cmd.Args[0] != "dotnet" || spec.Cmd.Args[1] != "run" {
			t.Errorf("args = %v", spec.Cmd.Args)
		}
	})
	t.Run("native release", func(t *testing.T) {
		opts := compiledOpts()
		opts.Mode = ModeNative
		opts.Release = true
		spec, _ := Dotnet(opts)
		if !strings.Contains(strings.Join(spec.Cmd.Args, " "), "Release") {
			t.Errorf("expected --configuration Release: %v", spec.Cmd.Args)
		}
	})
	t.Run("docker debug port", func(t *testing.T) {
		opts := compiledOpts()
		opts.Mode = ModeDocker
		opts.Debug = true
		spec, _ := Dotnet(opts)
		if !strings.Contains(strings.Join(spec.Cmd.Args, " "), "-p 4711:4711") {
			t.Errorf("port forward missing: %v", spec.Cmd.Args)
		}
	})
}

func TestCpp(t *testing.T) {
	t.Run("native uses make run", func(t *testing.T) {
		opts := compiledOpts()
		opts.Mode = ModeNative
		spec, _ := Cpp(opts)
		if spec.Cmd.Args[0] != "make" || spec.Cmd.Args[1] != "run" {
			t.Errorf("args = %v, want make run", spec.Cmd.Args)
		}
	})
	t.Run("docker", func(t *testing.T) {
		opts := compiledOpts()
		opts.Mode = ModeDocker
		spec, _ := Cpp(opts)
		if !strings.Contains(strings.Join(spec.Cmd.Args, " "), cppDockerImage) {
			t.Errorf("missing image: %v", spec.Cmd.Args)
		}
	})
}

func TestScala(t *testing.T) {
	t.Run("native", func(t *testing.T) {
		opts := compiledOpts()
		opts.Mode = ModeNative
		spec, _ := Scala(opts)
		if spec.Cmd.Args[0] != "sbt" || spec.Cmd.Args[1] != "run" {
			t.Errorf("args = %v", spec.Cmd.Args)
		}
		if len(spec.ExtraEnv) != 0 {
			t.Errorf("non-debug should have no JDWP env: %v", spec.ExtraEnv)
		}
	})
	t.Run("native debug adds JDWP agent env", func(t *testing.T) {
		opts := compiledOpts()
		opts.Mode = ModeNative
		opts.Debug = true
		spec, _ := Scala(opts)
		if len(spec.ExtraEnv) == 0 || !strings.Contains(spec.ExtraEnv[0], "address=*:5005") {
			t.Errorf("JDWP agent missing: %v", spec.ExtraEnv)
		}
	})
}

func TestHaskell(t *testing.T) {
	t.Run("native", func(t *testing.T) {
		opts := compiledOpts()
		opts.Mode = ModeNative
		spec, _ := Haskell(opts)
		if spec.Cmd.Args[0] != "cabal" || spec.Cmd.Args[1] != "run" {
			t.Errorf("args = %v", spec.Cmd.Args)
		}
	})
	t.Run("debug not supported", func(t *testing.T) {
		opts := compiledOpts()
		opts.Mode = ModeNative
		opts.Debug = true
		_, err := Haskell(opts)
		if err == nil {
			t.Error("expected error for --debug on haskell")
		}
	})
	t.Run("docker", func(t *testing.T) {
		opts := compiledOpts()
		opts.Mode = ModeDocker
		spec, _ := Haskell(opts)
		if !strings.Contains(strings.Join(spec.Cmd.Args, " "), haskellDockerImage) {
			t.Errorf("missing image: %v", spec.Cmd.Args)
		}
	})
}
