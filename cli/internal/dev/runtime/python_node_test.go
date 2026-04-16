package runtime

import (
	"strings"
	"testing"
)

func baseOpts() Opts {
	return Opts{
		Cwd:      "/home/user/bot",
		Script:   "main.py",
		BotID:    "abc",
		CodeDir:  "/home/user/.the0/dev/abc/code",
		StateDir: "/home/user/.the0/dev/abc/state",
	}
}

func TestPython_NativeBasic(t *testing.T) {
	opts := baseOpts()
	opts.Mode = ModeNative

	spec, err := Python(opts)
	if err != nil {
		t.Fatalf("Python: %v", err)
	}
	if spec.Cmd.Dir != "/home/user/bot" {
		t.Errorf("Dir = %q", spec.Cmd.Dir)
	}
	last := spec.Cmd.Args[len(spec.Cmd.Args)-1]
	if last != "/home/user/bot/main.py" {
		t.Errorf("last arg = %q, want script path", last)
	}
	// No debugpy in the chain
	if strings.Contains(strings.Join(spec.Cmd.Args, " "), "debugpy") {
		t.Errorf("debugpy leaked into non-debug native invocation: %v", spec.Cmd.Args)
	}
}

func TestPython_NativeDebug(t *testing.T) {
	opts := baseOpts()
	opts.Mode = ModeNative
	opts.Debug = true

	spec, err := Python(opts)
	if err != nil {
		t.Fatalf("Python: %v", err)
	}
	joined := strings.Join(spec.Cmd.Args, " ")
	if !strings.Contains(joined, "-m debugpy") {
		t.Errorf("expected debugpy in args: %v", spec.Cmd.Args)
	}
	if !strings.Contains(joined, "--listen 0.0.0.0:5678") {
		t.Errorf("expected port 5678: %v", spec.Cmd.Args)
	}
	if strings.Contains(joined, "--wait-for-client") {
		t.Errorf("--wait-for-client should only appear with DebugWait")
	}
}

func TestPython_NativeDebugWait(t *testing.T) {
	opts := baseOpts()
	opts.Mode = ModeNative
	opts.Debug = true
	opts.DebugWait = true

	spec, _ := Python(opts)
	if !strings.Contains(strings.Join(spec.Cmd.Args, " "), "--wait-for-client") {
		t.Errorf("expected --wait-for-client: %v", spec.Cmd.Args)
	}
}

func TestPython_CustomDebugPort(t *testing.T) {
	opts := baseOpts()
	opts.Mode = ModeNative
	opts.Debug = true
	opts.DebugPort = 7777

	spec, _ := Python(opts)
	if !strings.Contains(strings.Join(spec.Cmd.Args, " "), "0.0.0.0:7777") {
		t.Errorf("expected custom port 7777: %v", spec.Cmd.Args)
	}
}

func TestPython_DockerMountsProjectAndState(t *testing.T) {
	opts := baseOpts()
	opts.Mode = ModeDocker

	spec, err := Python(opts)
	if err != nil {
		t.Fatalf("Python: %v", err)
	}
	args := strings.Join(spec.Cmd.Args, " ")
	if !strings.Contains(args, "docker run") {
		t.Errorf("expected docker run: %v", spec.Cmd.Args)
	}
	if !strings.Contains(args, "-v /home/user/bot:/home/user/bot") {
		t.Errorf("missing project mount: %v", spec.Cmd.Args)
	}
	if !strings.Contains(args, "-v /home/user/.the0/dev/abc/state:/home/user/.the0/dev/abc/state") {
		t.Errorf("missing state mount: %v", spec.Cmd.Args)
	}
	if !strings.Contains(args, pythonDockerImage) {
		t.Errorf("missing image: %v", spec.Cmd.Args)
	}
}

func TestPython_DockerDebugForwardsPort(t *testing.T) {
	opts := baseOpts()
	opts.Mode = ModeDocker
	opts.Debug = true

	spec, _ := Python(opts)
	args := strings.Join(spec.Cmd.Args, " ")
	if !strings.Contains(args, "-p 5678:5678") {
		t.Errorf("expected port forward 5678:5678: %v", spec.Cmd.Args)
	}
	if !strings.Contains(args, "debugpy") {
		t.Errorf("expected debugpy install+launch in container: %v", spec.Cmd.Args)
	}
}

func TestNode_NativeBasic(t *testing.T) {
	opts := baseOpts()
	opts.Mode = ModeNative
	opts.Script = "main.js"

	spec, err := Node(opts)
	if err != nil {
		t.Fatalf("Node: %v", err)
	}
	if spec.Cmd.Args[0] != "node" {
		t.Errorf("Args[0] = %q, want node", spec.Cmd.Args[0])
	}
	if spec.Cmd.Args[1] != "/home/user/bot/main.js" {
		t.Errorf("Args[1] = %q", spec.Cmd.Args[1])
	}
}

func TestNode_NativeDebug(t *testing.T) {
	opts := baseOpts()
	opts.Mode = ModeNative
	opts.Script = "main.js"
	opts.Debug = true

	spec, _ := Node(opts)
	joined := strings.Join(spec.Cmd.Args, " ")
	if !strings.Contains(joined, "--inspect=127.0.0.1:9229") {
		t.Errorf("expected --inspect on 127.0.0.1:9229: %v", spec.Cmd.Args)
	}
	if strings.Contains(joined, "--inspect-brk") {
		t.Errorf("--inspect-brk should only appear with DebugWait")
	}
}

func TestNode_NativeDebugWait(t *testing.T) {
	opts := baseOpts()
	opts.Mode = ModeNative
	opts.Script = "main.js"
	opts.Debug = true
	opts.DebugWait = true

	spec, _ := Node(opts)
	if !strings.Contains(strings.Join(spec.Cmd.Args, " "), "--inspect-brk=127.0.0.1:9229") {
		t.Errorf("expected --inspect-brk: %v", spec.Cmd.Args)
	}
}

func TestNode_DockerMountsAndImage(t *testing.T) {
	opts := baseOpts()
	opts.Mode = ModeDocker
	opts.Script = "main.js"

	spec, _ := Node(opts)
	args := strings.Join(spec.Cmd.Args, " ")
	if !strings.Contains(args, nodeDockerImage) {
		t.Errorf("missing image: %v", spec.Cmd.Args)
	}
	if !strings.Contains(args, "-v /home/user/bot:/home/user/bot") {
		t.Errorf("missing mount: %v", spec.Cmd.Args)
	}
}

func TestDispatcher_UnknownModeErrors(t *testing.T) {
	opts := baseOpts()
	opts.Mode = Mode("wibble")
	if _, err := Python(opts); err == nil {
		t.Error("expected error for unknown mode")
	}
	if _, err := Node(opts); err == nil {
		t.Error("expected error for unknown mode")
	}
}

func TestPython_DefaultScriptName(t *testing.T) {
	opts := baseOpts()
	opts.Mode = ModeNative
	opts.Script = ""

	spec, _ := Python(opts)
	last := spec.Cmd.Args[len(spec.Cmd.Args)-1]
	if !strings.HasSuffix(last, "/main.py") {
		t.Errorf("default script should be main.py, got %q", last)
	}
}

func TestNode_DefaultScriptName(t *testing.T) {
	opts := baseOpts()
	opts.Mode = ModeNative
	opts.Script = ""

	spec, _ := Node(opts)
	last := spec.Cmd.Args[len(spec.Cmd.Args)-1]
	if !strings.HasSuffix(last, "/main.js") {
		t.Errorf("default script should be main.js, got %q", last)
	}
}
