package dev

import (
	"encoding/json"
	"strings"
	"testing"
)

// argvContainsPair reports whether argv contains a consecutive (flag, value)
// pair — e.g. ("-e", "BOT_ID=x"). Needed because argv ordering is prescribed
// by docker semantics but which adjacent pair we assert about isn't.
func argvContainsPair(argv []string, flag, value string) bool {
	for i := 0; i < len(argv)-1; i++ {
		if argv[i] == flag && argv[i+1] == value {
			return true
		}
	}
	return false
}

// argvContains reports whether argv contains a single value.
func argvContains(argv []string, v string) bool {
	for _, a := range argv {
		if a == v {
			return true
		}
	}
	return false
}

// argvContainsSubstring reports whether any argv entry contains sub.
func argvContainsSubstring(argv []string, sub string) bool {
	for _, a := range argv {
		if strings.Contains(a, sub) {
			return true
		}
	}
	return false
}

func baseRunParams() RunParams {
	return RunParams{
		BotID:      "smoke",
		Runtime:    RuntimePython,
		Entrypoint: "main.py",
		CodeDir:    "/host/bot",
		StateDir:   "/host/state",
		BotConfig:  json.RawMessage(`{"name":"smoke"}`),
	}
}

func TestBuildRunArgs_Python_Basic(t *testing.T) {
	args, err := BuildRunArgs(baseRunParams())
	if err != nil {
		t.Fatalf("BuildRunArgs: %v", err)
	}

	if args[0] != "run" {
		t.Errorf("args[0] = %q, want \"run\"", args[0])
	}
	for _, want := range []string{"--rm", "--init"} {
		if !argvContains(args, want) {
			t.Errorf("missing %q in argv: %v", want, args)
		}
	}
	pairs := []struct{ flag, value string }{
		{"-v", "/host/bot:/bot"},
		{"-v", "/host/state:/state"},
		{"-e", "BOT_ID=smoke"},
		{"-e", `BOT_CONFIG={"name":"smoke"}`},
		{"-e", "RUNTIME=python3.11"},
		{"-e", "ENTRYPOINT=main.py"},
		{"-e", "STATE_DIR=/state"},
	}
	for _, p := range pairs {
		if !argvContainsPair(args, p.flag, p.value) {
			t.Errorf("missing pair %q %q in argv: %v", p.flag, p.value, args)
		}
	}
	for _, want := range []string{RuntimeImage, "execute", "--skip-init", "--skip-sync"} {
		if !argvContains(args, want) {
			t.Errorf("missing %q in argv: %v", want, args)
		}
	}
	if argvContains(args, "--skip-query-server") {
		t.Errorf("--skip-query-server must NOT be passed: we want queries to work in dev")
	}
}

func TestBuildRunArgs_Node_MapsToNodejs20(t *testing.T) {
	p := baseRunParams()
	p.Runtime = RuntimeNode
	p.Entrypoint = "main.js"
	args, err := BuildRunArgs(p)
	if err != nil {
		t.Fatalf("BuildRunArgs: %v", err)
	}
	if !argvContainsPair(args, "-e", "RUNTIME=nodejs20") {
		t.Errorf("RUNTIME=nodejs20 missing: %v", args)
	}
	if !argvContainsPair(args, "-e", "ENTRYPOINT=main.js") {
		t.Errorf("ENTRYPOINT=main.js missing: %v", args)
	}
}

func TestBuildRunArgs_UnsupportedRuntime_Errors(t *testing.T) {
	p := baseRunParams()
	p.Runtime = Runtime("rust")
	_, err := BuildRunArgs(p)
	if err == nil {
		t.Fatal("expected error for unsupported runtime")
	}
	if !strings.Contains(err.Error(), "Python and Node") {
		t.Errorf("error %q should mention Python and Node; phase-2 guidance for users", err)
	}
}

func TestBuildRunArgs_Debug_ForwardsPortAndPassesFlag(t *testing.T) {
	p := baseRunParams()
	p.Debug = true
	p.DebugPort = 5678
	args, err := BuildRunArgs(p)
	if err != nil {
		t.Fatalf("BuildRunArgs: %v", err)
	}
	if !argvContainsPair(args, "-p", "127.0.0.1:5678:5678") {
		t.Errorf("expected loopback port-forward for debug: %v", args)
	}
	if !argvContains(args, "--debug-port") {
		t.Errorf("missing --debug-port flag: %v", args)
	}
	if !argvContains(args, "5678") {
		t.Errorf("missing debug port value 5678: %v", args)
	}
}

func TestBuildRunArgs_Debug_DefaultPythonPort(t *testing.T) {
	p := baseRunParams()
	p.Debug = true // DebugPort = 0 → default
	args, err := BuildRunArgs(p)
	if err != nil {
		t.Fatal(err)
	}
	if !argvContainsPair(args, "-p", "127.0.0.1:5678:5678") {
		t.Errorf("expected Python default port 5678: %v", args)
	}
}

func TestBuildRunArgs_Debug_DefaultNodePort(t *testing.T) {
	p := baseRunParams()
	p.Runtime = RuntimeNode
	p.Entrypoint = "main.js"
	p.Debug = true
	args, err := BuildRunArgs(p)
	if err != nil {
		t.Fatal(err)
	}
	if !argvContainsPair(args, "-p", "127.0.0.1:9229:9229") {
		t.Errorf("expected Node default port 9229: %v", args)
	}
}

func TestBuildRunArgs_DebugWait_PassesFlag(t *testing.T) {
	p := baseRunParams()
	p.Debug = true
	p.DebugWait = true
	args, err := BuildRunArgs(p)
	if err != nil {
		t.Fatal(err)
	}
	if !argvContains(args, "--debug-wait") {
		t.Errorf("missing --debug-wait: %v", args)
	}
}

func TestBuildRunArgs_NoDebug_NoPortForwardNoDebugFlag(t *testing.T) {
	args, err := BuildRunArgs(baseRunParams()) // Debug=false
	if err != nil {
		t.Fatal(err)
	}
	if argvContainsSubstring(args, "5678") {
		t.Errorf("no debug port forward when Debug=false: %v", args)
	}
	if argvContains(args, "--debug-port") {
		t.Errorf("no --debug-port flag when Debug=false: %v", args)
	}
}

func TestBuildRunArgs_Realtime_ForwardsQueryPort(t *testing.T) {
	p := baseRunParams()
	p.BotType = "realtime"
	p.QueryEntrypoint = "query.py"
	args, err := BuildRunArgs(p)
	if err != nil {
		t.Fatal(err)
	}
	if !argvContainsPair(args, "-p", "127.0.0.1:9476:9476") {
		t.Errorf("query port must forward for realtime bots: %v", args)
	}
	if !argvContainsPair(args, "-e", "BOT_TYPE=realtime") {
		t.Errorf("missing BOT_TYPE=realtime: %v", args)
	}
	if !argvContainsPair(args, "-e", "QUERY_ENTRYPOINT=query.py") {
		t.Errorf("missing QUERY_ENTRYPOINT=query.py: %v", args)
	}
}

func TestBuildRunArgs_Scheduled_NoQueryPortForward(t *testing.T) {
	p := baseRunParams()
	p.BotType = "scheduled"
	args, err := BuildRunArgs(p)
	if err != nil {
		t.Fatal(err)
	}
	if argvContainsSubstring(args, "9476") {
		t.Errorf("no query port forward for scheduled bots: %v", args)
	}
}

func TestBuildRunArgs_RealtimeWithoutQueryEntrypoint_NoQueryForward(t *testing.T) {
	p := baseRunParams()
	p.BotType = "realtime"
	// QueryEntrypoint empty
	args, err := BuildRunArgs(p)
	if err != nil {
		t.Fatal(err)
	}
	if argvContainsSubstring(args, "9476") {
		t.Errorf("no query forward when there's no query entrypoint: %v", args)
	}
}

// TestBuildRunArgs_ArgvOrder_ImagePrecedesExecute sanity-checks that the
// image name appears before "execute" so docker parses the argv correctly.
func TestBuildRunArgs_ArgvOrder_ImagePrecedesExecute(t *testing.T) {
	args, err := BuildRunArgs(baseRunParams())
	if err != nil {
		t.Fatal(err)
	}
	imageIdx, execIdx := -1, -1
	for i, a := range args {
		if a == RuntimeImage {
			imageIdx = i
		}
		if a == "execute" {
			execIdx = i
		}
	}
	if imageIdx < 0 || execIdx < 0 {
		t.Fatalf("image and execute must both appear: %v", args)
	}
	if execIdx < imageIdx {
		t.Errorf("execute must follow image in argv: image@%d execute@%d", imageIdx, execIdx)
	}
}
