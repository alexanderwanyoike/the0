package dev

import (
	"encoding/json"
	"strings"
	"testing"
)

func argvContainsPair(argv []string, flag, value string) bool {
	for i := 0; i < len(argv)-1; i++ {
		if argv[i] == flag && argv[i+1] == value {
			return true
		}
	}
	return false
}

func argvContains(argv []string, v string) bool {
	for _, a := range argv {
		if a == v {
			return true
		}
	}
	return false
}

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

// TestBuildRunArgs_Python_Basic covers the env contract, mount shape,
// and flag list for a plain Python bot.
func TestBuildRunArgs_Python_Basic(t *testing.T) {
	args, err := BuildRunArgs(baseRunParams())
	if err != nil {
		t.Fatalf("BuildRunArgs: %v", err)
	}

	if args[0] != "run" {
		t.Errorf("args[0] = %q", args[0])
	}
	for _, want := range []string{"--rm", "--init", RuntimeImage, "execute", "--skip-init", "--skip-sync"} {
		if !argvContains(args, want) {
			t.Errorf("missing %q in argv: %v", want, args)
		}
	}
	if argvContains(args, "--skip-query-server") {
		t.Errorf("--skip-query-server must NOT be passed: queries must work in dev")
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
		t.Errorf("error %q should mention Python and Node for phase-2 guidance", err)
	}
}

// TestBuildRunArgs_Debug covers the debug code path as a table: explicit
// port, default Python port, default Node port, and the --debug-wait
// propagation.
func TestBuildRunArgs_Debug(t *testing.T) {
	cases := []struct {
		name    string
		runtime Runtime
		port    int
		wait    bool
		wantFwd string
	}{
		{"python explicit", RuntimePython, 5678, false, "127.0.0.1:5678:5678"},
		{"python default", RuntimePython, 0, false, "127.0.0.1:5678:5678"},
		{"node default", RuntimeNode, 0, false, "127.0.0.1:9229:9229"},
		{"python wait", RuntimePython, 0, true, "127.0.0.1:5678:5678"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			p := baseRunParams()
			p.Runtime = tc.runtime
			p.Debug = true
			p.DebugPort = tc.port
			p.DebugWait = tc.wait

			args, err := BuildRunArgs(p)
			if err != nil {
				t.Fatalf("BuildRunArgs: %v", err)
			}
			if !argvContainsPair(args, "-p", tc.wantFwd) {
				t.Errorf("missing port forward %q in argv: %v", tc.wantFwd, args)
			}
			if !argvContains(args, "--debug-port") {
				t.Errorf("missing --debug-port flag")
			}
			hasWait := argvContains(args, "--debug-wait")
			if hasWait != tc.wait {
				t.Errorf("--debug-wait presence = %v, want %v", hasWait, tc.wait)
			}
		})
	}
}

// TestBuildRunArgs_NoForwardGuards collects the three "should not
// forward" cases: Debug=false adds no debug port; scheduled bot adds
// no query port; realtime without a query entrypoint adds no query
// port either.
func TestBuildRunArgs_NoForwardGuards(t *testing.T) {
	cases := []struct {
		name        string
		mutate      func(*RunParams)
		forbidSub   string
		description string
	}{
		{"Debug=false", func(p *RunParams) {}, "5678", "no debug forward"},
		{"Scheduled", func(p *RunParams) { p.BotType = "scheduled" }, "9476", "no query forward for scheduled"},
		{"Realtime no entrypoint", func(p *RunParams) { p.BotType = "realtime" }, "9476", "no query forward without query entrypoint"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			p := baseRunParams()
			tc.mutate(&p)
			args, err := BuildRunArgs(p)
			if err != nil {
				t.Fatal(err)
			}
			if argvContainsSubstring(args, tc.forbidSub) {
				t.Errorf("%s: argv should not contain %q: %v", tc.description, tc.forbidSub, args)
			}
		})
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
