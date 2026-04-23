package dev

import (
	"encoding/json"
	"fmt"
	"strconv"
)

// RuntimeImage is the container image delegating bot execution. It ships
// all 7 language runtimes, the wrappers, and the `runtime execute` binary;
// `the0 dev` invokes it with flags that skip MinIO sync for local dev.
const RuntimeImage = "the0/runtime:latest"

// QueryPort is the in-container port on which the runtime's query server
// subprocess listens when BOT_TYPE=realtime + QUERY_ENTRYPOINT are set.
// The frontend server proxies /query/* → 127.0.0.1:QueryPort on the host.
const QueryPort = 9476

// RunParams describes a one-shot bot run via the runtime image.
type RunParams struct {
	BotID           string
	Runtime         Runtime // RuntimePython | RuntimeNode
	Entrypoint      string  // "main.py", "main.js"
	BotType         string  // "realtime" | "scheduled" | ""
	QueryEntrypoint string  // optional — enables query server when BotType=realtime

	BotConfig json.RawMessage

	CodeDir  string // absolute host path → bind-mounted at /bot
	StateDir string // absolute host path → bind-mounted at /state

	Debug     bool
	DebugPort int // 0 = use language default (5678 py, 9229 node)
	DebugWait bool
}

// BuildRunArgs returns the argv that follows `docker` — i.e. invoking
// `docker <BuildRunArgs...>` launches the bot via the runtime image.
// Errors for unsupported runtimes (compiled langs are phase 2).
//
// Design notes:
//
//   - Port forwards bind to 127.0.0.1 only: dev traffic is local-machine
//     only; LAN peers and cross-origin pages cannot reach the bot.
//
//   - Query port 9476 is forwarded iff the runtime would spawn the query
//     server subprocess (realtime + query entrypoint). Mirrors the
//     runtime's own `shouldStartQueryServer` decision.
//
//   - STATE_DIR is exported as `/state` explicitly so the SDK writes
//     state files at the mount root rather than a nested `.the0-state`
//     subdir (the runtime's default when STATE_DIR is unset — see
//     runtime/internal/execute/helpers.go BuildBotEnv).
//
//   - --skip-query-server is intentionally NOT passed: debugging the
//     bot/dashboard query integration is a primary reason to run
//     `the0 dev`; disabling it would defeat the tool.
func BuildRunArgs(p RunParams) ([]string, error) {
	runtimeEnv, err := runtimeEnvValue(p.Runtime)
	if err != nil {
		return nil, err
	}

	debugPort := 0
	if p.Debug {
		debugPort = p.DebugPort
		if debugPort == 0 {
			debugPort = defaultDebugPort(p.Runtime)
		}
	}

	args := []string{"run", "--rm", "-i", "--init"}

	args = append(args, "-v", fmt.Sprintf("%s:/bot", p.CodeDir))
	args = append(args, "-v", fmt.Sprintf("%s:/state", p.StateDir))

	args = append(args,
		"-e", "BOT_ID="+p.BotID,
		"-e", "BOT_CONFIG="+string(p.BotConfig),
		"-e", "RUNTIME="+runtimeEnv,
		"-e", "ENTRYPOINT="+p.Entrypoint,
		"-e", "STATE_DIR=/state",
	)
	if p.BotType != "" {
		args = append(args, "-e", "BOT_TYPE="+p.BotType)
	}
	if p.QueryEntrypoint != "" {
		args = append(args, "-e", "QUERY_ENTRYPOINT="+p.QueryEntrypoint)
	}

	if p.BotType == "realtime" && p.QueryEntrypoint != "" {
		args = append(args, "-p", fmt.Sprintf("127.0.0.1:%d:%d", QueryPort, QueryPort))
	}
	if debugPort > 0 {
		args = append(args, "-p", fmt.Sprintf("127.0.0.1:%d:%d", debugPort, debugPort))
	}

	args = append(args, "--entrypoint", "/app/runtime", RuntimeImage)
	args = append(args, "execute", "--skip-init", "--skip-sync")
	if debugPort > 0 {
		args = append(args, "--debug-port", strconv.Itoa(debugPort))
		if p.DebugWait {
			args = append(args, "--debug-wait")
		}
	}

	return args, nil
}

// runtimeEnvValue maps a dev.Runtime to the RUNTIME env value the runtime
// image's BuildBotCommand switch dispatches on.
func runtimeEnvValue(rt Runtime) (string, error) {
	switch rt {
	case RuntimePython:
		return "python3.11", nil
	case RuntimeNode:
		return "nodejs20", nil
	}
	return "", fmt.Errorf("runtime %q not supported in v1 (Python and Node only; compiled languages coming in phase 2)", rt)
}

// defaultDebugPort returns the conventional debugger port per runtime.
// Python debugpy defaults to 5678; Node Inspector to 9229.
func defaultDebugPort(rt Runtime) int {
	switch rt {
	case RuntimePython:
		return 5678
	case RuntimeNode:
		return 9229
	}
	return 0
}
