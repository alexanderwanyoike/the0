package execute

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/assert"
	"github.com/stretchr/testify/require"
)

// botFixture is a bot directory laid out the way the runtime mounts one, plus
// the interpreter and wrapper that run it.
type botFixture struct {
	interpreter string
	wrapper     string
	script      string
	dir         string
}

type wrapperRun struct {
	exitCode int
	output   string
	result   map[string]any
}

// errorLogMessages returns the messages of the structured error lines in the
// run's output, the format the frontend shows as error-level logs.
func (r wrapperRun) errorLogMessages() []string {
	var messages []string
	for _, line := range strings.Split(r.output, "\n") {
		var entry map[string]any
		if json.Unmarshal([]byte(line), &entry) != nil || entry["level"] != "error" {
			continue
		}
		if message, ok := entry["message"].(string); ok {
			messages = append(messages, message)
		}
	}
	return messages
}

func requireInterpreter(t *testing.T, name string) string {
	t.Helper()
	path, err := exec.LookPath(name)
	if err != nil {
		t.Skipf("%s not available: %v", name, err)
	}
	return path
}

func wrapperPath(t *testing.T, name string) string {
	t.Helper()
	path, err := filepath.Abs(filepath.Join("..", "..", "wrappers", name))
	require.NoError(t, err)
	return path
}

func writeBotFile(t *testing.T, path, content string) {
	t.Helper()
	require.NoError(t, os.MkdirAll(filepath.Dir(path), 0755))
	require.NoError(t, os.WriteFile(path, []byte(content), 0644))
}

// pythonBot lays out main.py next to a copy of the Python SDK, the same way a
// deployed bot vendors it.
func pythonBot(t *testing.T, mainPy string) botFixture {
	t.Helper()
	interpreter := requireInterpreter(t, "python3")

	sdkDir, err := filepath.Abs(filepath.Join("..", "..", "..", "sdk", "python", "the0"))
	require.NoError(t, err)
	sdkFiles, err := filepath.Glob(filepath.Join(sdkDir, "*.py"))
	require.NoError(t, err)
	if len(sdkFiles) == 0 {
		t.Skipf("Python SDK not found at %s", sdkDir)
	}

	dir := t.TempDir()
	for _, src := range sdkFiles {
		content, err := os.ReadFile(src)
		require.NoError(t, err)
		writeBotFile(t, filepath.Join(dir, "the0", filepath.Base(src)), string(content))
	}
	writeBotFile(t, filepath.Join(dir, "main.py"), mainPy)

	return botFixture{interpreter: interpreter, wrapper: wrapperPath(t, "python_bot.py"), script: "main.py", dir: dir}
}

// nodeSDKStandIn has the result.json contract of the0-node's success(),
// error() and result() (sdk/nodejs/src/index.ts), which cannot be loaded here
// without a TypeScript build.
const nodeSDKStandIn = `const fs = require('fs');

function writeResult(result) {
    fs.writeFileSync('/' + (process.env.CODE_MOUNT_DIR || 'bot') + '/result.json', JSON.stringify(result));
}

exports.success = function (message, data) {
    writeResult({ status: 'success', message, ...(data && { data }) });
};

exports.error = function (message, data) {
    writeResult({ status: 'error', message, ...(data && { data }) });
    process.exit(1);
};

exports.result = function (result) {
    writeResult(result);
};
`

func nodeBot(t *testing.T, mainJs string) botFixture {
	t.Helper()
	interpreter := requireInterpreter(t, "node")

	dir := t.TempDir()
	writeBotFile(t, filepath.Join(dir, "node_modules", "the0-node", "index.js"), nodeSDKStandIn)
	writeBotFile(t, filepath.Join(dir, "main.js"), mainJs)

	return botFixture{interpreter: interpreter, wrapper: wrapperPath(t, "node_bot.js"), script: "main.js", dir: dir}
}

func runWrapper(t *testing.T, bot botFixture, earlierResult string) wrapperRun {
	t.Helper()
	resultPath := filepath.Join(bot.dir, "result.json")
	if earlierResult != "" {
		writeBotFile(t, resultPath, earlierResult)
	}

	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Second)
	defer cancel()

	cmd := exec.CommandContext(ctx, bot.interpreter, bot.wrapper)
	cmd.Dir = bot.dir
	cmd.Env = append(os.Environ(),
		"CODE_MOUNT_DIR="+strings.TrimPrefix(bot.dir, "/"),
		"STATE_DIR="+t.TempDir(),
		"BOT_ID=wrapper-test-bot",
		"BOT_CONFIG={}",
		"SCRIPT_PATH="+bot.script,
	)
	var output bytes.Buffer
	cmd.Stdout = &output
	cmd.Stderr = &output

	run := wrapperRun{}
	err := cmd.Run()
	require.NoError(t, ctx.Err(), "wrapper did not finish in time; output:\n%s", output.String())
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) {
		run.exitCode = exitErr.ExitCode()
	} else {
		require.NoError(t, err)
	}
	run.output = output.String()

	content, err := os.ReadFile(resultPath)
	if err == nil {
		require.NoError(t, json.Unmarshal(content, &run.result), "result.json is not JSON: %s", content)
	} else {
		require.ErrorIs(t, err, os.ErrNotExist)
	}
	return run
}

type failedRun struct {
	name     string
	bot      func(t *testing.T) botFixture
	exitCode int
	message  string
	// sdkErrorLines are the messages of the error log lines the bot's own SDK
	// prints during the run.
	sdkErrorLines []string
}

func failedRuns() []failedRun {
	return []failedRun{
		{
			name: "python bot calls the0.error",
			bot: func(t *testing.T) botFixture {
				return pythonBot(t, `import the0

def main(bot_id, config):
    the0.error("broker rejected the order")
`)
			},
			exitCode:      1,
			message:       "broker rejected the order",
			sdkErrorLines: []string{"broker rejected the order"},
		},
		{
			name: "python bot raises",
			bot: func(t *testing.T) botFixture {
				return pythonBot(t, `def main(bot_id, config):
    raise RuntimeError("exchange unreachable")
`)
			},
			exitCode: 1,
			message:  "exchange unreachable",
		},
		{
			name: "python bot writes an error result and returns",
			bot: func(t *testing.T) botFixture {
				return pythonBot(t, `import the0

def main(bot_id, config):
    the0.result({"status": "error", "message": "position limit breached"})
`)
			},
			exitCode: 0,
			message:  "position limit breached",
		},
		{
			name: "python bot exits with its own code",
			bot: func(t *testing.T) botFixture {
				return pythonBot(t, `import sys

def main(bot_id, config):
    sys.exit(3)
`)
			},
			exitCode: 3,
			message:  "Bot exited with code 3",
		},
		{
			name: "python bot writes an error result and exits with its own code",
			bot: func(t *testing.T) botFixture {
				return pythonBot(t, `import sys
import the0

def main(bot_id, config):
    the0.result({"status": "error", "message": "position limit breached"})
    sys.exit(3)
`)
			},
			exitCode: 3,
			message:  "position limit breached",
		},
		{
			name: "node bot calls error",
			bot: func(t *testing.T) botFixture {
				return nodeBot(t, `const { error } = require('the0-node');

exports.main = async function (botId, config) {
    error('broker rejected the order');
};
`)
			},
			exitCode: 1,
			message:  "broker rejected the order",
		},
		{
			name: "node bot rejects",
			bot: func(t *testing.T) botFixture {
				return nodeBot(t, `exports.main = async function (botId, config) {
    throw new Error('exchange unreachable');
};
`)
			},
			exitCode: 1,
			message:  "exchange unreachable",
		},
		{
			name: "node bot writes an error result and returns",
			bot: func(t *testing.T) botFixture {
				return nodeBot(t, `const { result } = require('the0-node');

exports.main = async function (botId, config) {
    result({ status: 'error', message: 'position limit breached' });
};
`)
			},
			exitCode: 0,
			message:  "position limit breached",
		},
		{
			name: "node bot returns an error result",
			bot: func(t *testing.T) botFixture {
				return nodeBot(t, `exports.main = async function (botId, config) {
    return { status: 'error', message: 'position limit breached' };
};
`)
			},
			exitCode: 0,
			message:  "position limit breached",
		},
		{
			name: "node bot exits with its own code",
			bot: func(t *testing.T) botFixture {
				return nodeBot(t, `exports.main = async function (botId, config) {
    process.exit(3);
};
`)
			},
			exitCode: 3,
			message:  "Bot exited with code 3",
		},
		{
			name: "node bot writes an error result and exits with its own code",
			bot: func(t *testing.T) botFixture {
				return nodeBot(t, `const { result } = require('the0-node');

exports.main = async function (botId, config) {
    result({ status: 'error', message: 'position limit breached' });
    process.exit(3);
};
`)
			},
			exitCode: 3,
			message:  "position limit breached",
		},
	}
}

// A wrapper exits with the bot's own code, so an error result and a normal
// return exit 0 here: the runner fails that run, for every runtime alike.
func TestBotWrapper_ExitCodeAndResultOfAFailedRun(t *testing.T) {
	for _, tc := range failedRuns() {
		t.Run(tc.name, func(t *testing.T) {
			run := runWrapper(t, tc.bot(t), "")

			assert.Equal(t, tc.exitCode, run.exitCode, "output:\n%s", run.output)
			assert.Equal(t, "error", run.result["status"])
			assert.Equal(t, tc.message, run.result["message"])
		})
	}
}

// The wrappers print no error line of their own: the runner prints the run's
// summary line, so a wrapper's line would repeat the SDK's error() line.
func TestBotWrapper_ErrorLogLinesOfAFailedRun(t *testing.T) {
	for _, tc := range failedRuns() {
		t.Run(tc.name, func(t *testing.T) {
			run := runWrapper(t, tc.bot(t), "")

			assert.Equal(t, tc.sdkErrorLines, run.errorLogMessages(), "output:\n%s", run.output)
		})
	}
}

func TestBotWrapper_KeepsTheResultTheBotWrote(t *testing.T) {
	cases := []struct {
		name string
		bot  func(t *testing.T) botFixture
	}{
		{
			name: "python bot calls the0.success with data",
			bot: func(t *testing.T) botFixture {
				return pythonBot(t, `import the0

def main(bot_id, config):
    the0.success("order filled", {"order_id": "o-42"})
`)
			},
		},
		{
			name: "node bot calls success with data and returns nothing",
			bot: func(t *testing.T) botFixture {
				return nodeBot(t, `const { success } = require('the0-node');

exports.main = async function (botId, config) {
    success('order filled', { order_id: 'o-42' });
};
`)
			},
		},
		{
			name: "node bot returns its result",
			bot: func(t *testing.T) botFixture {
				return nodeBot(t, `exports.main = async function (botId, config) {
    return { status: 'success', message: 'order filled', data: { order_id: 'o-42' } };
};
`)
			},
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			run := runWrapper(t, tc.bot(t), "")

			assert.Equal(t, 0, run.exitCode, "output:\n%s", run.output)
			assert.Equal(t, map[string]any{
				"status":  "success",
				"message": "order filled",
				"data":    map[string]any{"order_id": "o-42"},
			}, run.result)
			assert.Empty(t, run.errorLogMessages())
		})
	}
}

func TestBotWrapper_PlainReturnRecordsTheDefaultSuccess(t *testing.T) {
	cases := []struct {
		name string
		bot  func(t *testing.T) botFixture
	}{
		{
			name: "python",
			bot: func(t *testing.T) botFixture {
				return pythonBot(t, `def main(bot_id, config):
    return None
`)
			},
		},
		{
			name: "node",
			bot: func(t *testing.T) botFixture {
				return nodeBot(t, `exports.main = async function (botId, config) {};
`)
			},
		},
	}

	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			run := runWrapper(t, tc.bot(t), "")

			assert.Equal(t, 0, run.exitCode, "output:\n%s", run.output)
			assert.Equal(t, map[string]any{"status": "success", "message": "Bot executed successfully"}, run.result)
			assert.Empty(t, run.errorLogMessages())
		})
	}

	t.Run("result left from an earlier run is not taken as this run's", func(t *testing.T) {
		for _, tc := range cases {
			t.Run(tc.name, func(t *testing.T) {
				run := runWrapper(t, tc.bot(t), `{"status": "error", "message": "left from an earlier run"}`)

				assert.Equal(t, 0, run.exitCode, "output:\n%s", run.output)
				assert.Equal(t, map[string]any{"status": "success", "message": "Bot executed successfully"}, run.result)
				assert.Empty(t, run.errorLogMessages())
			})
		}
	})
}
