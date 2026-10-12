package main

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"reflect"
	"sort"
	"strings"
	"testing"
	"time"

	"github.com/stretchr/testify/require"
)

// The runtime's side of the bot contract (docs/custom-bot-development/bot-contract.md),
// end to end. Python and Node.js bots built on the repo's own SDKs play the
// shared sdk/contract cases through `runtime execute` in the runtime image,
// with this tree's runtime binary and wrappers mounted where the image keeps
// its own, the way ContainerBuilder.WithDevRuntime mounts a dev binary.
//
// A case's expectations are written for an SDK on its own. runtimeExpect adds
// what the wrappers and the runner put on top, each addition being a rule of
// the contract page.

const (
	contractImageEnv     = "THE0_RUNTIME_IMAGE"
	defaultContractImage = "the0/runtime:latest"
	contractRunTimeout   = 2 * time.Minute
)

type contractLanguage struct {
	name            string
	runtime         string
	entrypoint      string
	queryEntrypoint string
	// prerequisite fails or skips the test when the language's SDK is not built.
	prerequisite func(t *testing.T)
	// layout puts the bot's files and its SDK in botDir, vendored the way a
	// deployed bot carries its dependencies.
	layout func(t *testing.T, botDir string)
}

func contractLanguages() []contractLanguage {
	return []contractLanguage{
		{
			name:            "python",
			runtime:         "python3.11",
			entrypoint:      "main.py",
			queryEntrypoint: "query.py",
			prerequisite:    func(t *testing.T) {},
			layout: func(t *testing.T, botDir string) {
				copyFiles(t, repoPath(t, "sdk", "python", "the0"), "*.py", filepath.Join(botDir, "the0"))
				copyFile(t, repoPath(t, "sdk", "python", "tests", "contract_bot.py"), filepath.Join(botDir, "contract_bot.py"))
				copyFiles(t, filepath.Join("testdata", "contract-bots", "python"), "*.py", botDir)
			},
		},
		{
			name:            "nodejs",
			runtime:         "nodejs22",
			entrypoint:      "main.js",
			queryEntrypoint: "query.js",
			prerequisite: func(t *testing.T) {
				if _, err := os.Stat(repoPath(t, "sdk", "nodejs", "dist", "index.js")); err != nil {
					needFor(t, "the Node.js SDK is not built (run yarn install and yarn build in sdk/nodejs): %v", err)
				}
			},
			layout: func(t *testing.T, botDir string) {
				sdkDir := filepath.Join(botDir, "node_modules", "the0-node")
				copyFile(t, repoPath(t, "sdk", "nodejs", "package.json"), filepath.Join(sdkDir, "package.json"))
				copyFiles(t, repoPath(t, "sdk", "nodejs", "dist"), "*", filepath.Join(sdkDir, "dist"))
				copyFiles(t, filepath.Join("testdata", "contract-bots", "node"), "*.js", botDir)
			},
		},
	}
}

type contractCase struct {
	name        string
	file        []byte
	Entrypoint  string                     `json:"entrypoint"`
	Env         map[string]string          `json:"env"`
	StateBefore map[string]json.RawMessage `json:"state_before"`
	Script      []map[string]any           `json:"script"`
	Requests    []any                      `json:"requests"`
	Expect      map[string]any             `json:"expect"`
}

func TestRuntimeExecute_FollowsTheBotContract(t *testing.T) {
	if testing.Short() {
		t.Skip("runs bots in the runtime image")
	}
	image := contractImage(t)
	runtimeBinary := buildRuntimeBinary(t)
	cases := loadContractCases(t, repoPath(t, "sdk", "contract", "cases"))

	for _, lang := range contractLanguages() {
		t.Run(lang.name, func(t *testing.T) {
			t.Parallel()
			lang.prerequisite(t)
			for _, c := range cases {
				t.Run(c.name, func(t *testing.T) {
					if reason := notOnALaunchPath(c); reason != "" {
						t.Skip(reason)
					}
					t.Parallel()
					want := runtimeExpect(t, c)
					run := runContractCase(t, image, runtimeBinary, lang, c)
					if problems := run.mismatches(want, c.Entrypoint); len(problems) > 0 {
						t.Errorf("%s\n\nexit code: %d\nstdout:\n%s\nstderr:\n%s\nbot.log:\n%s",
							strings.Join(problems, "\n"), run.exitCode, run.stdout, run.stderr, run.botLog)
					}
				})
			}
		})
	}
}

// notOnALaunchPath says why the runtime never starts the case's entrypoint the
// way the case does, or is empty when it does.
func notOnALaunchPath(c contractCase) string {
	if len(c.Requests) > 0 {
		return "query server mode: the SDK harnesses cover it"
	}
	if c.Entrypoint == "query" && c.Env["QUERY_PATH"] == "" {
		return "the runtime starts a query entrypoint with QUERY_PATH set, or as a realtime query server"
	}
	return ""
}

// runtimeExpect is what must come out of `runtime execute` for the case.
func runtimeExpect(t *testing.T, c contractCase) map[string]any {
	t.Helper()
	want := make(map[string]any, len(c.Expect))
	for key, value := range c.Expect {
		switch key {
		case "exit_code", "stdout", "stderr", "result", "query_result", "state_after":
			want[key] = value
		default:
			t.Fatalf("the case expects %q, which this check does not compare", key)
		}
	}
	if c.Entrypoint != "bot" {
		return want
	}

	// What the wrappers add: a result for a run whose bot wrote none.
	if result, ok := want["result"]; ok && result == nil {
		if isFailedExit(want["exit_code"]) {
			want["result"] = map[string]any{"status": "error", "message": uncaughtErrorMessage(t, c)}
		} else {
			want["result"] = map[string]any{"status": "success", "message": "Bot executed successfully"}
		}
	}
	errorMessage, isError := errorResultMessage(want["result"])
	// Exit codes: an error result fails a run that exits 0.
	if isError && !isFailedExit(want["exit_code"]) {
		want["exit_code"] = float64(1)
	}
	// The run summary line ends a failed run.
	if isFailedExit(want["exit_code"]) {
		if !isError {
			code, ok := want["exit_code"].(float64)
			require.True(t, ok, "a failed bot case without an error result must give its exit code, which the run summary line names")
			errorMessage = fmt.Sprintf("Bot exited with code %d", int(code))
		}
		summary := map[string]any{
			"level":     "error",
			"message":   "Run failed: " + errorMessage,
			"timestamp": map[string]any{"$match": "timestamp"},
		}
		if stderr, ok := want["stderr"].([]any); ok {
			want["stderr"] = append(append([]any{}, stderr...), summary)
		} else {
			want["stderr_last"] = summary
		}
	}
	return want
}

func isFailedExit(exitCode any) bool {
	if matcher, ok := exitCode.(map[string]any); ok {
		return matcher["$match"] == "nonzero"
	}
	code, ok := exitCode.(float64)
	return ok && code != 0
}

// errorResultMessage is the message the run summary line takes from an error
// result, and whether result is one.
func errorResultMessage(result any) (string, bool) {
	object, ok := result.(map[string]any)
	if !ok || object["status"] != "error" {
		return "", false
	}
	switch message := object["message"].(type) {
	case string:
		return message, true
	case nil:
		return "Bot reported an error", true
	default:
		return showJSON(message), true
	}
}

// uncaughtErrorMessage is the message of the error that ends the case's
// script, which the wrappers record as the run's error result.
func uncaughtErrorMessage(t *testing.T, c contractCase) string {
	t.Helper()
	for _, step := range c.Script {
		if _, caught := step["catch"]; step["op"] == "raise" && !caught {
			message, ok := step["message"].(string)
			require.True(t, ok, "raise op without a message")
			return message
		}
	}
	t.Fatalf("the case fails without writing a result, but its script raises no uncaught error")
	return ""
}

type contractRun struct {
	exitCode       int
	stdout         string
	stderr         string
	botLog         string
	result         any
	hasResult      bool
	queryResult    any
	hasQueryResult bool
	state          map[string]any
	readErrors     []string
}

func runContractCase(t *testing.T, image, runtimeBinary string, lang contractLanguage, c contractCase) contractRun {
	t.Helper()
	root := t.TempDir()
	dirs := map[string]string{"bot": "/bot", "state": "/state", "query": "/query", "the0": "/var/the0"}
	for dir := range dirs {
		require.NoError(t, os.MkdirAll(filepath.Join(root, dir), 0755))
	}
	botDir := filepath.Join(root, "bot")
	lang.layout(t, botDir)
	require.NoError(t, os.WriteFile(filepath.Join(botDir, "case.json"), c.file, 0644))

	stateDir := filepath.Join(root, "state", ".the0-state")
	for key, value := range c.StateBefore {
		require.NoError(t, os.MkdirAll(stateDir, 0755))
		require.NoError(t, os.WriteFile(filepath.Join(stateDir, key+".json"), value, 0644))
	}

	wrappers, err := filepath.Abs(filepath.Join("..", "..", "wrappers"))
	require.NoError(t, err)
	name := fmt.Sprintf("the0-contract-%s-%s-%d", lang.name, c.name, time.Now().UnixNano())
	args := []string{
		"run", "--rm", "--name", name,
		"--network", "none",
		"--read-only", "--tmpfs", "/tmp:rw,mode=1777",
		"--user", fmt.Sprintf("%d:%d", os.Getuid(), os.Getgid()),
		"-v", runtimeBinary + ":/app/runtime:ro",
		"-v", wrappers + ":/app/wrappers:ro",
	}
	for dir, mountPoint := range dirs {
		args = append(args, "-v", filepath.Join(root, dir)+":"+mountPoint)
	}
	for _, variable := range launchEnv(t, lang, c) {
		args = append(args, "-e", variable)
	}
	args = append(args, image, "/app/runtime", "execute", "--skip-sync", "--skip-query-server")

	ctx, cancel := context.WithTimeout(context.Background(), contractRunTimeout)
	defer cancel()
	t.Cleanup(func() { _ = exec.Command("docker", "rm", "-f", name).Run() })
	cmd := exec.CommandContext(ctx, "docker", args...)
	var stdout, stderr bytes.Buffer
	cmd.Stdout = &stdout
	cmd.Stderr = &stderr
	err = cmd.Run()
	require.NoError(t, ctx.Err(), "the run did not finish in %v; stderr:\n%s", contractRunTimeout, stderr.String())

	run := contractRun{stdout: stdout.String(), stderr: stderr.String()}
	var exitErr *exec.ExitError
	if errors.As(err, &exitErr) {
		run.exitCode = exitErr.ExitCode()
	} else {
		require.NoError(t, err)
	}
	// docker run exits 125 to 127 for its own failures and says so on stderr.
	if run.exitCode >= 125 && strings.Contains(run.stderr, "docker: ") {
		t.Fatalf("docker run failed (%d): %s", run.exitCode, run.stderr)
	}

	if content, err := os.ReadFile(filepath.Join(root, "the0", "logs", "bot.log")); err == nil {
		run.botLog = string(content)
	}
	run.result, run.hasResult = run.readJSON(filepath.Join(botDir, "result.json"), "result")
	run.queryResult, run.hasQueryResult = run.readJSON(filepath.Join(root, "query", "result.json"), "query_result")
	run.state = map[string]any{}
	files, err := filepath.Glob(filepath.Join(stateDir, "*.json"))
	require.NoError(t, err)
	for _, file := range files {
		if value, ok := run.readJSON(file, "state_after"); ok {
			run.state[strings.TrimSuffix(filepath.Base(file), ".json")] = value
		}
	}
	return run
}

// launchEnv is the environment the runtime's launchers (podgen's PodBuilder
// and the Docker ContainerBuilder) give `runtime execute` for the case, less
// CODE_FILE and the storage settings: nothing is downloaded, so the code and
// the state are the mounted directories.
func launchEnv(t *testing.T, lang contractLanguage, c contractCase) []string {
	t.Helper()
	env := []string{
		"RUNTIME=" + lang.runtime,
		"ENTRYPOINT=" + lang.entrypoint,
		"QUERY_ENTRYPOINT=" + lang.queryEntrypoint,
		"CODE_PATH=/bot",
		"STATE_PATH=/state",
		"LOGS_PATH=/var/the0/logs",
		"STATE_DIR=/state/.the0-state",
		"PYTHONUNBUFFERED=1",
		"PYTHONDONTWRITEBYTECODE=1",
	}
	placeholders := map[string]string{"CODE_MOUNT_DIR": "{{code_mount_dir}}", "STATE_DIR": "{{state_dir}}"}
	for _, key := range sortedKeys(c.Env) {
		value := c.Env[key]
		switch key {
		case "BOT_ID", "BOT_CONFIG", "BOT_TYPE", "QUERY_PATH", "QUERY_PARAMS":
			env = append(env, key+"="+value)
		case "CODE_MOUNT_DIR", "STATE_DIR":
			require.Equal(t, placeholders[key], value, "BuildBotEnv sets %s, so the case can only mark where", key)
		default:
			t.Fatalf("the case sets %s, and this check does not know how the runtime sets it", key)
		}
	}
	if c.Entrypoint == "bot" && c.Env["BOT_TYPE"] == "scheduled" {
		env = append(env, "IS_SCHEDULED=true")
	}
	return env
}

func (r *contractRun) readJSON(path, name string) (any, bool) {
	content, err := os.ReadFile(path)
	if errors.Is(err, os.ErrNotExist) {
		return nil, false
	}
	var value any
	if err == nil {
		err = json.Unmarshal(content, &value)
	}
	if err != nil {
		r.readErrors = append(r.readErrors, fmt.Sprintf("%s: %s: %v", name, path, err))
		return nil, false
	}
	return value, true
}

func (r contractRun) mismatches(want map[string]any, entrypoint string) []string {
	problems := append([]string{}, r.readErrors...)
	stdout := r.botStdoutLines()
	stderr := nonBlankLines(r.stderr)

	if expected, ok := want["exit_code"]; ok {
		problems = append(problems, contractMismatches(expected, float64(r.exitCode), "exit_code")...)
	}
	if expected, ok := want["stdout"].([]any); ok {
		problems = append(problems, lineMismatches(expected, stdout, "stdout")...)
	}
	if expected, ok := want["stderr"].([]any); ok {
		problems = append(problems, lineMismatches(expected, stderr, "stderr")...)
	}
	if expected, ok := want["stderr_last"]; ok {
		if len(stderr) == 0 {
			problems = append(problems, "stderr: expected the run summary line last, got nothing")
		} else {
			problems = append(problems, lineMismatches([]any{expected}, stderr[len(stderr)-1:], "stderr (last line)")...)
		}
	}
	if expected, ok := want["result"]; ok {
		problems = append(problems, fileMismatches(expected, r.result, r.hasResult, "result")...)
	}
	if expected, ok := want["query_result"]; ok {
		problems = append(problems, fileMismatches(expected, r.queryResult, r.hasQueryResult, "query_result")...)
	}
	if expected, ok := want["state_after"]; ok {
		state := make(map[string]any, len(r.state))
		for key, value := range r.state {
			state[key] = value
		}
		problems = append(problems, contractMismatches(expected, state, "state_after")...)
	}

	if entrypoint == "bot" {
		// bot.log is the copy the runtime saves and streams, so it must hold
		// the bot's output and the summary. The two streams are copied
		// concurrently, so only the lines are compared, not their order.
		logged := sortedCopy(nonBlankLines(r.botLog))
		printed := sortedCopy(append(append([]string{}, stdout...), stderr...))
		if !reflect.DeepEqual(logged, printed) {
			problems = append(problems, fmt.Sprintf("bot.log: expected the bot's stdout and stderr lines %q, got %q", printed, logged))
		}
	} else {
		for _, line := range stderr {
			if strings.Contains(line, "Run failed: ") {
				problems = append(problems, "stderr: a query run has no run summary line, got "+line)
			}
		}
	}
	return problems
}

// botStdoutLines is the container's stdout without the runtime's own log
// records, which its logger writes there as slog JSON (time, level, msg).
func (r contractRun) botStdoutLines() []string {
	var lines []string
	for _, line := range nonBlankLines(r.stdout) {
		var record map[string]any
		if json.Unmarshal([]byte(line), &record) == nil {
			_, hasTime := record["time"]
			_, hasMsg := record["msg"]
			if hasTime && hasMsg {
				continue
			}
		}
		lines = append(lines, line)
	}
	return lines
}

func fileMismatches(expected, actual any, exists bool, name string) []string {
	if expected == nil {
		if exists {
			return []string{fmt.Sprintf("%s: expected no file, found %s", name, showJSON(actual))}
		}
		return nil
	}
	if !exists {
		return []string{name + ": no file written"}
	}
	return contractMismatches(expected, actual, name)
}

func lineMismatches(expected []any, lines []string, stream string) []string {
	if len(lines) != len(expected) {
		return []string{fmt.Sprintf("%s: expected %d lines, got %d", stream, len(expected), len(lines))}
	}
	var problems []string
	for i, line := range lines {
		where := fmt.Sprintf("%s[%d]", stream, i)
		var got any
		if err := json.Unmarshal([]byte(line), &got); err != nil {
			problems = append(problems, fmt.Sprintf("%s: not JSON: %s", where, line))
			continue
		}
		problems = append(problems, contractMismatches(expected[i], got, where)...)
	}
	return problems
}

// contractMismatches compares by the matching rules in sdk/contract/README.md.
func contractMismatches(expected, actual any, where string) []string {
	mismatch := []string{fmt.Sprintf("%s: expected %s, got %s", where, showJSON(expected), showJSON(actual))}
	switch want := expected.(type) {
	case map[string]any:
		if _, ok := want["$match"]; ok {
			if matches(want, actual) {
				return nil
			}
			return mismatch
		}
		got, ok := actual.(map[string]any)
		if !ok {
			return mismatch
		}
		var problems []string
		for _, key := range sortedKeys(want) {
			value, present := got[key]
			if !present {
				problems = append(problems, where+"."+key+": missing")
				continue
			}
			problems = append(problems, contractMismatches(want[key], value, where+"."+key)...)
		}
		for _, key := range sortedKeys(got) {
			if _, expected := want[key]; !expected {
				problems = append(problems, fmt.Sprintf("%s.%s: unexpected, got %s", where, key, showJSON(got[key])))
			}
		}
		return problems
	case []any:
		got, ok := actual.([]any)
		if !ok || len(got) != len(want) {
			return mismatch
		}
		var problems []string
		for i := range want {
			problems = append(problems, contractMismatches(want[i], got[i], fmt.Sprintf("%s[%d]", where, i))...)
		}
		return problems
	default:
		// JSON numbers decode as float64, so 5 matches 5.0, and a bool never matches a number.
		if reflect.DeepEqual(expected, actual) {
			return nil
		}
		return mismatch
	}
}

func matches(matcher map[string]any, actual any) bool {
	text, isText := actual.(string)
	switch matcher["$match"] {
	case "timestamp":
		return isText && isoTimestamp.MatchString(text)
	case "string":
		return isText
	case "contains":
		value, _ := matcher["value"].(string)
		return isText && strings.Contains(text, value)
	case "nonzero":
		number, ok := actual.(float64)
		return ok && number != 0
	case "unordered":
		items, _ := matcher["items"].([]any)
		got, ok := actual.([]any)
		if !ok || len(got) != len(items) {
			return false
		}
		remaining := append([]any{}, got...)
		for _, item := range items {
			found := -1
			for i, candidate := range remaining {
				if len(contractMismatches(item, candidate, "")) == 0 {
					found = i
					break
				}
			}
			if found < 0 {
				return false
			}
			remaining = append(remaining[:found], remaining[found+1:]...)
		}
		return true
	default:
		return false
	}
}

func contractImage(t *testing.T) string {
	t.Helper()
	image := os.Getenv(contractImageEnv)
	if image == "" {
		image = defaultContractImage
	}
	if _, err := exec.LookPath("docker"); err != nil {
		needFor(t, "docker is not available: %v", err)
	}
	if out, err := exec.Command("docker", "image", "inspect", image).CombinedOutput(); err != nil {
		needFor(t, "image %s is not built (docker build -t %s ./runtime, or set %s): %v: %s",
			image, defaultContractImage, contractImageEnv, err, out)
	}
	return image
}

// buildRuntimeBinary builds this tree's runtime for the image's Linux, so the
// cases run the code under test even when the image is older.
func buildRuntimeBinary(t *testing.T) string {
	t.Helper()
	binary := filepath.Join(t.TempDir(), "runtime")
	cmd := exec.Command("go", "build", "-o", binary, ".")
	cmd.Env = append(os.Environ(), "CGO_ENABLED=0", "GOOS=linux")
	out, err := cmd.CombinedOutput()
	require.NoError(t, err, "go build: %s", out)
	return binary
}

func loadContractCases(t *testing.T, dir string) []contractCase {
	t.Helper()
	paths, err := filepath.Glob(filepath.Join(dir, "*.json"))
	require.NoError(t, err)
	require.NotEmpty(t, paths, "no cases in %s", dir)
	cases := make([]contractCase, 0, len(paths))
	for _, path := range paths {
		content, err := os.ReadFile(path)
		require.NoError(t, err)
		c := contractCase{name: strings.TrimSuffix(filepath.Base(path), ".json"), file: content}
		require.NoError(t, json.Unmarshal(content, &c), path)
		cases = append(cases, c)
	}
	return cases
}

// needFor fails the test in CI, where the runtime workflow provides what it
// needs, and skips it anywhere else.
func needFor(t *testing.T, format string, args ...any) {
	t.Helper()
	if os.Getenv("CI") != "" {
		t.Fatalf(format, args...)
	}
	t.Skipf(format, args...)
}

func repoPath(t *testing.T, parts ...string) string {
	t.Helper()
	path, err := filepath.Abs(filepath.Join(append([]string{"..", "..", ".."}, parts...)...))
	require.NoError(t, err)
	return path
}

func copyFile(t *testing.T, src, dst string) {
	t.Helper()
	content, err := os.ReadFile(src)
	require.NoError(t, err)
	require.NoError(t, os.MkdirAll(filepath.Dir(dst), 0755))
	require.NoError(t, os.WriteFile(dst, content, 0644))
}

func copyFiles(t *testing.T, srcDir, pattern, dstDir string) {
	t.Helper()
	paths, err := filepath.Glob(filepath.Join(srcDir, pattern))
	require.NoError(t, err)
	require.NotEmpty(t, paths, "nothing matches %s in %s", pattern, srcDir)
	for _, path := range paths {
		copyFile(t, path, filepath.Join(dstDir, filepath.Base(path)))
	}
}

func nonBlankLines(text string) []string {
	var lines []string
	for _, line := range strings.Split(text, "\n") {
		if strings.TrimSpace(line) != "" {
			lines = append(lines, line)
		}
	}
	return lines
}

func sortedKeys[V any](m map[string]V) []string {
	keys := make([]string, 0, len(m))
	for key := range m {
		keys = append(keys, key)
	}
	sort.Strings(keys)
	return keys
}

func sortedCopy(lines []string) []string {
	sorted := append([]string{}, lines...)
	sort.Strings(sorted)
	return sorted
}

func showJSON(value any) string {
	text, err := json.Marshal(value)
	if err != nil {
		return fmt.Sprintf("%v", value)
	}
	return string(text)
}
