package internal

import (
	"errors"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// hostPython plays Docker for the checker step: it runs the step's command
// with the host's python3, mapping each bind's container path to its host
// path, so the tests exercise the real checker script.
type hostPython struct{}

func (hostPython) Ping() error              { return nil }
func (hostPython) EnsureImage(string) error { return nil }
func (hostPython) Close() error             { return nil }

func (hostPython) Run(step BuildStep) error {
	args := append([]string{"python3"}, step.Cmd[1:]...)
	for _, bind := range step.Binds {
		bind = strings.TrimSuffix(bind, ":ro")
		split := strings.LastIndex(bind, ":")
		host, container := bind[:split], bind[split+1:]
		for i := range args {
			if strings.HasPrefix(args[i], container+"/") {
				args[i] = host + strings.TrimPrefix(args[i], container)
			}
		}
	}
	return exec.Command(args[0], args[1:]...).Run()
}

// checkSource writes source as main.py in a temporary directory and checks
// it with the real checker script.
func checkSource(t *testing.T, source string) error {
	t.Helper()
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 is not installed")
	}
	t.Chdir(t.TempDir())
	if err := os.WriteFile("main.py", []byte(source), 0o644); err != nil {
		t.Fatal(err)
	}
	return checkEntrypointWith(hostPython{}, "main.py")
}

// pythonVerdict runs the checker script on source with the host's python3
// and returns its verdict, so a pass is told apart from a skipped check.
func pythonVerdict(t *testing.T, source string) string {
	t.Helper()
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 is not installed")
	}
	dir := t.TempDir()
	script, entrypoint, verdict := filepath.Join(dir, "check.py"), filepath.Join(dir, "main.py"), filepath.Join(dir, "verdict")
	if err := os.WriteFile(script, []byte(entrypointChecker), 0o644); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(entrypoint, []byte(source), 0o644); err != nil {
		t.Fatal(err)
	}
	if out, err := exec.Command("python3", script, entrypoint, verdict).CombinedOutput(); err != nil {
		t.Fatalf("checker failed: %v\n%s", err, out)
	}
	got, err := os.ReadFile(verdict)
	if err != nil {
		t.Fatal(err)
	}
	return strings.TrimSpace(string(got))
}

func TestCheckBotEntrypoint_PassesAnyModuleLevelMain(t *testing.T) {
	for _, tc := range []struct {
		name   string
		source string
	}{
		{"a plain def", "def main(bot_id, config):\n    pass\n"},
		{"a def with annotations and defaults", "def main(bot_id: str = None, config: dict = None):\n    pass\n"},
		{"a decorated def", "def traced(f):\n    return f\n\n@traced\ndef main(bot_id, config):\n    pass\n"},
		{"a wrapped signature with an async helper named main inside", "def main(\n    bot_id: str,\n    config: dict,\n) -> None:\n    async def main() -> None:\n        pass\n\n    asyncio.run(main())\n"},
		{"a class named main", "class main:\n    def __init__(self, bot_id, config):\n        pass\n"},
		{"an import", "from strategy import main\n"},
		{"a relative import", "from .strategy import main\n"},
		{"an import renamed to main", "from strategy import run as main\n"},
		{"a parenthesised import with comments", "from strategy import (\n    helper,  # used by tests\n    # the entry point\n    main,\n)\n"},
		{"a backslash-continued import at column 0", "from strategy import helper, \\\nmain\n"},
		{"a module imported as main", "import strategy.entry as main\n"},
		{"a star import, which may bring main", "from strategy import *\n"},
		{"an assignment", "main = build_main()\n"},
		{"an annotated assignment", "main: Callable = build_main()\n"},
		{"a chained assignment", "runner = main = build()\n"},
		{"tuple, starred and list targets", "main, query = build()\nmain, *rest = build()\n[main, q] = build()\n"},
		{"an assignment after a semicolon", "import strategy; main = strategy.main\n"},
		{"a walrus", "if (main := build()):\n    pass\n"},
		{"a walrus inside a comprehension, which binds the module", "fs = [print]\n[(main := f) for f in fs]\n"},
		{"a with target", "with ctx() as main:\n    pass\n"},
		{"a for target", "for main in [run]:\n    pass\n"},
		{"one-line compound imports", "if FAST: from fast import main\nelse: from slow import main\n"},
		{"a def inside a top-level try after a multi-line string", "try:\n    import uvloop\n    HELP = \"\"\"\n    usage (fast\n    \"\"\"\n    def main(bot_id, config):\n        pass\nexcept ImportError:\n    pass\n"},
		{"a global main assigned in a function", "def setup():\n    global main\n    main = build()\n\nsetup()\n"},
		{"a byte order mark", "\ufeffdef main(bot_id, config):\n    pass\n"},
		{"Windows line endings", "def main(bot_id, config):\r\n    pass\r\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			if got := pythonVerdict(t, tc.source); got != "ok" {
				t.Errorf("verdict = %q, want ok", got)
			}
		})
	}
}

func TestCheckBotEntrypoint_RefusesAnEntrypointWithoutModuleLevelMain(t *testing.T) {
	for _, tc := range []struct {
		name   string
		source string
	}{
		{"no main at all", "def run(bot_id, config):\n    pass\n"},
		{"a main only inside a class", "class Bot:\n    def main(self, bot_id, config):\n        pass\n"},
		{"a main only inside a function", "def build():\n    def main(bot_id, config):\n        pass\n    return main\n"},
		{"a main only in a comment or a string", "# def main(bot_id, config):\nHELP = \"call main() to start\"\n"},
		{"a def whose name starts with main", "def main_loop(bot_id, config):\n    pass\n"},
		{"an import whose name starts with main", "from strategy import mainly\n"},
		{"a main that is only called, as a renamed bot's __main__ guard does", "def run(bot_id, config):\n    pass\n\nif __name__ == \"__main__\":\n    main(\"local\", {})\n"},
		{"an annotation without a value", "from typing import Callable\n\nmain: Callable\n"},
		{"an empty file", ""},
	} {
		t.Run(tc.name, func(t *testing.T) {
			err := checkSource(t, tc.source)

			want := "main.py has no main(bot_id, config) function at module level, which the runtime calls to start your bot. Define one, or import one as main"
			if err == nil || err.Error() != want {
				t.Errorf("error = %v, want %q", err, want)
			}
		})
	}
}

func TestCheckBotEntrypoint_RefusesAnAsyncMain(t *testing.T) {
	for _, tc := range []struct {
		name   string
		source string
	}{
		{"the only main is async", "import asyncio\n\nasync def main(bot_id, config):\n    pass\n"},
		{"an async main run by the __main__ guard", "import asyncio\n\nasync def main(bot_id, config):\n    pass\n\nif __name__ == \"__main__\":\n    asyncio.run(main(\"local\", {}))\n"},
		{"an async main passed by reference", "import anyio\n\nasync def main(bot_id, config):\n    pass\n\nif __name__ == \"__main__\":\n    anyio.run(main, \"local\", {})\n"},
		{"an async def beside a class with a sync main method", "class Helper:\n    def main(self):\n        pass\n\nasync def main(bot_id, config):\n    pass\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			err := checkSource(t, tc.source)

			want := "main.py defines main with async def, but the runtime calls main(bot_id, config) without awaiting it, so the bot would exit without running. Use a regular def main(bot_id, config) that calls asyncio.run(...)"
			if err == nil || err.Error() != want {
				t.Errorf("error = %v, want %q", err, want)
			}
		})
	}
}

func TestCheckBotEntrypoint_PassesAnAsyncMainThatMayBeMadeSync(t *testing.T) {
	for _, tc := range []struct {
		name   string
		source string
	}{
		{"a decorated async def", "@async_to_sync\nasync def main(bot_id, config):\n    pass\n"},
		{"an async def under a multi-line decorator call", "@run_sync(\n    timeout=30,\n)\nasync def main(bot_id, config):\n    pass\n"},
		{"an async def rebound later", "async def main(bot_id, config):\n    pass\n\nmain = sync_wrapper(main)\n"},
		{"an async def replacing an earlier def", "def main(bot_id, config):\n    pass\n\nasync def main(bot_id, config):\n    pass\n"},
		{"a sync def in one branch and an async def in the other", "if FAST:\n    def main(bot_id, config):\n        pass\nelse:\n    async def main(bot_id, config):\n        pass\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			if got := pythonVerdict(t, tc.source); got != "ok" {
				t.Errorf("verdict = %q, want ok", got)
			}
		})
	}
}

func TestCheckBotEntrypoint_RefusesAnEntrypointPythonCannotParse(t *testing.T) {
	err := checkSource(t, "def main(bot_id, config):\n    pass\n\ndef run(:\n    pass\n")

	if err == nil || !strings.HasPrefix(err.Error(), "main.py does not parse as Python 3.11 (line 4: ") {
		t.Errorf("error = %v, want the syntax error and its line", err)
	}
}

func TestCheckBotEntrypoint_SkipsWhatItCannotCheck(t *testing.T) {
	t.Run("another runtime", func(t *testing.T) {
		t.Chdir(t.TempDir())
		if err := os.WriteFile("main.js", []byte("export function run() {}\n"), 0o644); err != nil {
			t.Fatal(err)
		}
		config := &BotConfig{Runtime: "nodejs22"}
		config.Entrypoints.Bot = "main.js"

		if err := CheckBotEntrypoint(config); err != nil {
			t.Errorf("error = %v, want nil", err)
		}
	})

	t.Run("a missing entrypoint, which other checks report", func(t *testing.T) {
		t.Chdir(t.TempDir())
		config := &BotConfig{Runtime: "python3.11"}
		config.Entrypoints.Bot = "missing.py"

		if err := CheckBotEntrypoint(config); err != nil {
			t.Errorf("error = %v, want nil", err)
		}
	})

	t.Run("a checker container that fails", func(t *testing.T) {
		t.Chdir(t.TempDir())
		if err := os.WriteFile("main.py", []byte("def run():\n    pass\n"), 0o644); err != nil {
			t.Fatal(err)
		}
		runner := &fakeRunner{onRun: func(BuildStep) error { return errors.New("container exited 137") }}

		if err := checkEntrypointWith(runner, "main.py"); err != nil {
			t.Errorf("error = %v, want nil", err)
		}
	})

	for _, entrypoint := range []string{"../outside.py", "/etc/outside.py"} {
		t.Run("an entrypoint outside the project, "+entrypoint, func(t *testing.T) {
			t.Chdir(t.TempDir())
			runner := &fakeRunner{}

			if err := checkEntrypointWith(runner, entrypoint); err != nil {
				t.Errorf("error = %v, want nil", err)
			}
			if len(runner.steps) != 0 {
				t.Errorf("ran %d steps, want none", len(runner.steps))
			}
		})
	}

	t.Run("a checker that reports an error", func(t *testing.T) {
		t.Chdir(t.TempDir())
		if err := os.WriteFile("main.py", []byte("def run():\n    pass\n"), 0o644); err != nil {
			t.Fatal(err)
		}
		runner := &fakeRunner{onRun: func(step BuildStep) error {
			work := strings.TrimSuffix(step.Binds[1], ":/check")
			return os.WriteFile(filepath.Join(work, "verdict"), []byte("error MemoryError\n"), 0o644)
		}}

		if err := checkEntrypointWith(runner, "main.py"); err != nil {
			t.Errorf("error = %v, want nil", err)
		}
	})

	t.Run("a checker that leaves no verdict", func(t *testing.T) {
		t.Chdir(t.TempDir())
		if err := os.WriteFile("main.py", []byte("def run():\n    pass\n"), 0o644); err != nil {
			t.Fatal(err)
		}

		if err := checkEntrypointWith(&fakeRunner{}, "main.py"); err != nil {
			t.Errorf("error = %v, want nil", err)
		}
	})
}

func TestCheckBotEntrypoint_RunsTheCheckerInThePythonImage(t *testing.T) {
	t.Chdir(t.TempDir())
	if err := os.MkdirAll("src", 0o755); err != nil {
		t.Fatal(err)
	}
	if err := os.WriteFile(filepath.Join("src", "bot.py"), []byte("def run():\n    pass\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	runner := &fakeRunner{}

	_ = checkEntrypointWith(runner, "src/bot.py")

	if len(runner.ensured) != 1 || runner.ensured[0] != pythonImage {
		t.Errorf("ensured %v, want [%s]", runner.ensured, pythonImage)
	}
	if len(runner.steps) != 1 {
		t.Fatalf("ran %d steps, want 1", len(runner.steps))
	}
	step := runner.steps[0]
	if step.Image != pythonImage || step.Cmd[0] != "python" || step.Cmd[2] != "/bot/src/bot.py" {
		t.Errorf("step = %+v, want python on /bot/src/bot.py in %s", step, pythonImage)
	}
	wd, _ := os.Getwd()
	if !strings.Contains(strings.Join(step.Binds, " "), wd+":/bot:ro") {
		t.Errorf("binds = %v, want the project mounted read-only at /bot", step.Binds)
	}
}

func TestEntrypointChecker_ReportsAnUnreadableEntrypointWithoutFailing(t *testing.T) {
	if _, err := exec.LookPath("python3"); err != nil {
		t.Skip("python3 is not installed")
	}
	dir := t.TempDir()
	script, verdict := filepath.Join(dir, "check.py"), filepath.Join(dir, "verdict")
	if err := os.WriteFile(script, []byte(entrypointChecker), 0o644); err != nil {
		t.Fatal(err)
	}

	// A non-zero exit would make the runner print a red failure for a check
	// that is only being skipped.
	if out, err := exec.Command("python3", script, filepath.Join(dir, "missing.py"), verdict).CombinedOutput(); err != nil {
		t.Fatalf("checker exited with %v, want 0\n%s", err, out)
	}
	got, err := os.ReadFile(verdict)
	if err != nil || !strings.HasPrefix(string(got), "error ") {
		t.Errorf("verdict = %q (read error %v), want an error verdict", got, err)
	}
}
