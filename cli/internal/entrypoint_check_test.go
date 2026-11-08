package internal

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// pythonBot writes source as the bot entrypoint of a python3.11 bot in a
// temporary directory and returns its config, with paths relative to that
// directory as bot-config.yaml writes them.
func pythonBot(t *testing.T, source string) *BotConfig {
	t.Helper()
	t.Chdir(t.TempDir())
	if err := os.WriteFile("main.py", []byte(source), 0o644); err != nil {
		t.Fatal(err)
	}
	config := &BotConfig{Runtime: "python3.11"}
	config.Entrypoints.Bot = "main.py"
	return config
}

func TestCheckBotEntrypoint_PassesAnyTopLevelMain(t *testing.T) {
	for _, tc := range []struct {
		name   string
		source string
	}{
		{"a plain def", "def main(bot_id, config):\n    pass\n"},
		{"a def with annotations and defaults", "def main(bot_id: str = None, config: dict = None):\n    pass\n"},
		{"a decorated def", "@traced\ndef main(bot_id, config):\n    pass\n"},
		{"an import", "from strategy import main\n"},
		{"an import renamed to main", "from strategy import run as main\n"},
		{"an import among others", "from strategy import helper, main, other\n"},
		{"a parenthesised multi-line import", "from strategy import (\n    helper,\n    main,\n)\n"},
		{"a backslash-continued import", "from strategy import helper, \\\n    main\n"},
		{"a module imported as main", "import strategy.entry as main\n"},
		{"an assignment", "main = build_main()\n"},
		{"an annotated assignment", "main: Callable = build_main()\n"},
		{"a def inside a top-level try", "try:\n    from fast import main\nexcept ImportError:\n    def main(bot_id, config):\n        pass\n"},
		{"a def inside a top-level if", "if FEATURE:\n    def main(bot_id, config):\n        pass\n"},
		{"a star import, which may bring main", "from strategy import *\n"},
		{"an async def rebound later", "async def run(bot_id, config):\n    pass\n\nasync def main(bot_id, config):\n    pass\n\nmain = sync_wrapper(main)\n"},
		{"Windows line endings", "def main(bot_id, config):\r\n    pass\r\n"},
		{"a comment inside a parenthesised import", "from strategy import (\n    helper,  # used by tests\n    main,\n)\n"},
		{"a comment line inside a parenthesised import", "from strategy import (\n    # the entry point\n    main,\n)\n"},
		{"a comment after an import's opening parenthesis", "from strategy import (  # noqa: F401\n    main,\n)\n"},
		{"an import with no space before its parenthesis", "from strategy import(main)\n"},
		{"a byte order mark", "\ufeffdef main(bot_id, config):\n    pass\n"},
		{"a wrapped signature with an async helper named main inside", "def main(\n    bot_id: str,\n    config: dict,\n) -> None:\n    async def main() -> None:\n        pass\n\n    asyncio.run(main())\n"},
		{"an async def under a decorator, which may make it sync", "@async_to_sync\nasync def main(bot_id, config):\n    pass\n"},
		{"an async def replacing an earlier def", "def main(bot_id, config):\n    pass\n\nasync def main(bot_id, config):\n    pass\n"},
		{"a sync def in one branch and an async def in the other", "if FAST:\n    def main(bot_id, config):\n        pass\nelse:\n    async def main(bot_id, config):\n        pass\n"},
		{"tuple unpacking", "main, query = build()\n"},
		{"an assignment after a semicolon", "import strategy; main = strategy.main\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			config := pythonBot(t, tc.source)

			if err := CheckBotEntrypoint(config); err != nil {
				t.Errorf("error = %v, want nil", err)
			}
		})
	}
}

func TestCheckBotEntrypoint_RefusesAnEntrypointWithoutMain(t *testing.T) {
	for _, tc := range []struct {
		name   string
		source string
	}{
		{"no main at all", "def run(bot_id, config):\n    pass\n"},
		{"a main only inside a class", "class Bot:\n    def main(self, bot_id, config):\n        pass\n"},
		{"a main only inside a function", "def build():\n    def main(bot_id, config):\n        pass\n    return main\n"},
		{"a main only in a comment", "# def main(bot_id, config):\ndef run():\n    pass\n"},
		{"a def whose name starts with main", "def main_loop(bot_id, config):\n    pass\n"},
		{"an import whose name starts with main", "from strategy import mainly\n"},
		{"a comparison with main", "main == other\n"},
		{"an empty file", ""},
	} {
		t.Run(tc.name, func(t *testing.T) {
			config := pythonBot(t, tc.source)

			err := CheckBotEntrypoint(config)

			want := "main.py has no top-level main(bot_id, config) function, which the runtime calls to start your bot. Define one, or import one as main"
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
		{"an async def beside a class with a sync main method", "class Helper:\n    def main(self):\n        pass\n\nasync def main(bot_id, config):\n    pass\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			config := pythonBot(t, tc.source)

			err := CheckBotEntrypoint(config)

			if err == nil || !strings.Contains(err.Error(), "main.py defines main with async def") ||
				!strings.Contains(err.Error(), "asyncio.run") {
				t.Errorf("error = %v, want the async main explained with asyncio.run as the fix", err)
			}
		})
	}
}

func TestCheckBotEntrypoint_ChecksOnlyPythonBotEntrypoints(t *testing.T) {
	t.Run("another runtime", func(t *testing.T) {
		config := pythonBot(t, "export function run() {}\n")
		config.Runtime = "nodejs22"

		if err := CheckBotEntrypoint(config); err != nil {
			t.Errorf("error = %v, want nil", err)
		}
	})

	t.Run("a query entrypoint without main", func(t *testing.T) {
		config := pythonBot(t, "def main(bot_id, config):\n    pass\n")
		if err := os.WriteFile("query.py", []byte("from the0 import query\nquery.run()\n"), 0o644); err != nil {
			t.Fatal(err)
		}
		config.Entrypoints.Query = "query.py"

		if err := CheckBotEntrypoint(config); err != nil {
			t.Errorf("error = %v, want nil", err)
		}
	})

	t.Run("an entrypoint in a subdirectory", func(t *testing.T) {
		config := pythonBot(t, "")
		if err := os.MkdirAll("src", 0o755); err != nil {
			t.Fatal(err)
		}
		if err := os.WriteFile(filepath.Join("src", "bot.py"), []byte("def run():\n    pass\n"), 0o644); err != nil {
			t.Fatal(err)
		}
		config.Entrypoints.Bot = "src/bot.py"

		err := CheckBotEntrypoint(config)

		if err == nil || !strings.HasPrefix(err.Error(), "src/bot.py has no top-level main") {
			t.Errorf("error = %v, want it to name src/bot.py", err)
		}
	})

	t.Run("a missing entrypoint, which other checks report", func(t *testing.T) {
		config := pythonBot(t, "")
		config.Entrypoints.Bot = "missing.py"

		if err := CheckBotEntrypoint(config); err != nil {
			t.Errorf("error = %v, want nil", err)
		}
	})
}
