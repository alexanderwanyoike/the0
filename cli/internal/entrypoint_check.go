package internal

import (
	_ "embed"
	"fmt"
	"os"
	"path"
	"path/filepath"
	"strings"

	"the0/internal/logger"
)

// entrypointChecker parses a Python entrypoint with Python's own parser and
// writes a verdict on how it binds main.
//
//go:embed entrypoint_check.py
var entrypointChecker string

// CheckBotEntrypoint refuses a Python bot entrypoint that the runtime
// wrapper could not start: the wrapper reads the module's main attribute
// and calls main(bot_id, config) in a thread without awaiting it. The file
// is parsed, never run, in the Python image the bot runs in. When Docker is
// not available the check is skipped, since it must never block a deploy
// it cannot judge.
func CheckBotEntrypoint(config *BotConfig) error {
	if !strings.HasPrefix(config.Runtime, "python") {
		return nil
	}
	entrypoint := config.Entrypoints.Bot
	if _, err := os.Stat(entrypoint); err != nil {
		// A missing entrypoint is reported by the build and upload steps.
		return nil
	}
	if err := CheckDockerInstalled(); err != nil {
		logger.Verbose("Skipping the entrypoint check: %v", err)
		return nil
	}
	runner, err := NewDockerRunner()
	if err != nil {
		logger.Verbose("Skipping the entrypoint check: %v", err)
		return nil
	}
	defer runner.Close()
	if err := runner.Ping(); err != nil {
		logger.Verbose("Skipping the entrypoint check: %v", err)
		return nil
	}
	return checkEntrypointWith(runner, entrypoint)
}

// checkEntrypointWith runs the checker on entrypoint, relative to the
// working directory, and turns its verdict into an error. A checker that
// cannot run or leaves no verdict skips the check.
func checkEntrypointWith(runner BuildRunner, entrypoint string) error {
	project, err := filepath.Abs(".")
	if err != nil {
		return nil
	}
	work, err := os.MkdirTemp("", "the0-entrypoint-check-")
	if err != nil {
		return nil
	}
	defer os.RemoveAll(work)
	if err := os.WriteFile(filepath.Join(work, "check.py"), []byte(entrypointChecker), 0o644); err != nil {
		return nil
	}

	if err := runner.EnsureImage(pythonImage); err != nil {
		logger.Verbose("Skipping the entrypoint check: %v", err)
		return nil
	}
	err = runner.Run(BuildStep{
		NamePrefix:      "entrypoint-check-",
		Image:           pythonImage,
		Cmd:             []string{"python", "/check/check.py", path.Join("/bot", filepath.ToSlash(entrypoint)), "/check/verdict"},
		Binds:           []string{project + ":/bot:ro", work + ":/check"},
		FailureHeadline: "Entrypoint check",
		FailedCommand:   "python check.py",
	})
	if err != nil {
		logger.Verbose("Skipping the entrypoint check: %v", err)
		return nil
	}
	verdict, err := os.ReadFile(filepath.Join(work, "verdict"))
	if err != nil {
		return nil
	}
	return entrypointError(entrypoint, strings.TrimSpace(string(verdict)))
}

func entrypointError(entrypoint, verdict string) error {
	switch {
	case verdict == "missing":
		return fmt.Errorf("%s has no main(bot_id, config) function at module level, which the runtime calls to start your bot. Define one, or import one as main", entrypoint)
	case verdict == "async":
		return fmt.Errorf("%s defines main with async def, but the runtime calls main(bot_id, config) without awaiting it, so the bot would exit without running. Use a regular def main(bot_id, config) that calls asyncio.run(...)", entrypoint)
	case strings.HasPrefix(verdict, "syntax "):
		line, message, _ := strings.Cut(strings.TrimPrefix(verdict, "syntax "), " ")
		return fmt.Errorf("%s does not parse as Python 3.11 (line %s: %s), so the runtime could not load it", entrypoint, line, message)
	default:
		return nil
	}
}
