package internal

import (
	"fmt"
	"os"
	"regexp"
	"strings"
)

// CheckBotEntrypoint refuses a Python bot entrypoint that the runtime
// wrapper could not start. The wrapper reads the module's main attribute
// and calls main(bot_id, config) in a thread without awaiting it.
//
// It reads words, not Python, so that no valid bot is ever refused. An
// entrypoint fails only when nothing in it could bind main, because every
// mention of main is a call or an undecorated async def. Scope, strings and
// comments are not judged, so some broken entrypoints pass.
func CheckBotEntrypoint(config *BotConfig) error {
	if !strings.HasPrefix(config.Runtime, "python") {
		return nil
	}
	path := config.Entrypoints.Bot
	source, err := os.ReadFile(path)
	if err != nil {
		// A missing entrypoint is reported by the build and upload steps.
		return nil
	}

	switch mainBindingOf(string(source)) {
	case noMain:
		return fmt.Errorf("%s has no main(bot_id, config) function, which the runtime calls to start your bot. Define one, or import one as main", path)
	case asyncMain:
		return fmt.Errorf("%s defines main with async def, but the runtime calls main(bot_id, config) without awaiting it, so the bot would exit without running. Use a regular def main(bot_id, config) that calls asyncio.run(...)", path)
	default:
		return nil
	}
}

type mainBinding int

const (
	noMain mainBinding = iota
	usableMain
	asyncMain
)

var (
	mainMention  = regexp.MustCompile(`(?P<definer>(?:async\s+)?def\s+|class\s+)?\bmain\b(?P<call>\s*\()?`)
	definerGroup = mainMention.SubexpIndex("definer")
	callGroup    = mainMention.SubexpIndex("call")
	starImport   = regexp.MustCompile(`(?m)^\s*from\s+\S+\s+import\s*\(?\s*\*`)
)

func mainBindingOf(source string) mainBinding {
	if starImport.MatchString(source) {
		return usableMain
	}
	mentions := mainMention.FindAllStringSubmatchIndex(source, -1)
	found := noMain
	for _, m := range mentions {
		definer := ""
		if start := m[2*definerGroup]; start >= 0 {
			definer = source[start:m[2*definerGroup+1]]
		}
		switch {
		case definer == "" && m[2*callGroup] >= 0:
			// asyncio.run(main()) runs main but binds nothing.
			continue
		case strings.HasPrefix(definer, "async") && !decorated(source, m[0]):
			found = asyncMain
		default:
			return usableMain
		}
	}
	return found
}

// decorated reports whether the line before the one at offset, skipping
// blank and comment lines, is a decorator, which may make an async def sync.
func decorated(source string, offset int) bool {
	lines := strings.Split(source[:offset], "\n")
	for i := len(lines) - 2; i >= 0; i-- {
		line := strings.TrimSpace(lines[i])
		if line == "" || strings.HasPrefix(line, "#") {
			continue
		}
		return strings.HasPrefix(line, "@")
	}
	return false
}
