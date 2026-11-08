package internal

import (
	"fmt"
	"os"
	"regexp"
	"strings"
)

// CheckBotEntrypoint refuses a Python bot entrypoint that the runtime
// wrapper could not start: the wrapper reads the module's main attribute
// and calls main(bot_id, config) in a thread without awaiting it. The scan
// is deliberately conservative and fails only when no top-level binding of
// main exists, or the last one is an async def. Anything it cannot read
// passes.
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

	switch lastMainBinding(string(source)) {
	case noMain:
		return fmt.Errorf("%s has no top-level main(bot_id, config) function, which the runtime calls to start your bot. Define one, or import one as main", path)
	case ausableMain:
		return fmt.Errorf("%s defines main with async def, but the runtime calls main(bot_id, config) without awaiting it, so the bot would exit without running. Use a regular def main(bot_id, config) that calls asyncio.run(...)", path)
	default:
		return nil
	}
}

type mainBinding int

const (
	noMain mainBinding = iota
	usableMain
	ausableMain
)

var (
	defMain        = regexp.MustCompile(`^(async\s+)?def\s+main\s*\(`)
	classMain      = regexp.MustCompile(`^class\s+main\b`)
	assignMain     = regexp.MustCompile(`^main\s*(:[^=]*)?=($|[^=])`)
	fromImport     = regexp.MustCompile(`^from\s+\S+\s+import\s+`)
	plainImport    = regexp.MustCompile(`^import\s+`)
	opensBody      = regexp.MustCompile(`^(async\s+def|def|class)\b`)
	importAliasSep = regexp.MustCompile(`\s+as\s+`)
)

// lastMainBinding returns how main is last bound at module level. Lines
// inside a top-level def or class are skipped, since they bind nothing on
// the module; lines inside a top-level if, try, with or loop are read,
// since they do.
func lastMainBinding(source string) mainBinding {
	lines := strings.Split(strings.ReplaceAll(source, "\r\n", "\n"), "\n")
	last := noMain
	inBody := false
	for i := 0; i < len(lines); i++ {
		line := lines[i]
		statement := strings.TrimLeft(line, " \t")
		if statement == "" || strings.HasPrefix(statement, "#") {
			continue
		}
		if statement == line {
			inBody = opensBody.MatchString(statement)
		} else if inBody {
			continue
		}

		if fromImport.MatchString(statement) || plainImport.MatchString(statement) {
			var consumed int
			statement, consumed = joinContinuation(lines[i:])
			i += consumed
		}
		if binding := mainBoundBy(statement); binding != noMain {
			last = binding
		}
	}
	return last
}

// joinContinuation joins an import split over lines by parentheses or
// trailing backslashes, and returns it with the number of extra lines used.
func joinContinuation(lines []string) (string, int) {
	joined := strings.TrimSpace(lines[0])
	consumed := 0
	for consumed+1 < len(lines) &&
		(strings.Count(joined, "(") > strings.Count(joined, ")") || strings.HasSuffix(joined, "\\")) {
		consumed++
		joined = strings.TrimSuffix(joined, "\\") + " " + strings.TrimSpace(lines[consumed])
	}
	return joined, consumed
}

func mainBoundBy(statement string) mainBinding {
	if match := defMain.FindStringSubmatch(statement); match != nil {
		if match[1] != "" {
			return ausableMain
		}
		return usableMain
	}
	if classMain.MatchString(statement) || assignMain.MatchString(statement) {
		return usableMain
	}
	if loc := fromImport.FindStringIndex(statement); loc != nil {
		return bindingFor(importedNames(statement[loc[1]:], func(name string) string { return name }))
	}
	if loc := plainImport.FindStringIndex(statement); loc != nil {
		// A plain import binds the first part of a dotted name.
		return bindingFor(importedNames(statement[loc[1]:], func(name string) string {
			return strings.SplitN(name, ".", 2)[0]
		}))
	}
	return noMain
}

// importedNames returns the names an import binds: the alias where there is
// one, otherwise unaliased(name).
func importedNames(names string, unaliased func(string) string) []string {
	names = strings.Trim(strings.SplitN(names, "#", 2)[0], " ()")
	var bound []string
	for _, name := range strings.Split(names, ",") {
		parts := importAliasSep.Split(strings.Trim(name, " ()"), 2)
		if len(parts) == 2 {
			bound = append(bound, parts[1])
		} else {
			bound = append(bound, unaliased(parts[0]))
		}
	}
	return bound
}

// bindingFor counts a star import, which may bring main in, as binding it.
func bindingFor(names []string) mainBinding {
	for _, name := range names {
		if name == "main" || name == "*" {
			return usableMain
		}
	}
	return noMain
}
