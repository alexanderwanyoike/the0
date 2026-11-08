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
// main exists, or every one is an undecorated async def. Anything it cannot
// read passes.
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
		return fmt.Errorf("%s has no top-level main(bot_id, config) function, which the runtime calls to start your bot. Define one, or import one as main", path)
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
	defMain = regexp.MustCompile(`^(async\s+)?def\s+main\s*\(`)
	// Also matches main as one target of a tuple assignment.
	assignMain     = regexp.MustCompile(`^\(?\s*(?:\w+\s*,\s*)*main\s*(?:,\s*\w+\s*)*,?\s*\)?\s*(?::[^=]*)?=(?:$|[^=])`)
	classMain      = regexp.MustCompile(`^class\s+main\b`)
	fromImport     = regexp.MustCompile(`^from\s+\S+\s+import(?:\s+|\s*\()`)
	plainImport    = regexp.MustCompile(`^import\s+`)
	opensBody      = regexp.MustCompile(`^(async\s+def|def|class)\b`)
	importAliasSep = regexp.MustCompile(`\s+as\s+`)
	stringLiteral  = regexp.MustCompile(`'(?:\\.|[^'\\])*'|"(?:\\.|[^"\\])*"`)
)

// mainBindingOf reads how main is bound at module level. Statements inside
// a top-level def or class are skipped, since they bind nothing on the
// module; statements inside a top-level if, try, with or loop are read,
// since they do. A usable binding anywhere wins over async ones, because
// the scan cannot tell which branch runs.
func mainBindingOf(source string) mainBinding {
	source = strings.TrimPrefix(strings.ReplaceAll(source, "\r\n", "\n"), "\ufeff")
	lines := strings.Split(source, "\n")
	found := noMain
	inBody, decorated := false, false
	for i := 0; i < len(lines); i++ {
		code := withoutComment(lines[i])
		statement := strings.TrimLeft(code, " \t")
		if strings.TrimSpace(statement) == "" {
			continue
		}
		if statement == code {
			inBody = opensBody.MatchString(statement)
		} else if inBody {
			continue
		}

		var consumed int
		statement, consumed = joinContinuation(statement, lines[i+1:])
		i += consumed
		if strings.HasPrefix(statement, "@") {
			decorated = true
			continue
		}
		for _, part := range strings.Split(statement, ";") {
			switch mainBoundBy(strings.TrimSpace(part), decorated) {
			case usableMain:
				found = usableMain
			case asyncMain:
				if found == noMain {
					found = asyncMain
				}
			}
		}
		decorated = false
	}
	return found
}

// joinContinuation joins a statement continued by open brackets or a
// trailing backslash, and returns it with the number of lines it used. A
// line at column 0 ends it unless it closes a bracket, so a miscounted
// bracket in a string cannot swallow the rest of the file.
func joinContinuation(statement string, rest []string) (string, int) {
	joined := strings.TrimSpace(statement)
	consumed := 0
	for consumed < len(rest) && continues(joined) {
		next := withoutComment(rest[consumed])
		if next != "" && next == strings.TrimLeft(next, " \t") && !strings.ContainsAny(next[:1], ")]}") {
			break
		}
		consumed++
		joined = strings.TrimSuffix(joined, "\\") + " " + strings.TrimSpace(next)
	}
	return joined, consumed
}

func continues(statement string) bool {
	code := stringLiteral.ReplaceAllString(statement, `""`)
	open := strings.Count(code, "(") + strings.Count(code, "[") + strings.Count(code, "{")
	closed := strings.Count(code, ")") + strings.Count(code, "]") + strings.Count(code, "}")
	return open > closed || strings.HasSuffix(code, "\\")
}

// withoutComment drops a # comment that is not inside a string literal.
func withoutComment(line string) string {
	var quote rune
	escaped := false
	for i, r := range line {
		switch {
		case escaped:
			escaped = false
		case quote != 0 && r == '\\':
			escaped = true
		case quote != 0 && r == quote:
			quote = 0
		case quote == 0 && (r == '\'' || r == '"'):
			quote = r
		case quote == 0 && r == '#':
			return strings.TrimRight(line[:i], " \t")
		}
	}
	return line
}

// mainBoundBy reads one statement. A decorator may turn an async def into
// a sync function, so a decorated async def counts as usable.
func mainBoundBy(statement string, decorated bool) mainBinding {
	if match := defMain.FindStringSubmatch(statement); match != nil {
		if match[1] != "" && !decorated {
			return asyncMain
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
	var bound []string
	for _, name := range strings.Split(strings.Trim(names, " ()"), ",") {
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
