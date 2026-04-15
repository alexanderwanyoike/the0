// Package runtime contains per-language dispatchers that translate a generic
// `the0 dev` Opts struct into a fully-populated *dev.RunSpec (i.e. a ready-to-
// run *exec.Cmd plus env-var paths).
//
// Each dispatcher supports two modes:
//
//   - Native: shell out to the host toolchain (e.g. `python`, `cargo run`).
//     Fast iteration, supports IDE debugger attach without plumbing.
//   - Docker: reuse the CLI's existing Docker builder to produce and run the
//     binary in a container. Slower but hermetic; auto-forwards the per-
//     language debug port so a host IDE can still attach.
//
// Dispatchers are pure functions: they build and return a RunSpec without
// touching the filesystem or network, which keeps them unit-testable without
// requiring any language toolchain installed.
package runtime

import "encoding/json"

type Mode string

const (
	ModeNative Mode = "native"
	ModeDocker Mode = "docker"
)

// Opts is the input to every per-language dispatcher.
type Opts struct {
	Mode Mode

	// Project layout
	Cwd    string // absolute path to the bot project root
	Script string // entrypoint, relative to Cwd (e.g. "main.py", "main.js")

	// Env contract (see dev.RunSpec for the CODE_MOUNT_DIR trick)
	BotID     string
	BotConfig json.RawMessage
	CodeDir   string // absolute host path; runner strips leading / for SDKs
	StateDir  string // absolute host path; SDKs consume as-is

	// Debugging
	Debug     bool
	DebugPort int  // 0 → use language-specific default
	DebugWait bool // native only; Python debugpy --wait-for-client, Node --inspect-brk

	// Build profile (compiled runtimes only)
	Release bool // false → debug build (fast iter); true → release build
}
