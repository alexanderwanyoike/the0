package dev

import (
	"fmt"
	"os"
	"path/filepath"
)

// Runtime names the language backing a bot project. Detected from the
// on-disk file layout (main.py → python, package.json → node).
type Runtime string

const (
	RuntimePython Runtime = "python"
	RuntimeNode   Runtime = "node"
)

// DetectRuntime inspects a project directory and returns the runtime it
// belongs to. Only Python and Node are supported in v1; compiled-language
// layouts (Cargo.toml, *.csproj, build.sbt, CMakeLists.txt/Makefile,
// *.cabal) return a friendly error directing users to phase 2.
//
// Manifest-first ordering matches the deploy-side detection in
// cli/internal/builder.go for the runtimes supported today.
func DetectRuntime(projectDir string) (Runtime, error) {
	has := func(name string) bool {
		_, err := os.Stat(filepath.Join(projectDir, name))
		return err == nil
	}
	hasGlob := func(pattern string) bool {
		matches, _ := filepath.Glob(filepath.Join(projectDir, pattern))
		return len(matches) > 0
	}

	// Compiled-language layouts: phase-2 scope. Reject early with an
	// actionable message.
	switch {
	case has("Cargo.toml"):
		return "", phase2Error("Rust (Cargo.toml)")
	case hasGlob("*.csproj"):
		return "", phase2Error(".NET (*.csproj)")
	case has("build.sbt"):
		return "", phase2Error("Scala (build.sbt)")
	case has("CMakeLists.txt") || has("Makefile"):
		return "", phase2Error("C++ (CMakeLists.txt / Makefile)")
	case hasGlob("*.cabal"):
		return "", phase2Error("Haskell (*.cabal)")
	}

	switch {
	case has("package.json"):
		return RuntimeNode, nil
	case has("main.js") || has("main.mjs"):
		return RuntimeNode, nil
	case has("requirements.txt") || has("pyproject.toml") || has("main.py"):
		return RuntimePython, nil
	}
	return "", fmt.Errorf("could not detect bot runtime in %s (no package.json, main.py, etc.)", projectDir)
}

func phase2Error(lang string) error {
	return fmt.Errorf(
		"%s bots are not yet supported by `the0 dev`; Python and Node are supported in v1, compiled-language support is coming in phase 2",
		lang,
	)
}

// DefaultScript is the conventional entrypoint file name per runtime.
func (r Runtime) DefaultScript() string {
	switch r {
	case RuntimePython:
		return "main.py"
	case RuntimeNode:
		return "main.js"
	}
	return ""
}
