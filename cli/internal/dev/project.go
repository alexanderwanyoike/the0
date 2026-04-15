package dev

import (
	"fmt"
	"os"
	"path/filepath"
)

// Runtime names the language backing a bot project. Detected from the on-disk
// file layout (Cargo.toml → rust, main.py → python, etc.). Used to pick the
// per-runtime dispatcher.
type Runtime string

const (
	RuntimePython  Runtime = "python"
	RuntimeNode    Runtime = "node"
	RuntimeRust    Runtime = "rust"
	RuntimeCpp     Runtime = "cpp"
	RuntimeDotnet  Runtime = "dotnet"
	RuntimeScala   Runtime = "scala"
	RuntimeHaskell Runtime = "haskell"
)

// DetectRuntime inspects a project directory and returns the runtime it
// belongs to. The order of checks matches the deploy-side detection in
// cli/internal/builder.go, so `the0 dev` agrees with `the0 custom-bot deploy`.
func DetectRuntime(projectDir string) (Runtime, error) {
	has := func(name string) bool {
		_, err := os.Stat(filepath.Join(projectDir, name))
		return err == nil
	}
	hasGlob := func(pattern string) bool {
		matches, _ := filepath.Glob(filepath.Join(projectDir, pattern))
		return len(matches) > 0
	}

	switch {
	case has("Cargo.toml"):
		return RuntimeRust, nil
	case hasGlob("*.csproj"):
		return RuntimeDotnet, nil
	case has("build.sbt"):
		return RuntimeScala, nil
	case has("CMakeLists.txt") || has("Makefile"):
		return RuntimeCpp, nil
	case hasGlob("*.cabal"):
		return RuntimeHaskell, nil
	case has("package.json"):
		return RuntimeNode, nil
	case has("main.js") || has("main.mjs"):
		return RuntimeNode, nil
	case has("requirements.txt") || has("pyproject.toml") || has("main.py"):
		return RuntimePython, nil
	}
	return "", fmt.Errorf("could not detect bot runtime in %s (no Cargo.toml, package.json, main.py, etc.)", projectDir)
}

// DefaultScript is the conventional entrypoint file name per runtime. Only
// used when the user hasn't set one explicitly.
func (r Runtime) DefaultScript() string {
	switch r {
	case RuntimePython:
		return "main.py"
	case RuntimeNode:
		return "main.js"
	default:
		return ""
	}
}
