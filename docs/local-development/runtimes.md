---
title: "Runtimes"
description: "Per-language setup for the0 dev"
order: 2
---

# Runtimes

`the0 dev` auto-detects your bot's runtime from the project files it finds (`Cargo.toml` → rust, `package.json` → nodejs, `main.py` → python, etc.) and picks a dispatcher.

Each runtime has two execution modes:

- **Native** (default when the host toolchain is installed): fastest iteration, IDE breakpoints work directly.
- **Docker** (`--docker`, or auto-fallback): hermetic, no local toolchain required, debug ports are forwarded to the host.

## Matrix

| Runtime | Detected by | Native command | Docker image | Debug port |
|---------|-------------|----------------|--------------|-----------|
| Python | `main.py`, `requirements.txt`, `pyproject.toml` | `python main.py` | `python:3.11-slim` | 5678 (debugpy) |
| Node.js | `package.json`, `main.js` | `node main.js` | `node:20-slim` | 9229 (inspect) |
| Rust | `Cargo.toml` | `cargo run [--release]` | `rust:latest` | 2345 (lldb-server) |
| .NET | `*.csproj` | `dotnet run` | `mcr.microsoft.com/dotnet/sdk:8.0` | 4711 (netcoredbg) |
| C++ | `CMakeLists.txt`, `Makefile` | `make run` | `mcr.microsoft.com/devcontainers/cpp` | 2346 (gdbserver) |
| Scala | `build.sbt` | `sbt run` | `sbtscala/scala-sbt:...` | 5005 (JDWP) |
| Haskell | `*.cabal` | `cabal run` | `haskell:9.6` | - (no remote debugger) |

## Choosing a mode

- **No flag**: `the0 dev` checks PATH for the toolchain. Present → native. Missing → docker.
- **`--native`**: force host toolchain. Fails clearly if not installed.
- **`--docker`**: always use the container image. Slower first invocation (image pull), but hermetic.

## Release vs debug builds

Compiled runtimes (rust, cpp, dotnet, scala) default to debug builds for fast iteration. Use `--release` to compile with optimisations - closer to production behaviour, much slower to rebuild.

```bash
the0 dev --release
```

## Runtime-specific notes

### Python

SDKs emit logs to stderr as JSON (`level`, `message`) and metrics to stdout as JSON tagged with `_metric`. Python's built-in `logging` formatter usually matches. If you use `print()` directly, lines show up as `print` events with the stdout stream tagged.

### Node.js

Same contract. If you use `console.log({_metric: "price", ...})` without `JSON.stringify`, Node stringifies the object - which produces `[object Object]` and shows as `print` instead of `metric`. Use the SDK's `metric()` or `JSON.stringify` explicitly.

### Rust

`cargo run` produces colourised output on stderr; `the0 dev` strips ANSI codes before classification. The first `cargo run` compiles deps - expect a slow first iteration, subsequent runs are incremental.

### .NET

`dotnet run` ignores `Release`/`Debug` folder conventions for this purpose - the `--configuration Release` flag is passed when `--release` is set. The first run resolves NuGet packages; keep your project root small.

### C++

C++ has no "one true" run command, so `the0 dev` shells out to `make run`. Your Makefile or CMakeLists is responsible for wiring `make run` to build-and-execute. Minimal CMake setup:

```makefile
# Makefile
run:
	cmake -B build
	cmake --build build
	./build/bot
```

### Scala

The first `sbt run` boots the JVM and resolves dependencies - expect 10-40 seconds of cold start. Subsequent runs are much faster because sbt keeps a server alive in the background.

### Haskell

`cabal run` invokes the default executable target in your `.cabal` file. GHC has no practical remote-debug protocol, so `--debug` returns an explicit error on Haskell.

## Example projects

The repo ships working examples for each runtime under `example-bots/`:

- `python-portfolio-tracker`
- `typescript-price-alerts`
- `rust-sma-crossover`
- `csharp-sma-crossover`
- `cpp-sma-crossover`
- `scala-sma-crossover`
- `haskell-sma-crossover`

Each has a `config.json` and will run with `the0 dev` out of the box.
