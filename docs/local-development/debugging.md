---
title: "Debugging"
description: "Attach an IDE debugger to a locally-running bot"
order: 3
---

# Debugging

`--debug` on `the0 dev` enables runtime-specific debugger support. Native mode relies on your IDE's "Attach to Process" flow; Docker mode forwards the per-runtime debug port to the host so the IDE can connect remotely.

## Native vs Docker

For **native mode** with interpreted runtimes (Python, Node), `--debug` injects the runtime's debug flag (`debugpy --listen`, `--inspect`). For **native mode** with compiled runtimes (Rust, C++, .NET, Scala), `--debug` is usually a no-op - your IDE attaches to the spawned process via PID without any CLI plumbing.

For **Docker mode**, `--debug` forwards the debug port to the host so your IDE can connect to `localhost:<port>`.

## Per-runtime recipes

### Python (VSCode, PyCharm)

```bash
the0 dev --debug
# or: the0 dev --debug --debug-wait    # block until debugger attaches
```

VSCode `launch.json`:

```json
{
  "name": "Attach to the0 dev",
  "type": "debugpy",
  "request": "attach",
  "connect": { "host": "localhost", "port": 5678 }
}
```

### Node (VSCode, WebStorm)

```bash
the0 dev --debug
```

Use the "Node.js: Attach" configuration pointed at port 9229. Chrome DevTools also works via `chrome://inspect`.

### Rust (VSCode with CodeLLDB, RustRover)

**Native**: just run `the0 dev` and attach to the process by PID in your IDE. No flags needed.

**Docker**: `the0 dev --debug --docker` forwards port 2345 for `lldb-server`. You'll need to install `lldb` in a custom image; the stock `rust:latest` doesn't ship it. Recommended image override (configure via environment in a future release):

```dockerfile
FROM rust:latest
RUN apt-get update && apt-get install -y lldb
```

### .NET (Rider, VSCode with C# Dev Kit)

**Native**: IDE attaches by PID.

**Docker**: `the0 dev --debug --docker` forwards port 4711 for `netcoredbg`. Install `netcoredbg` on your host (via GitHub releases) and point Rider or VSCode at `localhost:4711`.

### C++ (VSCode, CLion)

**Native**: attach to the PID spawned by `make run`. CLion's "Run → Attach to Process" and VSCode's "(gdb) Attach" both work.

**Docker**: port 2346 is forwarded for `gdbserver`. Container needs gdb installed.

### Scala (IntelliJ)

```bash
the0 dev --debug
```

Sets `JAVA_TOOL_OPTIONS=-agentlib:jdwp=...` so the JVM listens on port 5005. In IntelliJ: **Run → Attach to Process** → connect to `localhost:5005`.

### Haskell

No remote debugger is supported. Use `print` debugging or GHCi's interactive mode outside `the0 dev`.

## Custom debug port

If the default port collides with something else on your machine:

```bash
the0 dev --debug --debug-port 5999
```

## Wait for the debugger to attach

For Python (`debugpy`) and Node (`--inspect-brk`), you can block the bot until the debugger connects:

```bash
the0 dev --debug --debug-wait
```

Useful for debugging startup-time logic. Without `--debug-wait`, the bot runs normally and you attach whenever you want.
