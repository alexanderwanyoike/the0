---
title: "Debugging"
description: "Attach an IDE debugger to a locally-running bot"
order: 3
---

# Debugging

`the0 dev --debug` launches your bot under a debugger inside the runtime container, forwarding the debug port to `127.0.0.1` on the host so your IDE can attach.

Python is debugged via `debugpy` (pre-installed in the runtime image). Node uses V8's built-in inspector. Both ship and work with no host-side toolchain setup.

## Python

```bash
the0 dev --debug
# or block until the debugger attaches:
the0 dev --debug --debug-wait
```

Port defaults to `5678`. VSCode `launch.json`:

```json
{
  "name": "Attach to the0 dev",
  "type": "debugpy",
  "request": "attach",
  "connect": { "host": "127.0.0.1", "port": 5678 }
}
```

PyCharm: **Run → Edit Configurations → Python Debug Server** → host `127.0.0.1`, port `5678`.

Under the hood, the CLI passes `--debug-port 5678` to the runtime's `execute` command, which runs `python3 -m debugpy --listen 0.0.0.0:5678 /bot/main.py`, bypassing the usual wrapper.

## Node.js

```bash
the0 dev --debug
# or with --debug-wait to wait for attach
```

Port defaults to `9229`. The runtime runs `node --inspect=0.0.0.0:9229 /bot/main.js` (or `--inspect-brk` with `--debug-wait`).

VSCode: **Run → Attach to Node Process** picks up the port automatically.
WebStorm / JetBrains IDEs: attach to `127.0.0.1:9229` via a "Node.js Remote" run configuration.
Chrome: open `chrome://inspect` and connect.

## Custom port

If the default port collides with another process:

```bash
the0 dev --debug --debug-port 5999
```

The port is used for both the in-container listen address and the host-side forward.

## Why debug mode bypasses the wrapper

The runtime image's Python/Node wrappers own signal handling, `BOT_CONFIG` parsing, and `result.json` writing in production. Loading `debugpy` or Node's inspector inside the wrapper would require wrapper-side changes, which we keep off-limits. Instead, `--debug-port` asks `runtime execute` to launch the entrypoint directly under the debugger, skipping the wrapper. You lose `result.json` writing in debug mode — harmless for local iteration.

## Compiled languages

Python and Node are the only runtimes supported in v1. Compiled-language debugging (Rust, C++, .NET, Scala, Haskell) ships in phase 2 together with the compiled-runtime image strategy.
