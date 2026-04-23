---
title: "Debugging"
description: "Attach an IDE debugger to a locally-running bot"
order: 2
---

# Debugging

`the0 dev --debug` launches your bot under a debugger inside the runtime container, forwarding the debug port to `127.0.0.1` on the host so your IDE can attach. Python is `debugpy` (pre-installed), Node is V8's inspector (built in). No host toolchain setup needed.

## Python

```bash
the0 dev --debug                   # port 5678
the0 dev --debug --debug-wait      # block until attach
the0 dev --debug --debug-port 5999 # override port
```

VSCode `launch.json`:

```json
{
  "name": "Attach to the0 dev",
  "type": "debugpy",
  "request": "attach",
  "connect": { "host": "127.0.0.1", "port": 5678 }
}
```

PyCharm: **Run → Edit Configurations → Python Debug Server** → `127.0.0.1:5678`.

## Node.js

```bash
the0 dev --debug                 # port 9229
the0 dev --debug --debug-wait    # --inspect-brk
```

VSCode: **Run → Attach to Node Process** picks it up. JetBrains: "Node.js Remote" targeting `127.0.0.1:9229`. Chrome: `chrome://inspect`.

## How it works

`the0 dev --debug` passes `--debug-port` to `runtime execute`, which launches the entrypoint directly under the debugger (`python3 -m debugpy --listen 0.0.0.0:PORT ...` or `node --inspect=0.0.0.0:PORT ...`), bypassing the production wrappers. You lose wrapper-owned `result.json` writing in debug mode — harmless for dev.

## Compiled languages

Python and Node only in v1. Rust/C++/.NET/Scala/Haskell debug ships in phase 2 with a `runtime-dev` image carrying gdbserver/vsdbg.
