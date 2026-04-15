---
title: "Getting Started"
description: "Run your first bot locally in under three minutes"
order: 1
---

# Getting Started

Goal: go from a fresh project to a bot producing metrics locally in under three minutes.

## Prerequisites

- A the0 bot project (see [Custom Bot Development](../custom-bot-development/overview)) with either a `main.py`, `main.js`, `Cargo.toml`, `package.json`, etc.
- The host toolchain for your chosen runtime installed (e.g. `python3`, `node`, `cargo`). Don't have it? Use `--docker`.

## Minimal project

A Python bot that emits one metric and exits:

```python
# main.py
from the0 import parse, success, metric

def main(bot_id, config):
    metric("heartbeat", {"alive": True, "bot_id": bot_id})
    success("done")

if __name__ == "__main__":
    bot_id, config = parse()
    main(bot_id, config)
```

```json
// config.json
{
  "name": "my-first-dev-bot",
  "type": "scheduled/heartbeat",
  "version": "0.1.0"
}
```

## Run it

```bash
the0 dev
```

Expected output:

```
i Detected runtime: python
i Running python bot "my-first-dev-bot" (mode=native)
14:02:13 METRIC heartbeat {"alive":true,"bot_id":"my-first-dev-bot"}
v Bot exited cleanly
```

That's it. Metrics are tagged `_metric` in the SDK output; `the0 dev` parses them back into typed events.

## Keep it running

Add `--watch` to re-run on every source edit:

```bash
the0 dev --watch
```

Edit `main.py`, save, watch it re-run automatically. A `-- restart --` separator appears between runs.

## Also see the dashboard

If your project has a `frontend/index.tsx` that uses `useThe0Events()`:

```bash
the0 dev --frontend
```

The CLI prints a localhost URL. Open it in a browser and your dashboard renders against the live event stream. See [Frontend Dashboard](./frontend) for the full story.

## Next steps

- [Runtimes](./runtimes) - run the same flow with Node, Rust, .NET, etc.
- [Debugging](./debugging) - wire up your IDE's debugger
