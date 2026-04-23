---
title: "Getting Started"
description: "Run your first bot locally in under three minutes"
order: 1
---

# Getting Started

Goal: go from a fresh project to a bot producing metrics locally in under three minutes.

## Prerequisites

- Docker running locally (Docker Desktop, OrbStack, or a plain docker daemon).
- A the0 bot project with a `main.py` or `main.js` and a `config.json`.
- The runtime image. On first run, pull it:
  ```bash
  docker pull the0/runtime:latest
  ```

No language toolchains need to be installed on your host — the runtime image has them.

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
i Running python bot "my-first-dev-bot"
14:02:13 METRIC heartbeat {"alive":true,"bot_id":"my-first-dev-bot"}
v Bot exited cleanly
```

The first run may be slower because Docker needs to start a container. Subsequent runs reuse the image layers.

## Keep it running

```bash
the0 dev --watch
```

Edit `main.py`, save, see a `-- restart --` separator, then the new run's output. File change → restart takes a few seconds because a fresh container is created per run. See [Watch mode](./watch) for the trade-off.

## Queries (realtime bots)

If your bot is `"type": "realtime/..."` and you have a `query.py` (or `query.js`) file, the dev tool detects it, keeps the in-container query server running, and forwards port `9476` to loopback. Combined with `--frontend`, the dashboard can call `fetch('/query/<path>')` against your live bot. See [Queries](./queries).

## Show the dashboard

If your project has `frontend/index.tsx`:

```bash
the0 dev --frontend
```

Open the printed URL. See [Frontend Dashboard](./frontend) for the one-line pattern your bundle needs so `useThe0Events()` works in dev.

## Next steps

- [State and Events](./state-and-events) — where `.the0/dev/<bot-id>/state` lives
- [Debugging](./debugging) — attach debugpy / Node inspector
- [FAQ](./faq) — common caveats
