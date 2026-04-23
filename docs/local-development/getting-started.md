---
title: "Getting Started"
description: "Run your first bot locally in under three minutes"
order: 1
---

# Getting Started

Go from a fresh project to a bot producing metrics locally in under three minutes.

## Prerequisites

- Docker running locally.
- The runtime image: `docker pull the0/runtime:latest`
- A bot project with `main.py` (or `main.js`) and `config.json`.

No host-side language toolchain required — the runtime image ships them.

## Minimal project

```python
# main.py
from the0 import parse, success, metric

def main(bot_id, config):
    metric("heartbeat", {"alive": True, "bot_id": bot_id})
    success("done")

if __name__ == "__main__":
    main(*parse())
```

```json
// config.json
{"name": "my-first-dev-bot", "type": "scheduled/heartbeat", "version": "0.1.0"}
```

## Run it

```bash
the0 dev
```

```
i Detected runtime: python
i Running python bot "my-first-dev-bot"
14:02:13 METRIC heartbeat {"alive":true,"bot_id":"my-first-dev-bot"}
v Bot exited cleanly
```

First run is slower (container start); subsequent runs reuse cached layers.

## Options

- `the0 dev --watch` — auto-restart on source change. See [Watch mode](./watch).
- `the0 dev --debug` — attach an IDE debugger. See [Debugging](./debugging).
- `the0 dev --frontend` — serve your dashboard. See [Frontend](./frontend).
- `the0 dev --reset` — wipe state and exit.

For realtime bots with a `query.py` / `query.js` file, the CLI forwards the bot's query port so [Queries](./queries) work end-to-end.

## Where things live

`.the0/dev/<bot-id>/state/` on your host, mounted at `/state` in the container. See [State and Events](./state-and-events).
