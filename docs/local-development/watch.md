---
title: "Watch Mode"
description: "Auto-restart on source change"
order: 5
---

# Watch Mode

```bash
the0 dev --watch
```

Edit a source file, see `-- restart --`, then a fresh run's output. Repeats until Ctrl-C.

## Filters

- **Python**: `*.py` + `requirements.txt`, `pyproject.toml`
- **Node**: `*.js`, `*.ts`, `*.mjs` + `package.json`, `package-lock.json`

Always excluded: `node_modules`, `dist`, `build`, `bin`, `.the0`, `vendor`, `__pycache__`, `.venv`, `venv`. Newly-created directories are watched recursively.

## Debounce

File writes within a 500 ms window collapse into a single restart. Implementation is a single `time.Timer` with `Stop+drain+Reset` so the historical "double-fire on save burst" failure mode is structurally impossible.

## Latency

Each restart recreates a container. Expect **1.5 – 3 s** between save and first output. That's the cost of matching production on every iteration. Drop `--watch` if you need sub-second loops; invoke `the0 dev` by hand instead.

## With `--frontend`

```bash
the0 dev --watch --frontend
```

On each restart: the bot relaunches, a `restart` sentinel goes over the event WebSocket (dashboard clears its buffer), and the TSX bundle rebuilds.
