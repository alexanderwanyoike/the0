---
title: "Watch Mode"
description: "Auto-restart the bot on source changes"
order: 5
---

# Watch Mode

```bash
the0 dev --watch
```

Edit a Python or Node source file. After a short debounce, you'll see:

```
-- restart --
```

followed by the output of a fresh run. Repeat forever until you Ctrl-C.

## What gets watched

Per-runtime file filters:

- **Python**: `*.py`, plus `requirements.txt`, `pyproject.toml`
- **Node**: `*.js`, `*.ts`, `*.mjs`, plus `package.json`, `package-lock.json`

Always excluded: `node_modules`, `dist`, `build`, `bin`, `.the0`, `vendor`, `__pycache__`, `.venv`, `venv`.

Newly created directories are watched recursively as they appear, so a `git checkout` of a branch with new packages doesn't miss nested edits.

## Debounce and restart

File writes within a 500 ms window collapse into a single restart. A burst of IDE-generated saves (autoformat, linter writes) won't produce duplicate restarts — the debouncer is a single `time.Timer` with `Stop+drain+Reset` on each new event so the "double fire" failure mode is structurally impossible.

## Latency expectations

Watch mode restarts a whole container per source change. On a cached image, that's **1.5 – 3 seconds** before the next run's first stdout line appears. That's the honest cost of matching production's execution environment on every iteration.

If that's too slow for a specific workflow, run without `--watch` and re-invoke `the0 dev` by hand; the image layer cache keeps invocation time the same.

## Combined with `--frontend`

```bash
the0 dev --watch --frontend
```

On each restart:

- The bot relaunches.
- The dev shell emits a `restart` sentinel over the event WebSocket so your dashboard's Provider clears accumulated events and starts fresh.
- The dashboard bundle is rebuilt from `frontend/index.tsx` so TSX changes land without a manual page refresh.

## Signals

Ctrl-C exits watch mode cleanly: the currently-running bot container stops, the watcher shuts down, and the CLI returns.
