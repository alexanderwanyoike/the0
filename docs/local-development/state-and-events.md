---
title: "State and Events"
description: "Where the0 dev stores state and how events are classified"
order: 6
---

# State and Events

Per-bot local data lives at `.the0/dev/<bot-id>/`:

```
.the0/dev/<bot-id>/
  .lock     flock preventing two the0 dev on the same bot
  state/    SDK state backing store; mounted at /state in the container
```

`<bot-id>` defaults to a slug of `config.name`. Override with `--bot-id`.

## State persistence

Every `the0.state.set(key, value)` writes `/state/<key>.json` in the container, which lands at `.the0/dev/<bot-id>/state/<key>.json` on your host. The next run reads the same file. `STATE_DIR=/state` is exported explicitly so state sits at the mount root, not a nested `.the0-state` subdir.

Inspect directly on the host:

```bash
ls .the0/dev/<bot-id>/state
cat .the0/dev/<bot-id>/state/portfolio.json | jq
```

Reset with `the0 dev --reset` (wipes the whole directory).

## Event classification

Each stdout/stderr line is classified by the parser (ANSI-stripped first):

| Kind | Triggered by | Rendered as |
|---|---|---|
| `metric` | JSON object with `_metric` string field | `HH:MM:SS METRIC <name> {...}` in cyan |
| `log` | JSON object with `level` + `message` | `HH:MM:SS LEVEL message` colour per level |
| `print` | anything else | `HH:MM:SS stdout/stderr raw` |

`--watch` adds `restart` and `bot_stopped` sentinels so the dashboard can clear its event buffer between runs.

## Concurrent invocations

A second `the0 dev` on the same bot ID fails with the holder's PID. Use `--bot-id <other>` for a parallel isolated instance.

## How `CODE_MOUNT_DIR` works

Production bots read `CODE_MOUNT_DIR` and prepend `/` to build paths like `/bot/result.json`. `the0 dev` sets `CODE_MOUNT_DIR=bot` (no leading slash) via the runtime image's `BuildBotEnv`, so the SDK's reconstruction gives the same path inside the container.
