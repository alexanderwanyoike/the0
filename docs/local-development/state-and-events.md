---
title: "State and Events"
description: "Where the0 dev stores state between runs and how events are classified"
order: 6
---

# State and Events

`the0 dev` writes everything to `.the0/dev/<bot-id>/` under your project root. One bot, one directory.

## Layout

```
.the0/dev/<bot-id>/
  .lock              flock file; prevents two the0 dev processes on the same bot
  state/             SDK state.set/get backing store — mounted at /state in the container
```

`<bot-id>` defaults to a slug of `config.name`. Override with `--bot-id`.

## State persistence

The runtime image mounts your host `state/` directory at `/state` and sets `STATE_DIR=/state`. Every call to `the0.state.set(key, value)` writes `/state/<key>.json`; `the0.state.get(key)` reads it back. The next `the0 dev` run sees exactly the state the previous run left behind — useful for iterating on logic that depends on history (moving averages, position counters, prior signals) without re-priming each time.

You can inspect state directly on the host:

```bash
ls .the0/dev/<bot-id>/state
cat .the0/dev/<bot-id>/state/portfolio.json | jq
```

## Reset

To wipe state and start fresh:

```bash
the0 dev --reset
```

Removes the entire `.the0/dev/<bot-id>/` directory.

## Events

Every line the bot writes to stdout or stderr is classified by the line parser into one of three event kinds:

| Kind | Triggered when | Displayed as |
|---|---|---|
| `metric` | Full-line JSON object with a `_metric` string field | `HH:MM:SS METRIC <name> {...payload}` in cyan |
| `log` | Full-line JSON object with `level` and `message` fields | `HH:MM:SS LEVEL message` with per-level colour |
| `print` | Anything else (includes plaintext, partial JSON) | `HH:MM:SS stdout/stderr raw-line` |

ANSI escape sequences are stripped before classification, so colourised build output (e.g. `cargo`, `sbt`) can still be recognised as logs or metrics when the JSON is embedded.

The `--watch` and `--frontend` modes add two synthetic kinds:

- `restart` — emitted on every source-change restart so the dashboard can clear its event buffer.
- `bot_stopped` — emitted when the bot exits on its own and watch mode is waiting for the next change.

## Concurrent invocations

`.lock` is a POSIX flock file. A second `the0 dev` on the same bot fails immediately with the holder's PID:

```
another `the0 dev` is running for this bot (PID 12345); stop it or use a different --bot-id
```

Pass `--bot-id <other>` to run a second copy in parallel against an isolated state dir.

## How `CODE_MOUNT_DIR` works

In production, bots run in a container with `/bot` mounted as their code directory. The SDKs hardcode `f"/{CODE_MOUNT_DIR}/result.json"` to produce `/bot/result.json`. `the0 dev` sets `CODE_MOUNT_DIR=bot` (no leading slash) so the SDK's reconstruction yields the same `/bot/result.json` path inside the container. This contract is owned by `runtime/internal/execute/helpers.go:BuildBotEnv` in the runtime image; `the0 dev` just passes `STATE_DIR=/state` explicitly and lets the rest follow.
