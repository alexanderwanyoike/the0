---
title: "State and Events"
description: "Where the0 dev stores state between runs and how to inspect the event log"
order: 5
---

# State and Events

`the0 dev` writes everything to `.the0/dev/<bot-id>/` under your project root. One bot, one directory.

## Layout

```
.the0/dev/<bot-id>/
  .lock              flock file; prevents two the0 dev processes on the same bot
  code/              CODE_MOUNT_DIR - the "mount" dir the bot writes result.json into
  state/             STATE_DIR - SDK state.set/get backing store
  events.jsonl       append-only newline-delimited JSON log of every event
```

`<bot-id>` defaults to a slug of `config.name`. Override with `--bot-id`.

## State persistence

Every call to `the0.state.set(key, value)` writes a file under `state/`. On the next run, `the0.state.get(key)` reads it back. This lets you iterate on logic that depends on prior runs (moving averages, position history, counters) without re-priming from scratch each time.

## Reset

To wipe state and start fresh:

```bash
the0 dev --reset
```

This removes the entire `.the0/dev/<bot-id>/` directory.

## Inspecting events.jsonl

Every metric, log, and print event the bot emits (plus `restart` and `bot_stopped` sentinels from `--watch`) is appended as a JSON record:

```jsonl
{"kind":"metric","stream":"stdout","timestamp":"2026-04-15T21:59:54Z","metric_type":"price","data":{"_metric":"price","value":44658}}
{"kind":"log","stream":"stderr","timestamp":"...","level":"INFO","data":{"level":"INFO","message":"bot started"}}
{"kind":"print","stream":"stdout","timestamp":"...","raw":"debug checkpoint"}
{"kind":"restart","timestamp":"..."}
```

Useful for post-hoc debugging:

```bash
# last 20 events
tail -n 20 .the0/dev/<bot-id>/events.jsonl | jq

# just the metrics
jq 'select(.kind=="metric")' .the0/dev/<bot-id>/events.jsonl

# errors only
jq 'select(.level=="ERROR")' .the0/dev/<bot-id>/events.jsonl
```

The file grows indefinitely - it's never rotated or trimmed. For long-lived `--watch` sessions, `--reset` before starting is a good habit.

## How `CODE_MOUNT_DIR` works

In production, bots run in a Kubernetes pod with `/bot` mounted as `CODE_MOUNT_DIR`. The SDKs hardcode `f"/{CODE_MOUNT_DIR}/result.json"` to produce `/bot/result.json` inside the container.

Locally, there is no `/bot` on your host filesystem. `the0 dev` uses a trick: it sets `CODE_MOUNT_DIR` to the host absolute path **without** its leading slash (e.g. `home/alex/.the0/dev/my-bot/code`). The SDK's `"/" + CODE_MOUNT_DIR + "/result.json"` reconstruction then produces the correct absolute host path. Zero SDK modifications needed; the dispatcher in `cli/internal/dev/runner.go` owns this translation.

## Concurrent invocations

`.lock` is a POSIX flock file. A second `the0 dev` on the same bot fails immediately with the holder's PID:

```
another `the0 dev` is running for this bot (PID 12345); stop it or use a different --bot-id
```

Pass `--bot-id <other>` to run a second copy in parallel against an isolated state dir.
