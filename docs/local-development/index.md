---
title: "Local Development"
description: "Run the0 bots on your laptop with live metrics, logs, and dashboards"
order: 4
---

# Local Development

`the0 dev` runs your bot on your laptop against the same env-var contract production uses. Metrics and logs stream to your terminal in real time, state persists between runs, and (optionally) the custom bot dashboard renders in your browser against the live event stream. No deploy cycle, no Docker unless you want it.

## Why this exists

Before `the0 dev`, iterating on a bot meant:

1. Edit `main.py`
2. `the0 custom-bot deploy` - zips, uploads, schedules (~90 s)
3. Wait for the next run
4. Read platform logs
5. Repeat

With `the0 dev`, the loop is ~1 s: save file, see output. IDE breakpoints work. The state directory persists so you can iterate on logic that depends on history.

## Design in one paragraph

`the0 dev` builds an `exec.Cmd` per runtime, sets `BOT_ID` / `BOT_CONFIG` / `CODE_MOUNT_DIR` / `STATE_DIR` the same way the production Kubernetes pods do, runs your bot, and pipes stdout/stderr through a small parser that classifies each line as a metric, log, or plain print. SDKs are not modified - the CLI adapts to the existing contract by stripping the leading slash from the host path before exporting `CODE_MOUNT_DIR`, so the SDK's `/` + `CODE_MOUNT_DIR` + `/result.json` reconstruction produces the correct host path.

## Start here

- [Getting Started](./getting-started) - 3-minute hello world
- [Runtimes](./runtimes) - per-language setup (python, nodejs, rust, cpp, dotnet, scala, haskell)
- [Debugging](./debugging) - IDE attach recipes
- [Frontend Dashboard](./frontend) - `--frontend` walkthrough
- [State and Events](./state-and-events) - where things are stored and how to inspect them
- [FAQ](./faq) - known rough edges and answers
