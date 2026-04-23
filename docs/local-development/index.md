---
title: "Local Development"
description: "Run the0 bots on your laptop with live metrics, logs, queries, and dashboards"
order: 4
---

# Local Development

`the0 dev` runs your bot on your laptop inside the same container image production uses. Metrics and logs stream to your terminal in real time, state persists between runs, queries can be exercised against the running bot, and the custom bot dashboard renders in your browser with a live event stream.

## How it works

`the0 dev` delegates bot execution to `the0/runtime:latest`. That's the container image running in Kubernetes today; it ships all seven language runtimes, the Python/Node wrappers, and the same env contract (`BOT_ID`, `BOT_CONFIG`, `STATE_DIR`, `CODE_MOUNT_DIR`) production uses. The CLI mounts your code at `/bot`, mounts `.the0/dev/<bot-id>/state` at `/state`, forwards the bot's query port to loopback, and pipes the container's stdout/stderr through a line parser that classifies each line as a metric, log, or print.

Net result: what works in `the0 dev` works in production.

## Supported runtimes (v1)

- Python 3.11
- Node.js 20

Compiled languages (Rust, C++, .NET, Scala, Haskell) are coming in phase 2 — they need a builder-image or pre-built-artifact strategy.

## Start here

- [Getting Started](./getting-started) — three-minute hello world
- [State and Events](./state-and-events) — where state lives and how metrics/logs are classified
- [Queries](./queries) — testing realtime bot query endpoints
- [Debugging](./debugging) — attach your IDE to debugpy or the Node inspector
- [Watch mode](./watch) — auto-restart on source change
- [Frontend Dashboard](./frontend) — `--frontend` walkthrough
- [FAQ](./faq) — known rough edges and answers
