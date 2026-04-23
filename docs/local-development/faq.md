---
title: "FAQ"
description: "Known rough edges"
order: 7
---

# FAQ

## My metric isn't appearing

The SDK emits metrics as JSON on stdout with `_metric` as a top-level string key:

```json
{"_metric":"price","value":45000}
```

If `_metric` is a number or the JSON isn't a single line, the parser falls back to `print`. Use the SDK's `metric()` helper.

## First run is slow

The runtime image is ~800 MB on first pull, and each run starts a fresh container. Subsequent starts take 1–3 s once layers are cached. Watch mode pays a per-restart container-start cost; see [Watch mode](./watch).

## Two terminals on the same bot = error

By design. `.the0/dev/<bot-id>/.lock` prevents concurrent state-stomping. Use `--bot-id <other>` for a parallel instance.

## Dashboards calling `fetch('/...')` 404

Only `/query/*` is proxied (for realtime bots). Other paths aren't forwarded. Mock or feature-flag them in dev.

## Compiled languages?

Python and Node are supported in v1. Rust/C++/.NET/Scala/Haskell need a builder-image strategy; coming in phase 2.

## `--debug` doesn't attach

Check: port (`5678` Python, `9229` Node) isn't taken; IDE targets `127.0.0.1` (not `0.0.0.0`); you passed `--debug`. See [Debugging](./debugging).

## I don't have Docker

Docker is currently required — `the0 dev` invokes `docker run` against the runtime image. A native-mode path may return in a future release.
