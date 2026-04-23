---
title: "FAQ"
description: "Known rough edges and common questions"
order: 7
---

# FAQ

## My metric isn't appearing

The SDK emits metrics as JSON objects on **stdout**, with a `_metric` key naming the metric type:

```json
{"_metric":"price","symbol":"BTC","value":45000}
```

`the0 dev` classifies a line as a metric only if:

- The entire line is a JSON object (after trimming whitespace and stripping ANSI codes)
- It has `_metric` as a top-level **string** key

If the `_metric` value is a number or your SDK call stringified the object non-trivially, the line falls back to `print`. Use the SDK's `metric()` helper rather than hand-rolling JSON.

## First run is slow

Expected. The runtime image pulls ~800 MB on first use, and each restart spawns a fresh container. Cached layers bring subsequent starts down to 1–3 s. Watch mode has a per-restart container-start cost; see [Watch mode](./watch).

## Two terminals, same bot = error

By design. `.the0/dev/<bot-id>/.lock` prevents concurrent invocations stomping state. Run `--bot-id <something-else>` for a second isolated instance.

## Dashboards calling `fetch('/something')` 404

The dev shell only proxies `/query/*` (for realtime bots). Other paths aren't forwarded. Keep non-query calls behind a "production" flag or mock them in dev.

## Where does state live?

`.the0/dev/<bot-id>/state/` on your host, mounted as `/state` in the container. See [State and Events](./state-and-events) for the details.

## What about compiled languages?

Python and Node are supported in v1. Rust, C++, .NET, Scala, and Haskell are phase 2 — they need a builder-image or pre-built-artifact strategy that warrants its own PR.

## How do I update the shell's React version?

Pinned to React 18 via unpkg CDN inside `cli/internal/dev/frontend/shell.html`. Custom override is deferred.

## `--debug` doesn't attach my IDE

Check: the port (`5678` Python, `9229` Node) isn't already in use on your host, your IDE's attach configuration targets `127.0.0.1` (not `0.0.0.0` or a LAN IP), and you ran `the0 dev --debug` (not just `the0 dev`). See [Debugging](./debugging).

## Docker isn't running / I don't want Docker

Docker Desktop (or any docker daemon) is required — `the0 dev` invokes `docker run` to launch the runtime image. If you can't run Docker, a native-mode path may come back in a future release, but prod parity was worth the swap.
