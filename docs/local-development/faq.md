---
title: "FAQ"
description: "Known rough edges and common questions"
order: 6
---

# FAQ

## My metric isn't appearing

The SDK emits metrics as JSON objects on **stdout**, with a `_metric` key naming the metric type:

```json
{"_metric":"price","symbol":"BTC","value":45000}
```

`the0 dev` only classifies a line as a metric if:

- The entire line is a JSON object (after trimming whitespace and stripping ANSI codes)
- It has `_metric` as a top-level **string** key

If the metric value is a number or your SDK call stringified the object non-trivially, the line falls back to `print`. Use the SDK's `metric()` helper rather than hand-rolling JSON.

## ANSI escape codes in my terminal

Compiled toolchains (cargo, sbt) emit colour codes that can wrap JSON output. The parser strips common CSI sequences before classification, so colourised metric lines still classify correctly. If you see escape codes in your terminal output (`\x1b[31m...`), that's the bot's own output - the shell's colour support is independent of the parser.

## Scala first run is slow

Expected. `sbt run` cold-starts the JVM and resolves dependencies (10-40 s). Keep a `the0 dev --watch` session running and subsequent rebuilds are fast thanks to the sbt server.

## Haskell `--debug` fails

Haskell (GHC) has no practical remote debugger. The dispatcher returns an explicit error rather than silently ignoring `--debug`.

## Native debugging "just works" for Rust/.NET/C++

For compiled languages, `--debug` on native mode is a no-op. Your IDE attaches to the running process by PID via the OS's standard debugging facilities (lldb on macOS, gdb/lldb on Linux). No port forwarding is involved.

Docker mode forwards the per-runtime debug port (2345/2346/4711) so your IDE can connect remotely - but the Docker image needs the debug server preinstalled, which the stock builder images don't include. Rolling a custom debug image is documented in [Debugging](./debugging).

## Two terminals, same bot = error

By design. `.the0/dev/<bot-id>/.lock` prevents concurrent invocations stomping state. Run `--bot-id <something-else>` for a second isolated instance.

## `result.json` is written to a weird host path

`the0 dev` uses the host absolute path as `CODE_MOUNT_DIR` with the leading slash stripped. That's the trick that keeps the SDKs unchanged. See [State and Events](./state-and-events) for the full explanation.

## Dashboards using `fetch('/api/...')` 404

The dev shell doesn't proxy platform API calls. Keep custom dashboards event-stream-only (i.e. read from `useThe0Events()`) when running locally, or gate those fetches on a "production" flag.

## How do I update the shell's React version?

It's currently pinned to React 18 via unpkg CDN inside `cli/internal/dev/frontend/shell.html`. Custom override is deferred to a future release.

## Why does `the0 dev` not run `custom-bot validate` first?

It will. A pre-deploy validation command (`custom-bot validate`) is planned; `the0 dev` will auto-run it once that ships.

## What about query bots?

Query bots read from `QUERY_PATH` and write to `/query/result.json`. `the0 dev` is scoped to scheduled and realtime bots for now. Query-bot local mode is on the backlog.

## Where are logs sent in Docker mode?

Same pipe as native mode - Docker's `--init` wrapper passes stdout/stderr through, `the0 dev` reads from the `docker run` process directly.
