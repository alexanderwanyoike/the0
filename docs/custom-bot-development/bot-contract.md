---
title: "Bot Contract"
description: "What the runtime, the wrappers and every SDK agree on: environment, entrypoints, results, output lines, state and queries"
order: 17
---

# Bot Contract

This page is for SDK authors and for anyone changing how the runtime starts bots. It states what a bot, its SDK and the runtime agree on. Bot authors don't need it: the SDK pages cover the same ground through each SDK's API.

Three parties take part:
- **The runtime.** `runtime execute` runs inside the bot's container. It downloads the code and the state, sets the environment, starts the entrypoint, and saves state and logs.
- **The wrappers.** `runtime/wrappers/python_bot.py` and `runtime/wrappers/node_bot.js` start Python and Node.js bot entrypoints and call their `main`. Compiled languages have no wrapper.
- **The SDK.** It reads the environment and writes the outputs described here.

The cases in [`sdk/contract/`](https://github.com/alexanderwanyoike/the0/tree/dev/sdk/contract) test these rules, and every SDK's test suite runs them. A change to the contract changes this page and the cases together.

Some SDKs don't follow every rule yet. [Bringing the SDKs into line](#bringing-the-sdks-into-line) lists which ones.

## Processes

The runtime starts an entrypoint in one of three ways:
- **Bot run.** The bot entrypoint runs once per scheduled run, or for as long as a realtime bot runs.
- **Ephemeral query.** The query entrypoint answers one query in a fresh container, with `QUERY_PATH` set. Scheduled bots answer every query this way.
- **Query server.** The query entrypoint runs alongside a realtime bot and serves HTTP for as long as the bot runs. In Docker it is a subprocess in the bot's container; in Kubernetes it is a sidecar container. Only realtime bots with a query entrypoint have one.

| Runtime | Bot entrypoint runs as | Query entrypoint runs as |
|---|---|---|
| `python3.11` | `python3 /app/wrappers/python_bot.py` | `python3 <query entrypoint>` |
| `nodejs22`, `nodejs20` | `node /app/wrappers/node_bot.js` | `node <query entrypoint>` |
| `rust-stable`, `gcc13`, `cpp-gcc13`, `ghc96` | `/bot/<entrypoint>` | `/bot/<query entrypoint>` |
| `dotnet8` | `dotnet <entrypoint>` for a `.dll`, otherwise `dotnet run --project <entrypoint>` | the same, with the query entrypoint |
| `scala3` | `java -jar <jar>`, or `java -cp <jar> <MainClass>` for an entrypoint written `<jar>:<MainClass>` | the same, with the query entrypoint |

The working directory is the code directory, `/bot`. A query entrypoint never runs through a wrapper; the SDK's query runner owns the whole process.

## Environment

`runtime execute` builds the environment for every process it starts (`BuildBotEnv` in `runtime/internal/execute/helpers.go`). Its values replace any of the same name the container was created with.

| Variable | Set for | Value |
|---|---|---|
| `BOT_ID` | every process | The bot's ID. |
| `BOT_CONFIG` | every process | The bot's configuration, as JSON object text. |
| `CODE_MOUNT_DIR` | every process | The code directory without its leading `/`, which is `bot`. A bot run's result file is `/{CODE_MOUNT_DIR}/result.json`. SDKs use `bot` when it is unset. |
| `STATE_DIR` | every process | The state directory, `/state/.the0-state`. SDKs use that path when it is unset. |
| `BOT_TYPE` | bot runs and query servers; ephemeral queries on Docker only | `realtime` or `scheduled`. Kubernetes query pods don't set it. SDKs only read it to decide whether to start the query server. |
| `QUERY_PATH` | ephemeral queries only | The query's path, such as `/portfolio`. It is never empty. Its presence is what makes state read-only. |
| `QUERY_PARAMS` | ephemeral queries with params; Docker always sets it, to `{}` when there are none | The params, as JSON object text. |
| `THE0_QUERY_PORT` | never set by the runtime | The port the SDK's query server listens on, default `9476`. The runtime always connects to 9476; the variable exists for tests and local runs. |
| `SCRIPT_PATH` | every process | The entrypoint file. Only the wrappers read it. |
| `PYTHONPATH` | `python3.11` | `/bot/vendor:/bot`, followed by any inherited value. |

The bot can also see variables that are not part of the contract, and it must not rely on them:
- **`QUERY_PARAM_<key>`.** The runtime sets one per param using Go's `%v` formatting, which loses information: an object becomes `map[...]`. No SDK reads it. Read `QUERY_PARAMS` instead.
- **The runtime's own settings**, such as `CODE_FILE`, `RUNTIME`, `ENTRYPOINT`, `QUERY_ENTRYPOINT`, `QUERY_RESULT_KEY`, `IS_SCHEDULED`, `ENTRYPOINT_TYPE`, and the storage and NATS settings. The bot inherits them because `runtime execute` passes its own environment through.

## Entrypoints

`bot-config.yaml` names the entrypoints:
- `entrypoints.bot`, which is required.
- `entrypoints.query`, which is optional. A bot without one answers no queries at all, built-in ones included.

### Python and Node.js bot entrypoints

The file exports `main(bot_id, config)`. The wrapper does the following, in order:
1. Changes to `/{CODE_MOUNT_DIR}`. The Python wrapper also puts that directory and its `vendor/` on `sys.path`.
2. Deletes any `result.json` an earlier run left in that directory, so it cannot pass for this run's.
3. Loads `SCRIPT_PATH`:
   - Python takes the module's `main`.
   - Node.js takes `module.main`, then `module.default`, then the module itself. Whichever it takes must be a function.
4. Calls `main(BOT_ID, <BOT_CONFIG parsed as JSON>)`. A Node.js `main` may return a promise.
5. Settles the result and the exit code, as described in [What the wrappers add](#what-the-wrappers-add).

If the file cannot be loaded, the wrapper exits 1. The Node.js wrapper also writes an error result.

### Compiled bot entrypoints

The entrypoint is the program itself. It reads `BOT_ID` and `BOT_CONFIG` through the SDK's `parse()`.

### `parse()`

`parse()` returns `BOT_ID` and `BOT_CONFIG` decoded as JSON, with every value keeping its JSON type. The runtime always sets both variables, so what `parse()` does without them is up to each SDK.

### Query entrypoints

A query entrypoint registers its handlers and then starts the SDK's query runner (`query.run()` or its equivalent). See [Queries](#queries).

## Results and exit codes

### The result file

A bot run's result file is `/{CODE_MOUNT_DIR}/result.json`, which is `/bot/result.json`. It holds one JSON object.

| SDK call | Writes |
|---|---|
| `success(message[, data])` | `{"status": "success", "message": message, "data": data}`, with `data` only when it is given |
| `error(message[, data])` | `{"status": "error", "message": message, "data": data}`, with `data` only when it is given. See [`error()`](#error). |
| `result(value)` | `value`, unchanged |

Each call replaces the file, so the bot's last call wins. An SDK never writes a result on its own: a bot that makes none of these calls leaves no file. The Rust, .NET, Scala, Haskell and C++ SDKs take no `data` argument; their `result()` can write the same object.

### `error()`

`error(message[, data])` does three things, in this order:
1. Writes the error result above.
2. Prints the message on stderr as a log line at level `error`, exactly as `log(message, "error")` would: `{"level": "error", "message": "...", "timestamp": "..."}`. The `data` goes in the result only.
3. Exits with code 1. Nothing after it runs.

Nothing reads a bot run's result file once the run is over, so the printed line and the exit code are the only trace a failed run leaves.

### Exit codes

- **0 means the run succeeded; anything else means it failed.** The runtime passes the process's exit code through as the container's, so a non-zero exit marks the Kubernetes Job or the Docker run as failed. Failed scheduled runs are not retried.
- **An uncaught error exits non-zero.** The SDK writes no result for it; the wrappers do, as described next.
- **An error result fails the run, even when the process would exit 0.** An error result is a result file holding a JSON object whose `status` is exactly `"error"`, for example after `result({"status": "error", ...})` and a normal return. The result file is the bot's own verdict, so the run must show as failed everywhere.
  - For Python and Node.js, the wrappers enforce this.
  - Compiled bots run without a wrapper, so nothing enforces it for them yet. Their `error()` already exits 1.

### What the wrappers add

After `main` ends, the Python and Node.js wrappers settle the result and the exit code:
- **They never overwrite.** A result the bot wrote during the run stays as it is. A wrapper writes a result only when the bot wrote none:
  - A plain return writes `{"status": "success", "message": "Bot executed successfully"}`. When a Node.js `main` returns a value, that value is written instead.
  - A failure writes `{"status": "error", "message": ...}`, with the error's message or `Bot exited with code N`. A failure is an uncaught error, a rejected promise, `error()`, or any non-zero exit.
- **They keep the bot's exit code.** An uncaught error gives 1, and an error result turns an exit 0 into 1.
- **They print an error result** on stderr as an error log line, so a failed run always leaves its message in the logs. An SDK whose `error()` already prints the message therefore shows it twice on these runtimes.
- **They handle stop signals.** On SIGTERM or SIGINT, the Python wrapper stops waiting for `main`, writes `{"status": "terminated", ...}` unless the bot wrote a result, and exits 0.

## Log and metric lines

Everything a bot prints on stdout and stderr goes to its logs. For a bot run, the runtime also writes a copy to `/var/the0/logs/bot.log` and saves it. Any line is shown, but lines in the two shapes below get special treatment.

Each one is a single JSON object on one line, UTF-8, ending in a newline.

**Metric lines** come from `metric(type, data)` and are printed on stdout:

```json
{"_metric": "price", "symbol": "BTC/USD", "value": 45000.5, "timestamp": "2026-10-12T09:30:00.123Z"}
```

`_metric` holds the type, followed by the fields of `data`, which must be an object, and then `timestamp`.

**Log lines** come from `log(message[, data][, level])` and are printed on stderr:

```json
{"level": "info", "message": "Order placed", "order_id": "o-1", "timestamp": "2026-10-12T09:30:00.123Z"}
```

`level` is `info` (the default), `warn` or `error`. The fields of `data` sit alongside `message`.

The rules for both:
- **Key order doesn't matter.**
- **`data` must not use the SDK's keys** (`_metric`, `level`, `message`, `timestamp`). What happens when it does differs between SDKs and is not part of the contract.
- **`error()`'s message and the query server's start-up line are log lines too**, though the SDK prints them for itself.
- **Timestamps are UTC ISO 8601 strings ending in `Z`**, such as `2026-10-12T09:30:00Z` or `2026-10-12T09:30:00.123Z`. Fractional seconds are optional and may have any precision: Python writes microseconds, Node.js milliseconds.
- **Consumers keep accepting the older forms**, because bots built on older SDK versions keep running. The frontend (`frontend/src/lib/events/event-parser.ts`) reads `timestamp`, `time` or `ts` in any of these forms:
  - an ISO string
  - a number of epoch seconds or milliseconds (values below 10^10 are taken as seconds)
  - a string of digits with an optional trailing `Z`

The frontend reads each line as follows:
- A JSON object with `_metric` is a metric.
- A JSON object with `level`, `message` or `msg` is a log line at its `level`.
- Anything else is plain text, at INFO unless it starts with `INFO:`, `WARN:` or `ERROR:`.

## State

- **Layout.** Each key is a file, `<STATE_DIR>/<key>.json`, holding the value as JSON. The SDK creates the directory on the first write.
- **Keys.** A key must not be empty and must not contain `/`, `\` or `..`. Any other key is rejected with an error, and nothing is written.
- **Operations:**
  - `get(key[, default])` returns the stored value, or the default when the key doesn't exist.
  - `set(key, value)` stores any JSON value, `null` included.
  - `delete(key)` removes the key and returns whether it existed.
  - `list()` returns the keys, in no particular order.
  - `exists(key)` returns whether the key exists.
  - `clear()` removes every key.
- **Persistence.** Before a bot run starts, the runtime restores the bot's saved state into `/state`. It saves `/state` back while the bot runs and when it ends. Everything under `/state` persists, not only the SDK's directory.
- **Read-only in query mode.** When `QUERY_PATH` is set, `set`, `delete` and `clear` fail with the SDK's read-only error, before the key is checked. Reads still work. An ephemeral query sees the state as it was saved, and nothing it does is saved back.
- **One read-only error type.** Each SDK has exactly one read-only error type. State operations throw it, and the query module offers that same type to handler authors, so a handler that catches it catches what state throws.
- **The query server can write.** `QUERY_PATH` is not set for the query server, so no SDK blocks state writes there. Handlers must still not write state, because the query server shares the realtime bot's state directory.

## Queries

### Handlers

A query entrypoint registers a handler for each path, then starts the SDK's query runner. A handler receives the request (its path and its params) and either returns any JSON value or fails with an error.

When the runner starts, every SDK registers two built-in paths, unless the bot registered the same path first:

| Path | Answers |
|---|---|
| `/health` | `{"status": "ok"}` |
| `/info` | `{"available_queries": [...]}`, every path the bot answers, built-ins included, in no particular order |

### Choosing the mode

The runner picks its mode from the environment:
1. If `QUERY_PATH` is set, it runs an [ephemeral query](#ephemeral-queries) for that path.
2. Otherwise, if `BOT_TYPE` is `realtime`, it starts the [query server](#query-server).
3. Otherwise, it runs an ephemeral query for `/info`.

### Params

Params are JSON values. A param keeps its type (string, number, boolean, array, object or `null`), and every SDK hands it to handlers without loss.
- **Ephemeral queries** read params from `QUERY_PARAMS`, a JSON object. When it is missing or invalid, there are no params.
- **The query server** reads params from the request body when the body is a JSON object. Otherwise (no body, or a body that isn't a JSON object), it reads them from the URL query string, URL-decoded, and every value is a string. The runtime sends both a body and a query string, so a manual `curl` call with a query string keeps working.
- **Accessors.**
  - Python's request object and Node.js's `req.params` and `req.get()` hand over the JSON values as they are.
  - The Rust, .NET, Scala, Haskell and C++ SDKs keep their string accessor, which returns a non-string value as its JSON text instead of dropping it. They also gain a typed accessor.

### Responses

Both modes answer with the same JSON object:

| Outcome | Body | Ephemeral exit code | Server status |
|---|---|---|---|
| The handler returned `value` | `{"status": "ok", "data": value}` | 0 | 200 |
| No handler for the path | `{"status": "error", "error": "No handler for path: <path>", "available": [...]}` | 1 | 404 |
| The handler failed | `{"status": "error", "error": "<message>"}` | 1 | 500 |

- `available` lists every path the bot answers, as `/info` does.
- `error` for a failed handler contains the error's message. The rest of the text is up to the language; Node.js, for one, puts the error's name in front.
- Every query mode can report a handler error.

### Ephemeral queries

The runtime restores the bot's saved state, then starts the query entrypoint in a fresh container with `QUERY_PATH` and `QUERY_PARAMS` set. The SDK does three things:
1. Runs the one handler.
2. Writes the response to `/query/result.json`, creating `/query` if needed.
3. Exits with the code in the table above.

The runtime then uploads `/query/result.json` (to `QUERY_RESULT_KEY` in the query results bucket) and answers the query from it:
- **Exit 0:** the file's contents.
- **Non-zero exit:** the file's contents when there are any, otherwise an error carrying the process's output.

When the body has both `error` and `available`, the runtime appends `(available: a, b, ...)` to the error text.

### Query server

- **Listening.** The server listens on `0.0.0.0` at `THE0_QUERY_PORT` (default 9476) for as long as the bot runs. The runtime reaches it at the container's or the pod's address, on port 9476.
- **Routing.** It answers at least `GET` and `POST`; the runtime sends `POST`. The URL path selects the handler.
- **Responses.** It responds with `Content-Type: application/json` and the status codes in the table above.
- **Start-up line.** When it starts, it prints a log line on stderr.

The runtime sends each query as a `POST` with `Content-Type: application/json` and the params as the JSON body, when there are any. It also copies each param into the query string:
- strings as they are
- any other value as its JSON text
- `null` params left out

It reads the response body whatever the status code.

## Conformance cases

[`sdk/contract/`](https://github.com/alexanderwanyoike/the0/tree/dev/sdk/contract) holds the cases, and its README describes their format. Each case is an environment in and the expected outputs out:
- the lines on stdout and stderr
- `result.json`
- the query result file
- the state files
- the exit code
- in server mode, the HTTP responses

Each SDK's harness starts a small program built on the SDK with the case's environment and compares what comes out.

A case an SDK does not pass yet is listed in its harness as an expected failure, with the reason. A listed case that starts passing fails the suite, so the list stays current. Every SDK workflow runs on changes to `sdk/contract/**`.

| SDK | Harness |
|---|---|
| Python | `sdk/python/tests/test_contract.py` |
| Node.js, Rust, .NET, Scala, Haskell, C++ | not yet |

## Bringing the SDKs into line

These are the rules each SDK does not follow yet. The SDKs come into line one at a time, each running the cases first.

| Rule | Not followed yet by |
|---|---|
| `error()` prints its message on stderr | Python, Node.js, Rust, .NET, Scala, Haskell, C++ |
| Metric and log timestamps are ISO 8601 strings (these SDKs write epoch milliseconds as a number) | Rust, .NET, Scala, Haskell, C++ |
| The query server's start-up line is a log line (these SDKs print `{"_log": "info", ...}` with no timestamp) | Python, Node.js, Rust, .NET, Scala, Haskell, C++ |
| One read-only error type (these SDKs define a second, unused type in the query module) | Python, Node.js, Rust, .NET, Scala, Haskell, C++ |
| `state.delete()` fails in query mode (Rust returns `false` instead) | Rust |
| Ephemeral query params keep their JSON types | Rust and .NET (drop every param when any is not a string), Scala (keeps only string pairs), Haskell and C++ (turn every value into a string) |
| The query server reads the JSON body | Python, Node.js, Rust, .NET, Scala, Haskell, C++ |
| The query server URL-decodes the query string | Rust |
| The query server's 404 includes `available` | Python, Node.js, Rust, .NET, Scala, Haskell, C++ |
| `/info` lists `/info` | Rust |
| A query handler can report an error (a Rust handler can only panic, which ends the process; Rust gains a way that keeps existing handlers compiling) | Rust |
