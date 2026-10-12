# Bot contract cases

These JSON cases pin down the contract between a bot, its SDK and the runtime, which is written up in [the bot contract](../../docs/custom-bot-development/bot-contract.md). Every SDK's own test suite loads every file in `cases/` and runs it. The spec page explains the rules; the cases are how an SDK proves it follows them.

Each case is an environment in and outputs out. The harness starts a small program built on the SDK (the **contract bot**) with the case's environment. Afterwards it compares what came out against the case:
- the lines on stdout and stderr
- the bot's `result.json`
- the query result file
- the state files
- the exit code
- in server mode, the HTTP responses

The cases need nothing beyond a JSON parser, a way to start a process, and an HTTP client.

The reference harness is the Python SDK's: `sdk/python/tests/test_contract.py` with its contract bot `sdk/python/tests/contract_bot.py`.

## Running a case

1. **Make a sandbox.** Use a fresh temporary directory per case, laid out the way the runtime lays out a container:

   | Sandbox path | Stands in for | Notes |
   |---|---|---|
   | `<root>/bot/` | `/bot`, the code directory | Always created. A bot run's `result.json` lands here. |
   | `<root>/state/.the0-state/` | `/state/.the0-state`, the state directory | Created only when the case has `state_before`, so the other cases also check that the SDK creates it on the first write. |
   | `<root>/query/result.json` | `/query/result.json`, the query result file | The `query/` directory is always created; the file is not. |

2. **Write the state.** For each key of `state_before`, write `<state dir>/<key>.json` containing the value as JSON.

3. **Build the environment.** Copy every entry of `env`, replacing these placeholders inside the values:

   | Placeholder | Replaced with |
   |---|---|
   | `{{code_mount_dir}}` | the sandbox code directory without its leading `/`. The SDK builds `/{CODE_MOUNT_DIR}/result.json`, so `/tmp/x/bot` becomes `tmp/x/bot`. |
   | `{{state_dir}}` | the sandbox state directory |
   | `{{port}}` | a free TCP port, for cases with `requests` |

   Add only what the language needs to start (`PATH`, `PYTHONPATH`, `HOME` and the like). Nothing else from the test runner's environment may leak in: a stray `QUERY_PATH` or `BOT_TYPE` changes what the SDK does.

4. **Start the contract bot.** Pass it the case file and the sandbox query result path. The contract bot is the SDK's own test program, described in [the contract bot](#the-contract-bot).
   - `entrypoint` is `bot`: the contract bot runs `script`, then returns normally.
   - `entrypoint` is `query`: the contract bot registers every entry of `handlers` and starts the SDK's query runner (`query.run()` or its equivalent).

   Which query mode runs is up to the SDK, from the environment. That choice is part of what the cases test.

5. **Server mode.** When the case has `requests`, the SDK is serving HTTP on `{{port}}`:
   1. Wait until the port accepts connections; 30 seconds is enough for a JVM.
   2. Send each request in order on a new connection, and compare its status and JSON body.
   3. Stop the process with SIGTERM.

   Its exit code is not compared.

   Otherwise, wait for the process to exit; 60 seconds is the limit.

6. **Compare** everything listed under `expect`, using the [matching rules](#matching). Report every mismatch, not just the first, along with the process's exit code, stdout and stderr.

## Case format

```json
{
  "description": "What the case pins down, in one sentence.",
  "entrypoint": "bot",
  "env": {"BOT_ID": "contract-bot", "BOT_CONFIG": "{}", "CODE_MOUNT_DIR": "{{code_mount_dir}}", "STATE_DIR": "{{state_dir}}"},
  "state_before": {"positions": []},
  "script": [{"op": "success", "message": "done"}],
  "expect": {"exit_code": 0, "result": {"status": "success", "message": "done"}}
}
```

| Field | Meaning |
|---|---|
| `description` | What the case pins down. |
| `entrypoint` | `bot` or `query`: which entrypoint the contract bot plays. |
| `env` | The process environment, the way the runtime sets it. Values may contain placeholders. |
| `state_before` | Optional. State keys and their values, written before the run. |
| `script` | `bot` only. The ops the bot performs, in order. |
| `handlers` | `query` only. Each query path the bot registers, mapped to the ops its handler performs on each request. |
| `requests` | Server mode only. The HTTP requests to send. |
| `expect` | What must come out. A field left out is not compared. |

Fields of `expect`:

| Field | Compared against |
|---|---|
| `exit_code` | the process's exit code. Not used in server mode. |
| `stdout`, `stderr` | the non-blank lines of that stream, each parsed as JSON, in order. The count must match: `[]` means nothing was printed. |
| `result` | the JSON in `<code dir>/result.json`. `null` means the file must not exist. |
| `query_result` | the JSON in the query result file. `null` means the file must not exist. |
| `state_after` | every `<key>.json` file in the state directory, as `{key: value}`. `{}` means none, including when the directory does not exist. |

Fields of a request:

| Field | Meaning |
|---|---|
| `method` | the HTTP method |
| `target` | the path and query string, already URL-encoded, sent as is |
| `body` | optional. A JSON value, sent as JSON with `Content-Type: application/json`. |
| `raw_body` | optional. Text sent as is, with `Content-Type` set to `content_type` (default `text/plain`). |
| `expect.status` | the response status code |
| `expect.body` | the response body, parsed as JSON |

## Ops

An op is an object with an `op` name and its arguments. Two more fields work on any op except `return`:

- `as`: binds the op's return value to a name, so a later op can use it. An op that returns nothing binds `null`.
- `catch`: the kind of error the op is expected to fail with. If it fails with that kind, the op binds `{"caught": "<kind>"}` and the script carries on. Any other error is not caught.

An uncaught error ends a bot script the way any uncaught error ends the bot. In a handler, the handler fails.

| Op | Arguments | SDK call | Returns |
|---|---|---|---|
| `parse` | | `parse()` | `{"bot_id": ..., "config": ...}` |
| `success` | `message`, `data`? | `success(message[, data])` | |
| `error` | `message`, `data`? | `error(message[, data])` | does not return |
| `result` | `value` | `result(value)` | |
| `metric` | `type`, `data` | `metric(type, data)` | |
| `log` | `message`, `data`?, `level`? | `log(message[, data][, level])` | |
| `raise` | `message` | fail with the language's ordinary error carrying `message` (an exception, an `Err`, ...) | does not return |
| `state.get` | `key`, `default`? | `state.get(key[, default])` | the value |
| `state.set` | `key`, `value` | `state.set(key, value)` | |
| `state.delete` | `key` | `state.delete(key)` | `true` or `false` |
| `state.list` | | `state.list()` | the keys |
| `state.exists` | `key` | `state.exists(key)` | `true` or `false` |
| `state.clear` | | `state.clear()` | |
| `query.params` | | the request's whole params map | an object |
| `query.param` | `key`, `default`? | the request's single-param getter (`req.get(key[, default])`) | the value |
| `query.config` | | the query module's config getter | the bot config |
| `return` | `value` | handler only: return `value` as the handler's result | |

Notes:
- **Ops tied to a request.** `query.params`, `query.param` and `return` are only valid inside a handler.
- **`state.get` defaults.** The cases always give a `default` when the key might be missing, because what a missing key returns without one is language-specific (`None`, `undefined`, an `Option`).
- **Op arguments.** In any argument, an object of exactly the form `{"$ref": "name"}` is replaced by the value bound to `name` in the same script or handler call. This applies at any depth. Each handler call starts with no bindings.
- **Catch kinds:**
  - `read_only_state`: the SDK's one read-only error type, the type its query module documents for handler authors.
  - `any`: any error.
  - The kind is all that's compared. Error messages are free text and differ between languages.

## Matching

Expected values are JSON, compared structurally:
- Objects match when they have the same keys and every value matches. Key order never matters, and an extra key is a mismatch.
- Arrays match element by element, in order.
- Numbers compare by value, so `5` matches `5.0`.
- A boolean only matches a boolean, never `0` or `1`.

An object with a `$match` key is a matcher instead of a literal:

| Matcher | Matches |
|---|---|
| `{"$match": "timestamp"}` | a string holding a UTC ISO 8601 date and time ending in `Z`, with optional fractional seconds of any precision: `^\d{4}-\d{2}-\d{2}T\d{2}:\d{2}:\d{2}(\.\d+)?Z$` |
| `{"$match": "string"}` | any string |
| `{"$match": "contains", "value": "x"}` | a string containing `x` |
| `{"$match": "unordered", "items": [...]}` | an array with the same items in any order |
| `{"$match": "nonzero"}` | any number other than 0. Used for exit codes. |

## The contract bot

The contract bot is a test-only program in the SDK's language. It must use the SDK the way a bot would, so the ops go through the public API.

The production query result path, `/query/result.json`, cannot be written in CI. The contract bot is therefore told the sandbox path, and must point the SDK there through a test-only seam the SDK provides. Python, for example, overrides a module attribute. That seam is the only private thing the contract bot may touch.

## Adopting the cases in an SDK

1. Add a harness and a contract bot to the SDK's test suite, so its usual test command (`make test` and its CI workflow) runs every case.
2. List every case the SDK fails as an expected failure, with the reason. Python's list is `KNOWN_FAILURES`. A listed case that starts to pass must fail the suite, so the list never goes stale.
3. Fix the SDK test first, one behaviour at a time:
   1. a commit that takes a case off the list, which makes it fail on an assertion;
   2. then the commit that makes it pass.

The SDK's workflow in `.github/workflows/` must trigger on `sdk/contract/**`, so a new or changed case runs against every SDK.

## Changing the contract

A change to the contract changes the spec page and the cases in the same pull request. Every SDK workflow then runs the new cases. An SDK that does not pass a new case lists it as an expected failure, with the reason, until it catches up.
