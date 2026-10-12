# the0-sdk (C/C++)

Header-only SDK for building trading bots on the0 platform in C++.

The SDK is not on a package registry. Bots fetch it from the the0 repository at a release tag, as shown below. SDK 0.4 follows the [bot contract](../../docs/custom-bot-development/bot-contract.md) and ships in the0 `v1.18.0`.

Uses [nlohmann/json](https://github.com/nlohmann/json) for JSON.

## Requirements

- C++17 or later
- [nlohmann/json](https://github.com/nlohmann/json) v3 (the tests use v3.11.3)
- CMake 3.14 or later, for the setup below

## Installation

Fetch the SDK and nlohmann/json with CMake's FetchContent, and link the `the0` target:

```cmake
cmake_minimum_required(VERSION 3.14)
project(my_bot)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

include(FetchContent)

FetchContent_Declare(json
    GIT_REPOSITORY https://github.com/nlohmann/json.git
    GIT_TAG v3.11.3
)
FetchContent_MakeAvailable(json)

FetchContent_Declare(the0
    GIT_REPOSITORY https://github.com/alexanderwanyoike/the0.git
    GIT_TAG v1.18.0
    SOURCE_SUBDIR sdk/cpp
)
FetchContent_MakeAvailable(the0)

add_executable(my_bot main.cpp)
target_link_libraries(my_bot PRIVATE the0 nlohmann_json::nlohmann_json)
```

Without CMake, copy the `the0/` directory into your project, put its parent directory on the include path, and install nlohmann/json (`apt install nlohmann-json3-dev`, `brew install nlohmann-json` or `vcpkg install nlohmann-json`).

The headers:
- `<the0/the0.h>`: `parse()`, results, metrics and logs. It includes `state.h`.
- `<the0/state.h>`: persistent state.
- `<the0/query.h>`: query handlers. It includes the other two.

## Usage

```cpp
#include <the0/the0.h>

int main() {
    auto [bot_id, config] = the0::parse();

    std::string symbol = config.value("symbol", "BTC/USDT");
    double amount = config.value("amount", 100.0);

    the0::log("Trading " + symbol, {{"amount", amount}});

    // Your trading logic here

    the0::success("Bought " + symbol, {{"amount", amount}});
    return 0;
}
```

## API Reference

### `the0::parse() -> std::pair<std::string, the0::json>`

Reads `BOT_ID` and `BOT_CONFIG` from the environment. `config` is `BOT_CONFIG` decoded as JSON, with every value keeping its type. Without `BOT_ID` the ID is empty; without a valid `BOT_CONFIG` the config is an empty object.

```cpp
auto [bot_id, config] = the0::parse();
std::string symbol = config.value("symbol", "BTC/USDT");
int count = config.value("count", 10);
```

### `the0::parse_raw() -> std::pair<std::string, std::string>`

As `parse()`, with `BOT_CONFIG` as unparsed text, for bots that use another JSON library.

### Results

A bot run's result goes to `/{CODE_MOUNT_DIR}/result.json`, which is `/bot/result.json`. Each call replaces the file, so the last call wins. The SDK writes no result unless the bot calls one of these.

| Call | Writes |
|---|---|
| `the0::success(message)` | `{"status": "success", "message": message}` |
| `the0::success(message, data)` | `{"status": "success", "message": message, "data": data}` |
| `the0::error(message)` | `{"status": "error", "message": message}` |
| `the0::error(message, data)` | `{"status": "error", "message": message, "data": data}` |
| `the0::result(value)` | `value`, unchanged |
| `the0::result(status, message, data)` | `{"status": status, "message": message}` with the fields of `data` merged in |

`data` is any JSON value. `the0::error()` also prints the message on stderr as an `error` log line, the same line `the0::log(message, {}, the0::LogLevel::Error)` prints, and then exits with code 1. It does not return.

```cpp
the0::success("Trade executed", {{"trade_id", "abc123"}, {"price", 45000.5}});

if (prices.empty()) {
    the0::error("No price data available", {{"symbol", symbol}});
}
```

### `the0::metric(type, data)`

Prints a metric line on stdout: `{"_metric": type, ...the fields of data, "timestamp": "2026-10-12T09:30:00.123Z"}`.

```cpp
the0::metric("price", {{"symbol", "BTC/USD"}, {"value", 45000.5}});
```

### `the0::log(message, data = {}, level = the0::LogLevel::Info)`

Prints a log line on stderr: `{"level": "info", "message": message, ...the fields of data, "timestamp": "..."}`. The levels are `Info`, `Warn` and `Error`.

```cpp
the0::log("Starting");
the0::log("Connection slow", {}, the0::LogLevel::Warn);
the0::log("Order placed", {{"order_id", "o-1"}, {"qty", 2}});
```

### Output

- Timestamps are UTC ISO 8601 strings with milliseconds, ending in `Z`.
- Results, metric lines and log lines go through nlohmann/json, so quotes, backslashes and control characters are escaped, in the metric type too.
- Text that is not valid UTF-8 is written with U+FFFD in place of the bad bytes, so writing it never throws.
- `data` must not use the SDK's own keys (`_metric`, `level`, `message`, `timestamp`).

### State (`<the0/state.h>`)

State persists between runs. Each key is a file, `<STATE_DIR>/<key>.json`.

```cpp
the0::state::set("portfolio", {{"AAPL", 100}, {"GOOGL", 50}});

auto portfolio = the0::state::get("portfolio");    // std::optional<json>
auto count = the0::state::get_or("trade_count", 0); // json, or the default
bool removed = the0::state::remove("portfolio");    // whether it existed
auto keys = the0::state::list();
bool known = the0::state::exists("portfolio");
the0::state::clear();
```

A key must not be empty or contain `/`, `\` or `..`; such a key throws `the0::state::InvalidKeyError`.

During an ephemeral query (`QUERY_PATH` set), state is read-only: `set`, `remove` and `clear` throw `the0::state::ReadOnlyStateError` before checking the key. `the0::query::ReadOnlyStateError` is the same type, so a handler can catch either name.

### Queries (`<the0/query.h>`)

A query entrypoint registers handlers and then calls `the0::query::run()`:

```cpp
#include <the0/query.h>

using json = nlohmann::json;

int main() {
    the0::query::handler("/portfolio", [](const the0::query::Request& req) {
        auto positions = the0::state::get_or("positions", json::array());
        return json{{"positions", positions}};
    });

    the0::query::handler("/signals", [](const the0::query::Request& req) {
        int limit = req.get_as("limit", 10);
        auto signals = the0::state::get("signals");
        if (!signals) {
            throw std::runtime_error("No signals yet");
        }
        return json{{"limit", limit}, {"signals", *signals}};
    });

    the0::query::run();
    return 0;
}
```

`run()` registers `/health` and `/info` unless the bot did, then:
- with `QUERY_PATH` set, answers that one query, writes the response to `/query/result.json` and exits (0 for an answer, 1 for an error)
- otherwise, with `BOT_TYPE=realtime`, serves HTTP on port 9476 (`THE0_QUERY_PORT` overrides it)
- otherwise, answers `/info`

**Params** are JSON values. An ephemeral query reads them from `QUERY_PARAMS`. The query server reads them from a JSON object body, and otherwise from the URL query string, where every value is a string. Params that are not a JSON object, including an object that repeats a key, give no params.

| Accessor | Returns |
|---|---|
| `req.get(key)`, `req.get(key, default)` | the param as text: a string as it is, any other value as its JSON text. A `null` param reads as missing, so the default applies. |
| `req.has(key)` | whether `get(key)` finds the param |
| `req.params` | every param as text, as `get()` reads it |
| `req.get_as<T>(key)` | the param as a `T` in a `std::optional`, or `std::nullopt` when it is missing or not a `T` |
| `req.get_as(key, default)` | the same, with a default |
| `req.values()` | every param with its JSON type, as a JSON object |

`get_as<T>` is strict and reads query strings:
- A string param is parsed as `T` when it is not one already, so `get_as<int>("limit")` reads `?limit=50` as 50.
- Numbers are not converted between kinds: `0.25` is not an `int`, `true` is not a number, and an integer must fit in `T`.
- `get_as<std::string>` gives any other value as its JSON text, and a `null` param as `std::nullopt`.
- `get_as<json>` gives the value as it is, `null` included.

**Errors.** A handler fails by throwing. The response is `{"status": "error", "error": <message>}`: the query server answers 500 and keeps serving, and an ephemeral query exits 1. The message is `what()` for a `std::exception` and the text for a thrown `const char*` or `std::string`. A path with no handler is an error too, with `available` listing the paths the bot answers; the query server answers it with 404.

**The query server** answers every request with `Content-Type: application/json` and `Connection: close`. It reads a body by its `Content-Length`. It answers 400 when it cannot read a request: a `Content-Length` that is not a number, a body over 1 MiB, a `Transfer-Encoding` body, or a client that sends no complete request within 5 seconds. It prints its start line and its error lines as log lines.

`the0::query::get_config()` returns the bot's config, and `the0::query::get_params()` the current params as text.

## Bot Configuration

Create `bot-config.yaml`. The entrypoints are the compiled binaries:

```yaml
name: my-cpp-bot
description: "A C++ trading bot"
version: "1.0.0"
author: "Your Name"
type: scheduled
runtime: gcc13

entrypoints:
  bot: build/my_bot
  query: build/my_query

schema:
  bot: bot-schema.json

readme: README.md
```

## Deployment

```bash
the0 custom-bot deploy
```

## Testing

The test suite uses [doctest](https://github.com/doctest/doctest), fetched with CMake FetchContent. `make test` builds it and runs it with ctest:

```bash
make test
```

That runs the unit tests and the bot contract cases. The harness in `tests/contract/` runs every case in [`sdk/contract/cases`](../contract/cases), and the C++-only cases in `tests/contract/cases`, against a contract bot built on the SDK.

## License

Apache 2.0. See the LICENSE file.
