# the0-sdk (Rust)

Optional SDK for building trading bots on the0 platform.

**Note:** This SDK is optional. The the0 runtime automatically wraps your bot's output:
- Exit code 0 → `{"status":"success","message":"Bot executed successfully"}`
- Exit code non-zero → `{"status":"error","message":"Bot execution failed"}`
- If you output JSON with a `"status"` field, it's passed through as-is

## Minimal Example (No SDK)

```rust
use std::env;

fn main() {
    // No SDK needed - just run your code and exit normally
    let bot_id = env::var("BOT_ID").unwrap_or_default();
    let config_str = env::var("BOT_CONFIG").unwrap_or_default();

    eprintln!("Bot {} running...", bot_id);

    // Your trading logic here

    // Exit normally - runtime outputs {"status":"success",...}
}
```

## With SDK (Optional)

The SDK provides convenience methods for parsing config and outputting custom results.

### Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
the0-sdk = "0.4"
serde_json = "1.0"  # Required for config access
```

Or from git:

```toml
[dependencies]
the0-sdk = { git = "https://github.com/alexanderwanyoike/the0", subdirectory = "sdk/rust" }
```

### Usage

```rust
use the0_sdk::input;

fn main() {
    // Parse bot configuration
    let (bot_id, config) = input::parse();

    eprintln!("Bot {} starting", bot_id);

    // Access config values
    if let Some(symbol) = config.get("symbol") {
        eprintln!("Trading symbol: {}", symbol);
    }

    // Your trading logic here

    // Optionally output custom result (otherwise runtime generates it)
    input::success("Bot executed successfully");
}
```

## API Reference

### `input::parse() -> (String, Value)`

Parse bot configuration from environment. Returns `(bot_id, config)`.

### `input::parse_as_map() -> (String, HashMap<String, Value>)`

Parse configuration as a HashMap for easier access.

### `input::success(message: &str)`

Output success result and continue execution.

### `input::success_with_data(message: &str, data: &Value)`

Output success result with `data` under its `data` key, and continue execution.

### `input::error(message: &str) -> !`

Output error result, print the message on stderr as an `error` log line, and exit with code 1.

### `input::error_with_data(message: &str, data: &Value) -> !`

Like `input::error`, with `data` under the result's `data` key. The log line carries only the message.

### `input::result(data: &Value)`

Output custom JSON result.

### `input::metric(metric_type: &str, data: &Value)` and `input::log(message, data, level)`

Print a metric line on stdout or a log line on stderr, each with a UTC ISO 8601 `timestamp`.

### `query::handler(path, handler)` and `query::try_handler(path, handler)`

Register a query handler. A `try_handler` handler returns `Result<Value, HandlerError>`, and an `Err` answers the query as an error. Params are JSON values: `req.get(key)` returns one as text (a non-string as its JSON text, a `null` as `None`), `req.get_as::<T>(key)` deserializes it (parsing a string value, such as `"5"` from a query string, when it has to), and `req.values()` returns them all.

### `state::delete(key: &str) -> Result<bool, StateError>`

Delete a key, returning whether it existed. Like `state::set` and `state::clear`, it fails with `StateError::ReadOnly(query::ReadOnlyStateError)` during a query.

## Example Bot

See the [Rust Quick Start Guide](https://docs.the0.app/custom-bot-development/rust-quick-start) for a complete example.

## Publishing (Maintainers)

This package is published to crates.io.

### Prerequisites

1. Login to crates.io:
   ```bash
   cargo login
   ```

2. Ensure you have ownership of the crate on crates.io

### Publish

```bash
# Verify the package
cargo publish --dry-run

# Publish to crates.io
cargo publish
```

### Version Bump

Update the version in `Cargo.toml`:

```toml
version = "0.2.0"
```

Then publish.

## License

Apache-2.0
