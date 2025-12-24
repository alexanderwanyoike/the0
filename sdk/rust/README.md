# the0 Rust SDK

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
the0 = { git = "https://github.com/alexanderwanyoike/the0", subdirectory = "sdk/rust" }
serde_json = "1.0"  # Required for config access
```

### Usage

```rust
use the0::input;

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

### `input::error(message: &str) -> !`

Output error result and exit with code 1.

### `input::result(data: &Value)`

Output custom JSON result.

## Example Bot

See the [Rust Quick Start Guide](https://docs.the0.dev/custom-bot-development/rust-quick-start) for a complete example.
