# the0 Rust SDK

Rust SDK for building trading bots on the0 platform.

## Installation

Add to your `Cargo.toml`:

```toml
[dependencies]
the0 = { git = "https://github.com/alexanderwanyoike/the0", subdirectory = "sdk/rust" }
serde_json = "1.0"  # Required for config access
```

## Usage

```rust
use the0::input;

fn main() {
    // Parse bot configuration
    let (bot_id, config) = input::parse();

    println!("Bot {} starting", bot_id);

    // Access config values
    if let Some(symbol) = config.get("symbol") {
        println!("Trading symbol: {}", symbol);
    }

    // Your trading logic here

    // Signal success
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
