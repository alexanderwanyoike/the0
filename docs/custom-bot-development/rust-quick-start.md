---
title: "Rust Quick Start"
description: "Build your first Rust trading bot with the0"
tags: ["custom-bots", "rust", "quick-start"]
order: 11
---

# Rust Quick Start Guide

Build a high-performance trading bot in Rust with the0's type-safe SDK.

---

## Prerequisites

- Rust installed (rustup recommended)
- the0 CLI installed
- Valid the0 API key

---

## Project Structure

```
my-rust-bot/
├── Cargo.toml         # Dependencies and project config
├── Cargo.lock         # Lock file (commit this!)
├── src/
│   └── main.rs        # Your bot entry point
├── bot-config.yaml    # Bot configuration
├── bot-schema.json    # Parameter schema
└── README.md          # Documentation
```

---

## Step 1: Create Your Project

```bash
# Create a new Rust project
cargo new my-rust-bot
cd my-rust-bot
```

---

## Step 2: Configure Cargo.toml

Add the the0 SDK and required dependencies:

```toml
[package]
name = "my-rust-bot"
version = "0.1.0"
edition = "2021"

[[bin]]
name = "my-rust-bot"
path = "src/main.rs"

[dependencies]
the0 = { git = "https://github.com/alexanderwanyoike/the0", subdirectory = "sdk/rust" }
serde_json = "1.0"
```

---

## Step 3: Write Your Bot

Create `src/main.rs`:

```rust
use the0::input;

fn main() {
    // Parse bot configuration
    let (id, config) = input::parse();

    println!("Bot {} starting...", id);

    // Access configuration values
    if let Some(symbol) = config.get("symbol") {
        println!("Trading symbol: {}", symbol);
    }

    if let Some(amount) = config.get("amount") {
        println!("Trade amount: {}", amount);
    }

    // Your trading logic here
    // Example: Check price, execute trade, log results

    // Signal success when done
    input::success("Bot executed successfully");
}
```

---

## Step 4: Create Bot Configuration

Create `bot-config.yaml`:

```yaml
name: my-rust-bot
description: "A high-performance Rust trading bot"
version: "1.0.0"
author: "Your Name"
type: scheduled
runtime: rust-stable

entrypoints:
  bot: src/main.rs

schema:
  bot: bot-schema.json

readme: README.md
```

---

## Step 5: Define Parameter Schema

Create `bot-schema.json`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Bot Configuration",
  "description": "Configuration for the Rust trading bot",
  "properties": {
    "symbol": {
      "type": "string",
      "title": "Trading Symbol",
      "description": "The trading pair symbol",
      "default": "BTC/USDT"
    },
    "amount": {
      "type": "number",
      "title": "Trade Amount",
      "description": "Amount to trade per execution",
      "default": 100
    }
  },
  "required": ["symbol"]
}
```

---

## Step 6: Deploy

```bash
# Deploy your bot
the0 custom-bot deploy
```

The build happens automatically in the cloud - no need to compile locally!

---

## SDK API Reference

The `the0` crate provides these functions in the `input` module:

### `input::parse() -> (String, Value)`

Parse bot configuration from environment. Returns `(bot_id, config)`.

```rust
let (bot_id, config) = input::parse();
```

### `input::parse_as_map() -> (String, HashMap<String, Value>)`

Parse configuration as a HashMap for easier access.

```rust
let (bot_id, config) = input::parse_as_map();
if let Some(value) = config.get("symbol") {
    println!("Symbol: {}", value);
}
```

### `input::success(message: &str)`

Output a success result.

```rust
input::success("Trade executed successfully");
```

### `input::error(message: &str) -> !`

Output an error result and exit with code 1.

```rust
input::error("Failed to connect to exchange");
```

### `input::result(data: &Value)`

Output a custom JSON result.

```rust
use serde_json::json;
input::result(&json!({
    "status": "success",
    "trade_id": "12345",
    "filled_amount": 0.5
}));
```

---

## Adding Dependencies

Add any crate from crates.io to your `Cargo.toml`:

```toml
[dependencies]
the0 = { git = "https://github.com/the0-dev/the0", subdirectory = "sdk/rust" }
serde_json = "1.0"
reqwest = { version = "0.11", features = ["blocking", "json"] }
chrono = "0.4"
```

The CLI builds your bot before deployment - no need to compile locally!

---

## Example: HTTP Request Bot

```rust
use the0::input;
use serde_json::Value;

fn main() {
    let (id, config) = input::parse();

    // Get API endpoint from config
    let endpoint = config.get("api_endpoint")
        .and_then(|v| v.as_str())
        .unwrap_or("https://api.example.com/price");

    println!("Bot {} fetching from {}", id, endpoint);

    // Make HTTP request (requires reqwest in Cargo.toml)
    match reqwest::blocking::get(endpoint) {
        Ok(response) => {
            if let Ok(data) = response.json::<Value>() {
                println!("Received: {}", data);
                input::success("Data fetched successfully");
            } else {
                input::error("Failed to parse response");
            }
        }
        Err(e) => {
            input::error(&format!("Request failed: {}", e));
        }
    }
}
```

---

## Best Practices

### Error Handling

Use Rust's Result type for robust error handling:

```rust
use the0::input;

fn execute_trade(symbol: &str, amount: f64) -> Result<String, String> {
    // Your trade logic
    Ok("trade_id_123".to_string())
}

fn main() {
    let (id, config) = input::parse();

    match execute_trade("BTC/USDT", 100.0) {
        Ok(trade_id) => {
            println!("Trade executed: {}", trade_id);
            input::success(&format!("Trade {} completed", trade_id));
        }
        Err(e) => {
            input::error(&format!("Trade failed: {}", e));
        }
    }
}
```

### Configuration Validation

Validate configuration early:

```rust
use the0::input;

fn main() {
    let (id, config) = input::parse();

    // Validate required fields
    let symbol = match config.get("symbol").and_then(|v| v.as_str()) {
        Some(s) => s,
        None => {
            input::error("Missing required field: symbol");
        }
    };

    let amount = config.get("amount")
        .and_then(|v| v.as_f64())
        .unwrap_or(100.0);

    println!("Trading {} with amount {}", symbol, amount);
    input::success("Validation passed");
}
```

### Logging

You can freely use `println!` for debugging - the SDK's output functions use a special marker (`THE0_RESULT:`) that the runtime recognizes:

```rust
// These all go to logs - use freely for debugging
println!("Starting trade execution...");
println!("Current price: {}", price);
eprintln!("Warning: High volatility detected");

// This is the bot's result (automatically prefixed with marker)
input::success("Trade completed successfully");
```

For structured logging and metrics, see the [Bot Metrics & Logging](/custom-bot-development/metrics) guide.

---

## Related Documentation

- [Configuration Reference](/custom-bot-development/configuration)
- [Bot Types](/custom-bot-development/bot-types)
- [Deployment Guide](/custom-bot-development/deployment)
