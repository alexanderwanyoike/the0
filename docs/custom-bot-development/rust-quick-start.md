---
title: "Rust Quick Start"
description: "Build your first Rust trading bot with the0"
tags: ["custom-bots", "rust", "quick-start"]
order: 11
---

# Rust Quick Start Guide

Build high-performance trading bots in Rust with the0's type-safe SDK. Rust is ideal for latency-sensitive strategies where every microsecond counts.

---

## Why Rust for Trading Bots?

Rust offers unique advantages for algorithmic trading:

- **Zero-Cost Abstractions**: High-level code compiles to optimal machine code
- **Memory Safety**: No null pointers, buffer overflows, or data races
- **Predictable Performance**: No garbage collector pauses during critical trades
- **Strong Type System**: Catch configuration errors at compile time
- **Small Binaries**: Fast startup times and minimal resource usage

**When to Choose Rust:**
- High-frequency trading strategies
- Latency-sensitive market making
- Processing large market data streams
- When you need maximum reliability

**Popular Crates for Trading:**
- `reqwest` - HTTP client for REST APIs
- `serde` / `serde_json` - JSON serialization
- `tokio` - Async runtime for concurrent operations
- `rust_decimal` - Precise decimal arithmetic for prices
- `chrono` - Date and time handling

---

## Prerequisites

- Rust installed ([rustup](https://rustup.rs/) recommended)
- the0 CLI installed
- Valid the0 API key
- Basic understanding of Rust ownership and borrowing

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
├── config.json        # Example configuration
└── README.md          # Documentation
```

---

## Step 1: Create Your Project

```bash
cargo new my-rust-bot
cd my-rust-bot
```

This creates a new Rust project with the standard layout.

---

## Step 2: Configure Cargo.toml

Add the the0 SDK and any dependencies you need:

```toml
[package]
name = "my-rust-bot"
version = "0.1.0"
edition = "2021"

[[bin]]
name = "my-rust-bot"
path = "src/main.rs"

[dependencies]
# the0 SDK - handles configuration parsing and result output
# From crates.io:
the0-sdk = "0.1"

# Or from git (use a specific release tag):
# the0-sdk = { git = "https://github.com/alexanderwanyoike/the0", tag = "v1.1.0", subdirectory = "sdk/rust" }

# JSON handling (required by the0 SDK)
serde_json = "1.0"

# Optional: HTTP client for API calls
reqwest = { version = "0.11", features = ["blocking", "json"] }

# Optional: Precise decimal arithmetic for prices
rust_decimal = "1.33"
```

---

## Step 3: Write Your Bot

Create `src/main.rs`:

```rust
use the0::input;
use serde_json::json;

/// Main entry point for the trading bot.
///
/// The the0 SDK handles:
/// - Parsing BOT_ID and BOT_CONFIG from environment variables
/// - Writing results to the correct output file
/// - Proper exit codes for success/failure
fn main() {
    // Parse bot configuration from environment
    // This reads BOT_ID and BOT_CONFIG set by the platform
    let (bot_id, config) = input::parse();

    // Log startup (appears in bot logs)
    eprintln!("Bot {} starting...", bot_id);

    // Extract configuration with type-safe accessors
    // config is a serde_json::Value, so we use .get() and type conversion
    let symbol = config
        .get("symbol")
        .and_then(|v| v.as_str())
        .unwrap_or("BTC/USDT");

    let amount = config
        .get("amount")
        .and_then(|v| v.as_f64())
        .unwrap_or(100.0);

    eprintln!("Trading {} with amount {}", symbol, amount);

    // ===========================================
    // YOUR TRADING LOGIC GOES HERE
    // ===========================================

    // Example: Fetch price, analyze, execute trade
    match execute_strategy(symbol, amount) {
        Ok(result) => {
            eprintln!("Strategy executed successfully");

            // Output detailed result
            input::result(&json!({
                "status": "success",
                "message": "Trade executed",
                "data": {
                    "symbol": symbol,
                    "amount": amount,
                    "result": result
                }
            }));
        }
        Err(e) => {
            // Signal failure - this writes error and exits with code 1
            input::error(&format!("Strategy failed: {}", e));
        }
    }
}

/// Example trading strategy implementation
fn execute_strategy(symbol: &str, amount: f64) -> Result<String, String> {
    // In a real bot, you would:
    // 1. Fetch current market data
    // 2. Calculate indicators or signals
    // 3. Execute trades via exchange API

    // Simulated logic for demonstration
    if amount <= 0.0 {
        return Err("Amount must be positive".to_string());
    }

    // Return trade ID or result
    Ok(format!("trade_{}_{}", symbol.replace("/", "_"), chrono::Utc::now().timestamp()))
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

# The entrypoint is the source file - it gets compiled automatically
entrypoints:
  bot: src/main.rs

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  categories: [trading]
  instruments: [crypto]
  tags: [rust, high-performance]
```

**Note:** The `runtime: rust-stable` tells the platform to compile your bot using the stable Rust toolchain. You don't need to compile locally.

---

## Step 5: Define Parameter Schema

Create `bot-schema.json`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Rust Bot Configuration",
  "description": "Configuration for the Rust trading bot",
  "properties": {
    "symbol": {
      "type": "string",
      "title": "Trading Symbol",
      "description": "The trading pair (e.g., BTC/USDT)",
      "default": "BTC/USDT"
    },
    "amount": {
      "type": "number",
      "title": "Trade Amount",
      "description": "Amount in base currency to trade",
      "default": 100,
      "minimum": 0.01
    },
    "api_key": {
      "type": "string",
      "title": "API Key",
      "description": "Your exchange API key"
    },
    "api_secret": {
      "type": "string",
      "title": "API Secret",
      "description": "Your exchange API secret"
    }
  },
  "required": ["symbol", "api_key", "api_secret"]
}
```

---

## Step 6: Test Locally

```bash
# Set environment variables for testing
export BOT_ID="test-bot-123"
export BOT_CONFIG='{"symbol":"BTC/USDT","amount":100}'
export CODE_MOUNT_DIR="/tmp"

# Build and run
cargo run
```

---

## Step 7: Deploy

```bash
the0 custom-bot deploy
```

The platform will:
1. Compile your Rust code using `cargo build --release`
2. Package the binary
3. Deploy to the runtime environment

No need to compile locally - it all happens in the cloud!

---

## SDK API Reference

The `the0` crate provides these functions in the `input` module:

### `input::parse() -> (String, Value)`

Parse bot configuration from environment. Returns `(bot_id, config)` where config is a `serde_json::Value`.

```rust
let (bot_id, config) = input::parse();

// Access nested values
let symbol = config["symbol"].as_str().unwrap_or("BTC/USDT");
let amount = config["amount"].as_f64().unwrap_or(100.0);
```

### `input::parse_as_map() -> (String, HashMap<String, Value>)`

Parse configuration as a HashMap for easier iteration:

```rust
let (bot_id, config) = input::parse_as_map();

for (key, value) in &config {
    eprintln!("{}: {}", key, value);
}
```

### `input::success(message: &str)`

Output a success result:

```rust
input::success("Trade executed successfully");
// Writes: {"status":"success","message":"Trade executed successfully"}
```

### `input::error(message: &str) -> !`

Output an error and exit with code 1. This function never returns.

```rust
if amount <= 0.0 {
    input::error("Amount must be positive"); // Exits here
}
// This line is never reached if amount <= 0
```

### `input::result(data: &Value)`

Output a custom JSON result:

```rust
use serde_json::json;

input::result(&json!({
    "status": "success",
    "trade_id": "abc123",
    "filled_amount": 0.5,
    "average_price": 45123.50
}));
```

---

## Example: Price Fetcher with reqwest

Here's a more complete example that fetches real price data:

```rust
use the0::input;
use serde_json::{json, Value};

fn main() {
    let (bot_id, config) = input::parse();

    let symbol = config["symbol"].as_str().unwrap_or("BTCUSDT");

    eprintln!("Bot {} fetching price for {}", bot_id, symbol);

    match fetch_binance_price(symbol) {
        Ok(price) => {
            eprintln!("Current price: ${:.2}", price);

            input::result(&json!({
                "status": "success",
                "message": "Price fetched",
                "data": {
                    "symbol": symbol,
                    "price": price,
                    "timestamp": chrono::Utc::now().to_rfc3339()
                }
            }));
        }
        Err(e) => {
            input::error(&format!("Failed to fetch price: {}", e));
        }
    }
}

/// Fetch current price from Binance public API
fn fetch_binance_price(symbol: &str) -> Result<f64, Box<dyn std::error::Error>> {
    let url = format!(
        "https://api.binance.com/api/v3/ticker/price?symbol={}",
        symbol
    );

    let response: Value = reqwest::blocking::get(&url)?.json()?;

    let price = response["price"]
        .as_str()
        .ok_or("Missing price field")?
        .parse::<f64>()?;

    Ok(price)
}
```

Add to `Cargo.toml`:

```toml
[dependencies]
reqwest = { version = "0.11", features = ["blocking", "json"] }
chrono = "0.4"
```

---

## Best Practices

### 1. Error Handling with Result

Use Rust's Result type for clean error handling:

```rust
fn execute_trade(symbol: &str, amount: f64) -> Result<TradeResult, TradeError> {
    let price = fetch_price(symbol)?;  // ? propagates errors
    let order = place_order(symbol, amount, price)?;
    Ok(TradeResult { order_id: order.id, filled: order.filled })
}

fn main() {
    let (id, config) = input::parse();

    match execute_trade(&config["symbol"].as_str().unwrap(), 100.0) {
        Ok(result) => input::success(&format!("Trade {}", result.order_id)),
        Err(e) => input::error(&e.to_string()),
    }
}
```

### 2. Configuration Validation

Validate early and fail fast:

```rust
fn main() {
    let (bot_id, config) = input::parse();

    // Validate required fields
    let symbol = match config.get("symbol").and_then(|v| v.as_str()) {
        Some(s) if !s.is_empty() => s,
        _ => {
            input::error("Missing or empty 'symbol' in configuration");
        }
    };

    let amount = config["amount"].as_f64().unwrap_or(100.0);
    if amount <= 0.0 {
        input::error("Amount must be positive");
    }

    // Continue with validated values...
}
```

### 3. Logging

Both stdout and stderr go to your bot's logs:

```rust
println!("Starting trade...");            // Goes to log
eprintln!("DEBUG: price = {}", price);    // Goes to log
```

### 4. Precise Decimal Math

Use `rust_decimal` for financial calculations:

```rust
use rust_decimal::Decimal;
use rust_decimal_macros::dec;

let price = Decimal::from_str("45123.50").unwrap();
let quantity = dec!(0.5);
let total = price * quantity;  // 22561.75 exactly
```

---

## Async Support

For async operations, you can use tokio:

```toml
[dependencies]
tokio = { version = "1", features = ["rt-multi-thread", "macros"] }
reqwest = "0.11"  # async by default
```

```rust
use the0::input;

#[tokio::main]
async fn main() {
    let (bot_id, config) = input::parse();

    // Async operations
    let price = fetch_price_async(&config["symbol"].as_str().unwrap()).await;

    match price {
        Ok(p) => input::success(&format!("Price: {}", p)),
        Err(e) => input::error(&e),
    }
}

async fn fetch_price_async(symbol: &str) -> Result<f64, String> {
    // Async HTTP request
    let resp = reqwest::get(format!("https://api.example.com/price/{}", symbol))
        .await
        .map_err(|e| e.to_string())?;

    // Parse response...
    Ok(45000.0)
}
```

---

## Related Documentation

- [Configuration Reference](/custom-bot-development/configuration)
- [Bot Types](/custom-bot-development/bot-types)
- [Custom Frontends](/custom-bot-development/custom-frontends)
- [Deployment Guide](/custom-bot-development/deployment)
