---
title: "Rust Quick Start"
description: "Build your first Rust trading bot with the0"
tags: ["custom-bots", "rust", "quick-start"]
order: 12
---

# Rust Quick Start

Rust delivers predictable performance with no garbage collector pauses, making it ideal for latency-sensitive trading strategies. The type system catches configuration errors at compile time, and the small binary size means fast startup. This guide walks through building an SMA crossover bot that monitors stock prices and emits trading signals.

By the end of this guide, you'll have a working realtime bot that calculates Simple Moving Averages and detects crossover signals.

## Prerequisites

Before starting, ensure you have the CLI installed and authenticated:

```bash
# Clone the repository and build the CLI
git clone https://github.com/alexanderwanyoike/the0.git
cd the0/cli
make install

# Authenticate
the0 auth login
```

You'll also need Rust installed locally for building. Use [rustup](https://rustup.rs/) for the recommended installation method.

## Project Structure

Create a new Cargo project:

```bash
cargo new sma-crossover
cd sma-crossover
```

A Rust bot requires these files:

```
sma-crossover/
├── Cargo.toml           # Dependencies and project configuration
├── Cargo.lock           # Dependency lock file
├── src/
│   └── main.rs          # Bot entry point
├── bot-config.yaml      # Bot metadata and runtime settings
├── bot-schema.json      # Configuration schema for users
└── target/
    └── release/
        └── sma-bot      # Compiled binary (after cargo build)
```

The entry point in `bot-config.yaml` must point to the compiled binary, not the source file. You compile locally before deploying.

## Defining Bot Metadata

Create `bot-config.yaml`:

```yaml
name: sma-crossover
description: "SMA crossover strategy bot with Yahoo Finance data"
version: 1.0.0
author: "your-name"
type: realtime
runtime: rust-stable

entrypoints:
  bot: target/release/sma-bot

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  categories: [trading, technical-analysis]
  tags: [sma, crossover, rust]
  complexity: beginner
```

The `entrypoints.bot` field points to the compiled binary. The binary name comes from your `Cargo.toml` configuration.

## Defining Configuration Schema

Create `bot-schema.json`:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "SMA Crossover Configuration",
  "description": "Configuration for the SMA crossover trading strategy bot",
  "properties": {
    "symbol": {
      "type": "string",
      "description": "Stock symbol to monitor (e.g., AAPL, MSFT, GOOGL)",
      "default": "AAPL"
    },
    "short_period": {
      "type": "integer",
      "description": "Number of periods for short SMA (fast moving average)",
      "default": 5,
      "minimum": 2,
      "maximum": 50
    },
    "long_period": {
      "type": "integer",
      "description": "Number of periods for long SMA (slow moving average)",
      "default": 20,
      "minimum": 5,
      "maximum": 200
    },
    "update_interval_ms": {
      "type": "integer",
      "description": "Milliseconds between price updates",
      "default": 60000,
      "minimum": 30000,
      "maximum": 3600000
    }
  },
  "additionalProperties": false
}
```

## Configuring Cargo.toml

Replace the default `Cargo.toml`:

```toml
[package]
name = "sma-bot"
version = "1.0.0"
edition = "2021"

[[bin]]
name = "sma-bot"
path = "src/main.rs"

[dependencies]
# the0 SDK for configuration and metrics
the0-sdk = "0.1"

# JSON handling
serde = { version = "1.0", features = ["derive"] }
serde_json = "1.0"

# HTTP client for API calls
reqwest = { version = "0.11", features = ["blocking", "json"] }

# Structured logging
tracing = "0.1"
tracing-subscriber = { version = "0.3", features = ["json"] }
```

The SDK is published on crates.io as `the0-sdk`. The binary name `sma-bot` must match the entry point path in `bot-config.yaml`.

## Writing the Bot Logic

Create `src/main.rs`:

```rust
use the0_sdk::input;
use serde::Deserialize;
use serde_json::json;
use std::thread;
use std::time::Duration;
use tracing::{info, error, Level};

// Yahoo Finance API response structures
#[derive(Deserialize)]
struct YahooResponse {
    chart: ChartResponse,
}

#[derive(Deserialize)]
struct ChartResponse {
    result: Option<Vec<ChartResult>>,
}

#[derive(Deserialize)]
struct ChartResult {
    indicators: Indicators,
}

#[derive(Deserialize)]
struct Indicators {
    quote: Vec<Quote>,
}

#[derive(Deserialize)]
struct Quote {
    close: Vec<Option<f64>>,
}

// Bot state persists across iterations
struct BotState {
    prev_short_sma: Option<f64>,
    prev_long_sma: Option<f64>,
}

fn main() {
    // Initialize structured logging
    tracing_subscriber::fmt()
        .json()
        .with_max_level(Level::INFO)
        .init();

    // Parse configuration from environment
    let (bot_id, config) = input::parse().expect("Failed to parse bot configuration");

    // Extract configuration with defaults
    let symbol = config["symbol"].as_str().unwrap_or("AAPL");
    let short_period = config["short_period"].as_i64().unwrap_or(5) as usize;
    let long_period = config["long_period"].as_i64().unwrap_or(20) as usize;
    let update_interval_ms = config["update_interval_ms"].as_i64().unwrap_or(60000) as u64;

    info!(bot_id = %bot_id, symbol = %symbol, short_period, long_period, "Bot started");

    let client = reqwest::blocking::Client::builder()
        .user_agent("the0-sma-bot/1.0")
        .build()
        .expect("Failed to create HTTP client");

    let mut state = BotState {
        prev_short_sma: None,
        prev_long_sma: None,
    };

    // Main loop - runs until process is terminated
    loop {
        match fetch_and_process(&client, symbol, short_period, long_period, &mut state) {
            Ok(_) => {}
            Err(e) => error!(error = %e, "Processing error"),
        }

        thread::sleep(Duration::from_millis(update_interval_ms));
    }
}

fn fetch_and_process(
    client: &reqwest::blocking::Client,
    symbol: &str,
    short_period: usize,
    long_period: usize,
    state: &mut BotState,
) -> Result<(), Box<dyn std::error::Error>> {
    let prices = fetch_yahoo_finance(client, symbol)?;

    if prices.len() < long_period {
        info!(symbol = %symbol, required = long_period, have = prices.len(), "Insufficient data");
        return Ok(());
    }

    // Calculate current price and change
    let current_price = prices[prices.len() - 1];
    let previous_price = prices[prices.len() - 2];
    let change_pct = ((current_price - previous_price) / previous_price) * 100.0;

    // Emit price metric
    input::metric("price", &json!({
        "symbol": symbol,
        "value": round(current_price, 2),
        "change_pct": round(change_pct, 3)
    }));

    // Calculate SMAs
    let short_sma = calculate_sma(&prices, short_period);
    let long_sma = calculate_sma(&prices, long_period);

    // Emit SMA metric
    input::metric("sma", &json!({
        "symbol": symbol,
        "short_sma": round(short_sma, 2),
        "long_sma": round(long_sma, 2),
        "short_period": short_period,
        "long_period": long_period
    }));

    // Check for crossover signal
    if let (Some(prev_short), Some(prev_long)) = (state.prev_short_sma, state.prev_long_sma) {
        if let Some(signal) = check_crossover(prev_short, prev_long, short_sma, long_sma) {
            let confidence = (short_sma - long_sma).abs() / long_sma * 100.0;

            input::metric("signal", &json!({
                "type": signal,
                "symbol": symbol,
                "price": round(current_price, 2),
                "confidence": round(confidence.min(0.95), 2),
                "reason": format!("SMA{} crossed {} SMA{}",
                    short_period,
                    if signal == "BUY" { "above" } else { "below" },
                    long_period)
            }));
        }
    }

    // Update state for next iteration
    state.prev_short_sma = Some(short_sma);
    state.prev_long_sma = Some(long_sma);

    Ok(())
}

fn fetch_yahoo_finance(
    client: &reqwest::blocking::Client,
    symbol: &str,
) -> Result<Vec<f64>, Box<dyn std::error::Error>> {
    let url = format!(
        "https://query1.finance.yahoo.com/v8/finance/chart/{}?interval=1d&range=1mo",
        symbol
    );

    let response: YahooResponse = client.get(&url).send()?.json()?;

    let prices: Vec<f64> = response
        .chart
        .result
        .and_then(|results| results.into_iter().next())
        .map(|result| {
            result.indicators.quote
                .into_iter()
                .next()
                .map(|quote| quote.close.into_iter().flatten().collect())
                .unwrap_or_default()
        })
        .unwrap_or_default();

    Ok(prices)
}

fn calculate_sma(prices: &[f64], period: usize) -> f64 {
    if prices.len() < period {
        return 0.0;
    }
    let start = prices.len() - period;
    prices[start..].iter().sum::<f64>() / period as f64
}

fn check_crossover(
    prev_short: f64,
    prev_long: f64,
    curr_short: f64,
    curr_long: f64,
) -> Option<&'static str> {
    // Golden cross: short SMA crosses above long SMA
    if prev_short <= prev_long && curr_short > curr_long {
        return Some("BUY");
    }
    // Death cross: short SMA crosses below long SMA
    if prev_short >= prev_long && curr_short < curr_long {
        return Some("SELL");
    }
    None
}

fn round(value: f64, decimals: i32) -> f64 {
    let multiplier = 10f64.powi(decimals);
    (value * multiplier).round() / multiplier
}
```

The bot fetches historical prices from Yahoo Finance, calculates short and long Simple Moving Averages, and emits signals when crossovers occur. State persists across loop iterations, allowing the bot to detect when averages cross.

## SDK Functions

The Rust SDK provides these core functions in the `input` module:

### input::parse()

Reads `BOT_ID` and `BOT_CONFIG` from environment variables. Returns a `Result<(String, serde_json::Value), Error>`:

```rust
use the0_sdk::input;

let (bot_id, config) = input::parse().expect("Failed to parse config");

let symbol = config["symbol"].as_str().unwrap_or("AAPL");
let amount = config["amount"].as_f64().unwrap_or(100.0);
```

### input::metric(type, data)

Emits a metric to the platform dashboard:

```rust
use the0_sdk::input;
use serde_json::json;

input::metric("price", &json!({
    "symbol": "AAPL",
    "value": 150.25,
    "change_pct": 1.5
}));

input::metric("signal", &json!({
    "type": "BUY",
    "confidence": 0.85
}));
```

### input::success(message)

Reports successful execution for scheduled bots:

```rust
use the0_sdk::input;

input::success("Analysis complete");
```

### input::error(message)

Reports failure and terminates with exit code 1. This function does not return:

```rust
use the0_sdk::input;

if prices.is_empty() {
    input::error("No price data available");
}
```

### input::result(data)

Outputs a custom JSON result:

```rust
use the0_sdk::input;
use serde_json::json;

input::result(&json!({
    "status": "success",
    "trade_id": "abc123",
    "filled_amount": 0.5
}));
```

## Building

Compile in release mode for optimal performance:

```bash
cargo build --release
```

The binary appears at `target/release/sma-bot`, matching the entry point in `bot-config.yaml`.

## Testing Locally

Test by setting environment variables:

```bash
export BOT_ID="test-bot"
export BOT_CONFIG='{"symbol":"AAPL","short_period":5,"long_period":20,"update_interval_ms":5000}'
export CODE_MOUNT_DIR="/tmp"

./target/release/sma-bot
```

The bot should start fetching prices and emitting metrics. Press Ctrl+C to stop.

## Deploying

Deploy your compiled bot to the platform:

```bash
the0 custom-bot deploy
```

The CLI packages the compiled binary along with configuration files and uploads everything. Unlike interpreted languages, you must compile before deploying.

## Creating Bot Instances

Once deployed, create instances that run your bot:

```json
{
  "name": "aapl-sma",
  "type": "realtime/sma-crossover",
  "version": "1.0.0",
  "config": {
    "symbol": "AAPL",
    "short_period": 5,
    "long_period": 20,
    "update_interval_ms": 60000
  }
}
```

Deploy the instance:

```bash
the0 bot deploy instance-config.json
```

## Monitoring

Monitor running instances:

```bash
# List running instances
the0 bot list

# View logs (use bot ID from deploy output or bot list)
the0 bot logs <bot_id>

# Stream logs in real-time
the0 bot logs <bot_id> -w

# Stop a realtime bot
the0 bot delete <bot_id>
```

## Structured Logging with Tracing

The example uses the `tracing` crate for structured JSON logging. All log output goes to the platform's log system:

```rust
use tracing::{info, error, warn};

info!(symbol = %symbol, price = price, "Price updated");
error!(error = %e, "Failed to fetch data");
warn!(attempts = retries, "Retrying request");
```

Configure tracing in your main function:

```rust
use tracing::Level;
use tracing_subscriber;

tracing_subscriber::fmt()
    .json()
    .with_max_level(Level::INFO)
    .init();
```

## Next Steps

With your first Rust bot deployed, explore these topics:

- [Configuration](./configuration) - Complete bot-config.yaml reference
- [Bot Types](./bot-types) - Scheduled vs realtime execution models
- [Metrics](./metrics) - Dashboard metrics and structured logging
- [Custom Frontends](./custom-frontends) - Build React dashboards for your bot
- [Testing](./testing) - Local testing patterns and best practices
