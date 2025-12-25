---
title: "Configuration"
description: "Complete guide to bot configuration and schemas"
tags: ["custom-bots", "configuration", "schemas"]
order: 3
---

# Bot Configuration Reference

This comprehensive guide covers all aspects of configuring custom bots, from basic metadata to advanced schema definitions.

---

## Bot Configuration File (bot-config.yaml)

The `bot-config.yaml` file is the main configuration file that defines your bot's metadata, structure, and requirements.

### Complete Structure

```yaml
name: string # Required: Bot identifier
description: string # Required: Brief description
version: string # Required: Semantic version (e.g., "1.0.0")
author: string # Required: Author name or organization
type: string # Required: scheduled | realtime
runtime: string # Required: python3.11 | nodejs20 | rust-stable

entrypoints: # Required: Entry point definitions
  bot: string # Required: Main bot file

schema: # Required: Schema definitions
  bot: string # Required: Bot parameter schema file

readme: string # Required: Documentation file

metadata: # Optional: Additional metadata
  categories: [string] # Bot categories for organization
  instruments: [string] # Supported instruments (stocks, crypto, forex)
  exchanges: [string] # Supported exchanges
  tags: [string] # Searchable tags
  #... Additional metadata fields
```

### Required Fields

#### Basic Information

```yaml
name: momentum-trading-bot
description: "A momentum-based trading strategy using technical indicators"
version: 1.2.0
author: TradingExperts
type: scheduled
runtime: python3.11
readme: README.md
```

**Field Descriptions:**

- **name**: Unique identifier for your custom bot (lowercase, hyphens allowed)
- **description**: Brief, clear description of what your bot does
- **version**: [Semantic versioning](https://semver.org/) format (MAJOR.MINOR.PATCH)
- **author**: Your name, organization, or handle
- **type**: Execution model for your bot
- **runtime**: Execution environment
- **readme**: Path to a markdown file with detailed documentation

#### Entry Points

```yaml
entrypoints:
  bot: main.py # Main trading logic
```

**Entry Point Types:**

- **bot**: Main execution entry point (required)
- **webhook**: Webhook event handler (for event-driven bots)
- **analysis**: Custom analysis functions
- **custom**: Any additional entry points

#### Schema Definitions

```yaml
schema:
  bot: bot-schema.json
```

Each entry point should have a corresponding schema file defining its parameters.

### Bot Types

#### Scheduled Bots

```yaml
type: scheduled
```

**Characteristics:**

- Execute on fixed schedules (cron expressions)
- Suitable for: DCA strategies, portfolio rebalancing, periodic analysis
- Execution: Single run per trigger
- State: Stateless between executions

**Use Cases:**

- Daily portfolio rebalancing
- Weekly DCA purchases
- End-of-day analysis
- Monthly performance reports

#### Realtime Bots

```yaml
type: realtime
```

**Characteristics:**

- Run continuously, processing live data
- Execution: Continuous loop
- State: Maintains state across iterations

**Use Cases:**

- Grid trading strategies
- Market making
- Arbitrage detection
- Scalping

### Runtime Environments

#### Python 3.11

```yaml
runtime: python3.11
```

**Benefits:**

- Extensive trading libraries (ccxt, pandas, numpy, scikit-learn)
- Machine learning capabilities
- Scientific computing ecosystem
- Popular in quantitative finance

**Dependency Management:**

- Use `requirements.txt` for dependencies
- Automatic Docker-based vendoring
- Support for compiled packages

#### Node.js 20

```yaml
runtime: nodejs20
```

**Benefits:**

- Excellent async/await support
- Large npm ecosystem
- Good web3 support for crypto bots

#### Rust (Stable)

```yaml
runtime: rust-stable
```

**Benefits:**

- High performance with zero-cost abstractions
- Memory safety without garbage collection
- Excellent for high-frequency and low-latency trading
- Strong type system catches errors at compile time
- Growing ecosystem for financial applications

**Dependency Management:**

- Use `Cargo.toml` for dependencies
- Include `Cargo.lock` for reproducible builds
- Dependencies are compiled at deploy time

**Entry Point:**

```rust
// src/main.rs
mod the0;  // Helper module (auto-injected)
use the0::input;

fn main() {
    let (id, config) = input::parse();

    // Your trading logic here
    println!("Bot {} running", id);

    input::success("Bot executed successfully");
}
```

### Metadata Configuration

Metadata provides additional context for your bot, improving discoverability and organization in the marketplace.

#### Categories

```yaml
metadata:
  categories:
    - trading # General trading strategies
    - arbitrage # Arbitrage strategies
    - market-making # Market making strategies
    - analysis # Analysis and signals
    - portfolio # Portfolio management
    - defi # DeFi strategies
```

#### Instruments

```yaml
metadata:
  instruments:
    - crypto # Cryptocurrencies
    - stocks # Stock markets
    - forex # Foreign exchange
    - commodities # Commodity markets
    - options # Options trading
    - futures # Futures contracts
```

#### Exchanges

```yaml
metadata:
  exchanges:
    - binance # Binance
    - coinbase # Coinbase Pro
    - kraken # Kraken
    - ftx # FTX
    - bitmex # BitMEX
    - interactive-brokers # Interactive Brokers
```

#### Tags

```yaml
metadata:
  tags:
    - momentum # Momentum strategies
    - mean-reversion # Mean reversion strategies
    - technical-analysis # Technical analysis
    - machine-learning # ML-based strategies
    - beginner # Beginner-friendly
    - high-frequency # High-frequency trading
```

#### Risk and Complexity

```yaml
metadata:
  risk_level: medium # low | medium | high
  complexity: intermediate # beginner | intermediate | advanced
  min_capital: 1000 # Minimum recommended capital ($)
  max_capital: 100000 # Maximum recommended capital ($)
```

---

## JSON Schema Configuration

JSON schemas define the parameters your bot accepts and validate user inputs.

### Bot Schema (bot-schema.json)

#### Basic Structure

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Bot Configuration",
  "description": "Configuration parameters for the trading bot",
  "properties": {
    // Parameter definitions
  },
  "required": ["symbol", "api_key"],
  "additionalProperties": false
}
```

For more information on JSON Schema, see the [JSON Schema documentation](https://json-schema.org/).

---

## Validation and Testing

### JSON Schema Validation

```bash
# Validate schema files
jsonschema --instance config.json --schema bot-schema.json
```

### YAML Configuration Validation

```bash
# Validate YAML configuration files
python -c "import yaml; yaml.safe_load(open('bot-config.yaml'))"

# Or use yq for more detailed validation
yq eval '.' bot-config.yaml > /dev/null
```

### Debugging Tips

1. **Use Validation Tools**: Validate schemas and configurations early
2. **Test Incrementally**: Test each component individually
3. **Check Examples**: Compare with working examples
4. **Read Error Messages**: Pay attention to specific validation errors
5. **Use the CLI**: Leverage CLI validation commands

---

## Related Documentation

- [Quick Start Guide](/custom-bot-development/python-quick-start) - Build your first bot
- [Testing Guide](/custom-bot-development/testing) - Validation and testing
- [Deployment Guide](/custom-bot-development/deployment) - Deploying your bot
