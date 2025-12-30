---
title: "Python Quick Start"
description: "Build your first Python trading bot with the0"
tags: ["custom-bots", "python", "quick-start"]
order: 10
---

# Python Quick Start

Python dominates algorithmic trading for good reason. Its clean syntax makes complex strategies readable, and its ecosystem includes libraries for virtually every exchange, data source, and analysis technique. This guide walks through building a complete Python bot from scratch, covering project setup, the SDK, metrics emission, and deployment.

By the end of this guide, you'll have a working scheduled bot that tracks portfolio values and emits metrics to the dashboard.

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

You'll also need Python 3.11 or higher installed locally for testing.

## Project Structure

Create a new directory for your bot and set up the standard project structure:

```bash
mkdir portfolio-tracker
cd portfolio-tracker
```

A Python bot requires four essential files:

```
portfolio-tracker/
├── bot-config.yaml      # Bot metadata and runtime settings
├── main.py              # Entry point with trading logic
├── bot-schema.json      # Configuration schema for users
└── requirements.txt     # Python dependencies
```

## Defining Bot Metadata

Start with `bot-config.yaml`, which tells the platform how to run your bot:

```yaml
name: portfolio-tracker
description: "Tracks portfolio value and emits position metrics"
version: 1.0.0
author: "your-name"
type: scheduled
runtime: python3.11

entrypoints:
  bot: main.py

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  categories: [portfolio, tracking]
  tags: [example, beginner]
  complexity: beginner
```

The `type: scheduled` setting means this bot runs on a cron schedule rather than continuously. Each execution is independent—the bot starts, performs its work, reports results, and exits. The scheduler then waits until the next trigger time.

The `runtime: python3.11` field specifies the Python version. The platform provides Python 3.11 with pip for dependency management.

## Defining Configuration Schema

The `bot-schema.json` file specifies what parameters users can configure when they create bot instances. The platform uses this schema to generate the configuration UI and validate user input:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Portfolio Tracker Configuration",
  "description": "Configuration for the portfolio tracker bot",
  "properties": {
    "initial_value": {
      "type": "number",
      "description": "Initial portfolio value in USD",
      "default": 10000,
      "minimum": 100
    },
    "volatility": {
      "type": "number",
      "description": "Price volatility factor (0.01 = 1%)",
      "default": 0.02,
      "minimum": 0.001,
      "maximum": 0.1
    },
    "symbols": {
      "type": "array",
      "description": "Symbols to track in the portfolio",
      "items": {
        "type": "string"
      },
      "default": ["BTC", "ETH", "SOL"]
    }
  },
  "additionalProperties": false
}
```

Every property with a `default` value becomes optional—users can override it or accept the default. Properties listed in a `required` array must be provided explicitly.

## Writing the Bot Logic

The entry point file contains your trading logic. The SDK provides functions for parsing configuration, emitting metrics, and reporting results. Create `main.py`:

```python
import random
import logging
from the0 import parse, success, error, metric

# Configure logging
logging.basicConfig(
    level=logging.INFO,
    format='{"level":"%(levelname)s","message":"%(message)s","timestamp":"%(asctime)s"}'
)
logger = logging.getLogger(__name__)


def main(bot_id: str = None, config: dict = None):
    """Bot entry point using the0 SDK."""

    # Parse configuration from environment if not passed directly
    if bot_id is None or config is None:
        bot_id, config = parse()

    # Extract configuration with defaults
    initial_value = config.get("initial_value", 10000)
    volatility = config.get("volatility", 0.02)
    symbols = config.get("symbols", ["BTC", "ETH", "SOL"])

    logger.info(f"Bot {bot_id} started with symbols: {symbols}")

    # Calculate portfolio state
    portfolio = calculate_portfolio(initial_value, volatility, symbols)

    # Emit portfolio value metric
    metric("portfolio_value", {
        "value": portfolio["total_value"],
        "change_pct": portfolio["change_pct"],
    })

    # Emit position metrics for each holding
    for position in portfolio["positions"]:
        metric("position", {
            "symbol": position["symbol"],
            "quantity": position["quantity"],
            "value": position["value"],
            "price": position["price"],
        })

    logger.info(f"Bot {bot_id} completed")

    # Signal success with result data
    success(f"Portfolio tracked: ${portfolio['total_value']:.2f}", {
        "portfolio_value": portfolio["total_value"],
        "positions_count": len(portfolio["positions"]),
    })


def calculate_portfolio(initial_value: float, volatility: float, symbols: list) -> dict:
    """Calculate portfolio with simulated price movements."""
    positions = []
    total_value = 0

    base_prices = {
        "BTC": 45000,
        "ETH": 2400,
        "SOL": 120,
        "AVAX": 35,
        "LINK": 15,
    }

    value_per_symbol = initial_value / len(symbols)

    for symbol in symbols:
        base_price = base_prices.get(symbol, 100)
        price_change = random.uniform(-volatility, volatility)
        current_price = base_price * (1 + price_change)

        quantity = value_per_symbol / current_price
        position_value = quantity * current_price

        positions.append({
            "symbol": symbol,
            "quantity": round(quantity, 6),
            "price": round(current_price, 2),
            "value": round(position_value, 2),
        })

        total_value += position_value

    change_pct = ((total_value - initial_value) / initial_value) * 100

    return {
        "total_value": round(total_value, 2),
        "change_pct": round(change_pct, 2),
        "positions": positions,
    }


if __name__ == "__main__":
    main()
```

The `main()` function accepts optional `bot_id` and `config` parameters. When running on the platform, the runtime calls `main()` directly with these values. For local testing, the function falls back to parsing from environment variables via `parse()`.

## SDK Functions

The Python SDK provides these core functions:

### parse()

Reads `BOT_ID` and `BOT_CONFIG` from environment variables. Returns a tuple of the bot ID string and parsed configuration dictionary:

```python
from the0 import parse

bot_id, config = parse()
symbol = config.get("symbol", "BTC/USD")
```

### metric(type, data)

Emits a metric to the platform dashboard. The type string identifies the metric category, and the data dictionary contains the metric payload:

```python
from the0 import metric

metric("price", {"symbol": "BTC/USD", "value": 45000, "change_pct": 2.5})
metric("signal", {"type": "BUY", "symbol": "ETH", "confidence": 0.85})
metric("portfolio_value", {"value": 10500, "change_pct": 5.0})
```

Metrics appear in the dashboard in real-time. Use them to track prices, positions, signals, and any other data relevant to your strategy.

### success(message, data)

Reports successful execution. The message appears in logs, and the optional data dictionary is stored with the execution record:

```python
from the0 import success

success("Trade completed")
success("Portfolio updated", {"total_value": 10500, "positions": 3})
```

### error(message)

Reports failure and terminates execution with exit code 1:

```python
from the0 import error

if not api_key:
    error("API key is required")
    # Execution stops here
```

## Adding Dependencies

Create `requirements.txt` with any packages your bot needs:

```txt
the0-sdk>=0.1.0
```

For a real trading bot, you might include exchange libraries:

```txt
the0-sdk>=0.1.0
ccxt>=4.0.0
pandas>=2.0.0
numpy>=1.24.0
```

The platform installs dependencies automatically during deployment using Docker to ensure compatibility with the runtime environment.

## Testing Locally

Test your bot by setting environment variables and running directly:

```bash
export BOT_ID="test-bot"
export BOT_CONFIG='{"initial_value":10000,"volatility":0.02,"symbols":["BTC","ETH","SOL"]}'
export CODE_MOUNT_DIR="/tmp"

python main.py
```

You should see log output and the success message. Verify that the metrics emit correctly by checking the structured output.

## Deploying

Deploy your bot to the platform:

```bash
the0 custom-bot deploy
```

The CLI validates your configuration, vendors dependencies using Docker, packages everything, and uploads to the platform. After deployment, the bot appears in your custom bots list.

## Creating Bot Instances

Once deployed, create instances that run your bot on a schedule. Create an instance configuration file:

```json
{
  "name": "daily-portfolio",
  "type": "scheduled/portfolio-tracker",
  "version": "1.0.0",
  "schedule": "0 9 * * *",
  "config": {
    "initial_value": 25000,
    "symbols": ["BTC", "ETH", "SOL", "AVAX"]
  }
}
```

Deploy the instance:

```bash
the0 bot deploy instance-config.json
```

The bot will now run daily at 9 AM UTC, tracking the specified portfolio and emitting metrics to the dashboard.

## Monitoring

After deploying instances, monitor their execution:

```bash
# List running instances
the0 bot list

# View logs
the0 bot logs <bot_id>

# Stream logs in real-time
the0 bot logs <bot_id> -w
```

## Alternative: Structured Logging for Metrics

Instead of using the SDK's `metric()` function, you can emit metrics through structured logging. Include a `_metric` field in your log output:

```python
import structlog

structlog.configure(
    processors=[
        structlog.processors.JSONRenderer()
    ]
)
logger = structlog.get_logger()

# Emit metrics via logging
logger.info("portfolio_snapshot",
    _metric="portfolio_value",
    value=10500.00,
    change_pct=2.5
)

logger.info("position_update",
    _metric="position",
    symbol="BTC",
    quantity=0.5,
    value=22500
)
```

The platform automatically detects and parses JSON log entries with `_metric` fields. This approach integrates well with existing logging infrastructure. See [Metrics](./metrics) for details on both approaches.

## Next Steps

With your first bot deployed, explore these topics to build more sophisticated strategies:

- [Configuration](./configuration) - Complete bot-config.yaml reference
- [Bot Types](./bot-types) - Scheduled vs realtime execution models
- [Metrics](./metrics) - Dashboard metrics and structured logging
- [Custom Frontends](./custom-frontends) - Build React dashboards for your bot
- [Testing](./testing) - Local testing patterns and best practices
