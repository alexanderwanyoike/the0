---
title: "Python Quick Start"
description: "Build your first Python trading bot with the0"
tags: ["custom-bots", "python", "quick-start"]
order: 9
---

# Python Quick Start Guide

Build trading bots in Python with the0's flexible runtime. Python is the most popular choice for algorithmic trading due to its simplicity, extensive libraries, and rapid development cycle.

---

## Why Python for Trading Bots?

Python offers unmatched advantages for algorithmic trading:

- **Rapid Development**: Write and iterate on strategies quickly
- **Rich Ecosystem**: Libraries for every exchange, broker, and data source
- **Data Science**: NumPy, Pandas, and scikit-learn for analysis
- **Community**: Largest trading bot community and resources
- **Simplicity**: Clean syntax makes complex strategies readable

**When to Choose Python:**
- Rapid prototyping and strategy development
- Data-heavy strategies with analysis requirements
- Integration with machine learning models
- Teams new to algorithmic trading

**Popular Libraries for Trading:**
- `alpaca-py` - Official Alpaca SDK for stocks/crypto
- `ccxt` - Unified API for 100+ cryptocurrency exchanges
- `pandas` - Data manipulation and analysis
- `numpy` - Numerical computing
- `ta-lib` - Technical analysis indicators

---

## Tutorial: Build a DCA Bot

In this guide, we'll create a simple Dollar Cost Averaging (DCA) bot that an asset type using alpaca-py.

---

## What is Dollar Cost Averaging (DCA)?

Dollar Cost Averaging (DCA) is an investment strategy where you invest a fixed amount of money into a particular asset at regular intervals, regardless of the asset's current price. Instead of trying to time the market by making one large purchase, DCA spreads your investment over time.

**Key Benefits of DCA:**

- Reduces the impact of market volatility on your investment
- Eliminates the need to time the market perfectly
- Can result in a lower average cost per unit over time
- Helps build disciplined investing habits
- Reduces emotional decision-making in volatile markets

For example, if you invest $100 every week in Bitcoin, you'll buy more Bitcoin when prices are low and less when prices are high, potentially lowering your average purchase price over time compared to making a single large purchase.

## What is alpaca-py?

[Alpaca-py](https://github.com/alpacahq/alpaca-py) is the official Python SDK for Alpaca Markets, a commission-free trading platform that provides APIs for stock and crypto trading. It's a modern, easy-to-use library that allows developers to:

- **Trade Stocks & Crypto**: Execute buy/sell orders for US equities and cryptocurrencies
- **Access Market Data**: Get real-time and historical price data, quotes, and market information
- **Manage Portfolios**: Track positions, account balances, and trading history
- **Paper Trading**: Test strategies risk-free with simulated trading
- **Stream Live Data**: Receive real-time market updates via WebSocket connections
- **Options Trading**: Execute options trades with built-in support for those wall street bets yoloers! 💰💰💰

**Key Features:**

- Type-safe Python API with full IntelliSense support
- Built-in error handling and rate limiting
- Comprehensive documentation and examples
- Support for both live and paper trading environments

Alpaca is particularly popular among algorithmic traders and developers building trading bots because it offers a simple REST API, generous free tier, and no minimum account balance requirements.

---

## What You'll Build

A DCA bot that:

- Buys a fixed amount of a listed asset type (stocks or crypto) at regular intervals (e.g., daily, weekly)
- Runs on a schedule (e.g., monthly)

## Prerequisites

- [the0 CLI installed](/the0-cli/installation)
- [API key configured](/the0-cli/authentication)
- Python 3.11+ (for this example)
- Basic understanding of trading concepts
- Familiarity with Python programming and pip package management

## Step 1: Define the `bot-config.yaml`

Edit the `bot-config.yaml` file:

```yaml
name: killer-dca-bot
description: "A Dollar Cost Averaging bot that brings home the bacon 🥓"
version: 1.0.0
author: Jim Simons
type: scheduled
runtime: python3.11

entrypoints:
  bot: main.py

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  categories: [trading]
  instruments: [crypto, stocks]
  exchanges: [alpaca]
  tags: [dca, crypto, stocks]
```

## Step 2: Define Configuration Schema

Create `bot-schema.json` to define what parameters your bot accepts:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "DCA Bot Configuration",
  "properties": {
    "api_key": {
      "type": "string",
      "description": "Your Alpaca API key"
    },
    "secret_key": {
      "type": "string",
      "description": "Your Alpaca secret key"
    },
    "paper": {
      "type": "boolean",
      "default": true,
      "description": "Whether to use paper trading (recommended for testing)"
    },
    "symbol": {
      "type": "string",
      "description": "Trading symbol (e.g., 'AAPL', 'BTC/USD')"
    },
    "asset_type": {
      "type": "string",
      "description": "Type of asset to trade"
    },
    "amount": {
      "type": "number",
      "description": "Amount to invest"
    }
  },
  "required": [
    "api_key",
    "secret_key",
    "paper",
    "symbol",
    "asset_type",
    "amount"
  ],
  "additionalProperties": false
}
```

## Step 3: Implement the Bot Logic

Create `main.py` with your DCA bot implementation:

```python
from typing import Dict, Any
import logging
from datetime import datetime
from alpaca.trading.client import TradingClient
from alpaca.trading.requests import MarketOrderRequest
from alpaca.trading.enums import OrderSide, TimeInForce
from alpaca.data.historical import StockHistoricalDataClient, CryptoHistoricalDataClient
from alpaca.data.requests import StockLatestQuoteRequest, CryptoLatestQuoteRequest

# Configure logging
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def main(id: str, config: Dict[str, Any]) -> Dict[str, Any]:
    """
    Main DCA bot execution function using Alpaca.

    Args:
        id: Unique bot instance ID
        config: Bot configuration from user

    Returns:
        Dict with execution status and results
    """
    logger.info(f"Starting DCA bot {id} with config: {config}")

    try:
        # Extract configuration
        api_key = config["api_key"]
        secret_key = config["secret_key"]
        paper = config.get("paper", True)
        symbol = config["symbol"]
        asset_type = config["asset_type"]
        amount = config["amount"]

        # Initialize Alpaca trading client
        trading_client = TradingClient(
            api_key=api_key,
            secret_key=secret_key,
            paper=paper
        )

        # Get current price based on asset type
        if asset_type.lower() in ["crypto", "cryptocurrency"]:
            data_client = CryptoHistoricalDataClient(api_key, secret_key)
            quote_request = CryptoLatestQuoteRequest(symbol_or_symbols=[symbol])
            quotes = data_client.get_crypto_latest_quote(quote_request)
            current_price = float(quotes[symbol].ask_price)
        else:  # stocks
            data_client = StockHistoricalDataClient(api_key, secret_key)
            quote_request = StockLatestQuoteRequest(symbol_or_symbols=[symbol])
            quotes = data_client.get_stock_latest_quote(quote_request)
            current_price = float(quotes[symbol].ask_price)

        # Calculate quantity based on dollar amount
        if asset_type.lower() in ["crypto", "cryptocurrency"]:
            # For crypto, we can buy fractional shares
            quantity = amount / current_price
        else:
            # For stocks, calculate how many whole shares we can buy
            quantity = int(amount / current_price)
            if quantity == 0:
                return {
                    "status": "error",
                    "message": f"Insufficient funds: ${amount} cannot buy even 1 share at ${current_price:.2f}",
                }

        logger.info(f"Current price of {symbol}: ${current_price:.2f}")
        logger.info(f"Purchasing {quantity} shares/units for approximately ${amount}")

        # Prepare market order
        market_order_data = MarketOrderRequest(
            symbol=symbol,
            qty=quantity,
            side=OrderSide.BUY,
            time_in_force=TimeInForce.DAY
        )

        # Execute the order
        order = trading_client.submit_order(order_data=market_order_data)

        logger.info(f"Order submitted successfully: {order.id}")

        # Calculate actual cost (will be filled when order executes)
        actual_cost = float(quantity) * current_price

        return {
            "status": "success",
            "message": f"DCA purchase completed for {symbol}",
        }

    except Exception as e:
        logger.error(f"Error in DCA bot execution: {str(e)}")
        return {
            "status": "error",
            "message": f"DCA bot failed: {str(e)}",
            "data": {
                "error": str(e),
                "timestamp": datetime.now().isoformat()
            }
        }

if __name__ == "__main__":
    import sys
    import json

    # Use command line arguments for testing
    if len(sys.argv) >= 3:
        bot_id = sys.argv[1]
        config_file = sys.argv[2]

        # Load configuration from file
        with open(config_file, 'r') as f:
            test_config = json.load(f)

        result = main(bot_id, test_config)
        print(f"Result: {result}")
    else:
        print("Usage: python main.py <bot_id> <config.json>")
        print("Example: python main.py test-bot config.json")
        sys.exit(1)
```

## Step 4: Add Dependencies

Create `requirements.txt`:

```txt
alpaca-py>=0.42.0
```

## Step 5: Add Documentation

Create `README.md`:

````markdown
# DCA Bot README

A simple Dollar Cost Averaging (DCA) bot that automatically purchases a fixed dollar amount of stocks or cryptocurrencies at regular intervals using the Alpaca API.

## Features

- ✅ **Multi-Asset Support**: Trade both stocks and cryptocurrencies
- ✅ **Paper Trading**: Test your strategy risk-free before going live
- ✅ **Fractional Shares**: Buy fractional amounts for crypto assets
- ✅ **Automatic Scheduling**: Runs on your defined schedule
- ✅ **Real-time Pricing**: Uses latest market quotes for accurate purchases
- ✅ **Error Handling**: Robust error handling and logging

## Configuration

This bot accepts the following parameters:

### Required Parameters

- **`api_key`** (string, required) - Your Alpaca API key
- **`secret_key`** (string, required) - Your Alpaca secret key
- **`symbol`** (string, required) - Trading symbol (e.g., 'AAPL', 'BTC/USD')
- **`asset_type`** (string, required) - Type of asset ('stock' or 'crypto')
- **`amount`** (number, required) - Dollar amount to invest per execution

### Optional Parameters

- **`paper`** (boolean, optional) - Use paper trading (default: true)

## Example Configuration

```json
{
  "api_key": "your-alpaca-api-key",
  "secret_key": "your-alpaca-secret-key",
  "paper": true,
  "symbol": "AAPL",
  "asset_type": "stock",
  "amount": 100,
  "schedule": "0 9 1 * *", // Runs monthly on the 1st at 9 AM UTC
  "name": "my-dca-bot",
  "type": "scheduled/killer-dca-bot",
  "version": "1.0.0"
}
```

## How It Works

1. **Get Current Price**: Fetches the latest ask price for your chosen asset
2. **Calculate Quantity**: Determines how much to buy based on your dollar amount
3. **Place Order**: Submits a market buy order through Alpaca

## Asset Types

### Stocks

- Purchases whole shares only
- Will skip purchase if amount is insufficient for 1 share
- Uses Stock Historical Data Client for pricing

### Cryptocurrencies

- Supports fractional purchases
- Can buy any dollar amount (no minimum share requirement)
- Uses Crypto Historical Data Client for pricing

## Dependencies

- `alpaca-py>=0.42.0` - Official Alpaca Python SDK

## Error Handling

The bot includes comprehensive error handling for:

- Invalid API credentials
- Insufficient funds

## Getting Started

1. Get your Alpaca API keys from [alpaca.markets](https://alpaca.markets)
2. Configure your bot parameters
3. Test with paper trading first (`"paper": true`)
4. Deploy and schedule your bot
````

## Step 6: Test Your Bot Locally

Test your bot implementation:

```bash
# Test the main bot logic with example configuration
cat > test-config.json << EOF
{
    "api_key": "your-alpaca-api-key",
    "secret_key": "your-alpaca-secret-key",
    "paper": true,
    "symbol": "AAPL",
    "asset_type": "stock",
    "amount": 100,
    "version": "1.0.0"
}
EOF

python main.py test-bot test-config.json

```

## Step 9: Deploy Your Bot

Deploy your bot to the the0 platform:

```bash
the0 custom-bot deploy
```

The CLI will:

1. Validate your configuration
2. Install dependencies using Docker
3. Package your bot files
4. Upload to the platform

## Step 10: Create a Bot Instance

Once deployed, create a bot instance:

```bash
# Create configuration file
cat > dca-bot-config.json << EOF
{
    "api_key": "your-alpaca-api-key",
    "secret_key": "your-alpaca-secret-key",
    "paper": true,
    "symbol": "AAPL",
    "asset_type": "stock",
    "amount": 100,
    "schedule": "0 9 1 * *",  // Runs monthly on the 1st at 9 AM UTC
    "name": "my-dca-bot",
    "type": "scheduled/killer-dca-bot",
    "version": "1.0.0"
}
EOF

# Deploy the bot instance
the0 bot deploy dca-bot-config.json
```

## Next Steps

Congratulations! You've built and deployed your first trading bot. Here's what you can do next:

1. Go to the [Dashboard](/dashboard) to monitor your bot's performance
2. Explore the [Custom Bot Page](/custom-bots) to review your custom bot publish it to the marketplace
3. Experiment with different asset types (stocks, crypto)
4. Modify the bot to implement more complex strategies (e.g., stop-loss, take-profit)
This is just the tip of the iceberg! DCA is the `hello world` of trading bots. You can build much more complex strategies using the same principles.

## Advanced Features to Explore

- **Dynamic Position Sizing**: Adjust purchase amounts based on market conditions
- **Multi-Asset DCA**: Spread purchases across multiple cryptocurrencies
- **Conditional Logic**: Only buy during market dips or specific conditions
- **Integration with Technical Indicators**: Use RSI, MACD, or other indicators
- **Portfolio Rebalancing**: Maintain target allocations across assets

---

## SDK API Reference

The `the0-sdk` Python SDK provides utilities for parsing configuration and outputting results. Install it with:

```bash
pip install the0-sdk
```

Or copy the `the0/` directory from `sdk/python/` to your project.

### `parse() -> Tuple[str, Dict]`

Parse bot configuration from environment variables:

```python
from the0 import parse

bot_id, config = parse()
# bot_id: Value of BOT_ID env var
# config: Parsed JSON from BOT_CONFIG env var

symbol = config.get("symbol", "BTC/USDT")
amount = config.get("amount", 100.0)
```

### `success(message: str, data: dict = None)`

Output a success result:

```python
from the0 import success

success("Trade completed")
success("Trade completed", {"trade_id": "12345", "filled": 0.5})
```

### `error(message: str, data: dict = None)`

Output an error result and exit with code 1:

```python
from the0 import error

if amount <= 0:
    error("Amount must be positive")
    # Program exits here
```

### `result(data: dict)`

Output a custom JSON result:

```python
from the0 import result

result({
    "status": "success",
    "trade_id": "abc123",
    "filled_amount": 0.5,
    "average_price": 45123.50
})
```

### `metric(type: str, data: dict)`

Emit a metric for the platform dashboard:

```python
from the0 import metric

# Price metric
metric("price", {"symbol": "BTC/USD", "value": 45000, "change_pct": 2.5})

# Trading signal
metric("signal", {"symbol": "ETH/USD", "direction": "long", "confidence": 0.85})

# Alert
metric("alert", {"type": "price_spike", "severity": "high"})
```

### `log(message: str, data: dict = None)`

Log a message to the bot's logs:

```python
from the0 import log

log("Starting trade...")
log("Order placed", {"order_id": "12345", "symbol": "BTC/USD"})
```

### Logging

Both stdout and stderr go to your bot's logs:

```python
print("Starting trade...")           # Goes to log
import sys
print("DEBUG: Details...", file=sys.stderr)  # Goes to log
```

---

## Related Documentation

- [Configuration Reference](/custom-bot-development/configuration)
- [Bot Types](/custom-bot-development/bot-types)
- [Custom Frontends](/custom-bot-development/custom-frontends)
- [Deployment Guide](/custom-bot-development/deployment)
