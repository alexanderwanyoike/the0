---
title: "Backtest Commands"
description: "Managing backtest instances with the CLI"
tags: ["cli", "backtests", "commands", "testing"]
order: 4
---

# Backtest Commands

The `backtest` commands allow you to deploy, monitor, and manage your trading strategy backtests through the CLI.

## Overview

Backtests are historical simulations of trading strategies that help you:

- Test trading strategies on historical data
- Analyze performance metrics and risk factors
- Compare different strategy parameters
- Optimize strategies before deploying with real capital

## Commands Reference

### Deploy Backtest

Deploy a new backtest instance using a configuration file:

```bash
the0 backtest deploy <config.json>
```

#### Configuration File Format

```json
{
  "name": "strategy-backtest",
  "type": "scheduled/rsi-momentum", // "{customBotType}/{customBotName}"
  "version": "1.0.0",
  "initial_capital": 10000,
  "timeframe": "1h",
  "start_date": "2023-01-01",
  "end_date": "2023-12-31",
  "symbol": "BTCUSDT",
  "rsi_period": 14,
  "rsi_overbought": 70,
  "rsi_oversold": 30
  // Other parameters specific to the backtest strategy (flat structure)
}
```

### Required Fields

- **name**: Unique identifier for your backtest instance
- **type**: Type of backtest to deploy
  - This is composed of:
    - **customBotType**: The type of custom bot (`scheduled`, `realtime`)
    - **customBotName**: The name of the custom bot strategy
- **version**: Version of the backtest strategy to deploy

### Common Backtest Parameters

> **Note**: These parameters are examples and may vary depending on your custom bot strategy. Each strategy defines its own configuration schema.

- **initial_capital**: Starting capital for the backtest
- **timeframe**: Time interval for data (e.g., "1m", "5m", "1h", "1d")
- **start_date**: Backtest start date (YYYY-MM-DD format)
- **end_date**: Backtest end date (YYYY-MM-DD format)
- **symbol**: Trading symbol (e.g., "BTCUSDT", "ETHUSDT")
- **symbols**: Array of symbols for multi-asset backtests

### Custom Backtest Configuration

Other parameters depend on the schema of the custom bot strategy you are testing. You can find out by running:

```bash
the0 custom-bot schema backtest <version> <custom-bot-name>
```

This will output the JSON schema for the backtest configuration, which you can use to fill in the required parameters.

#### Example

```bash
# Get backtest schema
the0 custom-bot schema backtest 1.0.0 rsi-momentum
📄 Fetching backtest schema for custom bot 'rsi-momentum'...
📄 Schema for custom bot 'rsi-momentum' version '1.0.0' (backtest entry point):

{
  "properties": {
    "end_date": {
      "description": "End date for backtest period",
      "type": "string"
    },
    "initial_capital": {
      "description": "Starting capital for backtest",
      "minimum": 100,
      "type": "number"
    },
    "rsi_overbought": {
      "description": "RSI overbought threshold",
      "maximum": 100,
      "minimum": 50,
      "type": "number"
    },
    "rsi_oversold": {
      "description": "RSI oversold threshold",
      "maximum": 50,
      "minimum": 0,
      "type": "number"
    },
    "rsi_period": {
      "description": "RSI calculation period",
      "minimum": 2,
      "type": "integer"
    },
    "start_date": {
      "description": "Start date for backtest period",
      "type": "string"
    },
    "symbol": {
      "description": "Trading symbol",
      "type": "string"
    },
    "timeframe": {
      "description": "Timeframe for data",
      "enum": ["1m", "5m", "15m", "1h", "4h", "1d"],
      "type": "string"
    }
  },
  "required": [
    "initial_capital",
    "timeframe",
    "start_date",
    "end_date",
    "symbol",
    "rsi_period",
    "rsi_overbought",
    "rsi_oversold"
  ],
  "type": "object"
}
```

### List Backtests

Display all your backtest instances with their status and progress:

```bash
the0 backtest list
```

#### Output Format

```bash
the0 backtest list
📊 Found 2 backtest(s):

┌──────────────┬─────────────────────┬─────────────┬────────────────────┬────────────────────┐
│      ID      │        NAME         │   STATUS    │     CREATED AT     │     UPDATED AT     │
├──────────────┼─────────────────────┼─────────────┼────────────────────┼────────────────────┤
│ bt_123456789 │ rsi-momentum-test   │ ✅ Completed│ 2025-01-15 10:30   │ 2025-01-15 12:45   │
│ bt_987654321 │ portfolio-backtest  │ 🔄 Running  │ 2025-01-15 14:20   │ 2025-01-15 15:30   │
└──────────────┴─────────────────────┴─────────────┴────────────────────┴────────────────────┘

📈 Backtest analysis complete. Ready to optimize your strategy!
```

#### Status Indicators

- **⏳ Pending**: Backtest is queued and waiting to start
- **🔄 Running**: Backtest is currently executing
- **✅ Completed**: Backtest finished successfully with results
- **❌ Failed**: Backtest encountered an error during execution

### Delete Backtest

Delete a backtest instance:

```bash
the0 backtest delete <backtest_id>
```

#### Safety Features

- Requires confirmation before deletion
- Cannot be undone

#### Example

```bash
the0 backtest delete 123456789
⚠️ Are you sure you want to delete backtest '123456789'?
This action cannot be undone
Type 'yes' to confirm: yes

🗑️ Starting backtest deletion process...
✅ Backtest '123456789' deleted successfully 🗑️
```

## Backtest Examples

### Simple RSI Strategy Backtest

```json
{
  "name": "rsi-strategy-test",
  "type": "scheduled/rsi-momentum",
  "version": "1.0.0",
  "initial_capital": 10000,
  "timeframe": "1h",
  "start_date": "2023-01-01",
  "end_date": "2023-12-31",
  "symbol": "BTCUSDT",
  "rsi_period": 14,
  "rsi_overbought": 70,
  "rsi_oversold": 30
}
```

### Moving Average Crossover Backtest

```json
{
  "name": "ma-crossover-test",
  "type": "scheduled/moving-average-crossover",
  "version": "1.0.0",
  "initial_capital": 25000,
  "timeframe": "4h",
  "start_date": "2023-01-01",
  "end_date": "2023-12-31",
  "symbol": "ETHUSDT",
  "fast_ma_period": 20,
  "slow_ma_period": 50,
  "position_size": 0.1
}
```

### Multi-Asset Portfolio Backtest

```json
{
  "name": "portfolio-rebalance-test",
  "type": "scheduled/portfolio-rebalancing",
  "version": "2.0.0",
  "initial_capital": 50000,
  "timeframe": "1d",
  "start_date": "2022-01-01",
  "end_date": "2023-12-31",
  "symbols": ["BTCUSDT", "ETHUSDT", "ADAUSDT"],
  "allocation_btcusdt": 0.5,
  "allocation_ethusdt": 0.3,
  "allocation_adausdt": 0.2,
  "rebalance_frequency": "monthly"
}
```

### Risk Management Backtest

```json
{
  "name": "risk-management-test",
  "type": "scheduled/risk-parity",
  "version": "1.3.0",
  "initial_capital": 25000,
  "timeframe": "1h",
  "start_date": "2023-06-01",
  "end_date": "2023-12-31",
  "symbols": ["BTCUSDT", "ETHUSDT"],
  "max_position_size": 5000,
  "stop_loss": 0.02,
  "take_profit": 0.06,
  "risk_per_trade": 0.01
}
```

## Best Practices

1. **Use meaningful backtest names** that describe the strategy and parameters
2. **Test with appropriate timeframes** - longer periods provide more reliable results
3. **Start with small capital** to validate the strategy before scaling up
4. **Monitor running backtests** to track progress and catch issues early
5. **Document your parameters** to easily reproduce successful backtests
6. **Clean up old backtests** to keep your workspace organized

## Related Commands

- [Bot Commands](/docs/the0-cli/bot-commands) - Manage live bot instances
- [Custom Bot Commands](/docs/the0-cli/custom-bot-commands) - Deploy your own bot templates and get schemas
- [Authentication](/docs/the0-cli/authentication) - Set up API access
