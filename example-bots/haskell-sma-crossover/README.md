# SMA Crossover Bot (Haskell)

A scheduled trading bot that implements the Simple Moving Average (SMA) crossover strategy using Haskell with pure functional programming and live data from Yahoo Finance.

## Strategy Overview

The SMA crossover strategy uses two moving averages:
- **Short SMA (Fast)**: Default 5 periods
- **Long SMA (Slow)**: Default 20 periods

**Trading Signals:**
- **BUY Signal**: When short SMA is above long SMA (bullish trend)
- **SELL Signal**: When short SMA is below long SMA (bearish trend)

This is a **scheduled bot** - it runs once per trigger, analyzes the current market state, and returns a recommendation.

## Data Source

This bot fetches real market data from Yahoo Finance REST API:
- Endpoint: `https://query1.finance.yahoo.com/v8/finance/chart/{symbol}`
- Interval: Daily prices
- Range: 1 month of historical data

## Configuration

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `symbol` | string | "AAPL" | Stock symbol to monitor |
| `short_period` | integer | 5 | Periods for short SMA |
| `long_period` | integer | 20 | Periods for long SMA |

## Metrics Emitted

### `price`
```json
{
  "_metric": "price",
  "symbol": "AAPL",
  "value": 150.25,
  "change_pct": 0.5,
  "timestamp": "1703635200.000Z"
}
```

### `sma`
```json
{
  "_metric": "sma",
  "symbol": "AAPL",
  "short_sma": 148.50,
  "long_sma": 145.20,
  "short_period": 5,
  "long_period": 20
}
```

### `signal`
```json
{
  "_metric": "signal",
  "type": "BUY",
  "symbol": "AAPL",
  "price": 150.25,
  "confidence": 0.75,
  "reason": "SMA5 is above SMA20 (bullish trend)"
}
```

## Local Development

### Prerequisites
- GHC 9.6+
- Cabal 3.0+

### Build
```bash
cabal build
```

### Run locally
```bash
BOT_ID=test BOT_CONFIG='{"symbol":"AAPL"}' cabal run sma-bot
```

## Dependencies

- **http-conduit**: HTTP client for Yahoo Finance API
- **aeson**: JSON parsing
- **text**: Text processing
- **time**: Timestamp handling

## Deployment

Deploy to the0 platform:
```bash
the0 custom-bot deploy
```

## Frontend Dashboard

The bot includes a custom React dashboard that displays:
- Latest stock price analysis
- Short and long SMA values with trend indicator
- Current trading signal recommendation
- History of scheduled runs with signals

## License

Apache 2.0
