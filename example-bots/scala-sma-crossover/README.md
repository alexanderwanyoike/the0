# SMA Crossover Bot (Scala 3)

A realtime trading bot that implements the Simple Moving Average (SMA) crossover strategy using Scala 3 with a functional programming approach and live data from Yahoo Finance.

## Strategy Overview

The SMA crossover strategy uses two moving averages:
- **Short SMA (Fast)**: Default 5 periods
- **Long SMA (Slow)**: Default 20 periods

**Trading Signals:**
- **BUY Signal**: When short SMA crosses above long SMA (Golden Cross)
- **SELL Signal**: When short SMA crosses below long SMA (Death Cross)

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
| `update_interval_ms` | integer | 60000 | Update interval in ms |

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
  "reason": "SMA5 crossed above SMA20"
}
```

## Local Development

### Prerequisites
- Scala 3.3+
- sbt 1.9+
- Java 17+

### Build
```bash
sbt assembly
```

### Run locally
```bash
BOT_ID=test BOT_CONFIG='{"symbol":"AAPL"}' java -jar target/scala-3.3.1/sma-bot-assembly-1.0.0.jar
```

## Dependencies

- **sttp**: HTTP client for Yahoo Finance API
- **circe**: Functional JSON parsing

## Deployment

Deploy to the0 platform:
```bash
the0 custom-bot deploy
```

## Frontend Dashboard

The bot includes a custom React dashboard that displays:
- Current stock price with live updates
- Short and long SMA values with trend indicator
- Trading signals (BUY/SELL) with confidence scores
- Price and SMA history table

## License

Apache 2.0
