# Price Alerts Bot

A realtime bot example that demonstrates:
- Structured metric emission with pino logging
- Custom React frontend with `@the0/react` SDK
- Continuous price monitoring with alerts

## What It Does

This bot runs continuously and:
1. Simulates price movements based on random walk
2. Emits price snapshots at configurable intervals
3. Triggers alerts when price changes exceed threshold
4. Emits buy/sell signals based on price trends

## Metrics Emitted

| Metric | Description |
|--------|-------------|
| `price` | Current price snapshot with change info |
| `alert` | Price movement alert when threshold crossed |
| `signal` | Buy/sell signal based on trend analysis |

## Configuration

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `symbol` | string | "BTC/USD" | Symbol to monitor |
| `base_price` | number | 45000 | Starting price for simulation |
| `alert_threshold` | number | 1.0 | % change to trigger alert |
| `update_interval_ms` | number | 5000 | Update frequency in ms |

## Frontend

The custom dashboard (`frontend/`) displays:
- Current price with real-time updates
- Recent alert cards
- Signal history

### Building the Frontend

```bash
cd frontend
yarn install
yarn build
```

## Deploying

```bash
the0 custom-bot deploy
```

## Example Output

```json
{"level":30,"_metric":"price","symbol":"BTC/USD","value":45123.45,"change_pct":0.27,"msg":"price_update"}
{"level":30,"_metric":"alert","symbol":"BTC/USD","type":"price_spike","change_pct":1.5,"msg":"price_alert"}
{"level":30,"_metric":"signal","symbol":"BTC/USD","direction":"long","confidence":0.75,"msg":"signal_generated"}
```
