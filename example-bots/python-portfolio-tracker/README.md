# Portfolio Tracker Bot

A scheduled bot example that demonstrates:
- Structured metric emission with structlog
- Custom React frontend with `@the0/react` SDK
- Portfolio value tracking and trade simulation

## What It Does

On each scheduled run, this bot:
1. Walks each price on from where it closed on the previous run
2. Marks its held quantities at the new prices
3. Randomly executes mock trades, which move value between cash and holdings
4. Emits metrics for the custom dashboard to visualize

`main.py` (the SDK version) persists holdings, prices and cash in bot state, so
the portfolio drifts over time and trades carry forward. `main_raw.py` has no
state, so it re-marks the same opening allocation each run — its value varies
with volatility but does not accumulate.

## Metrics Emitted

| Metric | Description |
|--------|-------------|
| `portfolio_value` | Total value, change since inception, change since last run, cash |
| `position` | Individual position details (symbol, quantity, price, value) |
| `trade` | Simulated trade execution details |

## Configuration

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `initial_value` | number | 10000 | Starting portfolio value in USD |
| `volatility` | number | 0.02 | Price movement factor (0.02 = 2%) |
| `symbols` | array | ["BTC", "ETH", "SOL"] | Symbols to track |

## Frontend

The custom dashboard (`frontend/`) displays:
- Current portfolio value with change indicator
- Portfolio value history (last 10 runs)
- Recent trades table

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
{"_metric": "portfolio_value", "value": 10243.31, "change_pct": 2.43, "change_since_last_pct": 0.7, "cash": 4.79}
{"_metric": "position", "symbol": "BTC", "quantity": 0.073486, "price": 47137.99, "value": 3463.92}
{"_metric": "trade", "symbol": "ETH", "side": "buy", "quantity": 0.0562, "price": 2402.1, "total": 135.0}
```
