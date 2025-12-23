# Portfolio Tracker Bot

A scheduled bot example that demonstrates:
- Structured metric emission with structlog
- Custom React frontend with `@the0/react` SDK
- Portfolio value tracking and trade simulation

## What It Does

On each scheduled run, this bot:
1. Simulates portfolio value changes based on configurable volatility
2. Randomly executes mock trades
3. Emits metrics for the custom dashboard to visualize

## Metrics Emitted

| Metric | Description |
|--------|-------------|
| `portfolio_value` | Total portfolio value with change percentage |
| `position` | Individual position details (symbol, quantity, value) |
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
{"event": "portfolio_snapshot", "_metric": "portfolio_value", "value": 10250.50, "change_pct": 2.5}
{"event": "position_update", "_metric": "position", "symbol": "BTC", "quantity": 0.15, "value": 6750.00}
{"event": "trade_executed", "_metric": "trade", "symbol": "ETH", "side": "buy", "quantity": 1.5, "price": 2400.00}
```
