# Example Bots with Custom Frontends

This directory contains example bots demonstrating how to build custom trading bots with React dashboards using the `@the0/react` SDK.

## Examples

| Example | Type | Runtime | Description |
|---------|------|---------|-------------|
| [python-portfolio-tracker](./python-portfolio-tracker/) | Scheduled | Python 3.11 | Simulates portfolio tracking with value history and trades |
| [typescript-price-alerts](./typescript-price-alerts/) | Realtime | Node.js 20 | Monitors prices and emits alerts/signals |

## Key Concepts

### Structured Metric Emission

Bots emit metrics via structured JSON logging with a `_metric` field:

**Python (structlog):**
```python
logger.info("portfolio_snapshot",
    _metric="portfolio_value",
    value=10500.00,
    change_pct=2.5
)
```

**TypeScript (pino):**
```typescript
logger.info({
    _metric: "price",
    symbol: "BTC/USD",
    value: 45000.00
}, "price_update");
```

### Custom Frontends

Each bot can include a `frontend/` directory with a React dashboard:

```
my-bot/
├── bot-config.yaml       # Must have hasFrontend: true
├── main.py
└── frontend/
    ├── package.json
    ├── index.tsx         # Default export: React component
    └── dist/
        └── bundle.js     # Built ESM bundle
```

The frontend uses the `@the0/react` SDK to access bot events:

```tsx
import { useThe0Events } from '@the0/react';

export default function Dashboard() {
  const { events, utils, loading } = useThe0Events();

  const trades = utils.filterByType('trade');
  const latestValue = utils.latest('portfolio_value');

  return (/* your dashboard */);
}
```

## SDK Utilities

The `useThe0Events()` hook provides:

| Method | Description |
|--------|-------------|
| `utils.filterByType(type)` | Get all events of a metric type |
| `utils.latest(type)` | Get the most recent event of a type |
| `utils.since(duration)` | Get events from last `'1h'`, `'24h'`, `'7d'`, etc. |
| `utils.between(start, end)` | Get events between two dates |
| `utils.metrics()` | Get only metric events (not logs) |
| `utils.logs()` | Get only log events |
| `utils.groupByRun()` | Group events by execution run (for scheduled bots) |
| `utils.extractTimeSeries(type, key)` | Extract `{ timestamp, value }[]` for charting |

## Building Frontends

Each frontend has a build script using esbuild:

```bash
cd example-bots/python-portfolio-tracker/frontend
yarn install
yarn build
```

The bundle must:
- Be ESM format
- Export a default React component
- Externalize `react` and `react-dom`

## Deploying

```bash
cd example-bots/python-portfolio-tracker
the0 custom-bot deploy
```

The CLI automatically:
1. Detects `frontend/dist/bundle.js`
2. Packages everything into a ZIP
3. Uploads to the platform

## SDK Installation

The examples use `@the0/react` from GitHub Packages:

```json
{
  "dependencies": {
    "@the0/react": "^0.1.0"
  }
}
```

Configure `.npmrc` for GitHub Packages:
```
@the0:registry=https://npm.pkg.github.com
```
