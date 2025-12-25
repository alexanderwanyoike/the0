---
title: "Custom Frontends"
description: "Build rich visual dashboards for your trading bots"
order: 7
---

# Custom Frontends

Build custom React dashboards for your trading bots. This guide walks you through creating a frontend from scratch.

## Prerequisites

Before starting, ensure your bot emits structured metrics. See [Metrics & Logging](./metrics) for details.

## Quick Start

### Step 1: Create the Frontend Directory

Add a `frontend/` folder to your bot project:

```bash
cd my-bot
mkdir -p frontend
```

Your project should look like:

```
my-bot/
├── main.py              # Bot entry point
├── bot-config.yaml      # Bot configuration
├── requirements.txt     # Python dependencies
└── frontend/
    ├── package.json     # Frontend dependencies
    ├── tsconfig.json    # TypeScript configuration
    └── index.tsx        # Dashboard component (required)
```

### Step 2: Initialize package.json

Create `frontend/package.json`:

```json
{
  "name": "my-bot-frontend",
  "version": "1.0.0",
  "private": true,
  "main": "index.tsx",
  "scripts": {
    "build": "the0-build",
    "typecheck": "tsc --noEmit"
  },
  "devDependencies": {
    "@alexanderwanyoike/the0-react": "^0.2.0",
    "@types/react": "^19.0.0",
    "typescript": "^5.0.0"
  }
}
```

The `the0-build` command is provided by the SDK and handles all the React bundling configuration automatically.

### Step 3: Add TypeScript Config

Create `frontend/tsconfig.json`:

```json
{
  "compilerOptions": {
    "target": "ES2020",
    "lib": ["ES2020", "DOM", "DOM.Iterable"],
    "module": "ESNext",
    "moduleResolution": "bundler",
    "jsx": "react-jsx",
    "strict": true,
    "noEmit": true,
    "skipLibCheck": true,
    "esModuleInterop": true
  },
  "include": ["*.tsx", "*.ts"],
  "exclude": ["node_modules", "dist"]
}
```

Install dependencies:

```bash
cd frontend
npm install
```

### Step 5: Create Your Dashboard

Create `frontend/index.tsx`:

```tsx
import React from "react";
import { useThe0Events, BotEvent } from "@alexanderwanyoike/the0-react";

export default function Dashboard() {
  const { events, utils, loading, error } = useThe0Events();

  if (loading) {
    return (
      <div className="flex items-center justify-center h-64">
        <div className="text-green-400 font-mono">Loading metrics...</div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="p-4 bg-red-950/30 border border-red-800 rounded">
        <p className="text-red-400">Error: {error}</p>
      </div>
    );
  }

  // Get your bot's metrics
  const portfolioValues = utils.filterByType("portfolio_value");
  const trades = utils.filterByType("trade");
  const latestPortfolio = utils.latest("portfolio_value");

  return (
    <div className="p-4 space-y-6 font-mono">
      <h1 className="text-xl font-bold text-green-400">My Bot Dashboard</h1>

      {/* Latest Portfolio Value */}
      {latestPortfolio && (
        <div className="bg-gray-900 border border-green-900/50 rounded p-6">
          <div className="text-sm text-gray-500">Portfolio Value</div>
          <div className="text-3xl font-bold text-green-400">
            ${(latestPortfolio.data as any).value?.toLocaleString()}
          </div>
        </div>
      )}

      {/* Trade History */}
      <div className="bg-gray-900 border border-gray-800 rounded p-4">
        <h2 className="text-sm font-semibold text-gray-400 mb-3">
          Recent Trades ({trades.length})
        </h2>
        {trades.length === 0 ? (
          <p className="text-gray-500 text-sm">No trades yet</p>
        ) : (
          <div className="space-y-2">
            {trades.slice(-5).reverse().map((event, i) => {
              const data = event.data as any;
              return (
                <div key={i} className="flex justify-between text-sm">
                  <span className="text-gray-400">{data.symbol}</span>
                  <span className={data.side === "buy" ? "text-green-400" : "text-red-400"}>
                    {data.side.toUpperCase()} {data.quantity}
                  </span>
                </div>
              );
            })}
          </div>
        )}
      </div>
    </div>
  );
}
```

### Step 6: Deploy

```bash
the0 custom-bot deploy
```

The CLI automatically:
1. Detects your `frontend/` directory
2. Builds the bundle using Docker (runs `npm run build`)
3. Packages and deploys your bot with the frontend

No manual build step required.

---

## The Events API

The `useThe0Events` hook provides access to your bot's event stream:

```typescript
const {
  events,      // All parsed events (logs + metrics)
  loading,     // Loading state
  error,       // Error state
  utils,       // Utility functions
  refresh,     // Manual refresh function
} = useThe0Events();
```

### Event Structure

```typescript
interface BotEvent {
  timestamp: Date;           // When the event occurred
  type: 'log' | 'metric';    // Event type
  data: string | object;     // Log message or metric payload
  metricType?: string;       // For metrics: the _metric value
  level?: string;            // For logs: DEBUG, INFO, WARN, ERROR
  raw: string;               // Original log line
}
```

### Utility Functions

```typescript
// Filter by metric type
const trades = utils.filterByType('trade');
const portfolioValues = utils.filterByType('portfolio_value');

// Get the latest metric of a type
const latestPortfolio = utils.latest('portfolio_value');

// Get all metrics or all logs
const allMetrics = utils.metrics();
const allLogs = utils.logs();

// Time-based filtering
const recent = utils.since('1h');           // Last hour
const today = utils.since('24h');           // Last 24 hours
const window = utils.between(startDate, endDate);

// For scheduled bots - group events by execution run
const runs = utils.groupByRun();

// Extract time series for charting
const series = utils.extractTimeSeries('portfolio_value', 'value');
// Returns: [{ timestamp: Date, value: number }, ...]
```

---

## Working Example

See `example-bots/python-portfolio-tracker/` for a complete working example:

```
example-bots/python-portfolio-tracker/
├── main.py              # Bot with structlog metrics
├── bot-config.yaml      # Configuration
├── requirements.txt     # Python deps
└── frontend/
    ├── package.json     # Uses @alexanderwanyoike/the0-react
    └── index.tsx        # Full dashboard with multiple components
```

**Bot (main.py)** emits metrics:

```python
logger.info(
    "portfolio_snapshot",
    _metric="portfolio_value",
    value=portfolio["total_value"],
    change_pct=portfolio["change_pct"]
)

logger.info(
    "trade_executed",
    _metric="trade",
    symbol=trade["symbol"],
    side=trade["side"],
    quantity=trade["quantity"],
    price=trade["price"]
)
```

**Frontend (index.tsx)** consumes them:

```tsx
const portfolioValues = utils.filterByType("portfolio_value");
const trades = utils.filterByType("trade");
const latestPortfolio = utils.latest("portfolio_value");
```

---

## Styling

Your frontend has access to:

- **Tailwind CSS** - All utility classes
- **CSS Variables** - Theme-aware colors:

```css
:root {
  --the0-bg: #000;
  --the0-card: #111;
  --the0-border: #333;
  --the0-primary: #22c55e;
  --the0-text: #fff;
  --the0-muted: #888;
}
```

Use them for consistent theming:

```tsx
<div className="bg-gray-900 border border-green-900/50 rounded p-4">
  ...
</div>
```

---

## Adding Charts

Use any React charting library. Add to `package.json`:

```json
{
  "dependencies": {
    "recharts": "^2.0.0"
  }
}
```

Example with time series:

```tsx
import { LineChart, Line, XAxis, YAxis, Tooltip } from 'recharts';

function PortfolioChart() {
  const { utils } = useThe0Events();
  const series = utils.extractTimeSeries('portfolio_value', 'value');

  return (
    <LineChart width={600} height={300} data={series}>
      <XAxis dataKey="timestamp" />
      <YAxis />
      <Tooltip />
      <Line type="monotone" dataKey="value" stroke="#22c55e" />
    </LineChart>
  );
}
```

---

## Error Handling

Add error boundaries for resilience:

```tsx
import { ErrorBoundary } from 'react-error-boundary';

export default function Dashboard() {
  return (
    <ErrorBoundary fallback={<div>Dashboard error - check console</div>}>
      <MyDashboardContent />
    </ErrorBoundary>
  );
}
```

If your frontend fails to load, the platform falls back to the standard console view.

---

## Troubleshooting

| Issue | Solution |
|-------|----------|
| Dashboard not appearing | Ensure `frontend/package.json` exists with a `build` script |
| "React not found" error | Ensure you're using `@alexanderwanyoike/the0-react` v0.2.0+ with `the0-build` |
| Metrics not showing | Check your bot emits `_metric` field in logs |
| Build errors during deploy | Check the CLI output for npm/esbuild errors in your frontend code |
| Docker not available | The CLI uses Docker to build frontends - ensure Docker is running |

## Advanced: Custom Build Configuration

For advanced users who need custom esbuild configuration, you can import the plugin directly:

```javascript
// esbuild.config.mjs
import * as esbuild from "esbuild";
import { reactGlobalPlugin } from "@alexanderwanyoike/the0-react/esbuild";

await esbuild.build({
  entryPoints: ["index.tsx"],
  bundle: true,
  format: "esm",
  outfile: "dist/bundle.js",
  minify: true,
  plugins: [reactGlobalPlugin, /* your custom plugins */],
});
```

Or use the build function with options:

```javascript
import { build } from "@alexanderwanyoike/the0-react/build";

await build({
  entryPoint: "src/Dashboard.tsx",
  outfile: "build/bundle.js",
  minify: false,
});
```

---

## Next Steps

- [Metrics & Logging](./metrics) - Learn how to emit structured metrics
- [Bot Types](./bot-types) - Understand scheduled vs real-time execution
- [Deployment](./deployment) - Deploy your bot with custom frontend
