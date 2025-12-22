---
title: "Custom Frontends"
description: "Build rich visual dashboards for your trading bots"
order: 7
---

# Custom Frontends

the0 allows you to build custom React dashboards for your trading bots. Instead of viewing raw logs, you can create rich visualizations - charts, tables, status cards, and more - that bring your trading data to life.

## How It Works

1. Your bot emits [structured metrics](./metrics) alongside regular logs
2. You include a `frontend/` directory in your bot package with React components
3. The CLI bundles your frontend during deployment
4. the0 serves your custom dashboard when viewing the bot

If no custom frontend is provided, bots display the standard console view with parsed metrics.

## Project Structure

Add a `frontend/` directory to your bot package:

```
my-bot/
├── main.py                 # Bot entry point
├── bot-config.yaml         # Bot configuration
├── requirements.txt        # Python dependencies
└── frontend/
    ├── package.json        # Frontend dependencies
    ├── index.tsx           # Dashboard entry point (required)
    └── components/         # Your custom components
        ├── PortfolioChart.tsx
        ├── TradeTable.tsx
        └── MetricCard.tsx
```

## Entry Point Contract

Your `frontend/index.tsx` must export a default React component:

```tsx
// frontend/index.tsx
import React from 'react';
import { useThe0Events } from '@the0/sdk';
import { PortfolioChart } from './components/PortfolioChart';
import { TradeTable } from './components/TradeTable';

export default function Dashboard() {
  const { events, utils, loading } = useThe0Events();

  if (loading) {
    return <div>Loading...</div>;
  }

  // Filter events by type
  const portfolioHistory = utils.filterByType('portfolio_value');
  const trades = utils.filterByType('trade');
  const signals = utils.filterByType('signal');

  return (
    <div className="p-4 space-y-6">
      <h1 className="text-2xl font-bold">My Trading Bot</h1>

      {/* Portfolio Value Chart */}
      <PortfolioChart data={portfolioHistory} />

      {/* Recent Trades Table */}
      <TradeTable trades={trades.slice(-20)} />

      {/* Signal Summary */}
      <div className="grid grid-cols-3 gap-4">
        {signals.slice(-3).map((signal, i) => (
          <SignalCard key={i} signal={signal} />
        ))}
      </div>
    </div>
  );
}
```

## The Events API

The `useThe0Events` hook provides access to your bot's event stream:

```typescript
const {
  events,      // All parsed events (logs + metrics)
  loading,     // Loading state
  error,       // Error state
  utils,       // Utility functions (see below)
  refresh,     // Manual refresh function
} = useThe0Events();
```

### Event Structure

Each event has the following shape:

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

The `utils` object provides powerful filtering and aggregation:

```typescript
// Time-based filtering
const recent = utils.since('1h');           // Events from last hour
const today = utils.since('24h');           // Events from last 24 hours
const window = utils.between(startDate, endDate);

// Type filtering
const trades = utils.filterByType('trade');
const signals = utils.filterByType('signal');
const allMetrics = utils.metrics();
const allLogs = utils.logs();

// Aggregation
const latestPortfolio = utils.latest('portfolio_value');
const byType = utils.groupByMetricType();

// For scheduled bots - group events by execution run
const runs = utils.groupByRun();

// Extract time series for charting
const portfolioSeries = utils.extractTimeSeries('portfolio_value', 'value');
// Returns: [{ timestamp: Date, value: number }, ...]
```

## Building Components

### Portfolio Chart Example

```tsx
// components/PortfolioChart.tsx
import React from 'react';
import { BotEvent } from '@the0/sdk';

interface Props {
  data: BotEvent[];
}

export function PortfolioChart({ data }: Props) {
  // Transform events to chart data
  const chartData = data.map(event => ({
    time: event.timestamp,
    value: (event.data as any).value
  }));

  return (
    <div className="bg-gray-900 p-4 rounded-lg">
      <h2 className="text-lg font-semibold mb-4">Portfolio Value</h2>
      {/* Use your preferred charting library */}
      <LineChart data={chartData} />
    </div>
  );
}
```

### Trade Table Example

```tsx
// components/TradeTable.tsx
import React from 'react';
import { BotEvent } from '@the0/sdk';

interface Props {
  trades: BotEvent[];
}

export function TradeTable({ trades }: Props) {
  return (
    <div className="bg-gray-900 p-4 rounded-lg">
      <h2 className="text-lg font-semibold mb-4">Recent Trades</h2>
      <table className="w-full">
        <thead>
          <tr className="text-left text-gray-400">
            <th>Time</th>
            <th>Symbol</th>
            <th>Side</th>
            <th>Quantity</th>
            <th>Price</th>
          </tr>
        </thead>
        <tbody>
          {trades.map((trade, i) => {
            const data = trade.data as any;
            return (
              <tr key={i} className="border-t border-gray-800">
                <td>{trade.timestamp.toLocaleTimeString()}</td>
                <td>{data.symbol}</td>
                <td className={data.side === 'buy' ? 'text-green-400' : 'text-red-400'}>
                  {data.side.toUpperCase()}
                </td>
                <td>{data.quantity}</td>
                <td>${data.price.toLocaleString()}</td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}
```

### Metric Card Example

```tsx
// components/MetricCard.tsx
import React from 'react';

interface Props {
  title: string;
  value: string | number;
  change?: number;
  icon?: React.ReactNode;
}

export function MetricCard({ title, value, change, icon }: Props) {
  return (
    <div className="bg-gray-900 p-4 rounded-lg">
      <div className="flex items-center justify-between">
        <span className="text-gray-400 text-sm">{title}</span>
        {icon}
      </div>
      <div className="mt-2">
        <span className="text-2xl font-bold">{value}</span>
        {change !== undefined && (
          <span className={`ml-2 text-sm ${change >= 0 ? 'text-green-400' : 'text-red-400'}`}>
            {change >= 0 ? '+' : ''}{change.toFixed(2)}%
          </span>
        )}
      </div>
    </div>
  );
}
```

## Frontend Package Configuration

Your `frontend/package.json` should include:

```json
{
  "name": "my-bot-frontend",
  "version": "1.0.0",
  "private": true,
  "main": "index.tsx",
  "dependencies": {
    "react": "^19.0.0",
    "react-dom": "^19.0.0"
  },
  "devDependencies": {
    "@types/react": "^19.0.0",
    "typescript": "^5.0.0"
  }
}
```

> **Note**: React and ReactDOM are provided by the0's runtime and will be externalized during bundling. You don't need to bundle them.

## Supported Bundle Locations

The CLI looks for your frontend bundle in these locations (in order):

1. `frontend/dist/bundle.js`
2. `frontend/dist/index.js`
3. `frontend/bundle.js`
4. `frontend.js`
5. `dist/frontend.js`

For most setups, build to `frontend/dist/bundle.js`.

## Building Your Frontend

### With esbuild (Recommended)

```bash
# Install esbuild
npm install -D esbuild

# Build command
npx esbuild frontend/index.tsx \
  --bundle \
  --format=esm \
  --outfile=frontend/dist/bundle.js \
  --external:react \
  --external:react-dom \
  --minify
```

### With Vite

```javascript
// vite.config.js
export default {
  build: {
    lib: {
      entry: 'frontend/index.tsx',
      formats: ['es'],
      fileName: 'bundle'
    },
    outDir: 'frontend/dist',
    rollupOptions: {
      external: ['react', 'react-dom']
    }
  }
}
```

### Build Script in package.json

```json
{
  "scripts": {
    "build:frontend": "esbuild frontend/index.tsx --bundle --format=esm --outfile=frontend/dist/bundle.js --external:react --external:react-dom --minify"
  }
}
```

## Deployment

When you deploy your bot with `the0 deploy`:

1. The CLI detects the `frontend/` directory
2. If a built bundle exists, it's extracted and stored separately
3. The bot's config is updated with `hasFrontend: true`
4. Your dashboard is served at `/api/custom-bots/{bot-name}/frontend`

```bash
# Build frontend first
cd my-bot
npm run build:frontend

# Then deploy
the0 deploy
```

## Styling

Your frontend runs inside the0's dashboard context. You have access to:

- **Tailwind CSS** - All utility classes available
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

Use these for consistent theming:

```tsx
<div style={{
  backgroundColor: 'var(--the0-card)',
  borderColor: 'var(--the0-border)'
}}>
  ...
</div>
```

## Charting Libraries

You can use any charting library that works with React. Popular choices:

- **Lightweight Charts** - TradingView-style financial charts
- **Recharts** - Simple, composable charts
- **Plotly** - Interactive scientific charts
- **Victory** - Modular charting components

Just add them to your `frontend/package.json` and import as needed.

## Error Handling

If your frontend fails to load, the platform falls back to the standard console view. Common issues:

- **Missing default export** - Ensure `index.tsx` exports a default component
- **Build errors** - Check your bundle builds correctly before deploying
- **Runtime errors** - Use error boundaries in your React code

```tsx
// Add error boundary for resilience
import { ErrorBoundary } from 'react-error-boundary';

export default function Dashboard() {
  return (
    <ErrorBoundary fallback={<div>Dashboard error - check console</div>}>
      <MyDashboardContent />
    </ErrorBoundary>
  );
}
```

## Example: Complete Bot with Frontend

See our example repositories for complete bot + frontend implementations:

- **Momentum Bot** - Real-time bot with portfolio tracking dashboard
- **DCA Bot** - Scheduled bot with trade history visualization
- **Signal Bot** - Event-driven bot with signal monitoring

## Next Steps

- [Metrics & Logging](./metrics) - Learn how to emit structured metrics
- [Bot Types](./bot-types) - Understand scheduled vs real-time execution
- [Deployment](./deployment) - Deploy your bot with custom frontend
