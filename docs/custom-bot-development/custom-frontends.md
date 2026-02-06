---
title: "Custom Frontends"
description: "Build rich visual dashboards for your trading bots"
order: 8
---

# Custom Frontends

Every bot emits metrics to the platform, but raw metric streams only tell part of the story. Custom frontends transform those metrics into rich visual dashboards tailored to your strategy—portfolio charts, trade histories, signal visualizations, whatever your bot needs. The platform renders your React components directly in the bot's dashboard view, giving users an experience designed specifically for your trading logic.

## How Custom Frontends Work

When you include a `frontend/` directory in your bot project, the CLI automatically bundles it during deployment. The platform then renders your React component instead of the default metrics view. Your component receives the bot's event stream through a React hook, which includes all metrics emitted by your bot whether through the SDK `metric()` function or structured logging with `_metric` fields.

The React SDK handles the complexity of connecting to the event stream, parsing metrics, and providing utility functions for common operations like filtering by type or extracting time series data. You focus on building the visualization.

## Project Setup

A bot with a custom frontend extends the standard project structure with a frontend directory:

```
my-bot/
├── main.py              # Bot entry point
├── bot-config.yaml      # Bot configuration
├── bot-schema.json      # Configuration schema
├── requirements.txt     # Python dependencies
└── frontend/
    ├── package.json     # Frontend dependencies
    ├── tsconfig.json    # TypeScript configuration
    └── index.tsx        # Dashboard component (required)
```

The `index.tsx` file must export a default React component. This component becomes the dashboard that users see when viewing your bot.

### Package Configuration

Create `frontend/package.json` with the React SDK as a dev dependency:

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

The `the0-build` script handles all bundling configuration automatically. You don't need to configure esbuild or any other build tools unless you have advanced requirements.

### TypeScript Configuration

Create `frontend/tsconfig.json` for type checking:

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

After creating these files, install dependencies:

```bash
cd frontend
npm install
```

## Building the Dashboard

The React SDK provides the `useThe0Events` hook that connects your component to the bot's event stream. This hook returns the events array, utility functions for filtering and analysis, and loading/error states.

Here's a complete dashboard example:

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

  // Filter events by metric type
  const portfolioValues = utils.filterByType("portfolio_value");
  const trades = utils.filterByType("trade");
  const latestPortfolio = utils.latest("portfolio_value");

  return (
    <div className="p-4 space-y-6 font-mono">
      <h1 className="text-xl font-bold text-green-400">Portfolio Dashboard</h1>

      {latestPortfolio && (
        <div className="bg-gray-900 border border-green-900/50 rounded p-6">
          <div className="text-sm text-gray-500">Portfolio Value</div>
          <div className="text-3xl font-bold text-green-400">
            ${(latestPortfolio.data as any).value?.toLocaleString()}
          </div>
        </div>
      )}

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

The dashboard renders loading and error states first, then displays the actual metrics. The `utils` object provides methods for filtering events by type, getting the latest metric of a given type, and extracting data for visualization.

## The Events API

The `useThe0Events` hook provides everything you need to work with your bot's event stream:

```typescript
const {
  events,      // All parsed events (logs + metrics)
  loading,     // Loading state
  error,       // Error state
  utils,       // Utility functions
  refresh,     // Manual refresh function
} = useThe0Events();
```

Each event in the stream follows a consistent structure:

```typescript
interface BotEvent {
  timestamp: Date;           // When the event occurred
  type: 'log' | 'metric';    // Event type
  data: string | object;     // Log message or metric payload
  metricType?: string;       // For metrics: the type identifier
  level?: string;            // For logs: DEBUG, INFO, WARN, ERROR
  raw: string;               // Original log line
}
```

The `metricType` field captures the type you specified when emitting the metric, whether through `metric("price", {...})` or `logger.info(..., _metric="price")`.

### Utility Functions

The `utils` object provides common operations for working with events:

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

The `extractTimeSeries` function is particularly useful for building charts. It pulls the specified field from each metric of the given type and pairs it with the timestamp.

## Connecting to Bot Metrics

Your frontend consumes whatever metrics your bot emits. The platform automatically parses metrics regardless of whether they were emitted using the SDK function or structured logging.

If your bot uses the SDK approach:

```python
from the0 import metric

metric("portfolio_value", {
    "value": 10500.00,
    "change_pct": 2.5
})
```

Or the structured logging approach:

```python
import structlog
logger = structlog.get_logger()

logger.info("portfolio_snapshot",
    _metric="portfolio_value",
    value=10500.00,
    change_pct=2.5
)
```

Both produce metrics your frontend can consume identically:

```tsx
const portfolioValues = utils.filterByType("portfolio_value");
const latest = utils.latest("portfolio_value");
// latest.data contains { value: 10500.00, change_pct: 2.5 }
```

## Styling

Your frontend has access to Tailwind CSS for styling. All utility classes work as expected. The platform also provides CSS variables for theme-aware colors:

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

Use Tailwind classes for consistent styling that matches the platform:

```tsx
<div className="bg-gray-900 border border-green-900/50 rounded p-4">
  <h2 className="text-green-400 font-mono">Portfolio Value</h2>
</div>
```

## Adding Charts

Any React charting library works with custom frontends. Add the library to your package.json dependencies and use the `extractTimeSeries` utility to prepare data:

```json
{
  "dependencies": {
    "recharts": "^2.0.0"
  }
}
```

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

## Error Handling

React error boundaries prevent dashboard failures from breaking the entire view. If your frontend encounters an error, the platform falls back to the standard console view:

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

## Deployment

Deploying a bot with a custom frontend requires no extra steps. The CLI automatically detects the `frontend/` directory:

```bash
the0 custom-bot deploy
```

The CLI runs `npm install` and `npm run build` inside a Docker container to ensure consistent builds, then packages the bundled output with your bot code. Users creating instances of your bot will see your custom dashboard instead of the default metrics view.

## Advanced Build Configuration

For advanced users who need custom esbuild configuration, you can import the plugin directly and create your own build script:

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
  plugins: [reactGlobalPlugin],
});
```

Or use the programmatic build function with options:

```javascript
import { build } from "@alexanderwanyoike/the0-react/build";

await build({
  entryPoint: "src/Dashboard.tsx",
  outfile: "build/bundle.js",
  minify: false,
});
```

## Troubleshooting

**Dashboard not appearing**: Ensure `frontend/package.json` exists with a `build` script that runs `the0-build`.

**React not found error**: Use `@alexanderwanyoike/the0-react` version 0.2.0 or higher, which includes React as a peer dependency.

**Metrics not showing**: Verify your bot emits metrics using either the SDK `metric()` function or structured logging with the `_metric` field.

**Build errors during deploy**: Check the CLI output for npm or esbuild errors. Run `npm run build` locally in the frontend directory to debug.

**Docker not available**: The CLI uses Docker to build frontends for environment consistency. Ensure Docker is running before deploying.

## Related

- [Metrics](./metrics) - Emit metrics that your frontend consumes
- [Bot Types](./bot-types) - Understand scheduled vs realtime execution
- [Deployment](./deployment) - Deploy your bot with custom frontend
