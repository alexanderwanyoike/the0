# @alexanderwanyoike/the0-react

React SDK for building custom bot dashboards on the0 platform.

## Installation

```bash
npm install @alexanderwanyoike/the0-react
# or
yarn add @alexanderwanyoike/the0-react
```

> Migrating from GitHub Packages? This package now lives on the public npm
> registry. Remove the `@alexanderwanyoike:registry` line from your `.npmrc`
> (project or `~/.npmrc`) to pick up new versions.

## Usage

```tsx
import { useThe0Events } from '@alexanderwanyoike/the0-react';

export default function Dashboard() {
  const { events, utils, loading, error } = useThe0Events();

  // Safe as-is: the platform only sets loading/error while there is nothing
  // to render, so background refreshes never unmount your dashboard
  if (loading) return <div>Loading...</div>;
  if (error) return <div>Error: {error}</div>;

  // Filter events by metric type
  const trades = utils.filterByType('trade');
  const portfolioHistory = utils.filterByType('portfolio_value');

  // Get latest metric value
  const latestPortfolio = utils.latest('portfolio_value');

  // Time-based filtering
  const recentEvents = utils.since('1h');

  // Extract time series for charting
  const chartData = utils.extractTimeSeries('portfolio_value', 'value');

  return (
    <div>
      <h1>Portfolio: ${latestPortfolio?.data.value}</h1>
      <Chart data={chartData} />
      <TradeList trades={trades} />
    </div>
  );
}
```

## API

### `useThe0Events()`

Returns the events context with:

- `events: BotEvent[]` - All parsed events (logs + metrics)
- `loading: boolean` - Loading state
- `error: string | null` - Error message if any
- `refresh: () => void` - Manual refresh function
- `utils: BotEventUtils` - Utility functions (see below)

### Event Utilities

```typescript
interface BotEventUtils {
  // Time filtering
  since(duration: string): BotEvent[];      // "1h", "24h", "7d"
  between(start: Date, end: Date): BotEvent[];

  // Type filtering
  filterByType(metricType: string): BotEvent[];
  metrics(): BotEvent[];
  logs(): BotEvent[];

  // Aggregation
  latest(metricType: string): BotEvent | null;
  groupByMetricType(): Record<string, BotEvent[]>;
  groupByRun(gapThresholdMs?: number): BotEvent[][];

  // Charting
  extractTimeSeries(metricType: string, valueKey?: string):
    { timestamp: Date; value: number }[];
}
```

### Event Types

```typescript
interface BotEvent {
  timestamp: Date;
  type: 'log' | 'metric';
  data: string | Record<string, unknown>;
  metricType?: string;  // For metrics: the _metric value
  level?: 'DEBUG' | 'INFO' | 'WARN' | 'ERROR';
  raw: string;
}
```

## Bundling

When building your custom frontend, externalize React:

```bash
esbuild frontend/index.tsx \
  --bundle \
  --format=esm \
  --outfile=frontend/dist/bundle.js \
  --external:react \
  --external:react-dom \
  --minify
```

## Publishing (Maintainers)

This package is published to the public npm registry.

### Prerequisites

Authenticate with npm as a user with publish rights to the
`@alexanderwanyoike` scope:

```bash
npm login
```

### Publish

```bash
# Build the package
yarn build

# Publish to npm
npm publish
```

### Version Bump

```bash
# Patch release (0.2.0 -> 0.2.1)
npm version patch

# Minor release (0.2.0 -> 0.3.0)
npm version minor

# Major release (0.2.0 -> 1.0.0)
npm version major
```

## License

Apache-2.0
