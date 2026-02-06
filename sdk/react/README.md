# @alexanderwanyoike/the0-react

React SDK for building custom bot dashboards on the0 platform.

## Installation

This package is published to GitHub Packages. First, configure npm to use GitHub Packages for the `@alexanderwanyoike` scope:

```bash
# Create or edit ~/.npmrc
echo "@alexanderwanyoike:registry=https://npm.pkg.github.com" >> ~/.npmrc
```

Then install:

```bash
npm install @alexanderwanyoike/the0-react
# or
yarn add @alexanderwanyoike/the0-react
```

## Usage

```tsx
import { useThe0Events } from '@alexanderwanyoike/the0-react';

export default function Dashboard() {
  const { events, utils, loading, error } = useThe0Events();

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

This package is published to GitHub Packages.

### Prerequisites

1. Create a GitHub Personal Access Token with `write:packages` scope:
   https://github.com/settings/tokens/new?scopes=write:packages,read:packages

2. Authenticate with GitHub Packages:
   ```bash
   npm login --registry=https://npm.pkg.github.com
   # Username: your-github-username
   # Password: your-personal-access-token
   ```

### Publish

```bash
# Build the package
yarn build

# Publish to GitHub Packages
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
