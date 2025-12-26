# @alexanderwanyoike/the0-node

Official SDK for building trading bots on the0 platform with Node.js and TypeScript.

## Installation

This package is published to GitHub Packages. First, configure npm to use GitHub Packages for the `@alexanderwanyoike` scope:

```bash
# Create or edit ~/.npmrc
echo "@alexanderwanyoike:registry=https://npm.pkg.github.com" >> ~/.npmrc
```

Then install:

```bash
npm install @alexanderwanyoike/the0-node
# or
yarn add @alexanderwanyoike/the0-node
```

## Quick Start

```typescript
import { parse, success, error, metric } from '@alexanderwanyoike/the0-node';

// Parse bot configuration from environment
const { id, config } = parse<{
  symbol: string;
  amount: number;
}>();

console.log(`Bot ${id} starting...`);
console.log(`Trading ${config.symbol} with amount ${config.amount}`);

// Emit metrics for the dashboard
metric('price', {
  symbol: config.symbol,
  value: 45000.50,
  change_pct: 2.5
});

// Signal completion
success('Trade executed successfully', {
  tradeId: '12345',
  filledAmount: config.amount
});
```

## API Reference

### `parse<T>(): BotInput<T>`

Parse bot configuration from environment variables.

```typescript
const { id, config } = parse();

// With typed config
interface MyConfig {
  symbol: string;
  amount: number;
  paper?: boolean;
}
const { id, config } = parse<MyConfig>();
```

### `success(message: string, data?: object): void`

Output a success result.

```typescript
success('Trade completed');
success('Trade completed', { tradeId: '12345' });
```

### `error(message: string, data?: object): never`

Output an error result and exit with code 1.

```typescript
error('Failed to connect');
error('Trade failed', { reason: 'Insufficient funds' });
```

### `result(data: object): void`

Output a custom result object.

```typescript
result({
  status: 'success',
  signals: [{ symbol: 'BTC/USD', direction: 'long' }]
});
```

### `metric(type: string, data: object): void`

Emit a metric for the platform dashboard.

```typescript
metric('price', { symbol: 'BTC/USD', value: 45000 });
metric('signal', { direction: 'long', confidence: 0.85 });
metric('alert', { type: 'price_spike', severity: 'high' });
```

### `log(message: string, data?: object): void`

Log a message to the bot's output.

```typescript
log('Starting execution');
log('Order placed', { orderId: '12345' });
```

### `sleep(ms: number): Promise<void>`

Async sleep utility.

```typescript
await sleep(5000); // Wait 5 seconds
```

## Bot Types

### Scheduled Bots

Run on a cron schedule, execute once, and exit:

```typescript
import { parse, success, error } from '@alexanderwanyoike/the0-node';

const { id, config } = parse();

try {
  // Execute your trading logic
  const result = await executeTrade(config);
  success('Trade completed', result);
} catch (err) {
  error(`Trade failed: ${err.message}`);
}
```

### Realtime Bots

Run continuously until stopped:

```typescript
import { parse, metric, sleep } from '@alexanderwanyoike/the0-node';

const { id, config } = parse();

// Main loop
while (true) {
  const price = await fetchPrice(config.symbol);

  metric('price', {
    symbol: config.symbol,
    value: price
  });

  await sleep(config.updateInterval || 5000);
}
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
# Patch release (0.1.0 -> 0.1.1)
npm version patch

# Minor release (0.1.0 -> 0.2.0)
npm version minor

# Major release (0.1.0 -> 1.0.0)
npm version major
```

## License

Apache-2.0
