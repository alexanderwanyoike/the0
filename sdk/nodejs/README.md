# @the0/sdk - Node.js/TypeScript SDK

Official SDK for building trading bots on the0 platform with Node.js and TypeScript.

## Installation

```bash
npm install @the0/sdk
# or
yarn add @the0/sdk
```

## Quick Start

```typescript
import { parse, success, error, metric } from '@the0/sdk';

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
import { parse, success, error } from '@the0/sdk';

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
import { parse, metric, sleep } from '@the0/sdk';

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

## License

Apache-2.0
