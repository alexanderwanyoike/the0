---
title: "Node.js/TypeScript Quick Start"
description: "Build your first Node.js or TypeScript trading bot with the0"
tags: ["custom-bots", "nodejs", "typescript", "javascript", "quick-start"]
order: 10
---

# Node.js/TypeScript Quick Start Guide

Build trading bots using JavaScript or TypeScript with the0's Node.js 20 runtime. This guide covers both scheduled bots (run on a schedule) and realtime bots (run continuously).

---

## Why Node.js for Trading Bots?

Node.js is an excellent choice for trading bots because of:

- **Async I/O**: Non-blocking operations are perfect for API calls to exchanges
- **Rich Ecosystem**: npm has packages for every exchange and data source
- **TypeScript Support**: Add type safety to catch errors before deployment
- **Fast Development**: Rapid prototyping with immediate feedback
- **JSON Native**: JavaScript objects map directly to JSON APIs

**Popular Trading Libraries:**
- `ccxt` - Unified API for 100+ cryptocurrency exchanges
- `alpaca-trade-api` - Official Alpaca SDK for stocks/crypto
- `technicalindicators` - Technical analysis (RSI, MACD, Bollinger)
- `axios` - HTTP client for REST APIs
- `ws` - WebSocket client for real-time data

---

## Prerequisites

- [the0 CLI installed](/the0-cli/installation)
- [API key configured](/the0-cli/authentication)
- Node.js 18+ (for local development)
- Basic understanding of JavaScript/TypeScript

---

## Project Structure

```
my-nodejs-bot/
├── package.json         # Dependencies and scripts
├── main.ts              # Your bot entry point (or main.js)
├── bot-config.yaml      # Bot configuration
├── bot-schema.json      # Parameter schema
├── config.json          # Example configuration
└── README.md            # Documentation
```

---

## Step 1: Create Your Project

```bash
mkdir my-nodejs-bot
cd my-nodejs-bot
npm init -y
```

---

## Step 2: Install the SDK

The the0 SDK provides utilities for parsing configuration and emitting results:

```bash
npm install @the0/sdk
```

Or copy it directly from `sdk/nodejs` in the repository.

---

## Step 3: Write Your Bot

### Option A: TypeScript (Recommended)

Create `main.ts`:

```typescript
import { parse, success, error, metric, log } from '@the0/sdk';

/**
 * Configuration interface for type safety
 */
interface BotConfig {
  symbol: string;
  amount: number;
  api_key: string;
  paper?: boolean;
}

/**
 * Main bot entry point
 *
 * This function is called when the bot runs. For scheduled bots,
 * it runs once per schedule. For realtime bots, it runs continuously.
 */
async function main(): Promise<void> {
  // Parse configuration from environment
  // The platform sets BOT_ID and BOT_CONFIG automatically
  const { id, config } = parse<BotConfig>();

  log(`Bot ${id} starting...`);
  log('Configuration loaded', { symbol: config.symbol, amount: config.amount });

  try {
    // ===========================================
    // YOUR TRADING LOGIC GOES HERE
    // ===========================================

    // Example: Fetch current price
    const price = await fetchPrice(config.symbol);
    log(`Current price of ${config.symbol}: $${price}`);

    // Emit a price metric for the dashboard
    metric('price', {
      symbol: config.symbol,
      value: price,
      timestamp: new Date().toISOString(),
    });

    // Example: Execute a trade (implement your logic)
    if (config.amount > 0) {
      log('Executing trade...', { symbol: config.symbol, amount: config.amount });
      // const order = await executeTrade(config);
    }

    // ===========================================
    // END OF TRADING LOGIC
    // ===========================================

    // Signal successful completion
    success('Bot executed successfully', {
      symbol: config.symbol,
      price,
    });
  } catch (err) {
    // Signal error (this also exits with code 1)
    error(`Bot failed: ${err instanceof Error ? err.message : String(err)}`);
  }
}

/**
 * Example: Fetch price from an API
 * Replace this with your actual data source
 */
async function fetchPrice(symbol: string): Promise<number> {
  // Simulated price for demo
  // In production, use ccxt, alpaca-trade-api, or direct API calls
  const basePrice = symbol.includes('BTC') ? 45000 : 150;
  return basePrice * (1 + (Math.random() - 0.5) * 0.02);
}

// Run the bot
main();
```

### Option B: JavaScript

Create `main.js`:

```javascript
const { parse, success, error, metric, log } = require('@the0/sdk');

async function main() {
  const { id, config } = parse();

  log(`Bot ${id} starting...`);

  try {
    // Your trading logic here
    const price = Math.random() * 1000 + 40000; // Simulated BTC price

    metric('price', {
      symbol: config.symbol,
      value: price,
    });

    success('Bot executed successfully', { price });
  } catch (err) {
    error(`Bot failed: ${err.message}`);
  }
}

main();
```

---

## Step 4: Create Bot Configuration

Create `bot-config.yaml`:

```yaml
name: my-nodejs-bot
description: "A Node.js/TypeScript trading bot"
version: "1.0.0"
author: "Your Name"
type: scheduled           # or "realtime" for continuous execution
runtime: nodejs20

entrypoints:
  bot: main.ts            # or main.js for JavaScript

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  categories: [trading]
  instruments: [crypto, stocks]
  tags: [nodejs, typescript]
```

---

## Step 5: Define Parameter Schema

Create `bot-schema.json` to define what configuration your bot accepts:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Bot Configuration",
  "description": "Configuration for the Node.js trading bot",
  "properties": {
    "symbol": {
      "type": "string",
      "title": "Trading Symbol",
      "description": "The trading pair (e.g., BTC/USD, AAPL)",
      "default": "BTC/USD"
    },
    "amount": {
      "type": "number",
      "title": "Trade Amount",
      "description": "Amount in USD to trade per execution",
      "default": 100,
      "minimum": 1
    },
    "api_key": {
      "type": "string",
      "title": "API Key",
      "description": "Your exchange API key"
    },
    "paper": {
      "type": "boolean",
      "title": "Paper Trading",
      "description": "Use paper trading mode (no real money)",
      "default": true
    }
  },
  "required": ["symbol", "api_key"]
}
```

---

## Step 6: Add Dependencies

Update `package.json`:

```json
{
  "name": "my-nodejs-bot",
  "version": "1.0.0",
  "main": "main.ts",
  "scripts": {
    "start": "npx ts-node main.ts",
    "test": "node --test"
  },
  "dependencies": {
    "@the0/sdk": "^1.0.0"
  },
  "devDependencies": {
    "typescript": "^5.3.0",
    "ts-node": "^10.9.0",
    "@types/node": "^20.0.0"
  }
}
```

For JavaScript-only projects, you can skip the TypeScript dependencies.

---

## Step 7: Test Locally

```bash
# Set environment variables for local testing
export BOT_ID="test-bot-123"
export BOT_CONFIG='{"symbol":"BTC/USD","amount":100,"api_key":"test-key","paper":true}'

# Run with TypeScript
npx ts-node main.ts

# Or with JavaScript
node main.js
```

---

## Step 8: Deploy

```bash
the0 custom-bot deploy
```

The CLI will:
1. Validate your configuration
2. Install npm dependencies
3. Package your bot files
4. Upload to the platform

---

## SDK API Reference

The `@the0/sdk` package provides these functions:

### `parse<T>(): { id: string, config: T }`

Parse bot configuration from environment variables.

```typescript
// Basic usage
const { id, config } = parse();

// With TypeScript generics for type safety
interface MyConfig {
  symbol: string;
  amount: number;
}
const { id, config } = parse<MyConfig>();
console.log(config.symbol); // Type-safe!
```

### `success(message: string, data?: object)`

Output a success result.

```typescript
success('Trade completed');
success('Trade completed', { tradeId: '12345', filled: 0.5 });
```

### `error(message: string, data?: object): never`

Output an error result and exit with code 1.

```typescript
error('Failed to connect to exchange');
error('Insufficient funds', { available: 50, required: 100 });
```

### `metric(type: string, data: object)`

Emit a metric for the platform dashboard. Metrics appear in real-time on your bot's dashboard.

```typescript
// Price metric
metric('price', { symbol: 'BTC/USD', value: 45000, change_pct: 2.5 });

// Trading signal
metric('signal', { symbol: 'ETH/USD', direction: 'long', confidence: 0.85 });

// Alert
metric('alert', { type: 'price_spike', severity: 'high', message: '+5% in 1 min' });
```

### `log(message: string, data?: object)`

Log a message (appears in bot logs).

```typescript
log('Starting trade execution');
log('Order placed', { orderId: '12345', symbol: 'BTC/USD' });
```

### `sleep(ms: number): Promise<void>`

Async sleep utility.

```typescript
await sleep(5000); // Wait 5 seconds
```

---

## Example: Price Alert Bot with CCXT

Here's a more complete example using the `ccxt` library:

```typescript
import { parse, success, error, metric, log, sleep } from '@the0/sdk';
import ccxt from 'ccxt';

interface Config {
  exchange: string;
  symbol: string;
  alert_threshold: number;
  check_interval_ms: number;
}

async function main(): Promise<void> {
  const { id, config } = parse<Config>();

  log(`Price Alert Bot ${id} starting...`);

  // Initialize exchange (read-only, no API keys needed for public data)
  const exchangeClass = ccxt[config.exchange as keyof typeof ccxt];
  if (!exchangeClass) {
    error(`Unknown exchange: ${config.exchange}`);
  }

  const exchange = new exchangeClass();
  let lastPrice: number | null = null;

  // Main loop for realtime bot
  while (true) {
    try {
      // Fetch current ticker
      const ticker = await exchange.fetchTicker(config.symbol);
      const currentPrice = ticker.last!;

      // Emit price metric
      metric('price', {
        symbol: config.symbol,
        value: currentPrice,
        bid: ticker.bid,
        ask: ticker.ask,
        volume: ticker.baseVolume,
      });

      // Check for price alert
      if (lastPrice !== null) {
        const changePct = ((currentPrice - lastPrice) / lastPrice) * 100;

        if (Math.abs(changePct) >= config.alert_threshold) {
          metric('alert', {
            symbol: config.symbol,
            type: changePct > 0 ? 'price_spike' : 'price_drop',
            change_pct: changePct.toFixed(2),
            previous_price: lastPrice,
            current_price: currentPrice,
            severity: Math.abs(changePct) >= config.alert_threshold * 2 ? 'high' : 'medium',
          });
        }
      }

      lastPrice = currentPrice;
      await sleep(config.check_interval_ms);
    } catch (err) {
      log(`Error fetching price: ${err}`);
      await sleep(config.check_interval_ms);
    }
  }
}

main();
```

Add `ccxt` to your dependencies:

```bash
npm install ccxt
```

---

## Realtime vs Scheduled Bots

### Scheduled Bots

Run once per schedule (cron), execute logic, and exit:

```yaml
type: scheduled
```

```typescript
async function main() {
  const { config } = parse();
  // Execute once and exit
  await executeTrade(config);
  success('Trade completed');
}
```

### Realtime Bots

Run continuously until stopped:

```yaml
type: realtime
```

```typescript
async function main() {
  const { config } = parse();
  // Run forever
  while (true) {
    await checkPrices(config);
    metric('price', { ... });
    await sleep(5000);
  }
}
```

---

## Best Practices

### 1. Type Safety with TypeScript

Define interfaces for your configuration:

```typescript
interface TradingConfig {
  symbol: string;
  amount: number;
  stopLoss?: number;
  takeProfit?: number;
}

const { config } = parse<TradingConfig>();
// Now config.symbol, config.amount, etc. are type-checked
```

### 2. Error Handling

Always wrap your logic in try-catch:

```typescript
try {
  const result = await riskyOperation();
  success('Completed', result);
} catch (err) {
  error(`Failed: ${err instanceof Error ? err.message : 'Unknown error'}`);
}
```

### 3. Logging for Debugging

Use structured logging for easier debugging:

```typescript
log('Order submitted', {
  orderId: order.id,
  symbol: order.symbol,
  side: order.side,
  quantity: order.quantity,
  timestamp: new Date().toISOString(),
});
```

### 4. Respect Rate Limits

Add delays between API calls:

```typescript
for (const symbol of symbols) {
  await fetchAndProcess(symbol);
  await sleep(100); // 100ms between calls
}
```

---

## Related Documentation

- [Configuration Reference](/custom-bot-development/configuration)
- [Bot Types](/custom-bot-development/bot-types)
- [Custom Frontends](/custom-bot-development/custom-frontends)
- [Metrics](/custom-bot-development/metrics)
- [Deployment Guide](/custom-bot-development/deployment)
