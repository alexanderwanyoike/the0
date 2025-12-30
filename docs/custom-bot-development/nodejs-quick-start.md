---
title: "Node.js/TypeScript Quick Start"
description: "Build your first Node.js or TypeScript trading bot with the0"
tags: ["custom-bots", "nodejs", "typescript", "javascript", "quick-start"]
order: 11
---

# Node.js/TypeScript Quick Start

Node.js excels at I/O-bound operations, making it ideal for trading bots that spend most of their time waiting on API responses. TypeScript adds type safety that catches configuration errors before deployment. This guide walks through building a realtime price alert bot that monitors prices and emits alerts when thresholds are crossed.

By the end of this guide, you'll have a working realtime bot that continuously monitors prices and emits metrics to the dashboard.

## Prerequisites

Before starting, ensure you have the CLI installed and authenticated:

```bash
# Clone the repository and build the CLI
git clone https://github.com/alexanderwanyoike/the0.git
cd the0/cli
make install

# Authenticate
the0 auth login
```

You'll also need Node.js 20 or higher installed locally for testing.

## Project Structure

Create a new directory for your bot:

```bash
mkdir price-alerts
cd price-alerts
```

A TypeScript bot requires these essential files:

```
price-alerts/
├── bot-config.yaml      # Bot metadata and runtime settings
├── main.ts              # Entry point with trading logic
├── bot-schema.json      # Configuration schema for users
├── package.json         # Dependencies and build scripts
└── tsconfig.json        # TypeScript configuration
```

## Defining Bot Metadata

Start with `bot-config.yaml`:

```yaml
name: price-alerts
description: "Monitors prices and emits alerts when thresholds are crossed"
version: 1.0.0
author: "your-name"
type: realtime
runtime: nodejs20

entrypoints:
  bot: main.js

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  categories: [alerts, monitoring]
  tags: [realtime, beginner]
  complexity: beginner
```

The `type: realtime` setting means this bot runs continuously from start until stopped. Unlike scheduled bots that execute once and exit, realtime bots maintain state across iterations and emit metrics continuously. The entry point is `main.js` because the platform runs the compiled JavaScript, not the TypeScript source.

## Defining Configuration Schema

Create `bot-schema.json` to specify what parameters users can configure:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "Price Alerts Configuration",
  "description": "Configuration for the price alerts bot",
  "properties": {
    "symbol": {
      "type": "string",
      "description": "Symbol to monitor",
      "default": "BTC/USD"
    },
    "base_price": {
      "type": "number",
      "description": "Base price for simulation",
      "default": 45000
    },
    "alert_threshold": {
      "type": "number",
      "description": "Price change percentage to trigger alert",
      "default": 1.0,
      "minimum": 0.1,
      "maximum": 10
    },
    "update_interval_ms": {
      "type": "number",
      "description": "Milliseconds between price updates",
      "default": 5000,
      "minimum": 1000,
      "maximum": 60000
    }
  },
  "additionalProperties": false
}
```

## Setting Up TypeScript

Create `package.json`:

```json
{
  "name": "price-alerts",
  "version": "1.0.0",
  "type": "module",
  "main": "main.js",
  "scripts": {
    "build": "tsc",
    "start": "node main.js"
  },
  "dependencies": {
    "@alexanderwanyoike/the0-node": "^0.1.0",
    "pino": "^9.0.0"
  },
  "devDependencies": {
    "typescript": "^5.3.0",
    "@types/node": "^20.0.0"
  }
}
```

Create `tsconfig.json`:

```json
{
  "compilerOptions": {
    "target": "ES2022",
    "module": "ESNext",
    "moduleResolution": "node",
    "esModuleInterop": true,
    "strict": true,
    "outDir": ".",
    "rootDir": ".",
    "declaration": false
  },
  "include": ["main.ts"]
}
```

Install the SDK from GitHub Packages:

```bash
# Configure npm for the @alexanderwanyoike scope
echo "@alexanderwanyoike:registry=https://npm.pkg.github.com" >> ~/.npmrc

# Install dependencies
npm install
```

## Writing the Bot Logic

Create `main.ts` with your bot implementation. The key pattern for TypeScript bots is exporting the `main` function—the runtime invokes it directly rather than your code calling it:

```typescript
import { parse, metric, sleep, success } from "@alexanderwanyoike/the0-node";
import pino from "pino";

const logger = pino({ level: "info" });

interface BotConfig {
  symbol: string;
  base_price: number;
  alert_threshold: number;
  update_interval_ms: number;
}

interface BotState {
  lastPrice: number;
  priceHistory: number[];
  lastAlertTime: number;
}

async function main(): Promise<void> {
  // Parse configuration from environment
  const { id, config } = parse<BotConfig>();

  const symbol = config.symbol || "BTC/USD";
  const basePrice = config.base_price || 45000;
  const alertThreshold = config.alert_threshold || 1.0;
  const updateInterval = config.update_interval_ms || 5000;

  logger.info({ botId: id, symbol, alertThreshold }, "Bot started");

  // Initialize state that persists across iterations
  const state: BotState = {
    lastPrice: basePrice,
    priceHistory: [basePrice],
    lastAlertTime: 0,
  };

  // Main loop - runs until process is terminated
  while (true) {
    const priceData = simulatePrice(state, symbol, basePrice);

    // Emit price metric
    metric("price", {
      symbol: priceData.symbol,
      value: priceData.value,
      change_pct: priceData.change_pct,
      high_24h: priceData.high_24h,
      low_24h: priceData.low_24h,
    });

    // Check for alert conditions
    const alert = checkAlertConditions(state, priceData, alertThreshold, symbol);
    if (alert) {
      metric("alert", {
        symbol: alert.symbol,
        type: alert.type,
        change_pct: alert.change_pct,
        message: alert.message,
        severity: alert.severity,
      });
    }

    // Update state
    state.lastPrice = priceData.value;
    state.priceHistory.push(priceData.value);
    if (state.priceHistory.length > 100) {
      state.priceHistory.shift();
    }

    await sleep(updateInterval);
  }
}

function simulatePrice(state: BotState, symbol: string, basePrice: number) {
  const volatility = 0.002;
  const meanReversion = 0.001;

  const randomChange = (Math.random() - 0.5) * 2 * volatility;
  const reversion = ((basePrice - state.lastPrice) / basePrice) * meanReversion;
  const changePercent = randomChange + reversion;

  const newPrice = state.lastPrice * (1 + changePercent);
  const changePct = ((newPrice - state.lastPrice) / state.lastPrice) * 100;

  return {
    symbol,
    value: Math.round(newPrice * 100) / 100,
    change_pct: Math.round(changePct * 1000) / 1000,
    high_24h: Math.max(...state.priceHistory, newPrice),
    low_24h: Math.min(...state.priceHistory, newPrice),
  };
}

function checkAlertConditions(
  state: BotState,
  priceData: { value: number; change_pct: number },
  threshold: number,
  symbol: string
) {
  const now = Date.now();

  // Cooldown: don't alert more than once per 30 seconds
  if (now - state.lastAlertTime < 30000) {
    return null;
  }

  const absChange = Math.abs(priceData.change_pct);

  if (absChange >= threshold) {
    state.lastAlertTime = now;

    const direction = priceData.change_pct > 0 ? "spike" : "drop";
    const severity = absChange >= threshold * 2 ? "high" : "medium";

    return {
      symbol,
      type: `price_${direction}`,
      change_pct: priceData.change_pct,
      message: `${symbol} ${direction} of ${absChange.toFixed(2)}%`,
      severity,
    };
  }

  return null;
}

// Export main for the runtime to invoke
export { main };
```

Notice that the file ends with `export { main };` rather than calling `main()`. The platform runtime imports your module and invokes the exported `main` function directly.

## SDK Functions

The Node.js SDK provides these core functions:

### parse()

Reads `BOT_ID` and `BOT_CONFIG` from environment variables. The generic type parameter provides TypeScript type safety for your configuration:

```typescript
import { parse } from "@alexanderwanyoike/the0-node";

interface MyConfig {
  symbol: string;
  threshold: number;
}

const { id, config } = parse<MyConfig>();
// config.symbol and config.threshold are now type-checked
```

### metric(type, data)

Emits a metric to the platform dashboard:

```typescript
import { metric } from "@alexanderwanyoike/the0-node";

metric("price", { symbol: "BTC/USD", value: 45000, change_pct: 2.5 });
metric("alert", { type: "price_spike", severity: "high", message: "BTC up 5%" });
metric("signal", { direction: "long", confidence: 0.85 });
```

### sleep(ms)

Async utility for waiting between iterations:

```typescript
import { sleep } from "@alexanderwanyoike/the0-node";

await sleep(5000); // Wait 5 seconds
```

### success(message, data)

Reports successful execution for scheduled bots:

```typescript
import { success } from "@alexanderwanyoike/the0-node";

success("Analysis complete", { processed: 100 });
```

### error(message)

Reports failure and terminates with exit code 1:

```typescript
import { error } from "@alexanderwanyoike/the0-node";

if (!config.api_key) {
  error("API key is required");
}
```

## Building and Testing

Compile TypeScript to JavaScript:

```bash
npm run build
```

Test locally by setting environment variables:

```bash
export BOT_ID="test-bot"
export BOT_CONFIG='{"symbol":"BTC/USD","base_price":45000,"alert_threshold":1.0,"update_interval_ms":2000}'
export CODE_MOUNT_DIR="/tmp"

npm start
```

The bot should start emitting price and alert metrics. Press Ctrl+C to stop.

## Deploying

Deploy your bot to the platform:

```bash
the0 custom-bot deploy
```

The CLI validates configuration, runs `npm install` to vendor dependencies, packages everything, and uploads to the platform.

## Creating Bot Instances

Once deployed, create instances that run your bot. For realtime bots, no schedule is needed—they run continuously:

```json
{
  "name": "btc-price-alerts",
  "type": "realtime/price-alerts",
  "version": "1.0.0",
  "config": {
    "symbol": "BTC/USD",
    "base_price": 45000,
    "alert_threshold": 2.0,
    "update_interval_ms": 10000
  }
}
```

Deploy the instance:

```bash
the0 bot deploy instance-config.json
```

The bot starts immediately and runs until you stop it.

## Monitoring

Monitor running instances:

```bash
# List running instances
the0 bot list

# View logs
the0 bot logs <bot_id>

# Stream logs in real-time
the0 bot logs <bot_id> -w

# Stop a realtime bot
the0 bot delete <bot_id>
```

## Alternative: Structured Logging for Metrics

Instead of using the SDK's `metric()` function, you can emit metrics through pino's structured logging. Include a `_metric` field:

```typescript
import pino from "pino";

const logger = pino();

// Emit metrics via logging
logger.info({
  _metric: "price",
  symbol: "BTC/USD",
  value: 45000,
  change_pct: 2.5,
}, "price_update");

logger.info({
  _metric: "alert",
  type: "price_spike",
  severity: "high",
  message: "BTC up 5%",
}, "alert_triggered");
```

The platform automatically detects and parses JSON log entries with `_metric` fields. See [Metrics](./metrics) for details on both approaches.

## JavaScript Alternative

If you prefer plain JavaScript over TypeScript, create `main.js` directly:

```javascript
import { parse, metric, sleep } from "@alexanderwanyoike/the0-node";

async function main() {
  const { id, config } = parse();

  const symbol = config.symbol || "BTC/USD";
  let lastPrice = config.base_price || 45000;

  while (true) {
    const change = (Math.random() - 0.5) * 0.02;
    const price = lastPrice * (1 + change);

    metric("price", {
      symbol,
      value: Math.round(price * 100) / 100,
      change_pct: change * 100,
    });

    lastPrice = price;
    await sleep(config.update_interval_ms || 5000);
  }
}

export { main };
```

Update `bot-config.yaml` to point to `main.js` and remove TypeScript dependencies from `package.json`.

## Next Steps

With your first realtime bot deployed, explore these topics:

- [Configuration](./configuration) - Complete bot-config.yaml reference
- [Bot Types](./bot-types) - Scheduled vs realtime execution models
- [Metrics](./metrics) - Dashboard metrics and structured logging
- [Custom Frontends](./custom-frontends) - Build React dashboards for your bot
- [Testing](./testing) - Local testing patterns and best practices
