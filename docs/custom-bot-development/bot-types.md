---
title: "Bot Types"
description: "Understanding different bot execution models"
tags: ["custom-bots", "bot-types", "execution-models"]
order: 9
---

# Bot Types

Every custom bot on the0 runs in one of two execution models: scheduled or realtime. This choice fundamentally shapes how you write your code. Scheduled bots execute once and exit, triggered by cron expressions. Realtime bots run continuously with an event loop until explicitly stopped. The execution model is orthogonal to your trading strategy—you can implement price alerts, SMA crossovers, or portfolio rebalancing in either model depending on how frequently you need to react to market conditions.

## Scheduled Bots

A scheduled bot executes discretely. The platform launches your code, it runs to completion, reports a result, and exits. The scheduler then waits for the next trigger time before launching it again. Each execution is independent with no shared memory between runs.

This model suits strategies that operate on fixed intervals. Dollar-cost averaging naturally fits a weekly schedule. Portfolio rebalancing might run daily at market open. End-of-day analysis processes the day's data after market close. These strategies don't need sub-second reaction times—they need reliable, periodic execution.

### Configuration

Set the type field in your bot-config.yaml:

```yaml
type: scheduled
```

When users create bot instances from your scheduled custom bot, they specify a cron expression that determines when the bot runs.

### Implementation Pattern

A scheduled bot follows a simple pattern: parse configuration, execute your logic, emit metrics, and report completion. The python-portfolio-tracker example demonstrates this:

```python
from the0 import parse, success, error, metric
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

def main(bot_id: str = None, config: dict = None):
    if bot_id is None or config is None:
        bot_id, config = parse()

    # Extract configuration with defaults
    initial_value = config.get("initial_value", 10000)
    symbols = config.get("symbols", ["BTC", "ETH", "SOL"])

    logger.info(f"Bot {bot_id} started with symbols: {symbols}")

    # Execute your strategy
    portfolio = calculate_portfolio_value(symbols, initial_value)

    # Emit metrics for the dashboard
    metric("portfolio_value", {
        "value": portfolio["total_value"],
        "change_pct": portfolio["change_pct"],
    })

    for position in portfolio["positions"]:
        metric("position", {
            "symbol": position["symbol"],
            "quantity": position["quantity"],
            "value": position["value"],
        })

    # Report completion
    success(f"Portfolio tracked: ${portfolio['total_value']:.2f}", {
        "portfolio_value": portfolio["total_value"],
        "positions_count": len(portfolio["positions"]),
    })

if __name__ == "__main__":
    main()
```

Notice that the bot does its work and exits. There's no loop, no persistent state. If you need to remember something between executions, store it externally—in your exchange account state, a database, or the bot's configuration.

### Cron Scheduling

Bot instances specify their schedule using standard cron expressions. The expression defines minute, hour, day of month, month, and day of week:

```json
{
  "name": "weekly-dca",
  "type": "scheduled/portfolio-tracker",
  "version": "1.0.0",
  "schedule": "0 9 * * 1"
}
```

Common patterns include:

| Expression | When it runs |
|------------|--------------|
| `*/5 * * * *` | Every 5 minutes |
| `0 * * * *` | Every hour on the hour |
| `0 9 * * *` | Daily at 9 AM |
| `0 9 * * 1-5` | Weekdays at 9 AM |
| `0 0 1 * *` | First day of each month at midnight |

## Realtime Bots

A realtime bot runs continuously from the moment it starts until someone stops it. Your code typically contains an event loop that fetches data, processes it, emits metrics, and sleeps before the next iteration. State persists across loop iterations, making it easy to track moving averages, maintain position history, or detect pattern changes.

This model suits strategies that need to react quickly to market conditions. Price alert bots monitor quotes and trigger notifications when thresholds are crossed. Grid trading bots manage multiple orders across price levels. Arbitrage detection requires constant comparison of prices across venues. These strategies benefit from continuous execution with minimal latency between observations.

### Configuration

Set the type field in your bot-config.yaml:

```yaml
type: realtime
```

Realtime bots don't use cron schedules—they run continuously until stopped.

### Implementation Pattern

A realtime bot contains a main loop that runs indefinitely. The typescript-price-alerts example shows the pattern:

```typescript
import { parse, metric, sleep } from "@alexanderwanyoike/the0-node";
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
    const { id, config } = parse<BotConfig>();

    const symbol = config.symbol || "BTC/USD";
    const alertThreshold = config.alert_threshold || 1.0;
    const updateInterval = config.update_interval_ms || 5000;

    logger.info({ botId: id, symbol, alertThreshold }, "Bot started");

    // Initialize state that persists across iterations
    const state: BotState = {
        lastPrice: config.base_price || 45000,
        priceHistory: [],
        lastAlertTime: 0,
    };

    // Main loop runs until process is terminated
    while (true) {
        const price = await fetchPrice(symbol);

        metric("price", {
            symbol,
            value: price,
            change_pct: calculateChange(state.lastPrice, price),
        });

        // Check alert conditions
        if (shouldAlert(state, price, alertThreshold)) {
            metric("alert", {
                symbol,
                type: "price_movement",
                message: `${symbol} moved ${alertThreshold}%`,
            });
        }

        // Update state for next iteration
        state.lastPrice = price;
        state.priceHistory.push(price);

        await sleep(updateInterval);
    }
}

// Export main for the runtime to invoke
export { main };
```

The key difference from scheduled bots is the `while (true)` loop and the state object that accumulates data across iterations. The `sleep()` function controls how frequently the loop iterates, which directly affects API rate limit consumption and metric emission frequency.

### Rust Example

Compiled languages follow the same pattern. The rust-sma-crossover example:

```rust
use the0_sdk::input;
use serde_json::json;
use std::thread;
use std::time::Duration;

struct BotState {
    prev_short_sma: Option<f64>,
    prev_long_sma: Option<f64>,
}

fn main() {
    let (bot_id, config) = input::parse()
        .expect("Failed to parse configuration");

    let symbol = config["symbol"].as_str().unwrap_or("AAPL");
    let short_period = config["short_period"].as_i64().unwrap_or(5) as usize;
    let long_period = config["long_period"].as_i64().unwrap_or(20) as usize;
    let interval_ms = config["update_interval_ms"].as_i64().unwrap_or(60000) as u64;

    let mut state = BotState {
        prev_short_sma: None,
        prev_long_sma: None,
    };

    loop {
        let prices = fetch_prices(symbol);
        let short_sma = calculate_sma(&prices, short_period);
        let long_sma = calculate_sma(&prices, long_period);

        input::metric("sma", &json!({
            "symbol": symbol,
            "short_sma": short_sma,
            "long_sma": long_sma,
        }));

        if let Some(signal) = detect_crossover(&state, short_sma, long_sma) {
            input::metric("signal", &json!({
                "type": signal,
                "symbol": symbol,
            }));
        }

        state.prev_short_sma = Some(short_sma);
        state.prev_long_sma = Some(long_sma);

        thread::sleep(Duration::from_millis(interval_ms));
    }
}
```

## Choosing Between Types

The decision comes down to how your strategy relates to time. If your strategy naturally operates on calendar intervals—daily rebalancing, weekly DCA purchases, monthly reporting—scheduled execution is the right fit. You write simpler code without loops, and the platform handles timing.

If your strategy needs to observe continuous market behavior—tracking price momentum, detecting sudden movements, maintaining live positions—realtime execution gives you that continuous presence. You control the observation frequency through your sleep interval, and state naturally persists in memory.

Some strategies could work either way. An SMA crossover strategy could run as a scheduled bot checking hourly, or as a realtime bot checking every minute. The choice depends on how quickly you need to detect crossovers and how important that timing is to your strategy's performance.

## Related

- [Configuration](./configuration) - Complete bot-config.yaml reference
- [Metrics](./metrics) - Emit data to the dashboard
- [Testing](./testing) - Test both bot types locally
