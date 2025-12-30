---
title: "Metrics"
description: "Emit structured metrics from your trading bots"
order: 7
---

# Metrics

Metrics are structured data points that your bot emits during execution. They appear in the dashboard in real-time, allowing you to monitor prices, positions, signals, and any other data your strategy produces. The platform supports two approaches for emitting metrics: the SDK's `metric()` function and structured logging with a `_metric` field.

## Approach 1: SDK metric() Function

The simplest approach uses the SDK's built-in `metric()` function. This handles formatting and ensures metrics are properly captured by the platform.

**Python:**

```python
from the0 import parse, success, metric

def main():
    bot_id, config = parse()

    # Emit a price metric
    metric("price", {
        "symbol": "AAPL",
        "value": 150.25,
        "change_pct": 1.2
    })

    # Emit a signal metric
    metric("signal", {
        "type": "BUY",
        "symbol": "AAPL",
        "confidence": 0.85
    })

    success("Complete")
```

**TypeScript:**

```typescript
import { parse, metric, success } from "@alexanderwanyoike/the0-node";

async function main(): Promise<void> {
    const { id, config } = parse();

    metric("price", {
        symbol: "BTC/USD",
        value: 45000,
        change_pct: 2.5
    });

    metric("alert", {
        symbol: "BTC/USD",
        type: "price_spike",
        message: "Price increased 2.5%"
    });

    success("Complete");
}

export { main };
```

**Rust:**

```rust
use the0_sdk::input;
use serde_json::json;

fn main() {
    let (bot_id, config) = input::parse().expect("Failed to parse config");

    input::metric("sma", &json!({
        "symbol": "AAPL",
        "short_sma": 152.30,
        "long_sma": 148.75
    }));

    input::metric("signal", &json!({
        "type": "BUY",
        "symbol": "AAPL",
        "confidence": 0.82
    }));
}
```

**C++:**

```cpp
#include <the0.h>

int main() {
    auto [botId, config] = the0::parse();

    the0::metric("price", {
        {"symbol", "AAPL"},
        {"value", 150.25},
        {"change_pct", 1.2}
    });

    return 0;
}
```

## Approach 2: Structured Logging with _metric Field

If you prefer integrating metrics into your existing logging infrastructure, you can use structured loggers like `structlog` (Python) or `pino` (Node.js). Include a `_metric` field to identify the log entry as a metric—the platform automatically detects and parses these.

**Python with structlog:**

```python
import structlog

structlog.configure(
    processors=[
        structlog.processors.JSONRenderer()
    ]
)
logger = structlog.get_logger()

# Regular logging
logger.info("Starting trading cycle")
logger.debug("Fetching market data", symbol="BTC/USD")

# Emit metrics by including _metric field
logger.info("portfolio_snapshot",
    _metric="portfolio_value",
    value=10500.00,
    change_pct=2.5,
    currency="USD"
)

logger.info("trade_executed",
    _metric="trade",
    symbol="BTC/USD",
    side="buy",
    quantity=0.1,
    price=45000.00
)

logger.info("signal_generated",
    _metric="signal",
    symbol="ETH/USD",
    direction="long",
    confidence=0.85,
    reason="RSI oversold"
)
```

**Python with python-json-logger:**

```python
import logging
from pythonjsonlogger import jsonlogger

handler = logging.StreamHandler()
handler.setFormatter(jsonlogger.JsonFormatter())
logger = logging.getLogger()
logger.addHandler(handler)
logger.setLevel(logging.INFO)

# Regular logging
logger.info("Starting trading cycle")

# Emit metrics via extra dict
logger.info("portfolio_snapshot", extra={
    "_metric": "portfolio_value",
    "value": 10500.00,
    "change_pct": 2.5
})

logger.info("trade_executed", extra={
    "_metric": "trade",
    "symbol": "BTC/USD",
    "side": "buy",
    "quantity": 0.1,
    "price": 45000.00
})
```

**Node.js with pino:**

```javascript
const pino = require('pino');
const logger = pino();

// Regular logging
logger.info("Starting trading cycle");
logger.info({ symbol: "BTC/USD" }, "Fetching market data");

// Emit metrics with _metric field
logger.info({
    _metric: "portfolio_value",
    value: 10500.00,
    change_pct: 2.5,
    currency: "USD"
}, "portfolio_snapshot");

logger.info({
    _metric: "trade",
    symbol: "BTC/USD",
    side: "buy",
    quantity: 0.1,
    price: 45000.00
}, "trade_executed");
```

The `_metric` field is required when using this approach—it identifies the JSON object as a metric and specifies its type.

## Which Approach to Use?

**Use the SDK `metric()` function when:**
- You want the simplest integration
- Your bot is primarily focused on trading logic
- You're starting a new bot from scratch

**Use structured logging with `_metric` when:**
- You have existing logging infrastructure you want to leverage
- You want unified logging and metrics in one output stream
- You prefer the flexibility of structured logging libraries

Both approaches produce metrics that the platform handles identically. Choose whichever fits your workflow.

## Common Metric Types

You can emit any metric type your strategy needs. The type string is freeform—use whatever makes sense for your dashboard.

### Price Metrics

```python
metric("price", {
    "symbol": symbol,
    "value": current_price,
    "change_pct": change_percentage,
    "high_24h": high_price,
    "low_24h": low_price
})
```

### Position Metrics

```python
metric("position", {
    "symbol": position["symbol"],
    "quantity": position["quantity"],
    "value": position["value"],
    "price": position["price"]
})

metric("portfolio_value", {
    "value": total_value,
    "change_pct": change_percentage
})
```

### Signal Metrics

```python
metric("signal", {
    "type": "BUY",
    "symbol": "AAPL",
    "price": 150.25,
    "confidence": 0.85,
    "reason": "RSI oversold"
})
```

### Technical Indicator Metrics

```python
metric("sma", {
    "symbol": "AAPL",
    "short_sma": 152.30,
    "long_sma": 148.75,
    "short_period": 5,
    "long_period": 20
})
```

### Alert Metrics

```python
metric("alert", {
    "symbol": "BTC/USD",
    "type": "price_drop",
    "change_pct": -5.2,
    "message": "BTC dropped 5%",
    "severity": "high"
})
```

### Trade Metrics

```python
metric("trade", {
    "symbol": "ETH",
    "side": "buy",
    "quantity": 2.5,
    "price": 2400.00,
    "total": 6000.00
})
```

## Metric Timing

For realtime bots, emit metrics on each iteration of your main loop:

```python
while True:
    price = fetch_price(symbol)

    metric("price", {
        "symbol": symbol,
        "value": price,
        "timestamp": time.time()
    })

    signal = analyze(price)
    if signal:
        metric("signal", {
            "type": signal,
            "symbol": symbol,
            "price": price
        })

    sleep(update_interval)
```

For scheduled bots, emit metrics at the end of each execution:

```python
def main():
    bot_id, config = parse()

    portfolio = calculate_portfolio()

    metric("portfolio_value", {
        "value": portfolio["total"],
        "positions": len(portfolio["holdings"])
    })

    for holding in portfolio["holdings"]:
        metric("position", holding)

    success("Analysis complete")
```

## Best Practices

**Use consistent metric types.** Define a standard set of types for your bot and use them consistently. This makes it easier to build dashboards that consume your metrics.

**Include the symbol.** Most metrics relate to a specific trading symbol. Always include it so dashboards can filter and group by symbol.

**Use numeric values for quantities.** Values you want to chart should be numbers, not formatted strings. Use `150.25` rather than `"$150.25"`.

**Emit at meaningful moments.** Don't emit metrics on every tick if nothing changed. Emit when prices update, signals trigger, or positions change.

**Include context.** Add fields that help with analysis—timestamps, confidence scores, reasons for signals. More context makes metrics more useful.

## Viewing Metrics

Metrics appear in the bot dashboard in real-time. Each metric type can be visualized differently depending on your custom frontend configuration. If you haven't built a custom frontend, metrics display in a default format showing the type and data.

For realtime bots, metrics stream continuously while the bot runs. For scheduled bots, metrics appear after each execution completes.

## Related

- [Custom Frontends](./custom-frontends) - Build dashboards that visualize your metrics
- [Bot Types](./bot-types) - Understand scheduled vs realtime execution
- [Development Overview](./overview) - SDK functions reference
