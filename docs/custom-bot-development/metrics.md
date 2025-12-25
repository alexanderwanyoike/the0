---
title: "Bot Metrics & Logging"
description: "Emit structured metrics from your trading bots for visualization and analysis"
order: 6
---

# Bot Metrics & Logging

the0 provides a powerful logging system that goes beyond simple text logs. By emitting **structured metrics**, your bot's data can be visualized in custom dashboards, analyzed over time, and used to build rich trading interfaces.

## The Concept

Logs in the0 are treated as **temporal event streams**. Your bot emits two types of output:

1. **Logs** - Text messages for debugging, monitoring, and structured metrics (goes to log storage)
2. **Result** - The final status/data returned by your bot (parsed by runtime)

The platform uses the `THE0_RESULT:` marker to distinguish between logs and results. This means **you can freely use print statements for debugging** in any language - they won't interfere with your bot's result output.

### How It Works

All SDK output functions (like `success()`, `error()`, `result()`) automatically prefix their output with `THE0_RESULT:`. The runtime:

1. Scans for the `THE0_RESULT:` marker in stdout
2. Uses the **last occurrence** as the bot's result (allows overwriting)
3. Everything else becomes log output

**For backtests:** Results are parsed and returned to the UI. Logs are stored for debugging.

**For long-running bots:** There's no "result" - all output is just logs. The marker is harmless if used.

## Emitting Metrics

### Python with structlog (Recommended)

Use `structlog` for clean, Datadog-style structured logging:

```python
import structlog

# Configure structlog for JSON output
structlog.configure(
    processors=[
        structlog.processors.JSONRenderer()
    ]
)
logger = structlog.get_logger()

# Regular logging - outputs structured JSON
logger.info("Starting trading cycle")
logger.debug("Fetching market data", symbol="BTC/USD")
logger.warning("High volatility detected", volatility=0.85)
logger.error("Failed to execute order", error="timeout")

# Emit metrics - just include _metric in the structured data
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
    price=45000.00,
    order_id="ord_123456"
)

logger.info("signal_generated",
    _metric="signal",
    symbol="ETH/USD",
    direction="long",
    confidence=0.85,
    rsi=32,
    macd="bullish_cross"
)
```

### Python with python-json-logger

If you prefer standard logging with JSON output:

```python
import logging
from pythonjsonlogger import jsonlogger

# Setup JSON logging
handler = logging.StreamHandler()
handler.setFormatter(jsonlogger.JsonFormatter())
logger = logging.getLogger()
logger.addHandler(handler)
logger.setLevel(logging.INFO)

# Regular logging
logger.info("Starting trading cycle")
logger.warning("High volatility", extra={"volatility": 0.85})

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

### Node.js with pino

```javascript
const pino = require('pino');
const logger = pino();

// Regular logging
logger.info("Starting trading cycle");
logger.info({ symbol: "BTC/USD" }, "Fetching market data");

// Emit metrics
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

The `_metric` field is **required** - it identifies the JSON object as a metric and specifies its type. the0 automatically detects and parses these from your log output.

### Compiled Languages (Rust, C++, C#, Scala, Haskell)

All compiled language SDKs support the same logging pattern. Use your language's standard print functions for logging - they won't interfere with the bot's result output.

**Rust:**

```rust
use the0::input;

fn main() {
    let (bot_id, config) = input::parse();

    // Free to use println! for debugging - goes to logs
    println!("Starting bot {} with config: {:?}", bot_id, config);
    println!("Processing trade for BTC/USD");

    // Your trading logic here

    // Final result - uses THE0_RESULT: marker automatically
    input::success("Trade executed successfully");
}
```

**C++:**

```cpp
#include "the0.h"

int main() {
    auto [bot_id, config] = the0::parse();

    // Free to use std::cout for debugging - goes to logs
    std::cout << "Starting bot " << bot_id << std::endl;
    std::cout << "Processing trade..." << std::endl;

    // Your trading logic here

    // Final result - uses THE0_RESULT: marker automatically
    the0::success("Trade executed successfully");
    return 0;
}
```

**C#:**

```csharp
using The0;

class Program {
    static void Main() {
        var (botId, config) = Input.Parse();

        // Free to use Console.WriteLine for debugging - goes to logs
        Console.WriteLine($"Starting bot {botId}");
        Console.WriteLine("Processing trade...");

        // Your trading logic here

        // Final result - uses THE0_RESULT: marker automatically
        Input.Success("Trade executed successfully");
    }
}
```

**Scala:**

```scala
import the0.Input

object Main extends App {
  val (botId, config) = Input.parse()

  // Free to use println for debugging - goes to logs
  println(s"Starting bot $botId")
  println("Processing trade...")

  // Your trading logic here

  // Final result - uses THE0_RESULT: marker automatically
  Input.success("Trade executed successfully")
}
```

**Haskell:**

```haskell
import The0.Input

main :: IO ()
main = do
    (botId, config) <- parse

    -- Free to use putStrLn for debugging - goes to logs
    putStrLn $ "Starting bot " ++ botId
    putStrLn "Processing trade..."

    -- Your trading logic here

    -- Final result - uses THE0_RESULT: marker automatically
    success "Trade executed successfully"
```

## Metric Types

You can emit any metric type you need. Here are some common patterns:

### Portfolio Metrics

```python
logger.info(
    "portfolio_snapshot",
    _metric="portfolio_value",
    total_value=10500.00,
    cash=2500.00,
    positions_value=8000.00,
    unrealized_pnl=350.00,
    daily_return_pct=1.2
)

logger.info(
    "position_update",
    _metric="position",
    symbol="BTC/USD",
    quantity=0.5,
    avg_entry=44000.00,
    current_price=45000.00,
    unrealized_pnl=500.00,
    weight_pct=21.4
)
```

### Trade Metrics

```python
logger.info(
    "trade_executed",
    _metric="trade",
    symbol="ETH/USD",
    side="buy",
    quantity=2.5,
    price=2400.00,
    total_value=6000.00,
    fees=6.00,
    order_type="market",
    execution_time_ms=45
)

logger.info(
    "order_filled",
    _metric="order",
    order_id="ord_789",
    status="filled",
    symbol="BTC/USD",
    side="sell",
    filled_qty=0.1,
    avg_fill_price=45100.00
)
```

### Signal & Strategy Metrics

```python
logger.info(
    "signal_generated",
    _metric="signal",
    symbol="SOL/USD",
    direction="long",
    strength=0.78,
    strategy="momentum_breakout",
    timeframe="4h"
)

logger.info(
    "strategy_update",
    _metric="strategy_state",
    name="mean_reversion",
    status="active",
    positions_open=3,
    signals_generated=12,
    win_rate=0.65
)
```

### Market Data Metrics

```python
logger.info(
    "market_data",
    _metric="market_snapshot",
    symbol="BTC/USD",
    bid=44990.00,
    ask=45010.00,
    spread_bps=4.4,
    volume_24h=1250000000
)

logger.info(
    "indicator_value",
    _metric="indicator",
    symbol="BTC/USD",
    name="RSI",
    value=68.5,
    timeframe="1h",
    signal="overbought"
)
```

### Risk Metrics

```python
logger.info(
    "risk_metrics",
    _metric="risk",
    portfolio_var_95=1250.00,
    max_drawdown_pct=5.2,
    sharpe_ratio=1.8,
    exposure_pct=75.0,
    margin_used_pct=45.0
)
```

## Best Practices

### 1. Use Consistent Metric Names

Define a standard set of metric types for your bot and stick to them:

```python
# Good - consistent naming (use snake_case for _metric values)
logger.info("trade_executed", _metric="trade", ...)
logger.info("portfolio_snapshot", _metric="portfolio_value", ...)
logger.info("signal_generated", _metric="signal", ...)

# Avoid - inconsistent naming
logger.info("Trade", _metric="Trade", ...)  # Not snake_case
logger.info("value", _metric="portfolio-value", ...)  # Dashes instead of underscores
```

### 2. Include Relevant Context

Add fields that help with filtering and analysis:

```python
logger.info(
    "trade_executed",
    _metric="trade",
    symbol="BTC/USD",      # Always include symbol
    strategy="momentum",    # Which strategy generated this
    timeframe="1h",        # Relevant timeframe
    run_id=run_id,         # Link to specific bot run
    # ... other fields
)
```

### 3. Use Numeric Values for Charting

Ensure values you want to chart are numbers, not strings:

```python
# Good - numeric values
logger.info("portfolio_snapshot", _metric="portfolio_value", value=10500.00)

# Avoid - string values that can't be charted
logger.info("portfolio_snapshot", _metric="portfolio_value", value="$10,500.00")
```

### 4. Emit at Key Moments

Emit metrics at important events in your bot's lifecycle:

- Bot startup/shutdown
- Trade execution
- Position changes
- Signal generation
- Error conditions
- Periodic snapshots (e.g., every hour)

### 5. Don't Over-Emit

Balance granularity with storage:

```python
# For real-time bots, emit portfolio value periodically
if time.time() - last_snapshot > 300:  # Every 5 minutes
    logger.info("portfolio_snapshot", _metric="portfolio_value", value=portfolio.total_value)
    last_snapshot = time.time()

# Always emit trade events immediately
logger.info("trade_executed", _metric="trade", symbol=trade.symbol, side=trade.side)
```

## Viewing Metrics

Metrics appear in the bot's console view with visual distinction:

- Regular logs show with colored status indicators (green for INFO, yellow for WARN, etc.)
- Metrics display with a blue background and structured key-value format

If your bot has a [custom frontend](./custom-frontends), you can build rich visualizations using the event utilities provided by the platform.

## Log Storage

All logs and metrics are stored in MinIO with daily partitioning:

- Path: `logs/{bot_id}/{YYYYMMDD}.log`
- Retention: Configurable per account
- Format: One event per line, timestamped

This temporal structure makes it efficient to query historical data for backtesting analysis or performance review.

## Next Steps

- [Custom Frontends](./custom-frontends) - Build rich dashboards that visualize your metrics
- [Bot Types](./bot-types) - Understand scheduled vs real-time execution
- [Testing](./testing) - Test your metrics locally before deployment
