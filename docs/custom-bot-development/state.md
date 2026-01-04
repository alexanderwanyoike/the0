---
title: "Persistent State"
description: "Persist data between bot executions using the state API"
order: 8
---

# Persistent State

The state API allows your bots to persist data between executions. This is essential for strategies that need to track historical values, maintain counters, or preserve computed state across restarts.

## How It Works

State is automatically synchronized with cloud storage between bot runs:

1. **On bot start**: Previous state is downloaded and made available
2. **During execution**: Your bot reads and writes state using the SDK
3. **On bot stop**: State is uploaded and preserved for the next run

State is stored as JSON files in a dedicated directory, making it easy to debug and inspect.

## Basic Usage

The state API provides a simple key-value interface with automatic JSON serialization:

::: code-group

```python [Python]
from the0 import state

# Store a value
state.set("portfolio", {"AAPL": 100, "GOOGL": 50})

# Retrieve a value (returns None if not found)
portfolio = state.get("portfolio")

# Retrieve with default
portfolio = state.get("portfolio", {"AAPL": 0})

# Check if key exists
if state.exists("portfolio"):
    print("Portfolio found")

# Delete a key
state.delete("portfolio")

# List all keys
keys = state.list()

# Clear all state
state.clear()
```

```typescript [TypeScript]
import { state } from "@alexanderwanyoike/the0-node";

// Store a value
state.set("portfolio", { AAPL: 100, GOOGL: 50 });

// Retrieve a value (returns undefined if not found)
const portfolio = state.get("portfolio");

// Retrieve with default
const portfolio = state.get("portfolio", { AAPL: 0 });

// Check if key exists
if (state.exists("portfolio")) {
    console.log("Portfolio found");
}

// Delete a key
state.delete("portfolio");

// List all keys
const keys = state.list();

// Clear all state
state.clear();
```

```rust [Rust]
use the0_sdk::state;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Default)]
struct Portfolio {
    aapl: u32,
    googl: u32,
}

// Store a value
state::set("portfolio", &Portfolio { aapl: 100, googl: 50 })?;

// Retrieve a value (returns None if not found)
let portfolio: Option<Portfolio> = state::get("portfolio")?;

// Retrieve with default
let portfolio: Portfolio = state::get("portfolio").unwrap_or_default();

// Check if key exists
if state::exists("portfolio") {
    println!("Portfolio found");
}

// Delete a key
state::delete("portfolio");

// List all keys
let keys = state::list();

// Clear all state
state::clear()?;
```

```cpp [C++]
#include <the0/state.h>

// Store a value (as JSON)
the0::state::set("portfolio", {
    {"AAPL", 100},
    {"GOOGL", 50}
});

// Retrieve a value (returns std::nullopt if not found)
auto portfolio = the0::state::get("portfolio");
if (portfolio.has_value()) {
    int aapl = (*portfolio)["AAPL"];
}

// Check if key exists
if (the0::state::exists("portfolio")) {
    std::cout << "Portfolio found" << std::endl;
}

// Delete a key
the0::state::remove("portfolio");

// List all keys
auto keys = the0::state::list();

// Clear all state
the0::state::clear();
```

```csharp [C#]
using The0;

// Store a value
State.Set("portfolio", new { AAPL = 100, GOOGL = 50 });

// Retrieve a value (returns null if not found)
var portfolio = State.Get<Portfolio>("portfolio");

// Check if key exists
if (State.Exists("portfolio"))
{
    Console.WriteLine("Portfolio found");
}

// Delete a key
State.Delete("portfolio");

// List all keys
var keys = State.List();

// Clear all state
State.Clear();
```

```scala [Scala]
import the0.State

// Store a value (as JSON string)
State.set("portfolio", """{"AAPL": 100, "GOOGL": 50}""")

// Retrieve a value (returns Option[String])
val portfolio = State.get("portfolio")
portfolio match {
  case Some(json) => println(s"Portfolio: $json")
  case None => println("No portfolio found")
}

// Retrieve with default
val portfolio = State.getOrElse("portfolio", "{}")

// Check if key exists
if (State.exists("portfolio")) {
  println("Portfolio found")
}

// Delete a key
State.delete("portfolio")

// List all keys
val keys = State.list()

// Clear all state
State.clear()
```

```haskell [Haskell]
import qualified The0.State as State
import Data.Aeson (object, (.=))

-- Store a value
State.set "portfolio" (object ["AAPL" .= (100 :: Int), "GOOGL" .= (50 :: Int)])

-- Retrieve a value (returns IO (Maybe a))
portfolio <- State.get "portfolio" :: IO (Maybe Portfolio)
case portfolio of
  Just p  -> putStrLn $ "Portfolio: " ++ show p
  Nothing -> putStrLn "No portfolio found"

-- Check if key exists
hasPortfolio <- State.exists "portfolio"
when hasPortfolio $ putStrLn "Portfolio found"

-- Delete a key
deleted <- State.delete "portfolio"

-- List all keys
keys <- State.list

-- Clear all state
State.clear
```

:::

## Common Patterns

### Tracking Previous Values for Crossover Detection

Store previous indicator values to detect crossovers between runs:

::: code-group

```python [Python]
from the0 import state, metric

# Load previous SMA values
prev_short = state.get("prev_short_sma")
prev_long = state.get("prev_long_sma")

# Calculate current SMAs
short_sma = calculate_sma(prices, 5)
long_sma = calculate_sma(prices, 20)

# Detect crossover
if prev_short and prev_long:
    if prev_short <= prev_long and short_sma > long_sma:
        metric("signal", {"type": "BUY", "reason": "Golden cross"})
    elif prev_short >= prev_long and short_sma < long_sma:
        metric("signal", {"type": "SELL", "reason": "Death cross"})

# Save current values for next run
state.set("prev_short_sma", short_sma)
state.set("prev_long_sma", long_sma)
```

```typescript [TypeScript]
import { state, metric } from "@alexanderwanyoike/the0-node";

interface SmaState {
    prevShortSma: number | null;
    prevLongSma: number | null;
}

// Load previous SMA values
const smaState = state.get<SmaState>("sma_state", {
    prevShortSma: null,
    prevLongSma: null
});

// Calculate current SMAs
const shortSma = calculateSma(prices, 5);
const longSma = calculateSma(prices, 20);

// Detect crossover
if (smaState.prevShortSma !== null && smaState.prevLongSma !== null) {
    if (smaState.prevShortSma <= smaState.prevLongSma && shortSma > longSma) {
        metric("signal", { type: "BUY", reason: "Golden cross" });
    } else if (smaState.prevShortSma >= smaState.prevLongSma && shortSma < longSma) {
        metric("signal", { type: "SELL", reason: "Death cross" });
    }
}

// Save current values for next run
state.set("sma_state", {
    prevShortSma: shortSma,
    prevLongSma: longSma
});
```

```rust [Rust]
use the0_sdk::{input, state};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Default)]
struct SmaState {
    prev_short_sma: Option<f64>,
    prev_long_sma: Option<f64>,
}

// Load previous SMA values
let sma_state: SmaState = state::get("sma_state").unwrap_or_default();

// Calculate current SMAs
let short_sma = calculate_sma(&prices, 5);
let long_sma = calculate_sma(&prices, 20);

// Detect crossover
if let (Some(prev_short), Some(prev_long)) = (sma_state.prev_short_sma, sma_state.prev_long_sma) {
    if prev_short <= prev_long && short_sma > long_sma {
        input::metric("signal", &json!({"type": "BUY", "reason": "Golden cross"}));
    } else if prev_short >= prev_long && short_sma < long_sma {
        input::metric("signal", &json!({"type": "SELL", "reason": "Death cross"}));
    }
}

// Save current values for next run
state::set("sma_state", &SmaState {
    prev_short_sma: Some(short_sma),
    prev_long_sma: Some(long_sma),
})?;
```

:::

### Maintaining Price History

Accumulate price data across executions for trend analysis:

::: code-group

```python [Python]
from the0 import state

# Load existing history (default to empty list)
price_history = state.get("price_history", [])

# Add new price
price_history.append({
    "price": current_price,
    "timestamp": time.time()
})

# Keep only last 100 entries
if len(price_history) > 100:
    price_history = price_history[-100:]

# Save updated history
state.set("price_history", price_history)

# Use history for analysis
if len(price_history) >= 20:
    moving_avg = sum(p["price"] for p in price_history[-20:]) / 20
```

```typescript [TypeScript]
import { state } from "@alexanderwanyoike/the0-node";

interface PriceEntry {
    price: number;
    timestamp: number;
}

// Load existing history
let priceHistory = state.get<PriceEntry[]>("price_history", []);

// Add new price
priceHistory.push({
    price: currentPrice,
    timestamp: Date.now()
});

// Keep only last 100 entries
if (priceHistory.length > 100) {
    priceHistory = priceHistory.slice(-100);
}

// Save updated history
state.set("price_history", priceHistory);

// Use history for analysis
if (priceHistory.length >= 20) {
    const recent = priceHistory.slice(-20);
    const movingAvg = recent.reduce((sum, p) => sum + p.price, 0) / 20;
}
```

```rust [Rust]
use the0_sdk::state;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct PriceEntry {
    price: f64,
    timestamp: u64,
}

// Load existing history
let mut price_history: Vec<PriceEntry> = state::get("price_history").unwrap_or_default();

// Add new price
price_history.push(PriceEntry {
    price: current_price,
    timestamp: std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_secs(),
});

// Keep only last 100 entries
if price_history.len() > 100 {
    price_history = price_history.split_off(price_history.len() - 100);
}

// Save updated history
state::set("price_history", &price_history)?;

// Use history for analysis
if price_history.len() >= 20 {
    let recent: Vec<_> = price_history.iter().rev().take(20).collect();
    let moving_avg: f64 = recent.iter().map(|p| p.price).sum::<f64>() / 20.0;
}
```

:::

### Tracking Metrics Across Runs

Count signals, trades, or other events across bot executions:

::: code-group

```python [Python]
from the0 import state, metric

# Load counters
total_signals = state.get("total_signals", 0)
total_trades = state.get("total_trades", 0)

# Process signals
if signal_detected:
    total_signals += 1
    metric("signal", {
        "type": signal_type,
        "total_signals": total_signals
    })

# Process trades
if trade_executed:
    total_trades += 1
    metric("trade", {
        "side": trade_side,
        "total_trades": total_trades
    })

# Save counters
state.set("total_signals", total_signals)
state.set("total_trades", total_trades)
```

```typescript [TypeScript]
import { state, metric } from "@alexanderwanyoike/the0-node";

// Load counters
let totalSignals = state.get<number>("total_signals", 0);
let totalTrades = state.get<number>("total_trades", 0);

// Process signals
if (signalDetected) {
    totalSignals++;
    metric("signal", {
        type: signalType,
        total_signals: totalSignals
    });
}

// Process trades
if (tradeExecuted) {
    totalTrades++;
    metric("trade", {
        side: tradeSide,
        total_trades: totalTrades
    });
}

// Save counters
state.set("total_signals", totalSignals);
state.set("total_trades", totalTrades);
```

```rust [Rust]
use the0_sdk::{input, state};
use serde_json::json;

// Load counters
let mut total_signals: u64 = state::get("total_signals").unwrap_or(0);
let mut total_trades: u64 = state::get("total_trades").unwrap_or(0);

// Process signals
if signal_detected {
    total_signals += 1;
    input::metric("signal", &json!({
        "type": signal_type,
        "total_signals": total_signals
    }));
}

// Process trades
if trade_executed {
    total_trades += 1;
    input::metric("trade", &json!({
        "side": trade_side,
        "total_trades": total_trades
    }));
}

// Save counters
state::set("total_signals", &total_signals)?;
state::set("total_trades", &total_trades)?;
```

:::

## Best Practices

### Save State Periodically

For realtime bots running continuously, save state periodically rather than on every iteration to reduce I/O overhead:

```python
iteration = 0
while True:
    # ... bot logic ...

    iteration += 1
    if iteration % 10 == 0:  # Save every 10 iterations
        state.set("bot_state", {
            "prev_short_sma": short_sma,
            "prev_long_sma": long_sma,
            "signal_count": signal_count
        })

    sleep(update_interval)
```

### Use Structured State Objects

Instead of many individual keys, group related state into objects:

```python
# Good: Single structured object
state.set("bot_state", {
    "prev_short_sma": short_sma,
    "prev_long_sma": long_sma,
    "signal_count": signal_count,
    "last_signal_time": last_signal_time
})

# Less ideal: Many separate keys
state.set("prev_short_sma", short_sma)
state.set("prev_long_sma", long_sma)
state.set("signal_count", signal_count)
state.set("last_signal_time", last_signal_time)
```

### Handle Missing State Gracefully

Always provide sensible defaults for first-run scenarios:

```python
# Load state with defaults
persisted = state.get("bot_state", {
    "prev_short_sma": None,
    "prev_long_sma": None,
    "signal_count": 0
})

# Safe access with defaults
prev_short = persisted.get("prev_short_sma")
signal_count = persisted.get("signal_count", 0)
```

### Keep State Size Reasonable

State is synced to cloud storage on each bot stop. Keep state small for efficient sync:

- Limit history arrays to 100-1000 entries
- Store only essential data, not full market data
- Use summary statistics rather than raw data when possible

## State vs Configuration

| State | Configuration |
|-------|---------------|
| Changes during execution | Set before deployment |
| Persists between runs | Read-only during execution |
| For computed/accumulated values | For user settings |
| Accessed via `state.get/set` | Accessed via `parse()` |

**Use state for:** Previous indicator values, price history, signal counts, accumulated metrics

**Use configuration for:** Trading parameters, symbols, thresholds, update intervals

## Related

- [Metrics](./metrics) - Emit data to the dashboard
- [Bot Types](./bot-types) - Scheduled vs realtime execution
- [Development Overview](./overview) - SDK functions reference
