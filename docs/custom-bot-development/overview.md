---
title: "Development Overview"
description: "Understanding custom bot development on the0"
tags: ["custom-bots", "development", "overview"]
order: 2
---

# Development Overview

Custom bots are trading algorithms packaged as reusable templates. When you deploy a custom bot, you're creating a definition that others (or you) can instantiate with different configurations. A single "SMA Crossover" custom bot might power dozens of bot instances, each tracking different symbols or using different parameters.

This guide walks through the structure of a custom bot project, explains how the runtime invokes your code, and outlines the development workflow from local testing to deployment.

## Project Structure

Every custom bot consists of a few essential files. The bot-config.yaml file tells the platform what runtime to use, where to find your entry point, and what schema validates user configuration. The entry point file contains your trading logic. The schema file defines what parameters users can configure when they create bot instances.

```
my-bot/
├── bot-config.yaml      # Bot metadata and runtime settings
├── main.py              # Entry point (or main.ts, main.rs, etc.)
├── bot-schema.json      # JSON Schema for configuration validation
├── requirements.txt     # Dependencies (language-specific)
├── README.md            # Documentation shown to users
└── frontend/            # Optional custom dashboard
    ├── package.json
    └── index.tsx
```

The specific files vary by language. A Rust bot has Cargo.toml instead of requirements.txt, and the entry point is a compiled binary rather than a script. See the language-specific quick start guides for exact requirements.

## The Entry Point Pattern

When the platform starts your bot, it sets two environment variables: `BOT_ID` identifies the specific bot instance, and `BOT_CONFIG` contains the JSON configuration that the user provided when creating the instance. The SDK reads these automatically through the `parse()` function.

The entry point pattern looks similar across languages, though the syntax differs:

**Python:**

```python
from the0 import parse, success, error, metric

def main(bot_id: str = None, config: dict = None):
    # Parse from environment if not provided directly
    if bot_id is None or config is None:
        bot_id, config = parse()

    # Extract configuration with defaults
    symbol = config.get("symbol", "BTC/USD")

    # Your trading logic here
    price = fetch_current_price(symbol)

    # Emit metrics for the dashboard
    metric("price", {"symbol": symbol, "value": price})

    # Report successful completion
    success(f"Processed {symbol}")

if __name__ == "__main__":
    main()
```

**TypeScript:**

```typescript
import { parse, metric, success } from "@alexanderwanyoike/the0-node";

interface BotConfig {
    symbol: string;
    alert_threshold: number;
}

async function main(): Promise<void> {
    const { id, config } = parse<BotConfig>();

    const symbol = config.symbol || "BTC/USD";

    // Your trading logic here
    const price = await fetchPrice(symbol);

    metric("price", { symbol, value: price });

    success("Bot completed successfully");
}

// Export main for the runtime to invoke
export { main };
```

**Rust:**

```rust
use the0_sdk::input;
use serde_json::json;

fn main() {
    let (bot_id, config) = input::parse()
        .expect("Failed to parse configuration");

    let symbol = config["symbol"].as_str().unwrap_or("AAPL");

    // Your trading logic here

    input::metric("price", &json!({
        "symbol": symbol,
        "value": 150.0
    }));
}
```

**C++:**

```cpp
#include <the0.h>

int main() {
    auto [botId, config] = the0::parse();

    std::string symbol = config.value("symbol", "AAPL");

    // Your trading logic here

    the0::metric("price", {
        {"symbol", symbol},
        {"value", 150.0}
    });

    return 0;
}
```

The SDK handles the details of reading environment variables and writing result files. You focus on trading logic.

## Development Workflow

Building a custom bot follows a straightforward progression: define what your bot does, specify what configuration it needs, write the trading logic, test locally, and deploy.

### Define Bot Metadata

Start with bot-config.yaml. This file tells the platform everything it needs to know about your bot:

```yaml
name: my-trading-bot
description: "A brief description of what this bot does"
version: 1.0.0
author: "your-name"
type: realtime
runtime: python3.11

entrypoints:
  bot: main.py

schema:
  bot: bot-schema.json

readme: README.md
```

The `type` field determines whether your bot runs continuously (realtime) or on a schedule (scheduled). The `runtime` field tells the platform which execution environment to use.

### Define Configuration Schema

The bot-schema.json file uses JSON Schema to specify what parameters users can configure. This schema powers the UI form when users create bot instances and validates their input:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "symbol": {
      "type": "string",
      "description": "Trading symbol to monitor",
      "default": "BTC/USD"
    },
    "threshold": {
      "type": "number",
      "description": "Alert threshold percentage",
      "default": 5.0,
      "minimum": 0.1
    }
  },
  "required": ["symbol"]
}
```

### Test Locally

Before deploying, test your bot by setting environment variables manually:

```bash
export BOT_ID="test-bot"
export BOT_CONFIG='{"symbol":"AAPL","threshold":5.0}'
export CODE_MOUNT_DIR="/tmp"
python main.py
```

This simulates how the platform invokes your bot. Check that metrics emit correctly and errors are handled gracefully.

### Deploy

Once testing passes, deploy to the platform:

```bash
the0 custom-bot deploy
```

The CLI packages your code, validates configuration, and uploads everything to the platform. After deployment, users can create bot instances from your custom bot definition.

## SDK Functions

All language SDKs provide a consistent set of functions for interacting with the platform:

| Function | Purpose |
|----------|---------|
| `parse()` | Read BOT_ID and BOT_CONFIG from environment |
| `success(message, data?)` | Report successful execution with optional result data |
| `error(message)` | Report failure and terminate |
| `metric(type, data)` | Emit metrics to the dashboard |
| `log(message)` | Write structured log entries |

Metrics appear in the dashboard in real-time. Use them to track prices, signals, positions, and any other data users should see.

## Next Steps

The quickest path forward is following a language-specific quick start guide. Each one walks through building a complete bot from scratch:

- [Python Quick Start](./python-quick-start)
- [TypeScript Quick Start](./nodejs-quick-start)
- [Rust Quick Start](./rust-quick-start)
- [C++ Quick Start](./cpp-quick-start)
- [C# Quick Start](./csharp-quick-start)
- [Scala Quick Start](./scala-quick-start)
- [Haskell Quick Start](./haskell-quick-start)

For deeper dives into specific topics, see [Configuration](./configuration) for the complete bot-config.yaml reference, or [Bot Types](./bot-types) for details on scheduled versus realtime execution.
