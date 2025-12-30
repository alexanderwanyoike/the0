---
title: "Configuration"
description: "Complete guide to bot configuration and schemas"
tags: ["custom-bots", "configuration", "schemas"]
order: 3
---

# Configuration Reference

Every custom bot requires two configuration files: bot-config.yaml defines your bot's metadata, runtime environment, and entry points, while bot-schema.json specifies what parameters users can configure when they create bot instances. Together, these files tell the platform how to run your bot and what configuration options to present in the UI.

## bot-config.yaml

The bot-config.yaml file is the heart of your custom bot definition. It tells the platform what language runtime to use, where to find your executable entry point, and what documentation to display to users.

### Required Fields

At minimum, your bot-config.yaml must specify a name, description, version, author, execution type, runtime, entry point, and schema:

```yaml
name: sma-crossover
description: "SMA crossover strategy for stock trading"
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

The `name` field serves as a unique identifier and must use lowercase letters with hyphens. This name appears in the UI and forms part of the URL path when users browse custom bots.

The `type` field determines the execution model. Set it to `realtime` for bots that run continuously with an event loop, or `scheduled` for bots that execute once and exit (triggered by cron expressions on bot instances).

### Runtime Options

The platform supports multiple language runtimes. Each runtime has specific requirements for how entry points are specified:

| Runtime | Identifier | Entry Point |
|---------|------------|-------------|
| Python 3.11 | `python3.11` | `main.py` |
| Node.js 20 | `nodejs20` | `main.js` |
| Rust | `rust-stable` | `target/release/my-bot` |
| C++ | `gcc13` | `build/my-bot` |
| C# .NET 8 | `dotnet8` | `bin/Release/net8.0/publish/MyBot.dll` |
| Scala 3 | `scala3` | `target/scala-3.3.1/my-bot-assembly-1.0.0.jar` |
| Haskell | `ghc96` | `dist-newstyle/build/.../my-bot` |

For interpreted languages like Python and JavaScript, the entry point is simply the filename. For compiled languages, it must be the exact path to the compiled binary or artifact as produced by your build system.

### Entry Point Examples by Language

The entry point path must match where your build system places the final executable. Here are complete examples for each compiled language:

**Rust:**
```yaml
name: rust-sma-crossover
runtime: rust-stable
entrypoints:
  bot: target/release/sma-bot
```

**C++:**
```yaml
name: cpp-sma-crossover
runtime: gcc13
entrypoints:
  bot: build/sma_bot
```

**C#:**
```yaml
name: csharp-sma-crossover
runtime: dotnet8
entrypoints:
  bot: bin/Release/net8.0/publish/SmaBot.dll
```

**Scala:**
```yaml
name: scala-sma-crossover
runtime: scala3
entrypoints:
  bot: target/scala-3.3.1/sma-bot-assembly-1.0.0.jar
```

**Haskell:**
```yaml
name: haskell-sma-crossover
runtime: ghc96
entrypoints:
  bot: dist-newstyle/build/x86_64-linux/ghc-9.6.7/sma-bot-1.0.0/x/sma-bot/opt/build/sma-bot/sma-bot
```

The Haskell path is verbose because Cabal generates deeply nested output directories. Use `cabal list-bin` to find the exact path after building.

### Optional Fields

Beyond the required fields, you can specify additional metadata that helps users discover and understand your bot:

```yaml
hasFrontend: true

metadata:
  categories: [trading, technical-analysis]
  instruments: [crypto, stocks]
  tags: [sma, crossover, beginner]
  complexity: beginner
```

Setting `hasFrontend: true` tells the platform that your bot includes a custom React dashboard in a `frontend/` directory. The metadata fields power filtering and search in the bot marketplace.

## bot-schema.json

The bot-schema.json file defines what configuration parameters your bot accepts. It uses JSON Schema draft-07 to specify types, defaults, constraints, and descriptions. The platform uses this schema to generate the configuration form in the UI and to validate user input before deploying bot instances.

### Basic Structure

A typical schema defines an object with properties for each configuration parameter:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "title": "SMA Crossover Configuration",
  "description": "Configure the SMA crossover strategy parameters",
  "properties": {
    "symbol": {
      "type": "string",
      "description": "Stock or crypto symbol to monitor",
      "default": "AAPL"
    },
    "short_period": {
      "type": "integer",
      "description": "Number of periods for the short SMA",
      "default": 5,
      "minimum": 2,
      "maximum": 50
    },
    "long_period": {
      "type": "integer",
      "description": "Number of periods for the long SMA",
      "default": 20,
      "minimum": 5,
      "maximum": 200
    },
    "update_interval_ms": {
      "type": "integer",
      "description": "Milliseconds between price checks",
      "default": 60000,
      "minimum": 1000
    }
  },
  "required": ["symbol"],
  "additionalProperties": false
}
```

Each property definition can include a type, description, default value, and constraints. The description appears as help text in the UI, so write it for users who may not be familiar with trading terminology.

### Supported Types

JSON Schema supports several primitive and complex types:

| Type | Description | Example |
|------|-------------|---------|
| `string` | Text values | `"AAPL"` |
| `integer` | Whole numbers | `5` |
| `number` | Decimal numbers | `0.02` |
| `boolean` | True/false values | `true` |
| `array` | Lists of values | `["BTC", "ETH"]` |
| `object` | Nested configuration | `{"key": "value"}` |

### Constraints

Constraints ensure users provide valid configuration. For numbers, use `minimum` and `maximum`. For strings with fixed options, use `enum`. For arrays, use `minItems` and `maxItems`:

```json
{
  "risk_percentage": {
    "type": "number",
    "description": "Maximum portfolio risk per trade",
    "default": 2.0,
    "minimum": 0.1,
    "maximum": 10.0
  },
  "strategy": {
    "type": "string",
    "description": "Technical indicator to use",
    "enum": ["SMA", "EMA", "MACD", "RSI"]
  },
  "symbols": {
    "type": "array",
    "description": "Symbols to monitor",
    "items": { "type": "string" },
    "minItems": 1,
    "maxItems": 10,
    "default": ["BTC", "ETH"]
  }
}
```

### Required Parameters

Parameters listed in the `required` array must be provided when creating a bot instance. The UI will not allow users to proceed without filling these in:

```json
{
  "required": ["symbol", "api_key"]
}
```

Use required fields sparingly. Most parameters should have sensible defaults so users can get started quickly.

## Private Dependencies

If your bot depends on packages from private registries, configure access credentials using the CLI before deployment:

```bash
# GitHub Packages (works for all languages)
the0 auth secrets set github-token ghp_xxxxxxxxxxxx

# Python private PyPI
the0 auth secrets set pip-index-url https://user:pass@pypi.example.com/simple/

# Node.js private npm
the0 auth secrets set npm-token npm_xxxxxxxxxxxx

# Rust private crates
the0 auth secrets set cargo-registry-token xxxxxxxxxxxxx

# C# private NuGet
the0 auth secrets set nuget-token oy2xxxxxxxxxxxxx

# Scala private Maven
the0 auth secrets set maven-user myuser
the0 auth secrets set maven-token xxxxxxxxxxxxx
```

These credentials are stored securely and injected into the build environment when deploying your bot. See [Secrets](/the0-cli/secrets) for complete documentation.

## Validating Configuration

Before deploying, validate that your configuration files are syntactically correct:

```bash
python -c "import yaml; yaml.safe_load(open('bot-config.yaml'))"
python -c "import json; json.load(open('bot-schema.json'))"
```

The CLI also validates configuration during `the0 custom-bot deploy` and reports detailed errors if anything is malformed.

## Related

- [Development Overview](./overview) - Bot structure and workflow
- [Bot Types](./bot-types) - Scheduled vs realtime execution
- [Secrets](/the0-cli/secrets) - Private dependency configuration
