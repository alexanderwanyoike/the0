---
title: "Custom Bots"
description: "Bot templates and blueprints in the0"
tags: ["terminology", "custom-bots", "development"]
order: 2
---

# Custom Bots

A custom bot is a reusable bot definition that contains trading logic, configuration schema, and metadata. Custom bots serve as templates from which multiple [bot instances](/terminology/bots) can be deployed, each with different configuration values.

## Structure

Every custom bot consists of the following components:

**bot-config.yaml** defines the bot's metadata, runtime, entry points, and schema references:

```yaml
name: sma-crossover
description: "SMA crossover strategy with Yahoo Finance data"
version: 1.0.0
author: "the0"
type: realtime
runtime: python3.11

entrypoints:
  bot: main.py

schema:
  bot: bot-schema.json

readme: README.md
```

**Entry point** is the file containing your trading logic. The runtime invokes this file when the bot executes. For Python bots, this is typically `main.py`; for TypeScript, `main.ts` or `main.js`.

**bot-schema.json** defines the configuration parameters your bot accepts using JSON Schema:

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "symbol": {
      "type": "string",
      "description": "Stock symbol to monitor",
      "default": "AAPL"
    },
    "short_period": {
      "type": "integer",
      "description": "Short SMA period",
      "default": 5
    }
  }
}
```

**Dependencies** are specified in language-specific files: `requirements.txt` for Python, `package.json` for Node.js, `Cargo.toml` for Rust, and so on.

## Supported Languages

Custom bots can be written in seven languages:

- Python (runtime: `python3.11`)
- TypeScript/Node.js (runtime: `nodejs20`)
- Rust (runtime: `rust-stable`)
- C# (runtime: `dotnet8`)
- Scala (runtime: `scala3`)
- C++ (runtime: `gcc13`)
- Haskell (runtime: `ghc96`)

Each language has an official SDK that provides consistent APIs for configuration parsing, result reporting, and metrics emission.

## Execution Types

The `type` field in bot-config.yaml determines how the bot executes:

**scheduled** - The bot runs on a cron schedule. Each execution is a discrete run: start, execute logic, report results, exit. Use this for periodic tasks like daily portfolio rebalancing or hourly price checks.

**realtime** - The bot runs continuously until stopped. Use this for strategies that need to monitor prices continuously and react to market conditions in real-time.

## Deployment

Custom bots are deployed to the platform using the CLI:

```bash
the0 custom-bot deploy
```

The CLI reads `bot-config.yaml` from the current directory, vendors dependencies, compiles code if necessary (for compiled languages), and uploads the packaged bot to the platform.

Once deployed, you can create bot instances from the custom bot using `the0 bot deploy`.

## Versioning

Custom bots use semantic versioning. When you deploy an updated version, the platform stores it alongside previous versions. Bot instances can specify which version to use, allowing you to test new versions while keeping production instances on stable releases.

## Related Topics

- [Bots](/terminology/bots) - Running instances of custom bots
- [Bot Deployment](/terminology/bot-deployment) - The deployment process
- [Configuration Reference](/custom-bot-development/configuration) - Complete bot-config.yaml documentation
