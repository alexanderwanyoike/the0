---
title: "Custom Bot Commands"
description: "Deploy and manage your custom trading algorithms"
tags: ["cli", "custom-bots", "development"]
order: 4
---

# Custom Bot Commands

The `custom-bot` commands allow you to deploy and manage your own trading algorithms on the the0 platform.

## Overview

Custom bots are trading algorithms you develop and deploy to the platform. These commands help you:

- Deploy custom bots to the marketplace
- List your deployed custom bots
- Manage bot versions
- Retrieve bot schemas

## Commands Reference

### Deploy Custom Bot

Package and deploy your custom bot from the current directory:

```bash
the0 custom-bot deploy
```

#### Required Files

Your bot directory must contain:

```bash
my-bot/
├── bot-config.yaml      # Bot configuration and metadata
├── main.py             # Main bot entry point
├── bot-schema.json     # Bot parameter schema
├── requirements.txt    # Python dependencies (optional)
├── vendor/             # Vendored dependencies (auto-generated during deployment)
└── README.md           # Bot documentation
```

#### Bot Configuration (bot-config.yaml)

```yaml
name: my-awesome-bot
description: "A trading bot that makes money while you sleep"
version: 1.0.0
author: your-name
type: scheduled # or 'realtime'
runtime: python3.11 # or 'nodejs20'

entrypoints:
  bot: main.py

schema:
  bot: bot-schema.json

readme: README.md

metadata:
  tags: ["arbitrage", "crypto", "defi"]
  categories:
    - trading
  instruments:
    - crypto
  exchanges:
    - binance
    - coinbase
```

#### Deployment Example

```bash
# Navigate to your bot directory
cd my-awesome-bot/

# Deploy the bot
the0 custom-bot deploy
✓ Access granted
Bot test-scheduled-bot-no-schema is ready for update! Current version: 1.2.0, New version: 1.3.0
Updating existing bot
✓ Bot packaged
🚀 Starting two-step deployment process...
📡 Requesting upload URL...
📦 Uploading bot_1751930118.zip to storage...
✅ File uploaded successfully!
🔧 Configuring deployment...
🎉 Deployment configured successfully!
Bot uploaded to the plaform ⚡
'test-scheduled-bot-no-schema' v1.3.0 deployed successfully
Awaiting review by the 0vers33r 👀
```

> Note: Ensure you have Docker installed as it is a requirement for deploying custom bots with dependencies,

> Note: That every deployment will need a new version number, so make sure to update the `version` field in `bot-config.yaml` before deploying. We follow [semantic versioning](https://semver.org/) (MAJOR.MINOR.PATCH) for bot versions.

### List Custom Bot Versions

List all available versions for a specific custom bot:

```bash
the0 custom-bot versions <type|name>
```

#### Examples

```bash
# List versions by bot type
the0 custom-bot versions scheduled/awesome-scheduled-bot
📋 Fetching versions for custom bot 'scheduled/awesome-scheduled-bot'...
📋 Found 2 version(s) for custom bot 'scheduled/awesome-scheduled-bot':

┌─────────┬──────────────────┬────────────────────────────────────────┐
│ VERSION │    CREATED AT    │                  TYPE                  │
├─────────┼──────────────────┼────────────────────────────────────────┤
│ 1.3.0   │ 2025-07-08 00:15 │ scheduled/awesome-scheduled-bot        │
│ 1.2.0   │ 2025-07-07 21:51 │ scheduled/awesome-scheduled-bot        │
└─────────┴──────────────────┴────────────────────────────────────────┘

🎉 Version history looking solid! 🚀

# List versions by type
the0 custom-bot versions trading/awesome-scheduled-bot
📋 Fetching versions for custom bot 'awesome-scheduled-bot'...
📋 Found 2 version(s) for custom bot 'awesome-scheduled-bot':

┌─────────┬──────────────────┬────────────────────────────────────────┐
│ VERSION │    CREATED AT    │                  TYPE                  │
├─────────┼──────────────────┼────────────────────────────────────────┤
│ 1.3.0   │ 2025-07-08 00:15 │ scheduled/awesome-scheduled-bot        │
│ 1.2.0   │ 2025-07-07 21:51 │ scheduled/awesome-scheduled-bot        │
└─────────┴──────────────────┴────────────────────────────────────────┘

🎉 Version history looking solid! 🚀
```

### List Custom Bots

Display all custom bots you've deployed:

```bash
the0 custom-bot list
Found 5 custom bot(s) ⚡

┌──────────────────────────────┬────────────────┬──────────────────┬────────────────────────────────────────┐
│       CUSTOM BOT NAME        │ LATEST VERSION │    CREATED AT    │                  TYPE                  │
├──────────────────────────────┼────────────────┼──────────────────┼────────────────────────────────────────┤
│ dca-accumulator              │ 1.3.0          │ 2025-07-07 21:51 │ scheduled/dca-accumulator              │
│ arbitrage-scanner            │ 1.12.0         │ 2025-06-29 13:15 │ realtime/arbitrage-scanner             │
│ momentum-trader              │ 2.11.0         │ 2025-06-30 15:31 │ scheduled/momentum-trader              │
│ grid-trading-bot             │ 2.22.0         │ 2025-06-22 18:27 │ realtime/grid-trading-bot              │
│ news-sentiment-trader        │ 1.2.0          │ 2025-06-25 23:16 │ scheduled/news-sentiment-trader        │
└──────────────────────────────┴────────────────┴──────────────────┴────────────────────────────────────────┘

Custom bot portfolio ready for deployment ⚡
```

### Get Custom Bot Schema

Retrieve the JSON schema for bot configuration:

```bash
the0 custom-bot schema bot 1.0.0 another-example-bot
📄 Fetching bot schema for custom bot 'another-example-bot'...
📄 Schema for custom bot 'another-example-bot' version '1.0.0' (bot entry point):

{
  "properties": {
    "description": {
      "description": "A brief description of the SMA bot's functionality.",
      "type": "string"
    },
    "name": {
      "description": "The name of the SMA bot.",
      "type": "string"
    },
    "strategy": {
      "properties": {
        "period": {
          "description": "The period for the simple moving average.",
          "minimum": 1,
          "type": "integer"
        },
        "symbol": {
          "description": "The trading symbol (e.g., BTCUSD).",
          "type": "string"
        },
        "type": {
          "description": "The type of strategy, fixed as 'SMA'.",
          "enum": [
            "SMA"
          ],
          "type": "string"
        }
      },
      "required": [
        "type",
        "period",
        "symbol"
      ],
      "type": "object"
    }
  },
  "required": [
    "name",
    "strategy"
  ],
  "type": "object"
}
```

### Bot Types

#### Scheduled Bot `scheduled`

- Runs on a fixed schedule (cron expression)
- Ideal for DCA strategies, periodic rebalancing
- Example: Daily portfolio rebalancing bot

#### Realtime Bot `realtime`

- Runs continuously, processing market data in real-time
- Suitable for market making, arbitrage
- Example: Grid trading bot

### The `.the0ignore` File

To exclude files from being uploaded during deployment, create a `.the0ignore` file in your bot directory. This works similarly to `.gitignore`.

```plaintext
# Ignore Python bytecode files
*.pyc
# Ignore local configuration files
config.local.yaml
# Ignore test files
tests/
# Ignore documentation files
docs/
```

## Related Commands

- [Bot Commands](./bot-commands) - Deploy bot instances
- [Authentication](./authentication) - Set up API access
