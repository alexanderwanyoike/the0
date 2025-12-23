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
* Starting deployment...
* Installing dependencies...
* Authenticating...
* Uploading bot package...
* Configuring deployment...
v 'my-awesome-bot' v1.0.0 deployed successfully
```

> **Note**: Docker is required for deploying custom bots with dependencies.

> **Note**: Every deployment needs a new version number. Update the `version` field in `bot-config.yaml` before deploying. We follow [semantic versioning](https://semver.org/) (MAJOR.MINOR.PATCH).

> **Note**: For private GitHub or PyPI dependencies, see [Secrets & Private Dependencies](./secrets).

### List Custom Bot Versions

List all available versions for a specific custom bot:

```bash
the0 custom-bot versions <type|name>
```

#### Examples

```bash
# List versions by bot name
the0 custom-bot versions awesome-scheduled-bot
* Fetching versions for awesome-scheduled-bot...
v Found 2 version(s)

VERSION  CREATED AT        TYPE
1.3.0    2025-07-08 00:15  scheduled/awesome-scheduled-bot
1.2.0    2025-07-07 21:51  scheduled/awesome-scheduled-bot
```

### List Custom Bots

Display all custom bots you've deployed:

```bash
the0 custom-bot list
* Fetching custom bots...
v Found 5 custom bot(s)

CUSTOM BOT NAME          LATEST VERSION  CREATED AT        TYPE
dca-accumulator          1.3.0           2025-07-07 21:51  scheduled/dca-accumulator
arbitrage-scanner        1.12.0          2025-06-29 13:15  realtime/arbitrage-scanner
momentum-trader          2.11.0          2025-06-30 15:31  scheduled/momentum-trader
grid-trading-bot         2.22.0          2025-06-22 18:27  realtime/grid-trading-bot
news-sentiment-trader    1.2.0           2025-06-25 23:16  scheduled/news-sentiment-trader
```

### Get Custom Bot Schema

Retrieve the JSON schema for bot configuration:

```bash
the0 custom-bot schema bot 1.0.0 another-example-bot
* Fetching schema for another-example-bot...
v Schema for 'another-example-bot' v1.0.0:

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
- [Secrets & Private Dependencies](./secrets) - Configure private repo access
