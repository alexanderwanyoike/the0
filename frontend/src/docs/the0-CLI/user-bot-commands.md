---
title: 'User Bot Commands'
description: 'Manage bots from the marketplace'
tags: ['cli', 'user-bots', 'marketplace']
order: 5
---

# User Bot Commands

The `user-bot` commands allow you to manage bots you've installed from the the0 marketplace.

---

## Overview

User bots are pre-built trading algorithms created by other developers that you can:

- Browse and install from the marketplace
- Configure and deploy for your own use
- Update to newer versions
- Manage through the CLI

---

## Commands Reference

### List User Bots

Display all bots you've acquired from the marketplace:

```bash
the0 user-bot list
Found 5 installed bot(s) ⚡

┌──────────────────────┬─────────┬────────────────────────────────┬──────────────────┐
│   CUSTOM BOT NAME    │ VERSION │              TYPE              │   ACQUIRED AT    │
├──────────────────────┼─────────┼────────────────────────────────┼──────────────────┤
│ momentum-trader-pro  │ 3.1.2   │ realtime/momentum-trader       │ 2025-06-20 09:45 │
│ grid-bot-eth         │ 2.5.0   │ realtime/grid-trader           │ 2025-06-19 14:30 │
│ arbitrage-scanner    │ 1.8.7   │ realtime/triangular-arb        │ 2025-06-18 11:15 │
│ dca-accumulator      │ 4.0.1   │ scheduled/dollar-cost-average  │ 2025-06-17 16:20 │
└──────────────────────┴─────────┴────────────────────────────────┴──────────────────┘

Bot collection ready for trading ⚡
```

### Get User Bot Version

Retrieve details about a specific user bot version:

```bash
the0 user-bot version <type> <version>
📋 Fetching versions for user bot 'realtime/momentum-trader'...
📋 Found 2 version(s) for user bot 'realtime/momentum-trader':

┌─────────┬──────────────────┬────────────────────────────────┐
│ VERSION │    CREATED AT    │              TYPE              │
├─────────┼──────────────────┼────────────────────────────────┤
│ 3.2.1   │ 2025-06-22 14:15 │ realtime/momentum-trader       │
│ 3.1.2   │ 2025-06-20 09:45 │ realtime/momentum-trader       │
│ 3.0.0   │ 2025-06-18 16:30 │ realtime/momentum-trader       │
│ 2.5.1   │ 2025-06-15 11:20 │ realtime/momentum-trader       │
│ 2.0.0   │ 2025-06-11 22:34 │ realtime/momentum-trader       │
│ 1.0.0   │ 2025-06-11 21:13 │ realtime/momentum-trader       │
└─────────┴──────────────────┴────────────────────────────────┘

🎉 Version history looking good! 📈
```

### Get User Bot Schema

Retrieve the JSON schema for a specific user bot:

```bash
the0 user-bot schema <type|name> <version> [bot|backtest]
```

#### Parameters

- **type|name**: Bot type (e.g., "realtime/momentum-trader") or name
- **version**: Specific version number
- **[bot|backtest]**: Optional, defaults to "bot". Use "schema" for backtest schema

#### Examples

```bash
# Get bot configuration schema
the0 user-bot schema scheduled/sma-pro 2.0.0 bot
📄 Fetching bot schema for user bot 'scheduled/sma-pro' version '2.0.0'...
📄 Schema for user bot 'scheduled/sma-pro' version '2.0.0' (bot entry point):

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

## Working with User Bots

### Deployment Workflow

1. **Browse Marketplace**: Use platform UI to find bots
2. **Install Bot**: Use Platform UI to install the bot from the marketplace. Check you have the bot in [My Bots](/user-bots) section
3. **Check Schema**: Review configuration requirements
4. **Create Config**: Prepare deployment configuration (Using the schema commands and required fields see [Required Fields](./bot-commands#required-fields))
5. **Deploy**: Use bot commands to deploy they can be deployed with `the0 bot deploy <config-file.json>`

#### Complete Example

```bash
# Step 1: Install a bot from marketplace
the0 user-bot install grid-trader

# Step 2: Check what configuration it needs
the0 user-bot schema grid-trader 3.0.0 bot

# Step 3: Create configuration based on schema
cat > grid-config.json << EOF
{
  "name": "my-grid-trader",
  "type": "trading/grid-trader",
  "version": "3.0.0",
  "parameters": {
    "symbol": "BTCUSDT",
    "grid_levels": 10,
    "grid_spacing": 0.01,
    "position_size": 100,
    "api_key": "your-api-key",
    "api_secret": "your-api-secret"
  }
}
EOF

# Step 4: Deploy the bot
the0 bot deploy grid-config.json
```

---

## Related Commands

- [Bot Commands](./bot-commands) - Deploy and manage bot instances
- [Custom Bot Commands](./custom-bot-commands) - Create your own bots
- [Authentication](./authentication) - Set up API access
