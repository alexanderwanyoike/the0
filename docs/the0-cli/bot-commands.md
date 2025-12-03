---
title: "Bot Commands"
description: "Managing bot instances with the CLI"
tags: ["cli", "bots", "commands"]
order: 3
---

# Bot Commands

The `bot` commands allow you to deploy, manage, and monitor your trading bot instances through the CLI.

## Overview

Bot instances are deployed configurations of Custom Bots. These commands help you:

- Deploy new bot instances
- List and monitor active bots
- Update bot configurations
- Delete bot instances

## Commands Reference

### Deploy Bot Instance

Deploy a new bot instance using a configuration file:

```bash
the0 bot deploy <config.json>
```

#### Configuration File Format

```json
{
  "name": "my-trading-bot",
  "type": "scheduled/rsi-momentum", // "{customBotType}/{customBotName}" for custom bots
  "version": "1.0.0",
  "schedule": "0 0 * * *",
  "symbol": "BTCUSDT",
  "risk_level": "medium"
  // Other parameters specific to the bot type (flat structure)
}
```

### Required Fields

- **name**: Unique identifier for your bot instance
- **type**: Type of bot to deploy
  - This is composed of:
    - **customBotType**: The type of custom bot (e.g., `scheduled`, `realtime`)
    - **customBotName**: The name of the custom bot
- **version**: Version of the bot to deploy

### Scheduled bot fields

- **schedule**: Cron expression for scheduled bots

### Custom bot configuration

Other parameters depend on the schema of the custom bot you are deploying.
you can find out by running the following command:

```bash
the0 custom-bot schema bot <version> <custom-bot-name>
```

This will output the JSON schema for the bot configuration, which you can use to fill in the required parameters.

#### Example

```bash
# Create configuration file
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

### List Bot Instances

Display all your deployed bot instances:

```bash
the0 bot list
```

#### Output Format

```bash
the0 bot list
Found 1 active bot(s) ⚡

┌──────────────────────────────────────┬──────────────────────────────┬────────────────────────────────────────┬─────────┬───────────┬──────────────────┬──────────────────┐
│                  ID                  │             NAME             │                  TYPE                  │ VERSION │ SCHEDULE  │    CREATED AT    │    UPDATED AT    │
├──────────────────────────────────────┼──────────────────────────────┼────────────────────────────────────────┼─────────┼───────────┼──────────────────┼──────────────────┤
│ b838f75d-cf53-4330-ae36-6ebbaf1cc46a │ test-scheduled-bot-no-schema │ scheduled/test-scheduled-bot-no-schema │ 1.2.0   │ * * * * * │ 2025-07-07 23:42 │ 2025-07-07 23:42 │
└──────────────────────────────────────┴──────────────────────────────┴────────────────────────────────────────┴─────────┴───────────┴──────────────────┴──────────────────┘

Bots locked and loaded. Ready to dominate the markets 📈
```

### Update Bot Instance

Update an existing bot instance with new configuration:

```bash
the0 bot update <bot_id> <config.json>
```

#### Example

```bash
# Create updated configuration
cat > updated-config.json << EOF
{
  "name": "my-trading-bot-updated",
  "type": "scheduled/rsi-momentum", // "{customBotType}/{customBotName}" for custom bots
  "version": "1.0.0",
  "schedule": "0 0 * * *",
  "symbol": "BTCUSDT",
  "risk_level": "low"
  // Other parameters specific to the bot type (flat structure)
}
EOF

# Update the bot
the0 bot update bot_123 updated-config.json
# 🔄 Starting bot update process...
# 🎉 Bot updated successfully!
```

### Delete Bot Instance

Delete a deployed bot instance:

```bash
the0 bot delete <bot_id>
```

#### Safety Features

- Requires confirmation before deletion
- Cannot be undone
- Active positions are handled according to bot configuration

#### Example

```bash
the0 bot delete b838f75d-cf53-4330-ae36-6ebbaf1cc46a
⚠️ Are you sure you want to delete bot 'b838f75d-cf53-4330-ae36-6ebbaf1cc46a'?
This action cannot be undone
Type 'yes' to confirm: yes
🗑️ Starting bot deletion process...
✓ Connected to the matrix
Bot terminated 💀
Bot ID: b838f75d-cf53-4330-ae36-6ebbaf1cc46a
Bot purged from the grid
```

## Schedule Format

For scheduled bots, use standard cron expressions:

```bash
# Format: minute hour day month weekday

# Examples:
"0 9 * * *"     # Daily at 9:00 AM
"*/15 * * * *"  # Every 15 minutes
"0 0 * * 1"     # Weekly on Monday at midnight
"0 */4 * * *"   # Every 4 hours
"30 16 * * 1-5" # Weekdays at 4:30 PM
```

## Related Commands

- [Custom Bot Commands](/the0-cli/custom-bot-commands) - Deploy your own bot templates and get schemas
- [Authentication](/the0-cli/authentication) - Set up API access
