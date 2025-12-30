---
title: "Bots"
description: "Understanding bot instances in the0 platform"
tags: ["terminology", "bots"]
order: 1
---

# Bots

A bot (or bot instance) is a running deployment of a custom bot with specific configuration values. While a custom bot defines the trading logic and configuration schema, a bot instance is the actual execution of that logic with concrete parameters.

## Relationship to Custom Bots

Custom bots and bot instances have a one-to-many relationship. A single custom bot can have multiple bot instances, each configured differently.

For example, consider a custom bot called `sma-crossover` that implements a simple moving average crossover strategy. You might deploy several bot instances from this template:

- One instance monitoring AAPL with a 5/20 period crossover
- Another instance monitoring MSFT with a 10/50 period crossover
- A third instance monitoring GOOGL with different parameters

Each instance runs independently with its own configuration, logs, and metrics.

## Creating Bot Instances

Bot instances are created using the CLI with a JSON configuration file:

```bash
the0 bot deploy config.json
```

The configuration file specifies which custom bot to use and provides values for its configuration parameters:

```json
{
  "name": "my-aapl-sma",
  "type": "realtime/sma-crossover",
  "version": "1.0.0",
  "symbol": "AAPL",
  "short_period": 5,
  "long_period": 20
}
```

The `type` field follows the format `{execution-model}/{custom-bot-name}`, where execution model is either `scheduled` or `realtime`.

## Managing Bot Instances

The CLI provides commands for managing bot instances throughout their lifecycle:

```bash
# List all bot instances
the0 bot list

# Update an instance's configuration
the0 bot update <bot_id> config.json

# View logs for an instance
the0 bot logs <bot_id>

# Delete an instance
the0 bot delete <bot_id>
```

## Execution Behavior

How a bot instance executes depends on its type:

**Scheduled bots** run on a cron schedule. Each execution is independent—the bot starts, executes its logic, reports results, and exits. The scheduler triggers the next run according to the configured schedule.

**Realtime bots** run continuously until stopped. They typically contain an event loop that monitors prices, emits metrics, and executes trades based on conditions.

## Related Topics

- [Custom Bots](/terminology/custom-bots) - The templates that bot instances are created from
- [Bot Deployment](/terminology/bot-deployment) - The deployment process in detail
- [Bot Commands](/the0-cli/bot-commands) - CLI reference for bot management
