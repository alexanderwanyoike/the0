---
title: "Bot Commands"
description: "Managing bot instances with the CLI"
tags: ["cli", "bots", "commands"]
order: 3
---

# Bot Commands

The `bot` commands manage bot instances—running deployments of custom bots with specific configurations.

## Deploy

Create a new bot instance from a configuration file:

```bash
the0 bot deploy <config.json>
```

The configuration file specifies which custom bot to use and provides parameter values:

```json
{
  "name": "my-aapl-tracker",
  "type": "realtime/sma-crossover",
  "version": "1.0.0",
  "symbol": "AAPL",
  "short_period": 5,
  "long_period": 20,
  "update_interval_ms": 60000
}
```

**Required fields:**

- `name` - Unique identifier for this instance
- `type` - Format: `{execution-model}/{custom-bot-name}`
- `version` - Semantic version of the custom bot

**Scheduled bots** require a `schedule` field with a cron expression:

```json
{
  "name": "daily-portfolio-check",
  "type": "scheduled/portfolio-tracker",
  "version": "1.0.0",
  "schedule": "0 9 * * *",
  "symbols": ["BTC", "ETH", "SOL"]
}
```

Additional fields depend on the custom bot's schema. Use `the0 custom-bot schema` to view required parameters.

## List

Display all deployed bot instances:

```bash
the0 bot list
```

Output shows instance ID, name, type, version, schedule (if applicable), and timestamps.

## Update

Update an instance's configuration parameters:

```bash
the0 bot update <bot_id> <config.json>
```

This updates configuration values but does not change the custom bot version. To use a different version, delete the instance and deploy a new one.

## Delete

Remove a bot instance:

```bash
the0 bot delete <bot_id>
```

The CLI prompts for confirmation before deletion. Use `-y` or `--yes` to skip the prompt:

```bash
the0 bot delete <bot_id> -y
```

## Logs

View execution logs for a bot instance:

```bash
the0 bot logs <bot_id>
```

**Options:**

- `--limit <n>` - Maximum entries to return (1-1000, default 100)
- `-w, --watch` - Stream logs in real-time (polls every 5 seconds)

**Date filtering:**

```bash
# Logs from a specific date
the0 bot logs <bot_id> 2025-01-15

# Logs from a date range
the0 bot logs <bot_id> 2025-01-15:2025-01-20
```

**Examples:**

```bash
# Last 50 log entries
the0 bot logs abc123 --limit 50

# Watch logs in real-time
the0 bot logs abc123 -w

# Logs from today with limit
the0 bot logs abc123 2025-01-15 --limit 200
```

## Cron Schedule Format

Scheduled bots use standard cron expressions:

```
┌───────────── minute (0-59)
│ ┌───────────── hour (0-23)
│ │ ┌───────────── day of month (1-31)
│ │ │ ┌───────────── month (1-12)
│ │ │ │ ┌───────────── day of week (0-6, Sunday=0)
│ │ │ │ │
* * * * *
```

**Examples:**

| Expression | Description |
|------------|-------------|
| `* * * * *` | Every minute |
| `0 * * * *` | Every hour |
| `0 9 * * *` | Daily at 9:00 AM |
| `0 9 * * 1-5` | Weekdays at 9:00 AM |
| `*/15 * * * *` | Every 15 minutes |
| `0 0 1 * *` | First day of each month |

## Related

- [Custom Bot Commands](./custom-bot-commands) - Deploy custom bots and view schemas
- [Bots](/terminology/bots) - Understanding bot instances
