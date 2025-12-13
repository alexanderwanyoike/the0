---
title: "MCP (Model Context Protocol)"
description: "Enable AI assistants to interact with the0 platform"
tags: ["integrations", "mcp", "ai", "claude"]
order: 1
---

# MCP (Model Context Protocol)

the0 implements the Model Context Protocol (MCP), enabling AI assistants like Claude Code to interact directly with the platform. This allows you to manage bots, view logs, and analyze performance through natural language.

---

## Overview

MCP is an open protocol that standardizes how AI assistants connect to external data sources and tools. With the0's MCP server, AI assistants can:

- Deploy, update, and delete trading bots
- View bot logs and execution history
- Analyze bot health and performance
- Browse the custom bot marketplace

---

## Setup

### Prerequisites

1. **API Key**: Generate an API key from [your account settings](https://the0.dev/settings/api-settings)
2. **Claude Code**: Install [Claude Code](https://claude.com/claude-code) CLI

### Configuration

#### Option 1: Claude Code CLI

Add the0 MCP server using the Claude Code CLI:

```bash
claude mcp add the0 --transport http https://api.the0.dev/mcp
```

For local development:

```bash
claude mcp add the0 --transport http http://localhost:3000/mcp
```

#### Option 2: Configuration File

Create or edit `~/.mcp.json`:

```json
{
  "mcpServers": {
    "the0": {
      "url": "https://api.the0.dev/mcp",
      "transport": "http"
    }
  }
}
```

### Authentication

When using Claude Code with the0, you'll be prompted for your API key. The key is passed via the `x-api-key` header for all requests.

---

## Available Tools

the0 MCP server provides 12 tools across four categories:

### Authentication

| Tool | Description |
|------|-------------|
| `auth_status` | Check if your API key is valid and view connection status |

### Bot Instance Management

| Tool | Description |
|------|-------------|
| `bot_list` | List all your deployed bot instances |
| `bot_get` | Get details of a specific bot instance |
| `bot_deploy` | Deploy a new bot instance with configuration |
| `bot_update` | Update an existing bot instance configuration |
| `bot_delete` | Delete a bot instance |

### Logs and Monitoring

| Tool | Description |
|------|-------------|
| `logs_get` | Retrieve execution logs for a bot (supports date filtering) |
| `logs_summary` | Get log statistics including error counts and date range |

### Custom Bot Registry

| Tool | Description |
|------|-------------|
| `custom_bot_list` | List all available custom bots in the marketplace |
| `custom_bot_get` | Get details about a specific custom bot |
| `custom_bot_schema` | Get the JSON schema for configuring a custom bot |

### Analysis

| Tool | Description |
|------|-------------|
| `analyze_bot_health` | Analyze recent logs to determine bot health status |
| `analyze_performance` | Generate a performance summary across all your bots |

---

## Example Interactions

Once configured, you can interact with the0 through natural language in Claude Code:

**Bot Management:**
```
"List my deployed bots"
"Deploy a new DCA bot with weekly schedule"
"Delete the bot named test-bot"
```

**Monitoring:**
```
"Show me the logs for my trading bot from today"
"Check the health of bot abc-123"
"Give me a performance summary"
```

**Exploration:**
```
"What custom bots are available?"
"Show me the configuration schema for the momentum-trader bot"
```

---

## Tool Details

### bot_deploy

Deploy a new bot instance with a specified configuration.

**Parameters:**
- `custom_bot_name` (required): Name of the custom bot to deploy
- `version` (required): Version of the custom bot
- `name` (required): Name for your bot instance
- `schedule` (required): Cron expression for scheduled bots
- `config` (required): Bot configuration matching the schema

### logs_get

Retrieve execution logs for a specific bot.

**Parameters:**
- `bot_id` (required): The bot instance ID
- `date` (optional): Specific date in YYYYMMDD format
- `date_range` (optional): Date range as "YYYYMMDD-YYYYMMDD"
- `limit` (optional): Maximum number of logs to return (max 500)

### analyze_bot_health

Analyze recent logs to determine the health status of a bot.

**Returns:**
- `healthy`: No errors, running normally
- `degraded`: Some errors but still operational
- `failing`: Critical errors, needs attention
- `unknown`: Insufficient data to determine status

---

## Security

- **User Isolation**: Each user can only access their own bots and data
- **API Key Authentication**: All operations require a valid API key
- **Rate Limiting**: API endpoints are protected against abuse
- **Masked Secrets**: Sensitive data in logs is automatically masked

---

## Troubleshooting

### Connection Issues

```bash
# Verify your API key works
the0 auth status

# Test the MCP endpoint directly
curl -X POST https://api.the0.dev/mcp \
  -H "Content-Type: application/json" \
  -H "x-api-key: your-api-key" \
  -d '{"jsonrpc": "2.0", "id": 1, "method": "ping"}'
```

### Common Errors

| Error | Solution |
|-------|----------|
| "API key is invalid" | Generate a new API key from account settings |
| "Bot not found" | Verify the bot ID with `bot_list` |
| "Permission denied" | Ensure you own the bot you're trying to access |

---

## Related Documentation

- [CLI Authentication](../the0-cli/authentication) - Set up CLI authentication
- [Bot Commands](../the0-cli/bot-commands) - CLI bot management
- [Custom Bot Commands](../the0-cli/custom-bot-commands) - Deploy custom bots via CLI
