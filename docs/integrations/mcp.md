---
title: "MCP (Model Context Protocol)"
description: "Enable AI assistants to interact with the0 platform"
tags: ["integrations", "mcp", "ai", "claude"]
order: 1
---

# MCP (Model Context Protocol)

the0 implements the Model Context Protocol (MCP), enabling AI assistants like Claude Code to interact directly with the platform. This allows you to manage bots, view logs, and deploy strategies through natural language.

---

## Overview

MCP is an open protocol that standardizes how AI assistants connect to external data sources and tools. With the0's MCP server, AI assistants can:

- Deploy, update, and delete trading bots
- View bot logs and execution history
- Browse the custom bot marketplace
- Get configuration schemas for bot deployment

---

## Setup

### Prerequisites

1. **API Key**: Generate an API key from your the0 dashboard or via CLI (`the0 auth login`)
2. **Claude Code**: Install [Claude Code](https://claude.ai/download) CLI

### Step 1: Add the MCP Server

The MCP server requires your API key to be passed via the `x-api-key` header. Use the `--header` flag:

```bash
claude mcp add the0 --transport http https://api.the0.dev/mcp \
  --header "x-api-key: YOUR_API_KEY"
```

For local development:

```bash
claude mcp add the0 --transport http http://localhost:3000/mcp \
  --header "x-api-key: YOUR_API_KEY"
```

### Step 2: Restart Claude Code

**Important**: After adding or modifying MCP configuration, you must restart Claude Code for changes to take effect:

```bash
exit  # or Ctrl+D
claude
```

### Step 3: Verify Connection

```bash
claude mcp list
```

You should see:
```
the0: https://api.the0.dev/mcp (HTTP) - ✓ Connected
```

Then test within Claude Code by asking: "list my bots"

---

## Configuration Options

### Option 1: CLI with Header (Recommended)

```bash
claude mcp add the0 --transport http https://api.the0.dev/mcp \
  --header "x-api-key: YOUR_API_KEY"
```

### Option 2: Project Configuration File

Create `.mcp.json` in your project root:

```json
{
  "mcpServers": {
    "the0": {
      "type": "http",
      "url": "https://api.the0.dev/mcp",
      "headers": {
        "x-api-key": "YOUR_API_KEY"
      }
    }
  }
}
```

### Option 3: Environment Variables (Recommended for Teams)

Use environment variable expansion to avoid committing secrets:

```json
{
  "mcpServers": {
    "the0": {
      "type": "http",
      "url": "https://api.the0.dev/mcp",
      "headers": {
        "x-api-key": "${THE0_API_KEY}"
      }
    }
  }
}
```

Then set the environment variable before starting Claude Code:

```bash
export THE0_API_KEY="your-api-key-here"
claude
```

---

## Available Tools

the0 MCP server provides 11 tools:

### Authentication

| Tool | Description |
|------|-------------|
| `auth_status` | Check if your API key is valid |

### Bot Instance Management

| Tool | Description |
|------|-------------|
| `bot_list` | List all your deployed bot instances |
| `bot_get` | Get details of a specific bot instance |
| `bot_deploy` | Deploy a new bot instance |
| `bot_update` | Update an existing bot instance |
| `bot_delete` | Delete a bot instance |

### Logs

| Tool | Description |
|------|-------------|
| `logs_get` | Get execution logs for a bot |
| `logs_summary` | Get log statistics (error counts, date range) |

### Custom Bot Registry

| Tool | Description |
|------|-------------|
| `custom_bot_list` | List available custom bots |
| `custom_bot_get` | Get custom bot details |
| `custom_bot_schema` | Get JSON schema for bot configuration |

---

## Example Usage

Once configured, interact with the0 using natural language:

**List bots:**
```
"list my bots"
"show me all deployed bots"
```

**Deploy a bot:**
```
"deploy a new MoE bot for AAPL"
"what custom bots are available?"
"show me the schema for alpaca-mixture-of-experts"
```

**Check logs:**
```
"get logs for bot abc123"
"show me today's logs for my META bot"
```

---

## Bot Deployment

When deploying via MCP, the config must include:

- `type`: Bot type (e.g., `scheduled/alpaca-mixture-of-experts`)
- `version`: Version of the custom bot
- `schedule`: Cron expression (e.g., `0 14 * * 1-5` for 9 AM EST weekdays)
- `bot`: Bot-specific configuration including API credentials

Example deployment request:
```
Deploy a new bot called "AAPL MoE" using alpaca-mixture-of-experts version 1.0.0,
scheduled at 9 AM EST on weekdays, trading AAPL with paper mode enabled.
```

---

## Troubleshooting

### "Authentication required" Error

The API key header is not being sent. Verify your configuration:

```bash
# Check current MCP servers
claude mcp list

# Remove and re-add with correct header
claude mcp remove the0
claude mcp add the0 --transport http https://api.the0.dev/mcp \
  --header "x-api-key: YOUR_API_KEY"

# Restart Claude Code
exit
claude
```

### Duplicate Configuration Error

If you see "MCP server exists in multiple scopes":

```bash
# Remove from both scopes
claude mcp remove the0 -s local
claude mcp remove the0 -s project

# Re-add
claude mcp add the0 --transport http https://api.the0.dev/mcp \
  --header "x-api-key: YOUR_API_KEY"
```

### Test MCP Endpoint Directly

```bash
curl -X POST https://api.the0.dev/mcp \
  -H "Content-Type: application/json" \
  -H "x-api-key: YOUR_API_KEY" \
  -d '{"jsonrpc": "2.0", "id": 1, "method": "tools/list"}'
```

### Common Errors

| Error | Cause | Solution |
|-------|-------|----------|
| "Authentication required" | Missing x-api-key header | Re-add MCP with `--header` flag |
| "API key is invalid" | Wrong or expired key | Generate new key from dashboard |
| "Custom bot not found" | Wrong type/version in config | Check `custom_bot_list` for available bots |
| "Bot not found" | Invalid bot_id | Use `bot_list` to get correct IDs |

---

## Security

- **User Isolation**: Each user can only access their own bots
- **API Key Authentication**: All operations require a valid API key
- **Secrets**: Never commit API keys to version control; use environment variables
- **Masked Logs**: Sensitive data in bot logs is automatically masked
