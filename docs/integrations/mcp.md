---
title: "MCP (Model Context Protocol)"
description: "Enable AI assistants to interact with the0 platform"
tags: ["integrations", "mcp", "ai", "claude"]
order: 1
---

# MCP (Model Context Protocol)

the0 implements the Model Context Protocol (MCP), an open standard that enables AI assistants to interact with external systems through a well-defined interface. With the0's MCP server, AI assistants like Claude Code can deploy bots, retrieve logs, and manage your trading infrastructure through natural language conversations.

## Understanding MCP

The Model Context Protocol standardizes how AI assistants connect to external data sources and tools. Rather than building custom integrations for each AI platform, MCP provides a universal interface that any compliant assistant can use. the0's MCP server exposes the platform's core functionality through this protocol, enabling AI assistants to perform the same operations available through the CLI and web dashboard.

Through MCP, AI assistants can:

- Deploy, update, and delete bot instances
- Retrieve execution logs and analyze bot behavior
- List available custom bots and their configuration schemas
- Check authentication status and connection health

## Prerequisites

Before configuring MCP integration, ensure you have:

- **API Key**: Generated from your the0 dashboard or via CLI (`the0 auth login`)
- **Claude Code**: Installed from [claude.ai/download](https://claude.ai/download)

## Configuration

### Adding the MCP Server

The MCP server authenticates using your API key passed via the `x-api-key` header. Configure Claude Code to connect to the0's MCP endpoint using the CLI:

```bash
claude mcp add the0 --transport http https://api.the0.dev/mcp \
  --header "x-api-key: YOUR_API_KEY"
```

For local development against a self-hosted instance:

```bash
claude mcp add the0 --transport http http://localhost:3000/mcp \
  --header "x-api-key: YOUR_API_KEY"
```

After adding or modifying MCP configuration, restart Claude Code for the changes to take effect:

```bash
exit  # or Ctrl+D
claude
```

Verify the connection is active:

```bash
claude mcp list
```

The output should show the0 server as connected:

```
the0: https://api.the0.dev/mcp (HTTP) - ✓ Connected
```

Test the integration by asking Claude Code to "list my bots"—it should invoke the MCP tools and return your bot instances.

### Configuration File Approach

Instead of CLI flags, you can configure MCP servers through a `.mcp.json` file in your project root:

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

For team environments where you don't want to commit secrets, use environment variable expansion:

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

Set the environment variable before starting Claude Code:

```bash
export THE0_API_KEY="your-api-key-here"
claude
```

## Available Tools

the0's MCP server exposes 11 tools organized into three categories:

### Authentication

| Tool | Description | Parameters |
|------|-------------|------------|
| `auth_status` | Check if API key is valid and get connection status | None |

### Bot Instance Management

| Tool | Description | Parameters |
|------|-------------|------------|
| `bot_list` | List all deployed bot instances | None |
| `bot_get` | Get details of a specific bot instance | `bot_id` |
| `bot_deploy` | Deploy a new bot instance | `config` (name, type, version, settings) |
| `bot_update` | Update an existing bot instance | `bot_id`, `config` |
| `bot_delete` | Delete a bot instance | `bot_id` |

### Logs

| Tool | Description | Parameters |
|------|-------------|------------|
| `logs_get` | Get execution logs for a bot | `bot_id`, optional: `date`, `date_range`, `limit` |
| `logs_summary` | Get log statistics | `bot_id` |

### Custom Bots

| Tool | Description | Parameters |
|------|-------------|------------|
| `custom_bot_list` | List all available custom bots | None |
| `custom_bot_get` | Get details of a custom bot | `name`, optional: `version` |
| `custom_bot_schema` | Get JSON schema for configuring a custom bot | `name`, optional: `version` |

## Usage Examples

Once configured, interact with the0 using natural language. The AI assistant translates your requests into the appropriate MCP tool calls.

### Managing Bots

```
"list my bots"
"show me all deployed bots"
"get details for bot abc123"
```

### Deploying Bots

```
"what custom bots are available?"
"show me the schema for alpaca-mixture-of-experts"
"deploy a new SMA bot for AAPL"
```

### Analyzing Logs

```
"get logs for bot abc123"
"show me today's logs for my META bot"
"summarize the error logs for my portfolio tracker"
```

## Bot Deployment via MCP

When deploying through MCP, provide the AI assistant with the necessary configuration details. The assistant will construct the appropriate deployment request.

A typical deployment request might look like:

```
Deploy a new bot called "AAPL Monitor" using sma-crossover version 1.0.0,
scheduled at 9 AM EST on weekdays, monitoring AAPL with a 5-day short SMA
and 20-day long SMA.
```

The assistant will use the `custom_bot_schema` tool to understand the configuration options, then construct a valid deployment using `bot_deploy`.

Deployment configurations include:

- `type`: Bot type (e.g., `scheduled/sma-crossover`, `realtime/price-alerts`)
- `version`: Version of the custom bot to deploy
- `schedule`: Cron expression for scheduled bots (e.g., `0 14 * * 1-5` for 9 AM EST weekdays)
- `config`: Bot-specific configuration matching the schema

## Troubleshooting

### Authentication Required Error

This error indicates the API key header is not being sent. Verify your configuration and re-add the MCP server:

```bash
claude mcp list
claude mcp remove the0
claude mcp add the0 --transport http https://api.the0.dev/mcp \
  --header "x-api-key: YOUR_API_KEY"
exit
claude
```

### Duplicate Configuration Error

If you see "MCP server exists in multiple scopes", remove the configuration from both scopes:

```bash
claude mcp remove the0 -s local
claude mcp remove the0 -s project
claude mcp add the0 --transport http https://api.the0.dev/mcp \
  --header "x-api-key: YOUR_API_KEY"
```

### Testing the Endpoint Directly

Verify the MCP endpoint is accessible using curl:

```bash
curl -X POST https://api.the0.dev/mcp \
  -H "Content-Type: application/json" \
  -H "x-api-key: YOUR_API_KEY" \
  -d '{"jsonrpc": "2.0", "id": 1, "method": "tools/list"}'
```

### Common Errors

| Error | Cause | Solution |
|-------|-------|----------|
| Authentication required | Missing x-api-key header | Re-add MCP with `--header` flag |
| API key is invalid | Wrong or expired key | Generate new key from dashboard |
| Custom bot not found | Wrong type/version | Use `custom_bot_list` to find available bots |
| Bot not found | Invalid bot_id | Use `bot_list` to get correct IDs |

## Security

MCP integration follows the same security model as all the0 platform access:

- **User Isolation**: Each user can only access their own bots through MCP
- **API Key Authentication**: All operations require a valid API key
- **Secret Protection**: Never commit API keys to version control; use environment variables
- **Log Masking**: Sensitive data in bot logs is automatically masked before being returned
