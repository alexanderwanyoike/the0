---
title: "Integrations"
description: "Connect the0 with AI assistants and external tools"
order: 4
---

# Integrations

the0 extends beyond traditional bot development through integrations with AI assistants and developer tools. These integrations enable natural language interactions with the platform, allowing you to deploy bots, monitor executions, and manage your trading infrastructure through conversational interfaces.

## AI Assistant Integration

Modern development workflows increasingly involve AI assistants that can interact with external systems. the0 implements the Model Context Protocol (MCP), an open standard that enables AI assistants like Claude Code to communicate directly with the platform.

With MCP integration, AI assistants gain the ability to:

- Deploy and manage bot instances on your behalf
- Retrieve execution logs and analyze bot behavior
- Explore available custom bots and their configuration schemas
- Update bot configurations and monitor health status

This creates a powerful workflow where you describe what you want in natural language, and the AI assistant handles the API interactions required to accomplish your goals.

## Getting Started

Integrating an AI assistant with the0 requires generating an API key and configuring the assistant to use the0's MCP endpoint. The process varies slightly depending on the tool you're using, but follows a consistent pattern:

1. Generate an API key from [your account settings](https://the0.app/settings/api-settings) or via the CLI with `the0 auth login`
2. Configure your AI tool to connect to the0's MCP server with your API key
3. Verify the connection and begin interacting through natural language

## Available Integrations

### Claude Code (MCP)

[Claude Code](./mcp) provides the deepest integration with the0 through the Model Context Protocol. Claude Code users can manage their entire bot infrastructure through conversation, making it ideal for developers who want to stay in their terminal while interacting with the platform.

The MCP integration exposes 11 tools covering authentication, bot instance management, log retrieval, and custom bot exploration. See the [MCP documentation](./mcp) for detailed setup instructions and usage examples.

## Security Considerations

All integrations authenticate using API keys that are scoped to your account. Each user can only access their own bots and data through any integration. API keys should be treated as sensitive credentials—never commit them to version control, and use environment variables when configuring integrations in shared or team environments.

Sensitive data in bot logs, such as API credentials, is automatically masked before being returned through any integration endpoint.
