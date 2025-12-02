# Theo API

Theo API is the backend service for the0 automated trading platform. It is built using the [NestJS](https://nestjs.com/) framework and provides functionality for managing bots and more.

## Features

- **Bot Validation**: Validate bot configurations against schemas retrieved from an external API.
- **Configuration Management**: Centralized configuration using `@nestjs/config`.
- **API Integration**: Communicates with external services using `@nestjs/axios`.
- **MCP Server**: Model Context Protocol server for AI tool integration (Claude Code, etc.).

## MCP Server (Claude Code Integration)

The API includes a built-in MCP (Model Context Protocol) server that enables AI assistants like Claude Code to interact with the0 platform directly.

### Available Tools

| Category | Tool | Description |
|----------|------|-------------|
| **Auth** | `auth_status` | Check API key validity and connection status |
| **Bot Instance** | `bot_list` | List all deployed bot instances |
| | `bot_get` | Get details of a specific bot instance |
| | `bot_deploy` | Deploy a new bot instance |
| | `bot_update` | Update bot instance configuration |
| | `bot_delete` | Delete a bot instance |
| **Logs** | `logs_get` | Get execution logs for a bot |
| | `logs_summary` | Get summarized log statistics |
| **Custom Bot** | `custom_bot_list` | List available custom bots |
| | `custom_bot_get` | Get custom bot details |
| | `custom_bot_schema` | Get JSON schema for bot configuration |

### Claude Code Configuration

#### Option 1: CLI Command

```bash
claude mcp add the0 --transport http http://localhost:3000/mcp
```

#### Option 2: Configuration File

Add to your `.mcp.json` file:

```json
{
  "mcpServers": {
    "the0": {
      "url": "http://localhost:3000/mcp",
      "transport": "http"
    }
  }
}
```

For production deployments, replace `http://localhost:3000` with your API URL.

### Authentication

MCP tools that modify data require authentication via API key. Include the `x-api-key` header with your requests:

```bash
# Example: Test MCP endpoint
curl -X POST http://localhost:3000/mcp \
  -H "Content-Type: application/json" \
  -H "x-api-key: your-api-key" \
  -d '{"jsonrpc": "2.0", "id": 1, "method": "tools/list"}'
```

### Example Usage with Claude Code

Once configured, you can ask Claude Code to:

- "List my deployed bots"
- "Show me the logs for my trading bot"
- "What custom bots are available?"
- "Get the configuration schema for bot-name"
- "Deploy a new scheduled bot with this configuration"

## Prerequisites

- **Node.js**: Version 18 or higher
- **Yarn**: Package manager
- **Firebase CLI**: For deployment and emulation

## Installation

Clone the repository and install dependencies:

```bash
$ git clone <repository-url>
$ cd theo-api
$ yarn install
```

## Environment Variables

The application uses environment variables for configuration. Create a `.env` file in the root directory and define the following variables:

```env
botManagerApi.URL=http://example.com
botManagerApi.APIKey=your-api-key
```

## Running the Application

### Development

Start the application in development mode:

```bash
$ yarn run start:dev
```

### Production

Build and start the application in production mode:

```bash
$ yarn run build
$ yarn run start:prod
```

## Testing

Run unit tests:

```bash
$ yarn run test
```

Run end-to-end (e2e) tests:

```bash
$ yarn run test:e2e
```

Generate test coverage reports:

```bash
$ yarn run test:cov
```

## Project Structure

- **`src/bot`**: Contains bot-related logic, including validation (`bot.validator.ts`).
- **`src/common`**: Shared utilities and types.
- **`test`**: Unit and e2e tests.

## Deployment

Deploy the application to Firebase:

```bash
$ yarn run deploy
```

## Contributing

1. Fork the repository.
2. Create a new branch for your feature or bugfix.
3. Submit a pull request.

## License

This project is licensed under the **UNLICENSED** license. See the `LICENSE` file for details.

## Support

For issues or questions, please open an issue in the repository.

## Acknowledgments

- Built with [NestJS](https://nestjs.com/).
- Uses [Ajv](https://ajv.js.org/) for JSON schema validation.


