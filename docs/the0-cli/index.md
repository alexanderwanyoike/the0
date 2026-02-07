---
title: "the0 CLI"
description: "Command-line interface for the0 platform"
order: 3
---

# the0 CLI

The `the0` CLI is the primary interface for deploying custom bots and managing bot instances on the0 platform. It handles authentication, dependency vendoring, compilation, and deployment.

## Command Groups

The CLI is organized into the following command groups:

**auth** - Manage authentication and configuration

```bash
the0 auth login          # Set API key
the0 auth logout         # Remove API key
the0 auth status         # Check authentication status
the0 auth config <url>   # Configure API endpoint
the0 auth secrets        # Manage build secrets
```

**custom-bot** - Deploy and manage custom bot definitions

```bash
the0 custom-bot deploy           # Deploy from current directory
the0 custom-bot list             # List deployed custom bots
the0 custom-bot versions <name>  # List versions of a custom bot
the0 custom-bot schema <version> <name>  # Get schema for a custom bot
```

**bot** - Manage bot instances

```bash
the0 bot deploy <config.json>    # Deploy a bot instance
the0 bot list                    # List bot instances
the0 bot update <id> <config>    # Update instance configuration
the0 bot delete <id>             # Delete an instance
the0 bot logs <id>               # View instance logs
```

**local** - Manage local Docker Compose development environment

```bash
the0 local init            # Initialize local environment
the0 local up              # Start all services
the0 local down            # Stop all services
the0 local status          # Show service status
the0 local logs [service]  # View service logs
the0 local dev             # Start in development mode
```

**update** - Self-update the CLI

```bash
the0 update                # Update to latest version
```

## Global Flags

All commands support the following flags:

```bash
-v, --verbose    Enable verbose output
-h, --help       Show help for any command
```

## Documentation

- [Installation](./installation) - Install the CLI
- [Authentication](./authentication) - Configure API credentials
- [Bot Commands](./bot-commands) - Bot instance management reference
- [Custom Bot Commands](./custom-bot-commands) - Custom bot deployment reference
- [Local Development](./local-development) - Local environment management reference
- [Secrets](./secrets) - Configure private dependencies
