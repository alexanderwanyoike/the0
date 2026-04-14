---
title: "Authentication"
description: "Setting up and managing API authentication"
tags: ["cli", "authentication", "api"]
order: 2
---

# Authentication

The CLI requires an API key to interact with the0 platform. You can generate an API key from your account settings in the platform dashboard.

## Login

Set or update your API key:

```bash
the0 auth login
```

The CLI prompts for your API key and validates it against the platform:

```
Enter your the0 API key:
Verifying API key...
Logged in successfully
```

## Check Status

Verify your current authentication:

```bash
the0 auth status
```

```
Checking authentication status...
Authenticated
Connected since: 2025-01-15 10:30:00
```

## Logout

Remove saved credentials:

```bash
the0 auth logout
```

## Pointing at a Different API

Use named environments to target multiple API endpoints (local dev, staging, prod) without clobbering credentials. See [Environments](./environments) for the full reference.

```bash
the0 env add local --url http://localhost:3000
the0 env add prod  --url https://api.the0.app
the0 env use prod
```

`the0 auth login` writes the key to the active environment. Use `--env <name>` on any command to override for a single invocation.

## Credential Storage

The CLI stores credentials under `~/.the0/`:

- `~/.the0/environments.json` - named environments and their API keys (created on first `the0 env add`).
- `~/.the0/auth.json` - legacy single-environment credential file. Still honoured when no named environments exist, so existing users are not logged out.

If you encounter permission errors, ensure the directory exists with appropriate permissions:

```bash
mkdir -p ~/.the0
chmod 700 ~/.the0
```

## Next Steps

- [Environments](./environments) - Manage multiple API endpoints
- [Secrets](./secrets) - Configure private dependency access
- [Custom Bot Commands](./custom-bot-commands) - Deploy your first bot
