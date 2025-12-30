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

## Configure API Endpoint

For self-hosted deployments, configure the API URL:

```bash
the0 auth config https://api.your-domain.com
```

The default endpoint is `http://localhost:3000` for local development. You can also set the `THE0_API_URL` environment variable.

## Credential Storage

The CLI stores credentials in `~/.the0/auth.json`. This file is created automatically on first login.

If you encounter permission errors, ensure the directory exists with appropriate permissions:

```bash
mkdir -p ~/.the0
chmod 700 ~/.the0
```

## Next Steps

- [Secrets](./secrets) - Configure private dependency access
- [Custom Bot Commands](./custom-bot-commands) - Deploy your first bot
