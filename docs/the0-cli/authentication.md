---
title: "Authentication"
description: "Setting up and managing API authentication"
tags: ["cli", "authentication", "api"]
order: 2
---

# Authentication

The the0 CLI requires authentication to interact with the platform. Authentication is managed through API keys that you generate in the platform UI.

---

## Prerequisites

1. **Create an Account**: You must have an active account on the the0 platform [https://the0.com](https://the0.com/register)
2. **Generate API Key**: Navigate to your account [settings](https://the0.dev/settings/api-settings) in the platform UI to generate an API key. Note that you will only ever see the key once, so copy it immediately after generation and store it securely.

> **Important**: Keep your API key private. Treat it like a password. If you suspect it has been compromised, revoke it immediately and generate a new one.

---

## Authentication Commands

### Login

Set or update your API key:

```bash
the0 auth login
```

When prompted, enter your API key:

```bash
# Example interaction
the0 auth login
Jacking into the0...
Enter your the0 API key: # your-api-key-here
✓ Access granted - welcome to the matrix
Ready to deploy trading bots
✓ Connection verified
```

### Check Status

Verify your API key validity:

```bash
the0 auth status
Running system diagnostics...
Connected: 2025-07-07 23:02:48
✓ Connection active
```

### Logout

Remove saved API key:

```bash
the0 auth logout
✓ Access codes wiped
Disconnected from the0
```

---

## Storage Location

The CLI stores your API key securely in your home directory:

- **Linux/Mac**: `~/.the0/auth.json`
- **Windows**: `%USERPROFILE%\.the0\auth.json`

---

## Troubleshooting

### Common Issues

#### Authentication Failed

```bash
# If you see this error:
# ❌ Authentication failed: API key is invalid or revoked

# Solution:
the0 auth logout
the0 auth login
# Enter your new API key
```

#### Permission Denied

```bash
# If you can't save credentials:
# ❌ Failed to save credentials: permission denied

# Solution: Check directory permissions
ls -la ~/.the0/
# Fix permissions if needed
chmod 700 ~/.the0/
```

---

## Next Steps

- [Bot Commands](./bot-commands) - Start managing bot instances
- [Custom Bot Commands](./custom-bot-commands) - Deploy your custom bots
