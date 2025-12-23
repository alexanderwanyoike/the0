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
# Example output
* Logging in...
Enter your the0 API key:
* Verifying API key...
v Logged in successfully
```

### Check Status

Verify your API key validity:

```bash
the0 auth status
* Checking authentication status...
v Authenticated
  Connected since: 2025-07-07 23:02:48
```

### Logout

Remove saved API key:

```bash
the0 auth logout
v Logged out successfully
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
# x Authentication failed: API key is invalid or revoked

# Solution:
the0 auth logout
the0 auth login
# Enter your new API key
```

#### Permission Denied

```bash
# If you can't save credentials:
# x Failed to save credentials: permission denied

# Solution: Check directory permissions
ls -la ~/.the0/
# Fix permissions if needed
chmod 700 ~/.the0/
```

---

## Build Secrets

Build secrets allow you to authenticate with private package repositories when deploying custom bots. These are used during the dependency vendoring process.

### Supported Secrets

| Secret Name | Description |
|-------------|-------------|
| `github-token` | GitHub Personal Access Token for accessing private git repositories |
| `pip-index-url` | Private PyPI index URL (include credentials in the URL) |

### Secrets Commands

#### Show Secrets

Display currently configured secrets (tokens are masked for security):

```bash
the0 auth secrets show
```

```bash
# Example output
Build Secrets:
--------------
  github-token:  ghp_...xxxx
  pip-index-url: (not set)

These secrets are used during 'the0 custom-bot deploy' to
authenticate with private package repositories.
```

#### Set a Secret

Configure a build secret:

```bash
the0 auth secrets set <secret-name> <value>
```

**Examples:**

```bash
# Set GitHub Personal Access Token for private repos
the0 auth secrets set github-token ghp_xxxxxxxxxxxxxxxxxxxx

# Set private PyPI index URL (include credentials)
the0 auth secrets set pip-index-url https://user:pass@pypi.example.com/simple/
```

```bash
# Example output
✓ Secret 'github-token' saved
  Value: ghp_...xxxx
```

#### Clear Secrets

Remove all saved build secrets:

```bash
the0 auth secrets clear
```

```bash
# Example output
✓ All build secrets cleared
```

### Storage Location

Build secrets are stored securely in your home directory:

- **Linux/Mac**: `~/.the0/secrets.json`
- **Windows**: `%USERPROFILE%\.the0\secrets.json`

> **Security**: Secrets are stored with restricted file permissions (0600). Keep this file secure and never commit it to version control.

### Use Cases

**Private GitHub Repositories**

If your bot depends on packages from private GitHub repos:

```bash
# Generate a GitHub PAT with repo scope
# Then set it as a secret
the0 auth secrets set github-token ghp_your_token_here
```

**Private PyPI Index**

If your organization uses a private Python package index:

```bash
# Set the full URL including credentials
the0 auth secrets set pip-index-url https://username:password@pypi.mycompany.com/simple/
```

---

## Next Steps

- [Bot Commands](./bot-commands) - Start managing bot instances
- [Custom Bot Commands](./custom-bot-commands) - Deploy your custom bots
