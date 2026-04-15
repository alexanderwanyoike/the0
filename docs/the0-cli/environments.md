---
title: "Environments"
description: "Manage named API environments with their own URL and credentials"
tags: ["cli", "environments", "api"]
order: 3
---

# Environments

Named environments let the CLI target multiple the0 API endpoints (for example, a local Docker Compose stack and a production cluster) without rewriting environment variables or overwriting your saved API key.

Each environment stores its own:

- **URL** - e.g. `http://localhost:3000` or `https://api.the0.app`
- **API key** - validated against the URL before it is saved

Environments live in `~/.the0/environments.json`.

## Add an Environment

```bash
the0 env add local --url http://localhost:3000
the0 env add prod  --url https://api.the0.app --api-key the0_xxxxxxxxxxxx
```

If `--api-key` is omitted, the CLI prompts for it. The key is tested against the URL and only persisted if the API accepts it, so typos fail fast.

The first environment you add becomes the active one automatically. If you previously logged in with `the0 auth login`, that existing credential is migrated into a `default` environment so nothing is lost.

## Switch the Active Environment

```bash
the0 env use prod
```

All subsequent commands use the new active environment.

## One-off Override

Every command accepts a global `--env <name>` flag that overrides the active environment for that single invocation:

```bash
the0 bot list --env prod
the0 custom-bot deploy --env local
```

## List Environments

```bash
the0 env list
```

```
ACTIVE  NAME   URL
*       local  http://localhost:3000
        prod   https://api.the0.app
```

API keys are never printed.

## Show the Active Environment

```bash
the0 env current
```

## Remove an Environment

```bash
the0 env remove prod
```

If you remove the active environment, no environment is active until you run `the0 env use <name>`.

## Resolution Precedence

When the CLI needs a URL and API key, it resolves them in this order (highest wins):

1. `--env <name>` flag passed on the command
2. Active environment in `environments.json`
3. Legacy `THE0_API_URL` env var + `~/.the0/auth.json` (back-compat for users who haven't run `the0 env add` yet)
4. Default URL `http://localhost:3000`

## Migrating From `THE0_API_URL`

The `THE0_API_URL` environment variable and the old `the0 auth config` command have been replaced by named environments. Existing setups keep working until you run your first `the0 env add`, which migrates your legacy credential into a `default` environment.

## Next Steps

- [Authentication](./authentication) - `auth login` writes to the active environment
- [Installation](./installation) - Install the CLI
