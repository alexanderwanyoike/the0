---
title: "Local Getting Started"
description: "Start and operate a local the0 stack with the CLI"
tags: ["deployment", "local", "docker-compose"]
order: 1
---

# Local Getting Started

Use `the0 local` when you want a full platform stack on one machine. The CLI writes Docker Compose files to `~/.the0/compose/`, manages the root admin credentials, and starts the API, frontend, docs, runtime workers, and backing services.

## Initialize

Prebuilt images are the fastest path:

```bash
the0 local init --email you@example.com --password testuse123
```

If you omit either flag, the CLI prompts interactively. The password prompt does not echo input when stdin is a terminal.

To build the stack from a local clone instead of GHCR images:

```bash
the0 local init --source /path/to/the0 --email you@example.com --password testuse123
```

Use `--source` without a path to use the current directory.

## Start And Sign In

```bash
the0 local start
```

When startup completes, open:

| Service | URL |
|---------|-----|
| Frontend | http://localhost:3001 |
| API | http://localhost:3000 |
| Docs | http://localhost:3002 |
| MinIO Console | http://localhost:9001 |

Sign in at `http://localhost:3001/login` with the root admin email and password configured during `init`.

## Check Status

```bash
the0 local status
```

Use this after startup or restart to confirm container state and health. If a service is unhealthy, inspect that service's logs first.

## View Logs

```bash
the0 local logs          # all services
the0 local logs api      # API service only
the0 local logs -f api   # follow API logs
```

Friendly service names such as `api`, `frontend`, `runner`, and `scheduler` are resolved by the CLI.

## Rotate Root Admin Password

The local root admin is deployment-managed. Rotate it by updating Compose configuration and restarting the API:

```bash
the0 local reset-admin-password new-password
```

To change both the email and password:

```bash
the0 local admin set --email you@example.com
```

If `--password` is omitted, the CLI prompts for it. The API validates the configured password policy at startup, so check `the0 local logs api` if the API does not become healthy after rotation.

## Development Mode

For frontend and API hot reload, initialize from source and start dev mode:

```bash
the0 local init --source /path/to/the0 --email you@example.com --password testuse123
the0 local dev
```

Dev mode starts infrastructure and runtime services normally, then runs the API and frontend with source mounts for faster iteration.

