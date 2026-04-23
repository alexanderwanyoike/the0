---
title: "Queries"
description: "Exercise your realtime bot's query endpoints locally"
order: 4
---

# Queries

Realtime bots expose a query HTTP server on port `9476` inside the container. The dashboard hits it with `fetch('/query/<path>')` requests to render derived data without touching state. `the0 dev` makes that flow work end-to-end on your laptop.

## Requirements

Your `config.json`:

```json
{
  "name": "my-bot",
  "type": "realtime/portfolio-tracker",
  "version": "0.1.0"
}
```

A `query.py` (or `query.js`) alongside `main.py`:

```python
# query.py
from the0 import query

def status(params):
    return {"ok": True, "params": params}

if __name__ == "__main__":
    query.run({"/status": status})
```

## Run it

```bash
the0 dev --frontend
```

When the CLI sees `"type": "realtime/..."` and a `query.*` file, it:

1. Lets the runtime's query server subprocess start inside the container (no `--skip-query-server`).
2. Forwards container port `9476` to `127.0.0.1:9476` on the host.
3. Proxies `GET /query/<path>` on the dashboard URL to `http://127.0.0.1:9476/<path>`.

From the dashboard:

```tsx
useEffect(() => {
  fetch('/query/status?limit=10')
    .then(r => r.json())
    .then(setStatus);
}, []);
```

The `/query` prefix is stripped before the request reaches the bot's query router — the bot sees the same URL structure production delivers.

## Without `--frontend`

The proxy lives in the frontend server, so `the0 dev` (no `--frontend`) does not expose `/query/*`. You can still hit the container directly:

```bash
curl http://127.0.0.1:9476/status
```

This is useful for scripted query smoke-tests.

## Scheduled bots

Scheduled bots (`"type": "scheduled/..."`) do not expose a live query server; queries run as ephemeral containers in production. `the0 dev` won't forward port `9476` for scheduled bots — there's nothing listening.

## Troubleshooting

- **`connect: connection refused` hitting `/query/*`**: the bot's query server hasn't started. Check that `config.json` has `"type": "realtime/..."` and that `query.py`/`query.js` exists at the project root.
- **Dashboard `fetch` 404s**: verify the request path starts with `/query/`. Other paths aren't proxied.
- **Port conflict**: something else on your host is using `9476`. Stop it, or (phase 2) a `--query-port` flag will let you override.
