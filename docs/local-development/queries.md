---
title: "Queries"
description: "Exercise realtime-bot query endpoints locally"
order: 4
---

# Queries

Realtime bots expose a query server on port `9476` inside the container. `the0 dev` forwards that port and proxies `/query/*` through the dashboard so the full bot ↔ dashboard ↔ query flow is testable locally.

## Requirements

```json
// config.json
{"name": "my-bot", "type": "realtime/portfolio-tracker", "version": "0.1.0"}
```

Add a `query.py` (or `query.js`) next to `main.py`:

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

The CLI sees `"type": "realtime/..."` + `query.py`, forwards container port `9476` to loopback, and registers a `/query/*` reverse proxy on the dashboard. From your dashboard:

```tsx
useEffect(() => {
  fetch('/query/status?limit=10').then(r => r.json()).then(setStatus);
}, []);
```

The `/query` prefix is stripped before the request reaches the bot's SDK-owned router.

## Without `--frontend`

The proxy lives in the frontend server. Without it, hit the port directly:

```bash
curl http://127.0.0.1:9476/status
```

Useful for scripted smoke-tests.

## Troubleshooting

- **`connection refused`**: `config.json` isn't `"type": "realtime/..."` or no `query.*` file at project root.
- **404**: dashboard `fetch` path doesn't start with `/query/`. Only that prefix is proxied.
- **Scheduled bots**: no query server runs; queries use ephemeral containers in production and aren't supported in v1 `the0 dev`.
