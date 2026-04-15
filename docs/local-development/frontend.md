---
title: "Frontend Dashboard"
description: "Render your custom bot dashboard locally against live events"
order: 4
---

# Frontend Dashboard

`the0 dev --frontend` serves your custom bot dashboard on a local port, wired to the same event stream your bot is producing. Save a file, both the bot and dashboard react.

## How it works

The platform frontend loads custom bot dashboards by:

1. Setting React globals on `window` (`__THE0_REACT__`, `__THE0_REACT_DOM__`, `__THE0_REACT_JSX__`)
2. Mounting a `BotEventsProvider` that fetches events from the platform API and exposes them via `window.__THE0_EVENTS_CONTEXT__`
3. Injecting the user's bundle via `<script>`

`the0 dev --frontend` mirrors that exact setup locally:

- Serves a tiny HTML shell that sets the same React globals
- Mounts an equivalent provider that subscribes to a local WebSocket
- Runs your bot, forwards every emitted event into that WebSocket
- Builds your `frontend/index.tsx` with the same `reactGlobalPlugin` the production `the0-build` uses

Result: your dashboard runs against the host-page React with the same ambient contract as production, consuming live data from your local bot.

## Run it

```bash
the0 dev --frontend
```

Output:

```
i Detected runtime: python
i Dashboard: http://127.0.0.1:54421
i Running python bot "my-bot" (mode=native)
...
```

Open the URL. Your dashboard renders. As the bot emits metrics, the dashboard updates in real time.

## Custom port

```bash
the0 dev --frontend --frontend-port 3333
```

`--frontend-port 0` (default) lets the OS pick a free port. That's friendlier in multi-project setups.

## Entry file

By default, `the0 dev --frontend` looks for `frontend/index.tsx` at the project root. Custom entry point selection is deferred to a follow-up release; name your entry `frontend/index.tsx` for now.

## Hot reload

Combined with `--watch`, both the bot and the dashboard bundle rebuild on source change:

```bash
the0 dev --watch --frontend
```

The dashboard WebSocket emits a `restart` sentinel on each bot restart, so your provider clears accumulated events and starts fresh with the new run.

## Caveats

- Dashboards that call `fetch('/api/...')` will 404 in dev. The dev shell doesn't proxy platform API calls. Keep dashboards event-stream-only in local mode.
- React versions are pinned to 18 via unpkg CDN. If your bundle targets a different major, override via a custom shell (deferred to a follow-up).
- The WebSocket runs on `localhost` with no auth. Don't expose the dev server to your network.
