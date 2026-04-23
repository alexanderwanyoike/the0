---
title: "Frontend Dashboard"
description: "Render your custom bot dashboard locally against live events"
order: 3
---

# Frontend Dashboard

`the0 dev --frontend` serves your custom dashboard at `http://127.0.0.1:<port>` wired to the same event stream your bot is producing.

## How it works

The platform loads custom dashboards by setting React globals (`__THE0_REACT__`, `__THE0_REACT_DOM__`, `__THE0_REACT_JSX__`), mounting a `BotEventsProvider`, and injecting the user's bundle. `the0 dev --frontend` mirrors that: serves a shell with matching globals, mounts a provider that subscribes to a local WebSocket, builds `frontend/index.tsx` with the same `reactGlobalPlugin` production uses, and feeds it live events from your bot.

## Render into the bundle root, not `#root`

**Important.** The dev shell mounts its Provider on `#root` and renders an inner div where your bundle should mount. React context doesn't propagate across separate `ReactDOM.createRoot()` trees — if your bundle creates a root on `#root` directly, `useThe0Events()` returns `null`.

```tsx
// Works in both dev and production.
const rootEl = window.__THE0_DEV_BUNDLE_ROOT__ || document.getElementById('root');
createRoot(rootEl).render(<App />);
```

## Run it

```bash
the0 dev --frontend               # OS-assigned port
the0 dev --frontend --frontend-port 3333
```

The CLI prints a localhost URL. Open it; your dashboard renders with live data.

## Queries

Realtime bots get their in-container query server port forwarded to loopback, and `/query/*` is proxied through the dashboard:

```tsx
fetch('/query/portfolio-summary').then(r => r.json()).then(setData);
```

See [Queries](./queries).

## Entry file

Default: `frontend/index.tsx` at the project root. Custom entry selection is a follow-up.

## Hot reload

`--watch --frontend` together: the bot restarts on source change, the dashboard bundle rebuilds, and the dashboard receives a `restart` sentinel so its provider clears accumulated events.

## Caveats

- Only `/query/*` is proxied. Other `fetch('/...')` calls 404. Mock them in dev or gate on a production flag.
- React is pinned to 18 via unpkg CDN.
- Server binds `127.0.0.1` only and checks WebSocket origins — LAN peers and other browser tabs cannot connect.
