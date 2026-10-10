---
title: "Macro Event Guard"
description: "A scheduled Python bot that flags upcoming market-moving macro releases"
tags: ["recipes", "python", "scheduled", "macro", "risk"]
order: 1
---

# Macro Event Guard

::: info Community recipe
Contributed by [Robert Tidball](https://github.com/roberttidball) of
[FXMacroData](https://fxmacrodata.com) in
[#385](https://github.com/alexanderwanyoike/the0/pull/385). Source:
[`example-bots/python-macro-event-guard`](https://github.com/alexanderwanyoike/the0/tree/dev/example-bots/python-macro-event-guard).
:::

Prices can gap and spreads widen around top-tier macro releases like CPI,
non-farm payrolls and central-bank rate decisions. Many strategies are better
off standing aside for a few minutes either side of them. This scheduled bot
reads the official release calendar from FXMacroData and tells you when one of
those windows is open or coming up.

USD works without an API key, so the recipe runs out of the box. Other
currencies (EUR, GBP, JPY, AUD, CAD, CHF and more) need an FXMacroData key.

## How It Works

Every 15 minutes the bot:

1. Reads the release calendar for each configured currency
2. Keeps releases at or above the chosen market tier within the lookahead window
3. Emits one `macro_event` metric per upcoming release and one `risk_window` metric
4. Stores the upcoming list in state so the query handlers can serve it

A currency that fails to load is reported in the run result and skipped. The
run only fails if every currency fails.

## Metrics

| Metric | Fields |
|--------|--------|
| `macro_event` | `currency`, `release`, `name`, `time_utc`, `market_tier`, `minutes_until` |
| `risk_window` | `active`, `events` (releases inside the window), `next_event`, `next_in_minutes` |

`risk_window.active` is `true` when any release is within `guard_minutes` of
now, either before or after its scheduled time.

```json
{"_metric": "macro_event", "currency": "USD", "release": "non_farm_payrolls", "name": "Non-Farm Payrolls", "time_utc": "2026-10-09T12:30:00+00:00", "market_tier": 1, "minutes_until": 912.0}
{"_metric": "risk_window", "active": false, "events": [], "next_event": "USD Non-Farm Payrolls", "next_in_minutes": 912.0}
```

## Configuration

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `currencies` | array | `["USD"]` | Calendars to watch |
| `min_tier` | integer | `1` | Least important tier to include: `1` is top-tier only, `3` is everything |
| `lookahead_hours` | number | `24` | How far ahead to list releases |
| `guard_minutes` | number | `30` | Minutes either side of a release that count as a risk window |
| `api_key` | string | `""` | FXMacroData API key, only needed for non-USD currencies |

The key is sent only in the `X-API-Key` header. The bot refuses redirects so
the header cannot be forwarded to another host, and the key never appears in
error messages or in the `/status` query response.

## Deploying

From the recipe directory, deploy the custom bot and then an instance of it:

```bash
cd example-bots/python-macro-event-guard
the0 custom-bot deploy
the0 bot deploy config.json
```

Watch the runs come in:

```bash
the0 bot logs <bot_id> -w
```

## Queries

The bot defines two [query handlers](/custom-bot-development/queries) that read
from the state written by the last run:

```bash
# Upcoming releases, soonest first
the0 bot query <bot_id> /events

# Only one currency
the0 bot query <bot_id> /events --params '{"currency": "USD"}'

# Configuration (without the API key) and a run summary
the0 bot query <bot_id> /status
```

## Adapting It

- **Gate your own strategy.** Copy `fetch_calendar` and `to_event` into a
  trading bot and skip new entries while a release is inside the window.
- **Widen the net.** Raise `min_tier` to `2` or `3` and add currencies to cover
  the markets you trade.
- **Add a dashboard.** The `macro_event` metrics carry everything a countdown
  view needs. See [Custom Frontends](/custom-bot-development/custom-frontends).
