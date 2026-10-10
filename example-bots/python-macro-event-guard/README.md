# Macro Event Guard Bot

A scheduled bot that reads the official macro release calendar and flags when a
market-moving release (CPI, payrolls, central-bank decisions and so on) is
close. Use it as a "stand aside" signal for other bots, or just as a dashboard
of what is coming up.

It uses real data from the [FXMacroData](https://fxmacrodata.com/?utm_source=github&utm_medium=referral&utm_campaign=the0&utm_content=readme)
release calendar. USD works without an API key. Other currencies (EUR, GBP,
JPY, AUD, CAD, CHF and more) need a key in the `api_key` config field.

## What It Does

On each scheduled run, this bot:
1. Reads the release calendar for each configured currency
2. Keeps releases at or above the chosen market tier within the lookahead window
3. Emits a `macro_event` metric per upcoming release and one `risk_window` metric
4. Stores the upcoming list in state for the query endpoints

## Metrics Emitted

| Metric | Description |
|--------|-------------|
| `macro_event` | One upcoming release: currency, release, name, UTC time, market tier, minutes until |
| `risk_window` | Whether any release is within `guard_minutes` of now, plus the next release |

## Configuration

| Parameter | Type | Default | Description |
|-----------|------|---------|-------------|
| `currencies` | array | ["USD"] | Calendars to watch |
| `min_tier` | integer | 1 | Highest market tier to include (1 = top-tier only) |
| `lookahead_hours` | number | 24 | How far ahead to list releases |
| `guard_minutes` | number | 30 | Minutes either side of a release that count as a risk window |
| `api_key` | string | "" | FXMacroData API key, only needed for non-USD currencies |

The key is sent in the `X-API-Key` header and never placed in the URL or in
error messages. Redirects are not followed, so the key cannot be forwarded to
another host.

## Queries

```bash
the0 bot query <bot_id> /events
the0 bot query <bot_id> /events --params '{"currency": "USD"}'
the0 bot query <bot_id> /status
```

## Running Locally

```bash
pip install -r requirements.txt
python main.py
```

## Deploying

```bash
the0 custom-bot deploy
```

## Example Output

```json
{"_metric": "macro_event", "currency": "USD", "release": "non_farm_payrolls", "name": "Non-Farm Payrolls", "time_utc": "2026-10-09T12:30:00+00:00", "market_tier": 1, "minutes_until": 912.0}
{"_metric": "risk_window", "active": false, "events": [], "next_event": "USD Non-Farm Payrolls", "next_in_minutes": 912.0}
```
