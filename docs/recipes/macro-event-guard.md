---
title: "Macro Event Guard"
description: "Build a scheduled Python bot that flags upcoming market-moving macro releases"
tags: ["recipes", "python", "scheduled", "macro", "risk"]
order: 1
---

# Macro Event Guard

::: info Community recipe
Thanks to [Robert Tidball](https://github.com/roberttidball) of
[FXMacroData](https://fxmacrodata.com), who contributed this bot in
[#385](https://github.com/alexanderwanyoike/the0/pull/385). The full source is in
[`example-bots/python-macro-event-guard`](https://github.com/alexanderwanyoike/the0/tree/dev/example-bots/python-macro-event-guard).
:::

Prices can gap and spreads widen around top-tier macro releases like CPI,
non-farm payrolls and central-bank rate decisions. Many strategies are better
off standing aside for a few minutes either side of them.

In this recipe you will build a scheduled bot that reads the official release
calendar from FXMacroData every 15 minutes and reports whether one of those
windows is open or coming up. Along the way you will see how to:

- Declare a scheduled bot with a query entrypoint
- Validate user configuration with a JSON schema
- Call an external API without leaking credentials
- Normalise data you do not control
- Emit metrics that a dashboard or another process can act on
- Share state between a bot run and its query handlers

USD calendars work without an API key, so you can follow along without signing
up for anything.

## Prerequisites

- Python 3.11
- The [the0 CLI](/the0-cli/installation), authenticated against your instance
- Familiarity with the [Python quick start](/custom-bot-development/python-quick-start)

## Project Layout

```
python-macro-event-guard/
├── bot-config.yaml      # What the bot is and how to run it
├── bot-schema.json      # The configuration users can set
├── config.json          # A ready-to-deploy instance configuration
├── main.py              # The scheduled run
├── query.py             # Read-only query handlers
├── requirements.txt
└── README.md
```

There is no `frontend/` directory. The bot's metrics show up in the default
dashboard, and you can add a [custom frontend](/custom-bot-development/custom-frontends)
later.

## Step 1: Declare the Bot

`bot-config.yaml` tells the platform what kind of bot this is and which files
to run:

```yaml
name: macro-event-guard
description: "Watches the official macro release calendar and flags when a top-tier release is close"
version: 1.0.0
author: "the0"
type: scheduled
runtime: python3.11

entrypoints:
  bot: main.py
  query: query.py

schema:
  bot: bot-schema.json

readme: README.md

hasFrontend: false
```

Two choices matter here:

- **`type: scheduled`.** The release calendar changes slowly, so there is no
  reason to hold a connection open. A scheduled bot starts, does its work,
  writes state and exits. The schedule itself lives in the instance
  configuration, so each deployment can choose its own cadence.
- **`entrypoints.query`.** Adding a query entrypoint lets you ask the bot
  questions between runs. Because a scheduled bot is not running most of the
  time, its query handlers answer from the state the last run left behind.
  You will write them in [Step 6](#step-6-answer-queries-from-state).

See [Configuration](/custom-bot-development/configuration) and
[Bot Types](/custom-bot-development/bot-types) for every option.

## Step 2: Define the Configuration Schema

`bot-schema.json` describes what a user can configure when they deploy an
instance. The platform validates instance configs against it, so the bot can
trust the shape of what it receives.

```json
{
  "$schema": "http://json-schema.org/draft-07/schema#",
  "type": "object",
  "properties": {
    "currencies": {
      "type": "array",
      "items": { "type": "string", "pattern": "^[A-Za-z]{3}$" },
      "default": ["USD"]
    },
    "min_tier": { "type": "integer", "default": 1, "minimum": 1, "maximum": 3 },
    "lookahead_hours": { "type": "number", "default": 24, "minimum": 1, "maximum": 336 },
    "guard_minutes": { "type": "number", "default": 30, "minimum": 0, "maximum": 240 },
    "api_key": { "type": "string", "default": "" }
  },
  "additionalProperties": false
}
```

The schema does real work:

- **Patterns and bounds stop bad input early.** A currency like `"US"` or a
  lookahead of 10 years is rejected at deploy time, not halfway through a run.
- **Unknown keys are dropped, not rejected.** With `additionalProperties:
  false`, the platform strips keys the schema does not declare before
  validating. A misspelt `guard_minuts` is ignored and the bot falls back to
  the default, so double-check field names.
- **`min_tier` counts downwards.** FXMacroData ranks releases from tier 1 (most
  market-moving) to tier 3. `min_tier: 1` keeps only tier 1, and `min_tier: 3`
  keeps everything.

The sample `config.json` is an instance of this schema plus the fields every
bot instance needs:

```json
{
  "name": "my-macro-event-guard",
  "type": "scheduled/macro-event-guard",
  "version": "1.0.0",
  "schedule": "*/15 * * * *",
  "currencies": ["USD"],
  "min_tier": 1,
  "lookahead_hours": 24,
  "guard_minutes": 30
}
```

`type` combines the bot type with the `name` from `bot-config.yaml`, and
`schedule` is a standard cron expression.

## Step 3: Fetch the Calendar Safely

Every external call is a place for things to go wrong: the network fails, the
API changes shape, or a credential ends up somewhere it should not. `main.py`
puts all of that in one function with a single exception type, so the rest of
the bot only has to handle `CalendarError`:

```python
CALENDAR_URL = "https://api.fxmacrodata.com/v1/calendar/{currency}"


class CalendarError(Exception):
    """Raised when the calendar cannot be read. Never carries the API key."""


def fetch_calendar(currency: str, api_key=None, timeout: float = 15) -> list:
    code = currency.strip().lower()
    if len(code) != 3 or not code.isalpha():
        raise CalendarError(f"invalid currency code {currency!r}")

    headers = {"Accept": "application/json"}
    if api_key:
        headers["X-API-Key"] = api_key

    try:
        response = requests.get(
            CALENDAR_URL.format(currency=code),
            headers=headers,
            timeout=timeout,
            allow_redirects=False,
        )
    except requests.RequestException as exc:
        raise CalendarError(f"{code.upper()}: request failed ({type(exc).__name__})") from None

    if response.status_code != 200:
        raise CalendarError(f"{code.upper()}: HTTP {response.status_code}")
    try:
        payload = response.json()
    except ValueError:
        raise CalendarError(f"{code.upper()}: response is not JSON") from None
    if not isinstance(payload, dict) or not isinstance(payload.get("data"), list):
        detail = payload.get("detail") if isinstance(payload, dict) else None
        raise CalendarError(f"{code.upper()}: unexpected response {detail or ''}".strip())
    return [row for row in payload["data"] if isinstance(row, dict)]
```

The details are worth copying into any bot that holds a credential:

- **The key only goes in a header.** URLs end up in proxy logs, error messages
  and tracebacks. Headers usually do not.
- **Redirects are refused.** On a redirect to another host, `requests` strips
  the `Authorization` header but forwards custom headers like `X-API-Key`.
  Refusing redirects means the key can only ever reach the host you called.
- **Errors are rebuilt, not re-raised.** `from None` drops the original
  exception chain, and each message names only the currency and the failure
  type. The original exception could include request details, and anything
  the bot logs is stored and shown on its dashboard.
- **The response shape is checked before use.** An outage page, a rate-limit
  message or an API change becomes a clear one-line error instead of a
  `KeyError` deep in the run.
- **A timeout is always set.** Without one, a hung connection would hold the
  scheduled run open indefinitely.

The key itself is cleaned once at the start of the run. `requests` rejects
header values that contain whitespace or control characters, and its error
message includes the value, so the bot rejects such keys first with a message
of its own:

```python
def clean_api_key(api_key):
    if not api_key:
        return None
    api_key = str(api_key).strip()
    if not api_key or any(ch.isspace() or ord(ch) < 32 for ch in api_key):
        raise CalendarError("api_key contains whitespace or control characters")
    return api_key
```

## Step 4: Normalise Each Release

The calendar returns rows the bot does not control. `to_event` turns one row
into the shape the bot emits, or returns `None` if the row is unusable:

```python
def to_event(currency: str, row: dict, now: datetime):
    epoch = row.get("announcement_datetime")
    tier = row.get("market_tier")
    if isinstance(epoch, bool) or not isinstance(epoch, (int, float)):
        return None
    if isinstance(tier, bool) or not isinstance(tier, int):
        return None
    when = datetime.fromtimestamp(epoch, tz=timezone.utc)
    return {
        "currency": currency.upper(),
        "release": row.get("release"),
        "name": row.get("name") or row.get("release"),
        "time_utc": when.isoformat(),
        "market_tier": tier,
        "minutes_until": round((when - now).total_seconds() / 60, 1),
    }
```

The `bool` checks look redundant but are not: in Python, `True` is an `int`,
so `isinstance(True, int)` is `True`. Without them a row with
`"market_tier": true` would be treated as tier 1.

Passing `now` in, rather than calling `datetime.now()` inside, keeps every
event in a run measured against the same instant and makes the function easy
to test with a fixed time.

## Step 5: Write the Run

`main()` ties it together. It reads the config with defaults that match the
schema, cleans the key, and then fetches each currency:

```python
def main(bot_id: str = None, config: dict = None):
    if bot_id is None or config is None:
        bot_id, config = parse()

    currencies = config.get("currencies", ["USD"])
    min_tier = int(config.get("min_tier", 1))
    lookahead_hours = float(config.get("lookahead_hours", 24))
    guard_minutes = float(config.get("guard_minutes", 30))

    try:
        api_key = clean_api_key(config.get("api_key"))
    except CalendarError as exc:
        error(str(exc))
        return

    now = datetime.now(timezone.utc)
    horizon_minutes = lookahead_hours * 60
    upcoming, failures = [], []

    for currency in currencies:
        try:
            rows = fetch_calendar(currency, api_key)
        except CalendarError as exc:
            logger.warning(str(exc))
            failures.append(str(exc))
            continue
        for row in rows:
            event = to_event(currency, row, now)
            if event is None or event["market_tier"] > min_tier:
                continue
            if -guard_minutes <= event["minutes_until"] <= horizon_minutes:
                upcoming.append(event)

    if failures and len(failures) == len(currencies):
        error("Could not read the release calendar", {"failures": failures})
        return
```

### Partial failure is still a result

If you watch USD and EUR and only the EUR call fails, the USD releases are
still worth reporting. The bot logs each failure, carries on, and only calls
`error()` when every currency failed. The failures are also included in the
final result so they are visible on the dashboard.

`error()` marks the run as failed and exits the process, so the `return` after
it never actually runs. It keeps the control flow obvious to a reader who does
not know that.

### The window looks backwards too

The filter keeps releases from `guard_minutes` in the past up to the lookahead
horizon. A release that came out five minutes ago is still moving the market,
so it should still hold the risk window open.

### Emit metrics

With the list built, the bot emits one `macro_event` per release and a single
`risk_window` summary:

```python
upcoming.sort(key=lambda e: e["time_utc"])
for event in upcoming:
    metric("macro_event", event)

in_window = [e for e in upcoming if abs(e["minutes_until"]) <= guard_minutes]
next_event = upcoming[0] if upcoming else None
metric("risk_window", {
    "active": bool(in_window),
    "events": [f"{e['currency']} {e['name']}" for e in in_window],
    "next_event": f"{next_event['currency']} {next_event['name']}" if next_event else None,
    "next_in_minutes": next_event["minutes_until"] if next_event else None,
})
```

Sorting on the ISO string works because every timestamp is UTC with the same
offset, so string order matches time order.

The split between the two metrics is deliberate. `macro_event` is a list for a
human or a dashboard. `risk_window` is the one value something automated needs:
a single `active` flag per run. See [Metrics](/custom-bot-development/metrics)
for how metrics are stored and displayed.

### Save state and finish

Finally the run saves what the query handlers will need and reports success:

```python
guarded_runs = state.get("guarded_runs", 0) + (1 if in_window else 0)
state.set("upcoming_events", upcoming)
state.set("guarded_runs", guarded_runs)
state.set("last_run", now.isoformat())

success(
    f"{len(upcoming)} tier<={min_tier} releases in the next {lookahead_hours:g}h"
    + ("; risk window active" if in_window else ""),
    {
        "upcoming": len(upcoming),
        "risk_window_active": bool(in_window),
        "failed_currencies": failures,
    },
)
```

`guarded_runs` reads its own previous value, which shows that
[state](/custom-bot-development/state) survives between scheduled runs.

## Step 6: Answer Queries from State

`query.py` registers two read-only handlers. Query handlers cannot write state,
so they can never disturb what the scheduled run is doing.

```python
from the0 import query, state


@query.handler("/events")
def get_events(req):
    events = state.get("upcoming_events", [])
    currency = req.get("currency")
    if currency:
        events = [e for e in events if e.get("currency") == str(currency).upper()]
    return {
        "events": events,
        "last_run": state.get("last_run"),
    }


@query.handler("/status")
def get_status(req):
    config = {k: v for k, v in query.get_config().items() if k != "api_key"}
    return {
        "config": config,
        "upcoming_count": len(state.get("upcoming_events", [])),
        "guarded_runs": state.get("guarded_runs", 0),
        "last_run": state.get("last_run"),
    }


if __name__ == "__main__":
    query.run()
```

Two things to notice:

- **`/events` returns `last_run`.** The data is only as fresh as the last
  scheduled run, so the caller gets the timestamp and can decide whether to
  trust it.
- **`/status` removes `api_key` before returning the config.** Anyone who can
  query the bot can read the response, so a handler that echoes config must
  strip secrets itself.

See [Query Handlers](/custom-bot-development/queries) for request parameters,
the built-in `/health` and `/info` queries, and how queries run for realtime
bots.

## Step 7: Run It Locally

`main.py` ends with a block that sets the environment the platform would
normally provide, so you can run it directly:

```python
if __name__ == "__main__":
    import json
    import os

    os.environ["BOT_ID"] = "test-bot-id"
    os.environ["BOT_CONFIG"] = json.dumps({
        "currencies": ["USD"],
        "min_tier": 2,
        "lookahead_hours": 72,
        "guard_minutes": 30,
    })
    os.environ["CODE_MOUNT_DIR"] = "/tmp"

    main()
```

State is written to `/state/.the0-state` by default, which only exists inside
the runtime, so point `STATE_DIR` somewhere writable:

```bash
cd example-bots/python-macro-event-guard
pip install -r requirements.txt
STATE_DIR=/tmp/macro-guard-state python main.py
```

The local config uses a wider net (tier 2, 72 hours) so you see some output
even on a quiet day. Each metric is printed as a JSON line (the SDK also adds
a `timestamp` field, left out here):

```json
{"_metric": "macro_event", "currency": "USD", "release": "non_farm_payrolls", "name": "Non-Farm Payrolls", "time_utc": "2026-10-09T12:30:00+00:00", "market_tier": 1, "minutes_until": 912.0}
{"_metric": "risk_window", "active": false, "events": [], "next_event": "USD Non-Farm Payrolls", "next_in_minutes": 912.0}
```

## Step 8: Deploy

Deploy the custom bot, then an instance of it from `config.json`:

```bash
the0 custom-bot deploy
the0 bot deploy config.json
```

The instance runs every 15 minutes. Follow its runs and query it:

```bash
the0 bot logs <bot_id> -w
the0 bot query <bot_id> /events
the0 bot query <bot_id> /events --params '{"currency": "USD"}'
the0 bot query <bot_id> /status
```

To watch other currencies, add them to `currencies` and set `api_key` to your
FXMacroData key in the instance config.

## Going Further

- **Gate a trading bot.** Copy `fetch_calendar` and `to_event` into your
  strategy and skip new entries while a release is inside the window.
- **Widen the net.** Set `min_tier` to `2` or `3` and add the currencies you
  trade.
- **Add a countdown dashboard.** The `macro_event` metrics carry everything a
  countdown view needs. See [Custom Frontends](/custom-bot-development/custom-frontends).
- **Write tests.** `to_event` and the window filter are pure functions of their
  inputs and a fixed `now`. See [Testing](/custom-bot-development/testing).
