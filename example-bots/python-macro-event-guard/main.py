"""
Macro Event Guard Bot
=====================
A scheduled bot that watches the official macro release calendar and flags
when a market-moving release is close, so other bots (or a person) can stand
aside around it.

Data comes from the FXMacroData release calendar REST endpoint, called
directly with `requests`. USD works without an API key; other currencies
need one in the `api_key` config field.

State Usage:
- Persists the upcoming events from the last run for the /events query
- Tracks how many runs ended inside a risk window
"""

import logging
from datetime import datetime, timezone

import requests
from the0 import error, metric, parse, state, success

logging.basicConfig(
    level=logging.INFO,
    format='{"level":"%(levelname)s","message":"%(message)s","timestamp":"%(asctime)s"}'
)
logger = logging.getLogger(__name__)

CALENDAR_URL = "https://api.fxmacrodata.com/v1/calendar/{currency}"


class CalendarError(Exception):
    """Raised when the calendar cannot be read. Never carries the API key."""


def clean_api_key(api_key):
    """Return a usable key or None, refusing values requests would echo back."""
    if not api_key:
        return None
    api_key = str(api_key).strip()
    if not api_key or any(ch.isspace() or ord(ch) < 32 for ch in api_key):
        raise CalendarError("api_key contains whitespace or control characters")
    return api_key


def fetch_calendar(currency: str, api_key=None, timeout: float = 15) -> list:
    """Fetch upcoming scheduled releases for one currency."""
    code = currency.strip().lower()
    if len(code) != 3 or not code.isalpha():
        raise CalendarError(f"invalid currency code {currency!r}")

    headers = {"Accept": "application/json"}
    if api_key:
        headers["X-API-Key"] = api_key

    try:
        # requests only drops Authorization on a cross-host redirect, so the
        # X-API-Key header would follow one. Redirects are refused instead.
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


def to_event(currency: str, row: dict, now: datetime):
    """Normalise one calendar row, or return None if it has no usable time."""
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


def main(bot_id: str = None, config: dict = None):
    """Bot entry point using the0 SDK."""
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
            # Keep releases that are still inside the guard window after
            # their scheduled time, plus everything up to the horizon.
            if -guard_minutes <= event["minutes_until"] <= horizon_minutes:
                upcoming.append(event)

    if failures and len(failures) == len(currencies):
        error("Could not read the release calendar", {"failures": failures})
        return

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
