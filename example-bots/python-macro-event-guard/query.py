"""
Macro Event Guard Query Handlers
================================
Read-only views of what the last scheduled run saw.

  - CLI: the0 bot query <bot_id> /events
  - API: POST /bot/:id/query

Available queries:
  /events - Upcoming releases from the last run (optional ?currency=USD)
  /status - Configuration and run summary
"""

from the0 import query, state


@query.handler("/events")
def get_events(req):
    """
    Get the upcoming releases stored by the last run.

    Params:
        currency: Optional three-letter code to filter on

    Returns:
        events: Upcoming releases, soonest first
        last_run: When the calendar was last read
    """
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
    """
    Get bot configuration and a run summary.

    The API key, if configured, is never returned.
    """
    config = {k: v for k, v in query.get_config().items() if k != "api_key"}
    return {
        "config": config,
        "upcoming_count": len(state.get("upcoming_events", [])),
        "guarded_runs": state.get("guarded_runs", 0),
        "last_run": state.get("last_run"),
    }


if __name__ == "__main__":
    query.run()
