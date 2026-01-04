"""
Portfolio Tracker Query Handlers
================================
Query endpoints for the portfolio tracker bot.

These handlers provide read-only access to bot state and computed data.
They can be executed via:
  - CLI: the0 bot query <bot_id> /portfolio
  - API: POST /bot/:id/query

Available queries:
  /portfolio - Get current portfolio summary with history
  /status    - Get bot configuration and state summary
  /history   - Get portfolio value history with optional limit
  /trades    - Get trade statistics
"""

from the0 import query, state


@query.handler("/portfolio")
def get_portfolio(req):
    """
    Get current portfolio summary.

    Returns:
        positions: Number of history entries
        total_trades: Total trades executed
        latest_value: Most recent portfolio value
        history: Last 10 portfolio values
    """
    portfolio_history = state.get("portfolio_history", [])
    total_trades = state.get("total_trades", 0)

    latest_value = None
    if portfolio_history:
        latest_value = portfolio_history[-1].get("value")

    return {
        "history_count": len(portfolio_history),
        "total_trades": total_trades,
        "latest_value": latest_value,
        "recent_history": portfolio_history[-10:] if portfolio_history else [],
    }


@query.handler("/status")
def get_status(req):
    """
    Get bot status and configuration.

    Returns:
        config: Bot configuration
        state_summary: Summary of persisted state
    """
    config = query.get_config()
    portfolio_history = state.get("portfolio_history", [])
    total_trades = state.get("total_trades", 0)

    return {
        "config": {
            "initial_value": config.get("initial_value", 10000),
            "volatility": config.get("volatility", 0.02),
            "symbols": config.get("symbols", ["BTC", "ETH", "SOL"]),
        },
        "state_summary": {
            "history_entries": len(portfolio_history),
            "total_trades": total_trades,
            "has_data": len(portfolio_history) > 0,
        },
    }


@query.handler("/history")
def get_history(req):
    """
    Get portfolio value history.

    Query params:
        limit: Maximum number of entries to return (default: 50)

    Returns:
        history: List of {value, timestamp} entries
        count: Total number of entries returned
        total: Total entries in history
    """
    limit = int(req.get("limit", 50))
    portfolio_history = state.get("portfolio_history", [])

    # Return most recent entries up to limit
    history = portfolio_history[-limit:] if portfolio_history else []

    return {
        "history": history,
        "count": len(history),
        "total": len(portfolio_history),
    }


@query.handler("/trades")
def get_trades(req):
    """
    Get trade statistics.

    Returns:
        total_trades: Number of trades executed
        average_per_run: Average trades per run (estimated)
    """
    portfolio_history = state.get("portfolio_history", [])
    total_trades = state.get("total_trades", 0)

    runs = len(portfolio_history)
    avg_per_run = total_trades / runs if runs > 0 else 0

    return {
        "total_trades": total_trades,
        "total_runs": runs,
        "average_per_run": round(avg_per_run, 2),
    }


if __name__ == "__main__":
    query.run()
