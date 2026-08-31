"""
Portfolio Tracker Bot (SDK Version)
====================================
A scheduled bot that simulates portfolio tracking with mock data.

This is the SDK version - compare with main_raw.py to see how the SDK
simplifies configuration parsing and metric emission.

Differences from main_raw.py:
- Uses `the0.parse()` instead of function signature for config
- Uses `the0.metric()` instead of structlog with _metric field
- Uses Python's logging module for structured logging
- Uses `the0.success()` for result output

State Usage:
- `holdings`   quantity held per symbol, carried between runs
- `prices`     last price per symbol, so each run walks on from the last
- `cash`       proceeds from simulated sells, spent by simulated buys
- `portfolio_history` value per run, for trend analysis
- `total_trades`      trade count across executions

Holdings and prices must persist for the value to mean anything. An earlier
version recomputed quantity from the current price on every run
(`quantity = allocation / price`, then `value = quantity * price`), which
cancels out to the starting allocation no matter what the price does - the
portfolio reported exactly its initial value forever.
"""

import random
import logging
from datetime import datetime, timezone
from the0 import parse, success, error, metric, state

# Configure logging - JSON format for structured logs
logging.basicConfig(
    level=logging.INFO,
    format='{"level":"%(levelname)s","message":"%(message)s","timestamp":"%(asctime)s"}'
)
logger = logging.getLogger(__name__)

BASE_PRICES = {
    "BTC": 45000,
    "ETH": 2400,
    "SOL": 120,
    "AVAX": 35,
    "LINK": 15,
}
DEFAULT_BASE_PRICE = 100


def main(bot_id: str = None, config: dict = None):
    """Bot entry point using the0 SDK."""

    # If not passed by runtime, parse from environment
    if bot_id is None or config is None:
        bot_id, config = parse()

    # Extract configuration with defaults
    initial_value = config.get("initial_value", 10000)
    volatility = config.get("volatility", 0.02)
    symbols = config.get("symbols", ["BTC", "ETH", "SOL"])

    # Load persistent state
    holdings = state.get("holdings", {})
    previous_prices = state.get("prices", {})
    cash = float(state.get("cash", 0.0))
    portfolio_history = state.get("portfolio_history", [])
    total_trades = state.get("total_trades", 0)

    logger.info(f"Bot {bot_id} started with symbols: {symbols}")
    logger.info(f"Loaded {len(portfolio_history)} historical values, {total_trades} total trades")

    # Walk each price on from where it closed last run.
    prices = simulate_prices(previous_prices, volatility, symbols)

    if not holdings:
        holdings = open_positions(initial_value, prices, symbols)
        logger.info(f"Opened initial positions worth {initial_value}")
    else:
        # A config change can drop a symbol; liquidate it rather than lose it.
        holdings, cash = liquidate_untracked(holdings, prices, symbols, cash)

    positions = [
        {
            "symbol": symbol,
            "quantity": round(holdings.get(symbol, 0.0), 6),
            "price": prices[symbol],
            "value": round(holdings.get(symbol, 0.0) * prices[symbol], 2),
        }
        for symbol in symbols
    ]

    total_value = round(cash + sum(p["value"] for p in positions), 2)
    change_pct = round((total_value - initial_value) / initial_value * 100, 2)

    previous_value = portfolio_history[-1]["value"] if portfolio_history else total_value
    change_since_last_pct = (
        round((total_value - previous_value) / previous_value * 100, 2)
        if previous_value
        else 0.0
    )

    # Emit portfolio value metric
    metric("portfolio_value", {
        "value": total_value,
        "change_pct": change_pct,
        "change_since_last_pct": change_since_last_pct,
        "cash": round(cash, 2),
    })

    # Emit position metrics for each holding
    for position in positions:
        metric("position", position)

    # Randomly simulate a trade (50% chance), priced off this run's prices
    # so a trade and the positions it moves cannot disagree.
    trade_executed = False
    if random.random() > 0.5:
        trade = simulate_trade(symbols, prices)
        holdings, cash, filled = apply_trade(trade, holdings, cash)
        if filled:
            metric("trade", trade)
            trade_executed = True
            total_trades += 1
        else:
            logger.info(
                f"Skipped {trade['side']} {trade['quantity']} {trade['symbol']}: "
                "insufficient cash or holdings"
            )

    # Update portfolio history (keep last 100 values)
    portfolio_history.append({
        "value": total_value,
        "timestamp": datetime.now(timezone.utc).isoformat(),
    })
    if len(portfolio_history) > 100:
        portfolio_history = portfolio_history[-100:]

    # Save state for next run
    state.set("holdings", holdings)
    state.set("prices", prices)
    state.set("cash", round(cash, 2))
    state.set("portfolio_history", portfolio_history)
    state.set("total_trades", total_trades)

    logger.info(
        f"Bot {bot_id} completed - value {total_value} ({change_since_last_pct:+.2f}% "
        f"since last run), saved {len(portfolio_history)} history entries"
    )

    # Signal success with result data
    success(f"Portfolio tracked: ${total_value:.2f}", {
        "portfolio_value": total_value,
        "change_pct": change_pct,
        "cash": round(cash, 2),
        "positions_count": len(positions),
        "history_entries": len(portfolio_history),
        "total_trades": total_trades,
        "trade_executed": trade_executed,
    })


def simulate_prices(previous_prices: dict, volatility: float, symbols: list) -> dict:
    """Random-walk each price on from its last value.

    Starting from the previous price rather than a fixed base is what lets the
    portfolio drift over time instead of oscillating around its opening value.
    """
    prices = {}
    for symbol in symbols:
        last = previous_prices.get(symbol) or BASE_PRICES.get(symbol, DEFAULT_BASE_PRICE)
        prices[symbol] = round(last * (1 + random.uniform(-volatility, volatility)), 2)
    return prices


def open_positions(initial_value: float, prices: dict, symbols: list) -> dict:
    """Split the opening value evenly across symbols. Quantities then persist."""
    allocation = initial_value / len(symbols)
    return {symbol: allocation / prices[symbol] for symbol in symbols}


def liquidate_untracked(holdings: dict, prices: dict, symbols: list, cash: float):
    """Sell anything no longer in the configured symbol list, into cash."""
    kept = {}
    for symbol, quantity in holdings.items():
        if symbol in symbols:
            kept[symbol] = quantity
        else:
            price = prices.get(symbol) or BASE_PRICES.get(symbol, DEFAULT_BASE_PRICE)
            cash += quantity * price
            logger.info(f"Liquidated {quantity:.6f} {symbol} - no longer tracked")
    for symbol in symbols:
        kept.setdefault(symbol, 0.0)
    return kept, cash


def simulate_trade(symbols: list, prices: dict) -> dict:
    """Simulate a random trade execution at this run's price."""
    symbol = random.choice(symbols)
    side = random.choice(["buy", "sell"])
    price = prices[symbol]

    if symbol == "BTC":
        quantity = round(random.uniform(0.001, 0.01), 6)
    elif symbol == "ETH":
        quantity = round(random.uniform(0.01, 0.1), 4)
    else:
        quantity = round(random.uniform(0.1, 1.0), 2)

    return {
        "symbol": symbol,
        "side": side,
        "quantity": quantity,
        "price": price,
        "total": round(quantity * price, 2),
    }


def apply_trade(trade: dict, holdings: dict, cash: float):
    """Move value between cash and holdings. Returns (holdings, cash, filled).

    Total portfolio value is unchanged by a trade itself - only later price
    moves change it - so an unfunded trade is skipped rather than allowed to
    conjure value out of negative cash.
    """
    symbol, quantity, total = trade["symbol"], trade["quantity"], trade["total"]
    held = holdings.get(symbol, 0.0)

    if trade["side"] == "buy":
        if cash < total:
            return holdings, cash, False
        holdings[symbol] = held + quantity
        cash -= total
    else:
        if held < quantity:
            return holdings, cash, False
        holdings[symbol] = held - quantity
        cash += total

    return holdings, cash, True


if __name__ == "__main__":
    # For local testing, set environment variables
    import os
    import json

    os.environ["BOT_ID"] = "test-bot-id"
    os.environ["BOT_CONFIG"] = json.dumps({
        "initial_value": 10000,
        "volatility": 0.02,
        "symbols": ["BTC", "ETH", "SOL"],
    })
    os.environ["CODE_MOUNT_DIR"] = "/tmp"

    main()
