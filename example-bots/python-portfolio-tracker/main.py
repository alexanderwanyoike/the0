"""
Portfolio Tracker Bot (SDK Version)
====================================
A scheduled bot that simulates portfolio tracking with mock data.

This is the SDK version - compare with main.py to see how the SDK
simplifies configuration parsing and metric emission.

Differences from main.py:
- Uses `the0.parse()` instead of function signature for config
- Uses `the0.metric()` instead of structlog with _metric field
- Uses `the0.log()` for structured logging
- Uses `the0.success()` for result output
"""

import random
from datetime import datetime, timezone
from the0 import parse, success, error, metric, log


def main():
    """Bot entry point using the0 SDK."""

    # Parse configuration from environment (set by the platform)
    bot_id, config = parse()

    # Extract configuration with defaults
    initial_value = config.get("initial_value", 10000)
    volatility = config.get("volatility", 0.02)
    symbols = config.get("symbols", ["BTC", "ETH", "SOL"])

    log("Bot started", {"bot_id": bot_id, "symbols": symbols})

    # Simulate portfolio state
    portfolio = simulate_portfolio(initial_value, volatility, symbols)

    # Emit portfolio value metric
    metric("portfolio_value", {
        "value": portfolio["total_value"],
        "change_pct": portfolio["change_pct"],
    })

    # Emit position metrics for each holding
    for position in portfolio["positions"]:
        metric("position", {
            "symbol": position["symbol"],
            "quantity": position["quantity"],
            "value": position["value"],
            "price": position["price"],
        })

    # Randomly simulate a trade (50% chance)
    if random.random() > 0.5:
        trade = simulate_trade(symbols)
        metric("trade", {
            "symbol": trade["symbol"],
            "side": trade["side"],
            "quantity": trade["quantity"],
            "price": trade["price"],
            "total": trade["total"],
        })

    log("Bot completed", {"bot_id": bot_id})

    # Signal success with result data
    success(f"Portfolio tracked: ${portfolio['total_value']:.2f}", {
        "portfolio_value": portfolio["total_value"],
        "positions_count": len(portfolio["positions"]),
    })


def simulate_portfolio(initial_value: float, volatility: float, symbols: list) -> dict:
    """Simulate portfolio with random price movements."""
    positions = []
    total_value = 0

    base_prices = {
        "BTC": 45000,
        "ETH": 2400,
        "SOL": 120,
        "AVAX": 35,
        "LINK": 15,
    }

    value_per_symbol = initial_value / len(symbols)

    for symbol in symbols:
        base_price = base_prices.get(symbol, 100)
        price_change = random.uniform(-volatility, volatility)
        current_price = base_price * (1 + price_change)

        quantity = value_per_symbol / current_price
        position_value = quantity * current_price

        positions.append({
            "symbol": symbol,
            "quantity": round(quantity, 6),
            "price": round(current_price, 2),
            "value": round(position_value, 2),
        })

        total_value += position_value

    change_pct = ((total_value - initial_value) / initial_value) * 100

    return {
        "total_value": round(total_value, 2),
        "change_pct": round(change_pct, 2),
        "positions": positions,
    }


def simulate_trade(symbols: list) -> dict:
    """Simulate a random trade execution."""
    symbol = random.choice(symbols)
    side = random.choice(["buy", "sell"])

    base_prices = {"BTC": 45000, "ETH": 2400, "SOL": 120, "AVAX": 35, "LINK": 15}
    price = base_prices.get(symbol, 100) * random.uniform(0.99, 1.01)

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
        "price": round(price, 2),
        "total": round(quantity * price, 2),
    }


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
