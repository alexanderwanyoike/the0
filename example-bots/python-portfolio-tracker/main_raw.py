"""
Portfolio Tracker Bot
=====================
A scheduled bot that simulates portfolio tracking with mock data.

This example demonstrates:
- Structured metric emission using structlog with _metric field
- How the @the0/react SDK consumes these metrics in custom frontends
- Scheduled bot pattern (runs once per trigger, then exits)

Metrics emitted:
- portfolio_value: Total portfolio value snapshot
- position: Individual position details
- trade: Simulated trade executions
"""

import random
import structlog
from typing import Dict, Any
from datetime import datetime

# Configure structlog for JSON output
# The platform parses JSON logs and extracts metrics with _metric field
structlog.configure(
    processors=[
        structlog.processors.TimeStamper(fmt="iso"),
        structlog.processors.JSONRenderer()
    ]
)
logger = structlog.get_logger()


def main(id: str, config: Dict[str, Any]) -> Dict[str, Any]:
    """
    Bot entry point - called on each scheduled run.

    Args:
        id: Unique bot instance identifier
        config: User-provided configuration (validated against bot-schema.json)

    Returns:
        Dict with status, message, and optional data
    """
    # Extract configuration with defaults
    initial_value = config.get("initial_value", 10000)
    volatility = config.get("volatility", 0.02)
    symbols = config.get("symbols", ["BTC", "ETH", "SOL"])

    logger.info("bot_started", bot_id=id, symbols=symbols)

    # Simulate portfolio state
    # In a real bot, this would come from a database or API
    portfolio = simulate_portfolio(initial_value, volatility, symbols)

    # Emit portfolio value metric
    # The _metric field tells the platform this is a metric, not just a log
    logger.info(
        "portfolio_snapshot",
        _metric="portfolio_value",
        value=portfolio["total_value"],
        change_pct=portfolio["change_pct"],
        timestamp=datetime.utcnow().isoformat()
    )

    # Emit position metrics for each holding
    for position in portfolio["positions"]:
        logger.info(
            "position_update",
            _metric="position",
            symbol=position["symbol"],
            quantity=position["quantity"],
            value=position["value"],
            price=position["price"]
        )

    # Randomly simulate a trade (50% chance)
    if random.random() > 0.5:
        trade = simulate_trade(symbols)
        logger.info(
            "trade_executed",
            _metric="trade",
            symbol=trade["symbol"],
            side=trade["side"],
            quantity=trade["quantity"],
            price=trade["price"],
            total=trade["total"]
        )

    logger.info("bot_completed", bot_id=id)

    return {
        "status": "success",
        "message": f"Portfolio tracked: ${portfolio['total_value']:.2f}",
        "data": {
            "portfolio_value": portfolio["total_value"],
            "positions_count": len(portfolio["positions"])
        }
    }


def simulate_portfolio(initial_value: float, volatility: float, symbols: list) -> Dict[str, Any]:
    """
    Simulate portfolio with random price movements.

    In a real bot, this would fetch actual positions from a broker API.
    """
    positions = []
    total_value = 0

    # Mock prices (in a real bot, fetch from market data API)
    base_prices = {
        "BTC": 45000,
        "ETH": 2400,
        "SOL": 120,
        "AVAX": 35,
        "LINK": 15
    }

    # Distribute value across symbols
    value_per_symbol = initial_value / len(symbols)

    for symbol in symbols:
        base_price = base_prices.get(symbol, 100)

        # Apply random volatility
        price_change = random.uniform(-volatility, volatility)
        current_price = base_price * (1 + price_change)

        quantity = value_per_symbol / current_price
        position_value = quantity * current_price

        positions.append({
            "symbol": symbol,
            "quantity": round(quantity, 6),
            "price": round(current_price, 2),
            "value": round(position_value, 2)
        })

        total_value += position_value

    # Calculate overall change percentage
    change_pct = ((total_value - initial_value) / initial_value) * 100

    return {
        "total_value": round(total_value, 2),
        "change_pct": round(change_pct, 2),
        "positions": positions
    }


def simulate_trade(symbols: list) -> Dict[str, Any]:
    """
    Simulate a random trade execution.

    In a real bot, this would execute actual orders via broker API.
    """
    symbol = random.choice(symbols)
    side = random.choice(["buy", "sell"])

    # Mock prices
    base_prices = {"BTC": 45000, "ETH": 2400, "SOL": 120, "AVAX": 35, "LINK": 15}
    price = base_prices.get(symbol, 100) * random.uniform(0.99, 1.01)

    # Random quantity based on symbol
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
        "total": round(quantity * price, 2)
    }


# For local testing
if __name__ == "__main__":
    result = main("test-bot-id", {
        "initial_value": 10000,
        "volatility": 0.02,
        "symbols": ["BTC", "ETH", "SOL"]
    })
    print(f"\nResult: {result}")
