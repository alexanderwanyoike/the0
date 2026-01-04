"""
the0 SDK for Python Trading Bots
================================

This SDK provides utilities for building trading bots on the0 platform.

Example:
    from the0 import parse, success, error, metric

    # Parse bot configuration
    bot_id, config = parse()

    # Your trading logic here
    print(f"Bot {bot_id} trading {config['symbol']}")

    # Signal completion
    success("Trade executed successfully")
"""

import os
import sys
import json
import time
from typing import Any, Dict, Tuple, Optional, Union, Literal
from datetime import datetime, timezone

__version__ = "1.0.0"
__all__ = ["parse", "success", "error", "result", "metric", "log", "sleep", "state"]

# Import state module for namespace access (e.g., the0.state.get())
from . import state


def _get_result_file_path() -> str:
    """Get the path to the result file."""
    mount_dir = os.environ.get("CODE_MOUNT_DIR", "bot")
    return f"/{mount_dir}/result.json"


def _write_result(data: Dict[str, Any]) -> None:
    """Write result to the result file."""
    try:
        result_path = _get_result_file_path()
        with open(result_path, "w") as f:
            json.dump(data, f)
    except Exception as e:
        print(f"RESULT_ERROR: Failed to write result file: {e}", file=sys.stderr)


def parse() -> Tuple[str, Dict[str, Any]]:
    """
    Parse bot configuration from environment variables.

    Reads BOT_ID and BOT_CONFIG from the environment and returns
    the bot ID and configuration dictionary.

    Returns:
        Tuple of (bot_id, config) where config is a dictionary

    Raises:
        ValueError: If environment variables are not set or config is invalid JSON

    Example:
        bot_id, config = parse()
        symbol = config.get("symbol", "BTC/USDT")
        amount = config.get("amount", 100.0)
    """
    bot_id = os.environ.get("BOT_ID")
    config_str = os.environ.get("BOT_CONFIG")

    if not bot_id:
        raise ValueError("BOT_ID environment variable not set")

    if not config_str:
        raise ValueError("BOT_CONFIG environment variable not set")

    try:
        config = json.loads(config_str)
    except json.JSONDecodeError as e:
        raise ValueError(f"Failed to parse BOT_CONFIG as JSON: {e}")

    return bot_id, config


def success(message: str, data: Optional[Dict[str, Any]] = None) -> None:
    """
    Output a success result.

    Writes a JSON result with status "success".

    Args:
        message: Success message to include in the result
        data: Optional additional data to include

    Example:
        # Simple success
        success("Trade completed")

        # Success with data
        success("Trade completed", {
            "trade_id": "12345",
            "filled_amount": 0.5,
            "price": 45000
        })
    """
    result_data = {"status": "success", "message": message}
    if data:
        result_data["data"] = data
    _write_result(result_data)


def error(message: str, data: Optional[Dict[str, Any]] = None) -> None:
    """
    Output an error result and exit with code 1.

    Writes a JSON result with status "error" and terminates the process.

    Args:
        message: Error message to include in the result
        data: Optional additional error context

    Example:
        # Simple error
        error("Failed to connect to exchange")

        # Error with context
        error("Trade failed", {
            "error_code": "INSUFFICIENT_FUNDS",
            "available": 100,
            "required": 150
        })
    """
    result_data = {"status": "error", "message": message}
    if data:
        result_data["data"] = data
    _write_result(result_data)
    sys.exit(1)


def result(result_data: Dict[str, Any]) -> None:
    """
    Output a custom result object.

    Use this when you need full control over the result structure.

    Args:
        result_data: Custom result object (should include status)

    Example:
        result({
            "status": "success",
            "message": "Analysis complete",
            "signals": [
                {"symbol": "BTC/USD", "direction": "long", "confidence": 0.85}
            ],
            "timestamp": datetime.now().isoformat()
        })
    """
    _write_result(result_data)


def metric(metric_type: str, data: Dict[str, Any]) -> None:
    """
    Emit a metric for the platform to collect.

    Metrics are logged as JSON with a special `_metric` field that
    the platform recognizes and processes for dashboards and alerts.

    Args:
        metric_type: The metric type (e.g., 'price', 'signal', 'alert')
        data: Metric data fields

    Example:
        # Emit a price metric
        metric("price", {
            "symbol": "BTC/USD",
            "value": 45000.50,
            "change_pct": 2.5
        })

        # Emit a trading signal
        metric("signal", {
            "symbol": "ETH/USD",
            "direction": "long",
            "confidence": 0.85,
            "reason": "MA crossover detected"
        })

        # Emit an alert
        metric("alert", {
            "symbol": "BTC/USD",
            "type": "price_spike",
            "severity": "high",
            "message": "Price increased 5% in 1 minute"
        })
    """
    output = {
        "_metric": metric_type,
        **data,
        "timestamp": datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
    }
    print(json.dumps(output))


LogLevel = Literal["info", "warn", "error"]


def log(
    message: str,
    data_or_level: Optional[Union[Dict[str, Any], LogLevel]] = None,
    level: Optional[LogLevel] = None,
) -> None:
    """
    Log a structured message to the bot's log output.

    Use this for debugging and monitoring. Messages appear in
    the bot's log viewer in the platform as structured JSON.

    Args:
        message: Message to log
        data_or_level: Optional structured data dict or log level string
        level: Optional log level (defaults to 'info')

    Example:
        # Simple log (defaults to info level)
        log("Starting trade execution")

        # Log with level
        log("Connection lost", "warn")
        log("Trade failed", "error")

        # Log with structured data (pino-style)
        log("Order placed", {"order_id": "12345", "symbol": "BTC/USD"})

        # Log with data and level
        log("Order failed", {"order_id": "12345", "reason": "insufficient funds"}, "error")
    """
    data: Optional[Dict[str, Any]] = None
    log_level: LogLevel = "info"

    if isinstance(data_or_level, str):
        log_level = data_or_level
    elif data_or_level is not None:
        data = data_or_level
        log_level = level or "info"

    entry = {
        "level": log_level,
        "message": message,
        **(data or {}),
        "timestamp": datetime.now(timezone.utc).isoformat().replace("+00:00", "Z"),
    }

    print(json.dumps(entry), file=sys.stderr)


def sleep(seconds: float) -> None:
    """
    Sleep utility for synchronous operations.

    Args:
        seconds: Seconds to sleep

    Example:
        # Wait 5 seconds between operations
        sleep(5)
    """
    time.sleep(seconds)
