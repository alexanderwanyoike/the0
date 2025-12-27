# the0-sdk (Python)

SDK for building trading bots on the0 platform.

## Installation

```bash
pip install the0-sdk
```

Or copy the `the0/` directory to your project.

## Quick Start

```python
from the0 import parse, success, error, metric, log

# Parse bot configuration from environment
bot_id, config = parse()

log(f"Bot {bot_id} starting...")

# Access configuration
symbol = config.get("symbol", "BTC/USDT")
amount = config.get("amount", 100.0)

try:
    # Your trading logic here
    log(f"Trading {symbol} with amount {amount}")

    # Emit metrics for the dashboard
    metric("price", {
        "symbol": symbol,
        "value": 45000.50,
        "change_pct": 2.5
    })

    # Signal success
    success("Trade executed", {
        "symbol": symbol,
        "amount": amount
    })

except Exception as e:
    error(f"Trade failed: {e}")
```

## API Reference

### `parse() -> Tuple[str, Dict]`

Parse bot configuration from environment variables.

```python
bot_id, config = parse()
# bot_id: Value of BOT_ID env var
# config: Parsed JSON from BOT_CONFIG env var
```

### `success(message: str, data: dict = None)`

Output a success result.

```python
success("Trade completed")
success("Trade completed", {"trade_id": "12345"})
```

### `error(message: str, data: dict = None)`

Output an error result and exit with code 1.

```python
if amount <= 0:
    error("Amount must be positive")
    # Program exits here
```

### `result(data: dict)`

Output a custom JSON result.

```python
result({
    "status": "success",
    "trade_id": "abc123",
    "filled_amount": 0.5,
    "average_price": 45123.50
})
```

### `metric(type: str, data: dict)`

Emit a metric for the platform dashboard.

```python
# Price metric
metric("price", {"symbol": "BTC/USD", "value": 45000})

# Trading signal
metric("signal", {"symbol": "ETH/USD", "direction": "long", "confidence": 0.85})

# Alert
metric("alert", {"type": "price_spike", "severity": "high"})
```

### `log(message: str, data: dict = None)`

Log a message to the bot's logs.

```python
log("Starting trade...")
log("Order placed", {"order_id": "12345"})
```

### `sleep(seconds: float)`

Sleep utility.

```python
sleep(5)  # Wait 5 seconds
```

## Publishing (Maintainers)

This package is published to PyPI.

### Prerequisites

1. Install build tools:
   ```bash
   pip install build twine
   ```

2. Create a PyPI API token at https://pypi.org/manage/account/token/

### Publish

```bash
# Build the package
python -m build

# Upload to PyPI
twine upload dist/*
```

### Version Bump

Update the version in `pyproject.toml`:

```toml
version = "0.2.0"
```

Then rebuild and publish.

## License

Apache-2.0
