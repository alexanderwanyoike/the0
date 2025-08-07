---
title: "Overview"
description: "Understanding custom bot development on the0"
tags: ["custom-bots", "development", "overview"]
order: 2
---

# Custom Bot Development Overview

Custom bots are the heart of the the0 platform - they're your trading algorithms packaged and deployed as reusable templates. This guide provides an overview of custom bot development.

---

## What Are Custom Bots?

Custom bots are the trading algorithms they:

- Define trading strategies and logic
- Accept configurable parameters
- Can be deployed multiple times with different settings
- Support backtesting and live trading
- Can be shared or sold in the marketplace

## Development Philosophy

### Framework Agnostic

the0 doesn't lock you into specific libraries or frameworks. You can:

- Use any Python or JavaScript library (within reason and codebase size limits)
- Implement any trading strategy
- Integrate with any data source or API
- Use your preferred development tools

### Open Standards

Custom bots use open standards:

- **YAML**: For bot configuration and metadata
- **JSON Schema**: For parameter validation
- **Standard Entry Points**: Consistent function signatures
- **Docker**: For dependency management and isolation

### Execution Models, Not Strategies

the0 provides execution models (how bots run) rather than trading strategies (what bots do):

- **scheduled**: Run on fixed intervals (cron expressions)
- **realtime**: Run continuously, processing live data

## Bot Architecture

### Core Components

#### Python structure

```bash
custom-bot/
├── bot-config.yaml          # Bot metadata and configuration
├── main.py                  # Main bot execution logic
├── backtest.py              # Backtesting implementation (optional)
├── bot-schema.json          # Bot parameter schema
├── backtest-schema.json     # Backtest parameter schema (optional but needed when backtesting)
├── requirements.txt         # Python dependencies
├── vendor/                  # Vendored dependencies (auto-generated)
├── lib/                     # Your custom utilities
|   └── __init__.py          # Module initialization (for Python only)
├── utils.py                 # Utility functions
├── tests/                   # Unit tests
└── README.md                # Documentation
```

#### JavaScript structure

```bash
custom-bot/
├── bot-config.yaml          # Bot metadata and configuration
├── main.js                  # Main bot execution logic
├── backtest.js              # Backtesting implementation (optional)
├── bot-schema.json          # Bot parameter schema
├── node_modules/            # Node.js dependencies
├── lib/                     # Your custom utilities
├── utils.js                 # Utility functions
├── backtest-schema.json     # Backtest parameter schema (optional but needed when backtesting)
└── package.json             # Node.js dependencies
```

---

## Entry Points

Every custom bot must define entry points:

### Bot Entry Point

#### Python

```python
def main(id: str, config: Dict[str, Any]) -> Dict[str, Any]:
    """
    Main bot execution function.

    Args:
        id: Unique bot instance identifier
        config: User-provided configuration

    Returns:
        Execution status and results
    """
    # Your trading logic here
    return {
        "status": "success",
        "message": "Bot executed successfully"
    }
```

#### JavaScript

```javascript
function main(id, config) {
  /**
   * Main bot execution function.
   *
   * @param {string} id - Unique bot instance identifier
   * @param {Object} config - User-provided configuration
   * @returns {Object} Execution status and results
   */
  // Your trading logic here
  return {
    status: "success",
    message: "Bot executed successfully",
  };
}

module.exports = { main };
```

---

### Backtest Entry Point

##### Python

```python
def main(id: str, config: Dict[str, Any]) -> Dict[str, Any]:
    """
    Backtest the trading strategy.

    Args:
        id: Unique backtest run identifier
        config: Backtest configuration

    Returns:
        Results with metrics, plots, and tables
    """

    # NOTE: The structure of the return value must match the this format
    return {
        "status": "success",
        "results": {
            "metrics": {...},
            "plots": [...],
            "tables": [...]
        }
    }
```

##### JavaScript

```javascript
function main(id, config) {
    /**
     * Backtest the trading strategy.
     *
     * @param {string} id - Unique backtest run identifier
     * @param {Object} config - Backtest configuration
     * @returns {Object} Results with metrics, plots, and tables
     */

    // NOTE: The structure of the return value must match the this format
    return {
        status: "success",
        results: {
            metrics: {...},
            plots: [...],
            tables: [...]
        }
    };
}

module.exports = { main };
```

---

## Development Workflow

### 1. Planning

- Define your trading strategy
- Identify required parameters
- Choose execution model (scheduled/realtime)
- Plan backtesting approach (optional)
- Test your entry points locally

### 2. Local Development

```bash
# Initialize bot structure

# Develop locally
code main.py
code bot-schema.json
code backtest.py  # Optional, if implementing backtesting
code backtest-schema.json  # Optional, if implementing backtesting
code requirements.txt  # Add dependencies
code bot-config.yaml  # Define metadata

# Test locally
python main.py
python backtest.py
```

### 3. Configuration

- Define `bot-config.yaml` with metadata
- Create JSON schemas for validation
- Write comprehensive documentation

### 4. Testing

- Unit test your components
- Test with various configurations
- Validate schema compliance
- Run backtests with historical data

### 5. Deployment

```bash
# Deploy to the0 platform
the0 custom-bot deploy

# Create test instance if the custom bot is approved
the0 bot deploy test-config.json
```

### 6. Publishing (Optional)

- Test thoroughly in production
- Polish documentation
- Set pricing
- Publish to marketplace

## Supported Languages

### Python 3.11 (`python3.11`)

**Advantages:**

- Rich ecosystem of trading libraries (ccxt, pandas, numpy, scikit-learn)
- Excellent for data analysis and machine learning
- Strong scientific computing capabilities
- Popular in quantitative finance

**Example Libraries:**

- `ccxt` - Exchange integrations
- `pandas` - Data manipulation
- `numpy` - Numerical computing
- `scikit-learn` - Machine learning
- `plotly` - Visualization

### JavaScript (`nodejs20`)

**Advantages:**

- Fast execution and low latency
- Excellent for real-time applications
- Modern async/await support
- Large npm ecosystem

**Example Libraries:**

- `ccxt` - Exchange integrations
- `axios` - HTTP requests
- `ws` - WebSocket connections
- `mathjs` - Mathematical operations
- `plotly.js` - Visualization

More information on bot types can be found in [Bot Types](./bot-types).

## Performance Considerations

### Resource Limits

- CPU: 0.5 cores
- Memory: 512MB

> Note: Trading bots are designed to be lightweight and efficient. Avoid heavy computations or large data processing within the bot logic. It will make it slow and
> unresponsive, leading to missed trades or errors.

### Optimization Tips

- Use efficient data structures
- Implement proper error handling
- Aim for stateless bots that use exchange APIs for data
- Profile performance bottlenecks locally before deploying

---

## Best Practices

### Code Quality

1. **Error Handling**: Implement comprehensive try-catch blocks
2. **Logging**: Use structured logging for debugging
3. **Testing**: Write unit tests for critical components
4. **Documentation**: Maintain clear, updated documentation

### Strategy Development

1. **Backtesting**: Always backtest before live deployment
2. **Paper Trading**: Test with simulated funds first
3. **Risk Management**: Implement stop-losses and position limits
4. **Monitoring**: Set up alerts for bot health and performance

### User Experience

1. **Clear Parameters**: Use descriptive parameter names and descriptions
2. **Sensible Defaults**: Provide reasonable default values
3. **Validation**: Implement input validation and error messages
4. **Documentation**: Write a comprehensive README for users

---

## Getting Started

Ready to build your first custom bot? Check out these resources:

1. [Quick Start Guide](/docs/custom-bot-development/quick-start-guide) - Build a DCA bot in 15 minutes
2. [Configuration Reference](/docs/custom-bot-development/configuration) - Detailed configuration options
3. [Testing Guide](/docs/custom-bot-development/testing) - Testing strategies and tools
