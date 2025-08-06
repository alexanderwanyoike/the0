---
title: "Backtesting"
description: "How to implement a comprehensive backtesting solution for your custom bots"
tags: ["custom-bots", "deployment", "backtesting"]
order: 10
---

# Implementing Backtesting for Custom Bots

This guide covers how to implement backtesting for your custom bots on the0 platform. Backtesting allows you and users to evaluate trading strategies using historical data, helping you optimize performance before deploying live.

---

## Implementation Flexibility

The0 provides complete freedom in backtesting implementation:

### Supported Approaches

- **Simple Backtesting**: Test against a fixed historical period
- **Walk-Forward Analysis**: Rolling window optimization and testing
- **Monte Carlo Simulation**: Random sampling for robustness testing
- **Custom Methods**: Any approach that suits your strategy

### Data Sources

Developers can:

- Use exchange-provided historical data
- Import custom datasets
- Generate synthetic data for stress testing
- Combine multiple data sources

> Really up to them to define how they want to implement backtesting, as long as it returns the required structure.

## Deployment Overview

## Required Output Structure

While implementation is flexible, the0 requires backtest functions to return results in a specific structure:

### 1. Metrics (Required)

Numerical performance indicators:

```json
{
  "metrics": {
    "total_return": 0.2534,
    "sharpe_ratio": 1.45,
    "max_drawdown": -0.08,
    "win_rate": 0.62,
    "total_trades": 150
  }
}
```

### 2. Plots (Optional but Recommended)

Visualizations must use Plotly format:

```json
{
  "plots": ["<plotly_json_string>", "<another_plotly_json_string>"]
}
```

or

```json
{
  "plots": [
    {
      "data": [
        {
          "x": [
            "2024-01-01",
            "2024-01-02",
            "2024-01-03",
            "2024-01-04",
            "2024-01-05"
          ],
          "y": [100, 105, 103, 108, 112],
          "type": "scatter",
          "mode": "lines",
          "name": "Portfolio Value",
          "line": { "color": "#1f77b4" }
        }
      ],
      "layout": {
        "title": "Portfolio Performance Over Time",
        "xaxis": { "title": "Date" },
        "yaxis": { "title": "Portfolio Value ($)" },
        "showlegend": true
      }
    }
  ] // Plotly json object
}
```

**Important**: Only Plotly plots are supported. The platform will render these in the UI.

### 3. Tables (Optional but Recommended)

Tabular data for detailed analysis:

```json
{
  "tables": [
    {
      "title": "Trade History",
      "data": [
        { "date": "2024-01-01", "action": "buy", "price": 100 },
        { "date": "2024-01-02", "action": "sell", "price": 105 }
      ]
    }
  ]
}
```

## Complete Return Structure

### Python Structure

```python
def main(id: str, config: dict) -> dict:
    # Your backtesting logic here

    return {
        "status": "success",
        "results": {
            "metrics": {
                # Your performance metrics
            },
            "plots": [
                # Plotly JSON strings
            ],
            "tables": [
                # Tabular results
            ]
        }
    }
```

### JavaScript Structure

```javascript
function main(id, config) {
  // Your backtesting logic here

  return {
    status: "success",
    results: {
      metrics: {
        // Your performance metrics
      },
      plots: [
        // Plotly JSON strings
      ],
      tables: [
        // Tabular results
      ],
    },
  };
}
```

---

## Example Backtest Implementation

### Python Example

```python
import plotly.graph_objects as go
import numpy as np
from datetime import datetime, timedelta

def main(id: str, config: dict) -> dict:
  """
  Simple backtest implementation example
  """

  # Generate sample data (1 year)
  np.random.seed(42)
  days = 252
  dates = [(datetime.now() - timedelta(days=days-i)).strftime("%Y-%m-%d") for i in range(days)]

  # Simulate portfolio growth
  daily_returns = np.random.normal(0.001, 0.02, days)
  portfolio_values = [10000]

  for ret in daily_returns:
    portfolio_values.append(portfolio_values[-1] * (1 + ret))

  # Calculate basic metrics
  total_return = (portfolio_values[-1] - portfolio_values[0]) / portfolio_values[0]
  sharpe_ratio = np.mean(daily_returns) / np.std(daily_returns) * np.sqrt(252)

  # Create simple portfolio plot
  fig = go.Figure()
  fig.add_trace(go.Scatter(
    x=dates,
    y=portfolio_values[:-1],  # Match dates length
    mode='lines',
    name='Portfolio Value'
  ))
  fig.update_layout(
    title="Portfolio Performance",
    xaxis_title="Date",
    yaxis_title="Value ($)"
  )

  return {
    "status": "success",
    "results": {
      "metrics": {
        "total_return": round(total_return, 4),
        "sharpe_ratio": round(sharpe_ratio, 2),
        "final_value": round(portfolio_values[-1], 2),
        "total_trades": 50
      },
      "plots": [fig.to_json()],
      "tables": [
        {
          "title": "Summary",
          "data": [
            {"metric": "Starting Value", "value": "$10,000"},
            {"metric": "Final Value", "value": f"${portfolio_values[-1]:,.2f}"}
          ]
        }
      ]
    }
  }
```

### JavaScript Example

```javascript
const plotly = require("plotly.js");
const { mean, std } = require("mathjs");

function main(id, config) {
  /**
   * Simplified backtest implementation using libraries
   */

  // Generate sample data
  const days = 252;
  const startDate = new Date();
  startDate.setDate(startDate.getDate() - days);

  // Use mathjs for better random distribution
  const dailyReturns = Array.from(
    { length: days },
    () => Math.random() * 0.04 - 0.02, // Simple uniform distribution
  );

  // Calculate portfolio values
  const portfolioValues = dailyReturns.reduce(
    (acc, ret) => {
      const lastValue = acc[acc.length - 1];
      acc.push(lastValue * (1 + ret));
      return acc;
    },
    [10000],
  );

  // Generate simplified trade data
  const trades = dailyReturns
    .map((_, i) =>
      Math.random() < 0.3
        ? {
            date: new Date(startDate.getTime() + i * 24 * 60 * 60 * 1000)
              .toISOString()
              .split("T")[0],
            action: Math.random() > 0.5 ? "buy" : "sell",
            price: Math.round(portfolioValues[i]),
            quantity: 10,
          }
        : null,
    )
    .filter(Boolean);

  // Calculate metrics using mathjs
  const totalReturn =
    (portfolioValues[portfolioValues.length - 1] - portfolioValues[0]) /
    portfolioValues[0];
  const sharpeRatio = (mean(dailyReturns) / std(dailyReturns)) * Math.sqrt(252);
  const maxDrawdown = Math.min(
    ...portfolioValues.map(
      (val, i) =>
        (val - Math.max(...portfolioValues.slice(0, i + 1))) /
        Math.max(...portfolioValues.slice(0, i + 1)),
    ),
  );

  // Create simple Plotly plots
  const dates = Array.from(
    { length: portfolioValues.length },
    (_, i) =>
      new Date(startDate.getTime() + i * 24 * 60 * 60 * 1000)
        .toISOString()
        .split("T")[0],
  );

  const plots = [
    JSON.stringify({
      data: [
        {
          x: dates,
          y: portfolioValues,
          type: "scatter",
          mode: "lines",
          name: "Portfolio",
        },
      ],
      layout: {
        title: "Portfolio Performance",
        xaxis: { title: "Date" },
        yaxis: { title: "Value" },
      },
    }),
    JSON.stringify({
      data: [
        {
          x: dailyReturns.map((r) => r * 100),
          type: "histogram",
          name: "Returns",
        },
      ],
      layout: { title: "Returns Distribution", xaxis: { title: "Return (%)" } },
    }),
  ];

  return {
    status: "success",
    results: {
      metrics: {
        total_return: Number(totalReturn.toFixed(4)),
        sharpe_ratio: Number(sharpeRatio.toFixed(2)),
        max_drawdown: Number(maxDrawdown.toFixed(4)),
        win_rate: Number(
          (
            trades.filter((t) => t.action === "sell").length / trades.length
          ).toFixed(2),
        ),
        total_trades: trades.length,
        final_value: Number(
          portfolioValues[portfolioValues.length - 1].toFixed(2),
        ),
      },
      plots: plots,
      tables: [
        { title: "Recent Trades", data: trades.slice(-5) },
        {
          title: "Summary",
          data: [
            {
              metric: "Total Return",
              value: `${(totalReturn * 100).toFixed(1)}%`,
            },
          ],
        },
      ],
    },
  };
}
```

---

## Backtesting rules of thumb

- **Use realistic data**: Ensure historical data reflects real market conditions
- **Incorporate slippage and fees**: Simulate realistic execution costs
- **Test edge cases**: Include extreme market conditions
- **Validate results**: Ensure metrics are accurate and meaningful
- **Document assumptions**: Clearly state any assumptions when implementing backtests for custom bots

---

## Related Documentation

1. [Quick Start Guide](/docs/custom-bot-development/quick-start-guide) - Build a DCA bot in 15 minutes
2. [Configuration Reference](/docs/custom-bot-development/configuration) - Detailed configuration options
3. [Testing Guide](/docs/custom-bot-development/testing) - Testing strategies and tools
