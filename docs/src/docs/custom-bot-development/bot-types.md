---
title: "Bot Types"
description: "Understanding different bot execution models"
tags: ["custom-bots", "bot-types", "execution-models"]
order: 9
---

# Bot Types and Execution Models

the0 platform provides two distinct bot execution models that determine how and when your trading algorithms run. Understanding these types is crucial for choosing the right approach for your strategy.

---

## Overview of Bot Types

### Execution Models vs Trading Strategies

the0 provides **execution models** (how bots run) rather than **trading strategies** (what bots do):

- **scheduled**: Run on fixed intervals using cron expressions
- **realtime**: Run continuously, processing live market data

These models give you complete flexibility to implement any trading strategy within the execution framework that best suits your needs.

## Scheduled Bots

### When to Use Scheduled Bots

Scheduled bots are ideal for strategies that:

- Execute at specific times or intervals
- Don't require continuous market monitoring
- Perform periodic analysis or rebalancing
- Have natural timing cycles (minutely, hourly, daily, weekly, monthly)

### Configuration

```yaml
# bot-config.yaml
type: scheduled
```

### Common Use Cases

- Dollar-Cost Averaging (DCA)
- Portfolio rebalancing
- Swing trading strategies
- Day trading with fixed intervals

### Implementation Pattern

```python
def main(id: str, config: Dict[str, Any]) -> Dict[str, Any]:
    """
    Scheduled bot entry point.

    Executes once per trigger, then exits.
    Should be stateless between executions.
    """
    try:
        # Fetch current market data
        market_data = fetch_market_data(config['symbol'])

        # Execute strategy logic
        result = execute_strategy(market_data, config)

        # Return execution results
        return {
            "status": "success",
            "message": "Scheduled execution completed",
            "data": result
        }

    except Exception as e:
        return {
            "status": "error",
            "message": f"Execution failed: {str(e)}"
        }
```

### Scheduling Examples

#### Cron Expression Reference

```bash
# Format: minute hour day month weekday
# Examples:

"0 0 * * *"     # Daily at midnight
"0 9 * * 1-5"   # Weekdays at 9 AM
"*/15 * * * *"  # Every 15 minutes
"0 0 1 * *"     # Monthly on the 1st at midnight
"0 12 * * 0"    # Weekly on Sunday at noon
"30 14 * * 1,3,5" # Mon, Wed, Fri at 2:30 PM
```

#### Advanced Scheduling

```json
{
  "name": "adaptive-dca",
  "type": "scheduled/adaptive-dca",
  "schedule": "0 */6 * * *", // Every 6 hours
  "parameters": {
    "symbol": "BTCUSDT",
    "base_amount": 50,
    "volatility_adjustment": true,
    "market_condition_check": true
  }
}
```

### Advantages

- **Resource Efficient**: Only runs when needed
- **Predictable**: Executes at known intervals
- **Simple State Management**: Stateless between executions
- **Cost Effective**: Lower compute costs than continuous bots cheaper than realtime bots

### Limitations

- **Not Real-time**: Cannot react immediately to market changes
- **Fixed Timing**: Limited flexibility in execution timing
- **Market Hours**: May execute during low liquidity periods

## Realtime Bots

### When to Use Realtime Bots

Realtime bots are perfect for strategies that:

- Require continuous market monitoring
- Need to react quickly to price movements
- Implement market making or arbitrage

### Configuration

```yaml
# bot-config.yaml
type: realtime
```

### Common Use Cases

- Grid trading strategies
- Market making
- Arbitrage detection
- Scalping strategies

### Implementation Pattern

```python
def main(id: str, config: Dict[str, Any]) -> Dict[str, Any]:
    """
    Realtime bot entry point.

    Runs continuously until stopped.
    Maintains state across iterations.
    """
    try:
        # Initialize bot state
        bot_state = initialize_bot_state(config)

        # Main execution loop
        while True:
            try:
                # Fetch real-time market data
                market_data = fetch_realtime_data(config['symbol'])

                # Process data and generate signals
                signals = process_market_data(market_data, bot_state)

                # Execute trades if signals present
                if signals:
                    execute_trades(signals, config, bot_state)

                # Update bot state
                update_bot_state(bot_state, market_data)

                # Sleep to respect rate limits
                time.sleep(config.get('update_interval', 1))

            except Exception as e:
                logger.error(f"Error in main loop: {e}")
                time.sleep(5)  # Brief pause before retry

    except KeyboardInterrupt:
        logger.info("Bot stopped by user")
        return {"status": "stopped", "message": "Bot stopped gracefully"}
    except Exception as e:
        logger.error(f"Fatal error: {e}")
        return {"status": "error", "message": f"Bot failed: {str(e)}"}
```

### WebSocket Integration

```javascript
// JavaScript example with WebSocket
class RealtimeBot {
  constructor(config) {
    this.config = config;
    this.positions = new Map();
    this.orders = new Map();
  }

  async start() {
    // Connect to exchange WebSocket
    const ws = new WebSocket(this.getWebSocketUrl());

    ws.on("message", (data) => {
      const marketData = JSON.parse(data);
      this.processMarketUpdate(marketData);
    });

    ws.on("error", (error) => {
      console.error("WebSocket error:", error);
      this.reconnect();
    });
  }

  processMarketUpdate(data) {
    // Process real-time market data
    const signals = this.generateSignals(data);

    // Execute trades immediately
    for (const signal of signals) {
      this.executeTrade(signal);
    }
  }

  async executeTrade(signal) {
    try {
      const order = await this.exchange.createOrder(signal);
      this.orders.set(order.id, order);
      console.log(`Order placed: ${order.id}`);
    } catch (error) {
      console.error("Trade execution failed:", error);
    }
  }
}

// Main entry point

function main(id, config) {
  /**
   * Realtime bot entry point.
   *
   * Creates and starts a RealtimeBot instance.
   * Handles graceful shutdown and error recovery.
   */
  try {
    // Create bot instance with configuration
    const bot = new RealtimeBot(config);

    // Start the bot
    console.log(`Starting realtime bot: ${id}`);
    bot.start();

    // Handle graceful shutdown
    process.on("SIGINT", () => {
      console.log("Shutdown signal received, stopping bot...");
      bot.stop();
    });

    process.on("SIGTERM", () => {
      console.log("Termination signal received, stopping bot...");
      bot.stop();
    });

    return {
      status: "success",
      message: `Realtime bot ${id} started successfully`,
    };
  } catch (error) {
    console.error(`Failed to start bot ${id}:`, error);
    return {
      status: "error",
      message: `Bot startup failed: ${error.message}`,
    };
  }
}
```

### Advantages

- **Immediate Response**: React to market changes instantly
- **Continuous Monitoring**: Never miss opportunities
- **Complex Strategies**: Support sophisticated algorithms

### Limitations

- **Higher Resource Usage**: Consumes more CPU and memory
- **More Complex**: Requires careful state management
- **Higher Costs**: More expensive than scheduled bots

## Choosing the Right Bot Type

### Decision Matrix

| Strategy Type         | Best Bot Type         | Reasoning                        |
| --------------------- | --------------------- | -------------------------------- |
| DCA                   | Scheduled             | Fixed timing, periodic execution |
| Grid Trading          | Realtime              | Continuous order management      |
| Portfolio Rebalancing | Scheduled             | Periodic analysis and adjustment |
| Market Making         | Realtime              | Continuous liquidity provision   |
| Arbitrage             | Realtime              | Immediate opportunity execution  |
| Mean Reversion        | Scheduled or Realtime | Depends on timeframe             |

## Performance Considerations

### Resource Usage by Type

#### Scheduled Bots

- **CPU**: Low (only during execution)
- **Memory**: Minimal
- **Network**: Periodic API calls
- **Storage**: Configuration and logs only

#### Realtime Bots

- **CPU**: Moderate to High (continuous processing)
- **Memory**: Moderate (state management)
- **Network**: High (continuous data streams)
- **Storage**: State data and extensive logs

## Best Practices by Type

### Scheduled Bots

1. **Keep Execution Fast**: Minimize processing time
2. **Handle Failures Gracefully**: Implement retry logic
3. **Log Well**: Track all executions
4. **Validate Timing**: Ensure appropriate schedule
5. **Stateless Design**: Don't rely on persistent state

### Realtime Bots

1. **Implement Circuit Breakers**: Prevent runaway execution
2. **Monitor Resource Usage**: Track CPU and memory (remember you have a 0.5 CPU and 512MB memory limit\*)
3. **Graceful Degradation**: Handle API rate limits
4. **State Management**: Carefully manage bot state
5. **Error Recovery**: Implement robust error handling

---

## Related Documentation

- [Quick Start Guide](/docs/custom-bot-development/quick-start-guide) - Build your first bot
- [Testing Guide](/docs/custom-bot-development/testing) - Validation and testing
- [Compliance](/docs/custom-bot-development/compliance) - Code review and platform requirements
