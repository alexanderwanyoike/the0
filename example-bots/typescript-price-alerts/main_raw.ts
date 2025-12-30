/**
 * Price Alerts Bot
 * ================
 * A realtime bot that monitors simulated prices and emits alerts.
 *
 * This example demonstrates:
 * - Structured metric emission using pino with _metric field
 * - Realtime bot pattern (runs continuously until stopped)
 * - How to emit different metric types (price, alert, signal)
 *
 * Metrics emitted:
 * - price: Current price snapshots
 * - alert: Price movement alerts
 * - signal: Buy/sell trading signals
 */

import pino from "pino";

// Configure pino for JSON output
// The platform parses JSON logs and extracts metrics with _metric field
const logger = pino({
  level: "info",
  // Ensure timestamps are included
  timestamp: pino.stdTimeFunctions.isoTime,
});

// Bot state (persists across iterations)
interface BotState {
  lastPrice: number;
  priceHistory: number[];
  lastAlertTime: number;
}

/**
 * Bot entry point - called once when bot starts.
 * For realtime bots, this runs continuously until stopped.
 */
export async function main(
  id: string,
  config: Record<string, unknown>
): Promise<{ status: string; message: string }> {
  // Extract configuration with defaults
  const symbol = (config.symbol as string) || "BTC/USD";
  const basePrice = (config.base_price as number) || 45000;
  const alertThreshold = (config.alert_threshold as number) || 1.0;
  const updateInterval = (config.update_interval_ms as number) || 5000;

  logger.info({ botId: id, symbol, alertThreshold }, "bot_started");

  // Initialize bot state
  const state: BotState = {
    lastPrice: basePrice,
    priceHistory: [basePrice],
    lastAlertTime: 0,
  };

  // Main loop - runs until process is terminated
  try {
    while (true) {
      // Simulate price update
      const priceData = simulatePrice(state, symbol, basePrice);

      // Emit price metric
      // The _metric field tells the platform this is a metric, not just a log
      logger.info(
        {
          _metric: "price",
          symbol: priceData.symbol,
          value: priceData.value,
          change_pct: priceData.change_pct,
          high_24h: priceData.high_24h,
          low_24h: priceData.low_24h,
        },
        "price_update"
      );

      // Check for alert conditions
      const alert = checkAlertConditions(
        state,
        priceData,
        alertThreshold,
        symbol
      );
      if (alert) {
        logger.info(
          {
            _metric: "alert",
            symbol: alert.symbol,
            type: alert.type,
            change_pct: alert.change_pct,
            message: alert.message,
            severity: alert.severity,
          },
          "price_alert"
        );
      }

      // Generate trading signals based on trend
      const signal = generateSignal(state, symbol);
      if (signal) {
        logger.info(
          {
            _metric: "signal",
            symbol: signal.symbol,
            direction: signal.direction,
            confidence: signal.confidence,
            reason: signal.reason,
          },
          "signal_generated"
        );
      }

      // Update state
      state.lastPrice = priceData.value;
      state.priceHistory.push(priceData.value);
      if (state.priceHistory.length > 100) {
        state.priceHistory.shift(); // Keep last 100 prices
      }

      // Wait for next update
      await sleep(updateInterval);
    }
  } catch (error) {
    if ((error as Error).message === "SIGTERM") {
      logger.info({ botId: id }, "bot_stopped");
      return { status: "stopped", message: "Bot stopped gracefully" };
    }
    throw error;
  }
}

/**
 * Simulate price movement using random walk
 */
function simulatePrice(
  state: BotState,
  symbol: string,
  basePrice: number
): {
  symbol: string;
  value: number;
  change_pct: number;
  high_24h: number;
  low_24h: number;
} {
  // Random walk with slight mean reversion
  const volatility = 0.002; // 0.2% per tick
  const meanReversion = 0.001;

  // Calculate price change
  const randomChange = (Math.random() - 0.5) * 2 * volatility;
  const reversion = (basePrice - state.lastPrice) / basePrice * meanReversion;
  const changePercent = randomChange + reversion;

  const newPrice = state.lastPrice * (1 + changePercent);
  const changePct =
    ((newPrice - state.lastPrice) / state.lastPrice) * 100;

  // Calculate 24h high/low from history
  const high24h = Math.max(...state.priceHistory, newPrice);
  const low24h = Math.min(...state.priceHistory, newPrice);

  return {
    symbol,
    value: Math.round(newPrice * 100) / 100,
    change_pct: Math.round(changePct * 1000) / 1000,
    high_24h: Math.round(high24h * 100) / 100,
    low_24h: Math.round(low24h * 100) / 100,
  };
}

/**
 * Check if alert conditions are met
 */
function checkAlertConditions(
  state: BotState,
  priceData: { value: number; change_pct: number },
  threshold: number,
  symbol: string
): {
  symbol: string;
  type: string;
  change_pct: number;
  message: string;
  severity: string;
} | null {
  const now = Date.now();

  // Don't spam alerts - minimum 30 seconds between alerts
  if (now - state.lastAlertTime < 30000) {
    return null;
  }

  // Check absolute change from last price
  const absChange = Math.abs(priceData.change_pct);

  if (absChange >= threshold) {
    state.lastAlertTime = now;

    const direction = priceData.change_pct > 0 ? "spike" : "drop";
    const severity =
      absChange >= threshold * 2
        ? "high"
        : absChange >= threshold * 1.5
          ? "medium"
          : "low";

    return {
      symbol,
      type: `price_${direction}`,
      change_pct: Math.round(priceData.change_pct * 100) / 100,
      message: `${symbol} ${direction} of ${absChange.toFixed(2)}%`,
      severity,
    };
  }

  return null;
}

/**
 * Generate trading signal based on price trend
 */
function generateSignal(
  state: BotState,
  symbol: string
): {
  symbol: string;
  direction: string;
  confidence: number;
  reason: string;
} | null {
  // Need at least 10 prices for trend analysis
  if (state.priceHistory.length < 10) {
    return null;
  }

  // Only generate signals occasionally (10% chance per tick)
  if (Math.random() > 0.1) {
    return null;
  }

  // Simple moving average crossover
  const shortPeriod = 5;
  const longPeriod = 10;

  const shortMA =
    state.priceHistory.slice(-shortPeriod).reduce((a, b) => a + b, 0) /
    shortPeriod;
  const longMA =
    state.priceHistory.slice(-longPeriod).reduce((a, b) => a + b, 0) /
    longPeriod;

  const maDiff = (shortMA - longMA) / longMA;

  // Generate signal if MA difference is significant
  if (Math.abs(maDiff) > 0.001) {
    const direction = maDiff > 0 ? "long" : "short";
    const confidence = Math.min(Math.abs(maDiff) * 100, 0.95);

    return {
      symbol,
      direction,
      confidence: Math.round(confidence * 100) / 100,
      reason: `MA${shortPeriod} ${direction === "long" ? "above" : "below"} MA${longPeriod}`,
    };
  }

  return null;
}

/**
 * Sleep utility
 */
function sleep(ms: number): Promise<void> {
  return new Promise((resolve) => setTimeout(resolve, ms));
}

// For local testing
if (require.main === module) {
  main("test-bot-id", {
    symbol: "BTC/USD",
    base_price: 45000,
    alert_threshold: 0.5,
    update_interval_ms: 2000,
  }).catch(console.error);
}
