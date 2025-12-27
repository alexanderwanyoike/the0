/**
 * Price Alerts Bot (SDK Version)
 * ==============================
 * A realtime bot that monitors simulated prices and emits alerts.
 *
 * This is the SDK version - compare with main.ts to see how the SDK
 * simplifies configuration parsing and metric emission.
 *
 * Differences from main.ts:
 * - Uses `parse()` instead of function signature for config
 * - Uses `metric()` instead of pino with _metric field
 * - Uses pino for structured logging
 * - Uses `sleep()` from SDK instead of custom implementation
 */

import { parse, metric, sleep, success } from "@alexanderwanyoike/the0-node";
import pino from "pino";

// Configure pino logger for structured JSON output
const logger = pino({
  level: "info",
});

// Bot state (persists across iterations)
interface BotState {
  lastPrice: number;
  priceHistory: number[];
  lastAlertTime: number;
}

interface BotConfig {
  symbol: string;
  base_price: number;
  alert_threshold: number;
  update_interval_ms: number;
}

/**
 * Bot entry point using the0 SDK.
 * For realtime bots, this runs continuously until stopped.
 */
async function main(): Promise<void> {
  // Parse configuration from environment (set by the platform)
  const { id, config } = parse<BotConfig>();

  // Extract configuration with defaults
  const symbol = config.symbol || "BTC/USD";
  const basePrice = config.base_price || 45000;
  const alertThreshold = config.alert_threshold || 1.0;
  const updateInterval = config.update_interval_ms || 5000;

  logger.info({ botId: id, symbol, alertThreshold }, "Bot started");

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
      metric("price", {
        symbol: priceData.symbol,
        value: priceData.value,
        change_pct: priceData.change_pct,
        high_24h: priceData.high_24h,
        low_24h: priceData.low_24h,
      });

      // Check for alert conditions
      const alert = checkAlertConditions(
        state,
        priceData,
        alertThreshold,
        symbol
      );
      if (alert) {
        metric("alert", {
          symbol: alert.symbol,
          type: alert.type,
          change_pct: alert.change_pct,
          message: alert.message,
          severity: alert.severity,
        });
      }

      // Generate trading signals based on trend
      const signal = generateSignal(state, symbol);
      if (signal) {
        metric("signal", {
          symbol: signal.symbol,
          direction: signal.direction,
          confidence: signal.confidence,
          reason: signal.reason,
        });
      }

      // Update state
      state.lastPrice = priceData.value;
      state.priceHistory.push(priceData.value);
      if (state.priceHistory.length > 100) {
        state.priceHistory.shift(); // Keep last 100 prices
      }

      // Wait for next update (using SDK's sleep)
      await sleep(updateInterval);
    }
  } catch (err) {
    if ((err as Error).message === "SIGTERM") {
      logger.info({ botId: id }, "Bot stopped");
      success("Bot stopped gracefully");
      return;
    }
    throw err;
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
  const volatility = 0.002;
  const meanReversion = 0.001;

  const randomChange = (Math.random() - 0.5) * 2 * volatility;
  const reversion =
    ((basePrice - state.lastPrice) / basePrice) * meanReversion;
  const changePercent = randomChange + reversion;

  const newPrice = state.lastPrice * (1 + changePercent);
  const changePct = ((newPrice - state.lastPrice) / state.lastPrice) * 100;

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

  if (now - state.lastAlertTime < 30000) {
    return null;
  }

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
  if (state.priceHistory.length < 10) {
    return null;
  }

  if (Math.random() > 0.1) {
    return null;
  }

  const shortPeriod = 5;
  const longPeriod = 10;

  const shortMA =
    state.priceHistory.slice(-shortPeriod).reduce((a, b) => a + b, 0) /
    shortPeriod;
  const longMA =
    state.priceHistory.slice(-longPeriod).reduce((a, b) => a + b, 0) /
    longPeriod;

  const maDiff = (shortMA - longMA) / longMA;

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

// Export main for the runtime
export { main };
