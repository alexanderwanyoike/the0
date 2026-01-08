/**
 * Price Alerts Query Handlers
 * ===========================
 * Query endpoints for the price alerts bot.
 *
 * These handlers provide read-only access to bot state and computed data.
 * They can be executed via:
 *   - CLI: the0 bot query <bot_id> /current-price
 *   - API: POST /bot/:id/query
 *
 * Available queries:
 *   /current-price - Get latest price with 24h range and change
 *   /alerts        - Get alert count and statistics
 *   /statistics    - Get overall bot statistics
 */

import { query, state } from "@alexanderwanyoike/the0-node";

// Persistent state structure (matches main.ts)
interface PersistedState {
  priceHistory: number[];
  alertCount: number;
}

/**
 * Get current price information.
 *
 * Returns:
 *   current_price: Latest price value
 *   high_24h: Highest price in history
 *   low_24h: Lowest price in history
 *   change_pct: Percentage change from oldest to newest
 *   data_points: Number of price data points
 */
query.handler("/current-price", (req) => {
  const persisted = state.get<PersistedState>("bot_state", {
    priceHistory: [],
    alertCount: 0,
  });

  const history = persisted.priceHistory;

  if (history.length === 0) {
    return {
      status: "no_data",
      message: "No price history available",
    };
  }

  const currentPrice = history[history.length - 1];
  const high24h = Math.max(...history);
  const low24h = Math.min(...history);
  const changePct =
    history.length > 1
      ? ((currentPrice - history[0]) / history[0]) * 100
      : 0;

  return {
    current_price: Math.round(currentPrice * 100) / 100,
    high_24h: Math.round(high24h * 100) / 100,
    low_24h: Math.round(low24h * 100) / 100,
    change_pct: Math.round(changePct * 1000) / 1000,
    data_points: history.length,
  };
});

/**
 * Get alert information.
 *
 * Query params:
 *   limit: Maximum number of alerts to return (for future use)
 *
 * Returns:
 *   total_alerts: Total alerts generated
 *   price_data_points: Number of price history entries
 */
query.handler("/alerts", (req) => {
  const limit = parseInt(req.get("limit", "10") || "10", 10);
  const persisted = state.get<PersistedState>("bot_state", {
    priceHistory: [],
    alertCount: 0,
  });

  return {
    total_alerts: persisted.alertCount,
    limit_applied: limit,
    // Note: Individual alerts are not persisted, only the count
    // To return actual alerts, we would need to persist alert history
  };
});

/**
 * Get bot statistics.
 *
 * Returns:
 *   total_alerts: Total alerts generated
 *   price_data_points: Number of price entries
 *   config: Bot configuration summary
 */
query.handler("/statistics", (req) => {
  const config = query.getConfig();
  const persisted = state.get<PersistedState>("bot_state", {
    priceHistory: [],
    alertCount: 0,
  });

  return {
    total_alerts: persisted.alertCount,
    price_data_points: persisted.priceHistory.length,
    config: {
      symbol: config.symbol || "BTC/USD",
      alert_threshold: config.alert_threshold || 1.0,
      update_interval_ms: config.update_interval_ms || 5000,
    },
  };
});

// Run query system
query.run();
