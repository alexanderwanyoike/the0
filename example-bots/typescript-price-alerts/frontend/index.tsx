/**
 * Price Alerts Dashboard
 * ======================
 * Custom frontend for the price-alerts bot.
 *
 * This component demonstrates:
 * - Using useThe0Events() hook for realtime metrics
 * - Filtering recent events with utils.since()
 * - Getting latest values with utils.latest()
 * - Displaying alerts and signals
 *
 * The dashboard shows:
 * - Current price with live updates
 * - Recent price alerts
 * - Trading signals history
 */

import React from "react";
import { useThe0Events, BotEvent } from "@alexanderwanyoike/the0-react";

// ============================================================================
// Main Dashboard Component (default export required)
// ============================================================================

export default function Dashboard() {
  const { events, utils, loading, error } = useThe0Events();

  // Loading state
  if (loading) {
    return (
      <div className="flex items-center justify-center h-64">
        <div className="text-green-400 font-mono">Connecting to bot...</div>
      </div>
    );
  }

  // Error state
  if (error) {
    return (
      <div className="p-4 bg-red-950/30 border border-red-800 rounded">
        <p className="text-red-400">Error loading events: {error}</p>
      </div>
    );
  }

  // Get metrics by type
  const prices = utils.filterByType("price");
  const alerts = utils.filterByType("alert");
  const signals = utils.filterByType("signal");

  // Get latest values
  const latestPrice = utils.latest("price");

  // Get recent events (last hour)
  const recentAlerts = alerts.filter(
    (a) => Date.now() - a.timestamp.getTime() < 3600000
  );
  const recentSignals = signals.filter(
    (s) => Date.now() - s.timestamp.getTime() < 3600000
  );

  return (
    <div className="p-4 space-y-6 font-mono">
      {/* Header */}
      <div className="border-b border-green-900/50 pb-4">
        <h1 className="text-xl font-bold text-green-400">Price Alerts</h1>
        <p className="text-gray-500 text-sm">
          {prices.length} price updates, {alerts.length} alerts, {signals.length}{" "}
          signals
        </p>
      </div>

      {/* Current Price Card */}
      <PriceCard price={latestPrice} />

      {/* Two column layout */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        {/* Alerts */}
        <AlertsList alerts={recentAlerts} />

        {/* Signals */}
        <SignalsList signals={recentSignals} />
      </div>

      {/* Price History */}
      <PriceHistory prices={prices.slice(-20)} />
    </div>
  );
}

// ============================================================================
// Sub-components
// ============================================================================

/**
 * Displays the current price with high/low
 */
function PriceCard({ price }: { price: BotEvent | null }) {
  if (!price) {
    return (
      <div className="bg-gray-900 border border-gray-800 rounded p-6">
        <p className="text-gray-500">Waiting for price data...</p>
      </div>
    );
  }

  const data = price.data as {
    symbol: string;
    value: number;
    change_pct: number;
    high_24h: number;
    low_24h: number;
  };

  const isPositive = data.change_pct >= 0;

  return (
    <div className="bg-gray-900 border border-green-900/50 rounded p-6">
      <div className="flex items-center justify-between mb-4">
        <div className="text-sm text-gray-500">{data.symbol}</div>
        <div className="flex items-center gap-2">
          <span className="w-2 h-2 rounded-full bg-green-400 animate-pulse" />
          <span className="text-xs text-gray-500">Live</span>
        </div>
      </div>

      <div className="flex items-baseline gap-4 mb-4">
        <span className="text-4xl font-bold text-green-400">
          ${data.value.toLocaleString()}
        </span>
        <span
          className={`text-lg ${isPositive ? "text-green-500" : "text-red-500"}`}
        >
          {isPositive ? "+" : ""}
          {data.change_pct.toFixed(3)}%
        </span>
      </div>

      <div className="flex gap-8 text-sm">
        <div>
          <span className="text-gray-500">24h High: </span>
          <span className="text-green-400">${data.high_24h?.toLocaleString()}</span>
        </div>
        <div>
          <span className="text-gray-500">24h Low: </span>
          <span className="text-red-400">${data.low_24h?.toLocaleString()}</span>
        </div>
      </div>

      <div className="text-xs text-gray-600 mt-4">
        Updated: {price.timestamp.toLocaleTimeString()}
      </div>
    </div>
  );
}

/**
 * Displays recent alerts
 */
function AlertsList({ alerts }: { alerts: BotEvent[] }) {
  return (
    <div className="bg-gray-900 border border-gray-800 rounded p-4">
      <h2 className="text-sm font-semibold text-gray-400 mb-3">
        Recent Alerts ({alerts.length})
      </h2>

      {alerts.length === 0 ? (
        <p className="text-gray-500 text-sm">No recent alerts</p>
      ) : (
        <div className="space-y-3">
          {alerts
            .slice(-5)
            .reverse()
            .map((alert, i) => {
              const data = alert.data as {
                symbol: string;
                type: string;
                change_pct: number;
                message: string;
                severity: string;
              };

              const severityColors: Record<string, string> = {
                high: "border-red-500 bg-red-950/30",
                medium: "border-yellow-500 bg-yellow-950/30",
                low: "border-blue-500 bg-blue-950/30",
              };

              const bgColor =
                severityColors[data.severity] ||
                "border-gray-700 bg-gray-800/30";
              const isSpike = data.type?.includes("spike");

              return (
                <div
                  key={i}
                  className={`border-l-4 ${bgColor} p-3 rounded-r`}
                >
                  <div className="flex justify-between items-start">
                    <div>
                      <span
                        className={`text-sm font-medium ${isSpike ? "text-green-400" : "text-red-400"}`}
                      >
                        {data.type?.replace("_", " ").toUpperCase()}
                      </span>
                      <p className="text-gray-400 text-sm mt-1">
                        {data.message}
                      </p>
                    </div>
                    <span className="text-xs text-gray-500">
                      {alert.timestamp.toLocaleTimeString()}
                    </span>
                  </div>
                </div>
              );
            })}
        </div>
      )}
    </div>
  );
}

/**
 * Displays trading signals
 */
function SignalsList({ signals }: { signals: BotEvent[] }) {
  return (
    <div className="bg-gray-900 border border-gray-800 rounded p-4">
      <h2 className="text-sm font-semibold text-gray-400 mb-3">
        Trading Signals ({signals.length})
      </h2>

      {signals.length === 0 ? (
        <p className="text-gray-500 text-sm">No signals yet</p>
      ) : (
        <div className="space-y-3">
          {signals
            .slice(-5)
            .reverse()
            .map((signal, i) => {
              const data = signal.data as {
                symbol: string;
                direction: string;
                confidence: number;
                reason: string;
              };

              const isLong = data.direction === "long";

              return (
                <div
                  key={i}
                  className={`border-l-4 ${isLong ? "border-green-500 bg-green-950/20" : "border-red-500 bg-red-950/20"} p-3 rounded-r`}
                >
                  <div className="flex justify-between items-start">
                    <div>
                      <span
                        className={`text-sm font-medium ${isLong ? "text-green-400" : "text-red-400"}`}
                      >
                        {data.direction.toUpperCase()} {data.symbol}
                      </span>
                      <p className="text-gray-400 text-sm mt-1">{data.reason}</p>
                      <div className="mt-2">
                        <span className="text-xs text-gray-500">
                          Confidence:{" "}
                        </span>
                        <span className="text-xs text-gray-400">
                          {(data.confidence * 100).toFixed(0)}%
                        </span>
                        <div className="w-24 h-1 bg-gray-700 rounded mt-1">
                          <div
                            className={`h-1 rounded ${isLong ? "bg-green-500" : "bg-red-500"}`}
                            style={{ width: `${data.confidence * 100}%` }}
                          />
                        </div>
                      </div>
                    </div>
                    <span className="text-xs text-gray-500">
                      {signal.timestamp.toLocaleTimeString()}
                    </span>
                  </div>
                </div>
              );
            })}
        </div>
      )}
    </div>
  );
}

/**
 * Displays price history
 */
function PriceHistory({ prices }: { prices: BotEvent[] }) {
  if (prices.length === 0) {
    return (
      <div className="bg-gray-900 border border-gray-800 rounded p-4">
        <h2 className="text-sm font-semibold text-gray-400 mb-3">
          Price History
        </h2>
        <p className="text-gray-500 text-sm">No price history yet</p>
      </div>
    );
  }

  return (
    <div className="bg-gray-900 border border-gray-800 rounded p-4">
      <h2 className="text-sm font-semibold text-gray-400 mb-3">
        Price History (Last {prices.length})
      </h2>
      <div className="overflow-x-auto">
        <table className="w-full text-sm">
          <thead>
            <tr className="text-left text-gray-500 border-b border-gray-800">
              <th className="pb-2">Time</th>
              <th className="pb-2 text-right">Price</th>
              <th className="pb-2 text-right">Change</th>
              <th className="pb-2 text-right">High</th>
              <th className="pb-2 text-right">Low</th>
            </tr>
          </thead>
          <tbody>
            {prices
              .slice()
              .reverse()
              .map((event, i) => {
                const data = event.data as {
                  value: number;
                  change_pct: number;
                  high_24h: number;
                  low_24h: number;
                };
                const isPositive = data.change_pct >= 0;
                return (
                  <tr key={i} className="border-b border-gray-800/50">
                    <td className="py-2 text-gray-500">
                      {event.timestamp.toLocaleTimeString()}
                    </td>
                    <td className="py-2 text-right text-green-400">
                      ${data.value.toLocaleString()}
                    </td>
                    <td
                      className={`py-2 text-right ${isPositive ? "text-green-500" : "text-red-500"}`}
                    >
                      {isPositive ? "+" : ""}
                      {data.change_pct.toFixed(3)}%
                    </td>
                    <td className="py-2 text-right text-gray-400">
                      ${data.high_24h?.toLocaleString()}
                    </td>
                    <td className="py-2 text-right text-gray-400">
                      ${data.low_24h?.toLocaleString()}
                    </td>
                  </tr>
                );
              })}
          </tbody>
        </table>
      </div>
    </div>
  );
}
