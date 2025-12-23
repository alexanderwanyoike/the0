/**
 * Portfolio Tracker Dashboard
 * ===========================
 * Custom frontend for the portfolio-tracker bot.
 *
 * This component demonstrates:
 * - Using useThe0Events() hook to access bot metrics
 * - Filtering events by type with utils.filterByType()
 * - Getting latest values with utils.latest()
 * - Grouping scheduled bot runs with utils.groupByRun()
 *
 * The dashboard shows:
 * - Current portfolio value with change indicator
 * - Portfolio value history across runs
 * - Recent trades table
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
        <div className="text-green-400 font-mono">Loading metrics...</div>
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
  const portfolioValues = utils.filterByType("portfolio_value");
  const trades = utils.filterByType("trade");
  const positions = utils.filterByType("position");

  // Get latest portfolio value
  const latestPortfolio = utils.latest("portfolio_value");

  // Group events by run (for scheduled bots)
  const runs = utils.groupByRun();

  return (
    <div className="p-4 space-y-6 font-mono">
      {/* Header */}
      <div className="border-b border-green-900/50 pb-4">
        <h1 className="text-xl font-bold text-green-400">Portfolio Tracker</h1>
        <p className="text-gray-500 text-sm">
          {portfolioValues.length} snapshots, {trades.length} trades
        </p>
      </div>

      {/* Portfolio Value Card */}
      <PortfolioValueCard portfolio={latestPortfolio} />

      {/* Two column layout */}
      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        {/* Value History */}
        <ValueHistory values={portfolioValues.slice(-10)} />

        {/* Current Positions */}
        <PositionsTable positions={positions} />
      </div>

      {/* Recent Trades */}
      <TradesTable trades={trades.slice(-10)} />

      {/* Run Summary (for scheduled bots) */}
      <RunSummary runs={runs} />
    </div>
  );
}

// ============================================================================
// Sub-components
// ============================================================================

/**
 * Displays the current portfolio value with change indicator
 */
function PortfolioValueCard({ portfolio }: { portfolio: BotEvent | null }) {
  if (!portfolio) {
    return (
      <div className="bg-gray-900 border border-gray-800 rounded p-6">
        <p className="text-gray-500">No portfolio data yet</p>
      </div>
    );
  }

  const data = portfolio.data as {
    value: number;
    change_pct: number;
  };

  const isPositive = data.change_pct >= 0;

  return (
    <div className="bg-gray-900 border border-green-900/50 rounded p-6">
      <div className="text-sm text-gray-500 mb-1">Portfolio Value</div>
      <div className="flex items-baseline gap-4">
        <span className="text-3xl font-bold text-green-400">
          ${data.value.toLocaleString()}
        </span>
        <span
          className={`text-lg ${isPositive ? "text-green-500" : "text-red-500"}`}
        >
          {isPositive ? "+" : ""}
          {data.change_pct.toFixed(2)}%
        </span>
      </div>
      <div className="text-xs text-gray-600 mt-2">
        Last updated: {portfolio.timestamp.toLocaleString()}
      </div>
    </div>
  );
}

/**
 * Displays portfolio value history
 */
function ValueHistory({ values }: { values: BotEvent[] }) {
  if (values.length === 0) {
    return (
      <div className="bg-gray-900 border border-gray-800 rounded p-4">
        <h2 className="text-sm font-semibold text-gray-400 mb-3">
          Value History
        </h2>
        <p className="text-gray-500 text-sm">No history yet</p>
      </div>
    );
  }

  return (
    <div className="bg-gray-900 border border-gray-800 rounded p-4">
      <h2 className="text-sm font-semibold text-gray-400 mb-3">
        Value History (Last {values.length})
      </h2>
      <div className="space-y-2">
        {values.map((event, i) => {
          const data = event.data as { value: number; change_pct: number };
          const isPositive = data.change_pct >= 0;
          return (
            <div
              key={i}
              className="flex justify-between items-center text-sm border-b border-gray-800 pb-1"
            >
              <span className="text-gray-500">
                {event.timestamp.toLocaleTimeString()}
              </span>
              <span className="text-green-400">
                ${data.value.toLocaleString()}
              </span>
              <span
                className={isPositive ? "text-green-500" : "text-red-500"}
              >
                {isPositive ? "+" : ""}
                {data.change_pct.toFixed(2)}%
              </span>
            </div>
          );
        })}
      </div>
    </div>
  );
}

/**
 * Displays current positions
 */
function PositionsTable({ positions }: { positions: BotEvent[] }) {
  // Get only the latest position for each symbol
  const latestPositions = new Map<string, BotEvent>();
  positions.forEach((p) => {
    const data = p.data as { symbol: string };
    latestPositions.set(data.symbol, p);
  });

  const positionList = Array.from(latestPositions.values());

  if (positionList.length === 0) {
    return (
      <div className="bg-gray-900 border border-gray-800 rounded p-4">
        <h2 className="text-sm font-semibold text-gray-400 mb-3">Positions</h2>
        <p className="text-gray-500 text-sm">No positions</p>
      </div>
    );
  }

  return (
    <div className="bg-gray-900 border border-gray-800 rounded p-4">
      <h2 className="text-sm font-semibold text-gray-400 mb-3">
        Current Positions
      </h2>
      <table className="w-full text-sm">
        <thead>
          <tr className="text-left text-gray-500 border-b border-gray-800">
            <th className="pb-2">Symbol</th>
            <th className="pb-2 text-right">Qty</th>
            <th className="pb-2 text-right">Price</th>
            <th className="pb-2 text-right">Value</th>
          </tr>
        </thead>
        <tbody>
          {positionList.map((event, i) => {
            const data = event.data as {
              symbol: string;
              quantity: number;
              price: number;
              value: number;
            };
            return (
              <tr key={i} className="border-b border-gray-800/50">
                <td className="py-2 text-green-400">{data.symbol}</td>
                <td className="py-2 text-right text-gray-400">
                  {data.quantity}
                </td>
                <td className="py-2 text-right text-gray-400">
                  ${data.price.toLocaleString()}
                </td>
                <td className="py-2 text-right text-green-400">
                  ${data.value.toLocaleString()}
                </td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}

/**
 * Displays recent trades
 */
function TradesTable({ trades }: { trades: BotEvent[] }) {
  if (trades.length === 0) {
    return (
      <div className="bg-gray-900 border border-gray-800 rounded p-4">
        <h2 className="text-sm font-semibold text-gray-400 mb-3">
          Recent Trades
        </h2>
        <p className="text-gray-500 text-sm">No trades yet</p>
      </div>
    );
  }

  return (
    <div className="bg-gray-900 border border-gray-800 rounded p-4">
      <h2 className="text-sm font-semibold text-gray-400 mb-3">
        Recent Trades ({trades.length})
      </h2>
      <table className="w-full text-sm">
        <thead>
          <tr className="text-left text-gray-500 border-b border-gray-800">
            <th className="pb-2">Time</th>
            <th className="pb-2">Symbol</th>
            <th className="pb-2">Side</th>
            <th className="pb-2 text-right">Qty</th>
            <th className="pb-2 text-right">Price</th>
            <th className="pb-2 text-right">Total</th>
          </tr>
        </thead>
        <tbody>
          {trades
            .slice()
            .reverse()
            .map((event, i) => {
              const data = event.data as {
                symbol: string;
                side: string;
                quantity: number;
                price: number;
                total: number;
              };
              const isBuy = data.side === "buy";
              return (
                <tr key={i} className="border-b border-gray-800/50">
                  <td className="py-2 text-gray-500">
                    {event.timestamp.toLocaleTimeString()}
                  </td>
                  <td className="py-2 text-gray-400">{data.symbol}</td>
                  <td
                    className={`py-2 ${isBuy ? "text-green-400" : "text-red-400"}`}
                  >
                    {data.side.toUpperCase()}
                  </td>
                  <td className="py-2 text-right text-gray-400">
                    {data.quantity}
                  </td>
                  <td className="py-2 text-right text-gray-400">
                    ${data.price.toLocaleString()}
                  </td>
                  <td className="py-2 text-right text-green-400">
                    ${data.total.toLocaleString()}
                  </td>
                </tr>
              );
            })}
        </tbody>
      </table>
    </div>
  );
}

/**
 * Shows summary of bot runs (for scheduled bots)
 */
function RunSummary({ runs }: { runs: BotEvent[][] }) {
  if (runs.length === 0) {
    return null;
  }

  return (
    <div className="bg-gray-900 border border-gray-800 rounded p-4">
      <h2 className="text-sm font-semibold text-gray-400 mb-3">
        Execution Runs ({runs.length})
      </h2>
      <div className="text-sm text-gray-500">
        {runs.length} scheduled runs detected. Each run contains{" "}
        {runs[0]?.length || 0} to {runs[runs.length - 1]?.length || 0} events.
      </div>
    </div>
  );
}
