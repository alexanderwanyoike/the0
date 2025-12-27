/**
 * Portfolio Tracker Dashboard
 * ===========================
 * Custom frontend for the portfolio-tracker bot.
 */

import React from "react";
import { useThe0Events, BotEvent } from "@alexanderwanyoike/the0-react";

export default function Dashboard() {
  const { events, utils, loading, error } = useThe0Events();

  if (loading) {
    return (
      <div className="flex items-center justify-center h-64">
        <div className="text-primary font-mono">Loading metrics...</div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="p-4 bg-destructive/10 border border-destructive/30 rounded">
        <p className="text-destructive">Error loading events: {error}</p>
      </div>
    );
  }

  const portfolioValues = utils.filterByType("portfolio_value");
  const trades = utils.filterByType("trade");
  const positions = utils.filterByType("position");
  const latestPortfolio = utils.latest("portfolio_value");
  const runs = utils.groupByRun();

  return (
    <div className="p-4 space-y-6 font-mono">
      <div className="border-b border-border pb-4">
        <h1 className="text-xl font-bold text-primary">Portfolio Tracker</h1>
        <p className="text-muted-foreground text-sm">
          {portfolioValues.length} snapshots, {trades.length} trades
        </p>
      </div>

      <PortfolioValueCard portfolio={latestPortfolio} />

      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        <ValueHistory values={portfolioValues.slice(-10)} />
        <PositionsTable positions={positions} />
      </div>

      <TradesTable trades={trades.slice(-10)} />
      <RunSummary runs={runs} />
    </div>
  );
}

function PortfolioValueCard({ portfolio }: { portfolio: BotEvent | null }) {
  if (!portfolio) {
    return (
      <div className="bg-muted/50 border border-border rounded p-6">
        <p className="text-muted-foreground">No portfolio data yet</p>
      </div>
    );
  }

  const data = portfolio.data as { value: number; change_pct: number };
  const isPositive = data.change_pct >= 0;

  return (
    <div className="bg-muted/50 border border-border rounded p-6">
      <div className="text-sm text-muted-foreground mb-1">Portfolio Value</div>
      <div className="flex items-baseline gap-4">
        <span className="text-3xl font-bold text-primary">
          ${data.value.toLocaleString()}
        </span>
        <span className={`text-lg ${isPositive ? "text-green-600" : "text-red-600"}`}>
          {isPositive ? "+" : ""}{data.change_pct.toFixed(2)}%
        </span>
      </div>
      <div className="text-xs text-muted-foreground mt-2">
        Last updated: {portfolio.timestamp.toLocaleString()}
      </div>
    </div>
  );
}

function ValueHistory({ values }: { values: BotEvent[] }) {
  if (values.length === 0) {
    return (
      <div className="bg-muted/50 border border-border rounded p-4">
        <h2 className="text-sm font-semibold text-foreground mb-3">Value History</h2>
        <p className="text-muted-foreground text-sm">No history yet</p>
      </div>
    );
  }

  return (
    <div className="bg-muted/50 border border-border rounded p-4">
      <h2 className="text-sm font-semibold text-foreground mb-3">
        Value History (Last {values.length})
      </h2>
      <div className="space-y-2">
        {values.map((event, i) => {
          const data = event.data as { value: number; change_pct: number };
          const isPositive = data.change_pct >= 0;
          return (
            <div key={i} className="flex justify-between items-center text-sm border-b border-border pb-1">
              <span className="text-muted-foreground">{event.timestamp.toLocaleTimeString()}</span>
              <span className="text-primary">${data.value.toLocaleString()}</span>
              <span className={isPositive ? "text-green-600" : "text-red-600"}>
                {isPositive ? "+" : ""}{data.change_pct.toFixed(2)}%
              </span>
            </div>
          );
        })}
      </div>
    </div>
  );
}

function PositionsTable({ positions }: { positions: BotEvent[] }) {
  const latestPositions = new Map<string, BotEvent>();
  positions.forEach((p) => {
    const data = p.data as { symbol: string };
    latestPositions.set(data.symbol, p);
  });

  const positionList = Array.from(latestPositions.values());

  if (positionList.length === 0) {
    return (
      <div className="bg-muted/50 border border-border rounded p-4">
        <h2 className="text-sm font-semibold text-foreground mb-3">Positions</h2>
        <p className="text-muted-foreground text-sm">No positions</p>
      </div>
    );
  }

  return (
    <div className="bg-muted/50 border border-border rounded p-4">
      <h2 className="text-sm font-semibold text-foreground mb-3">Current Positions</h2>
      <table className="w-full text-sm">
        <thead>
          <tr className="text-left text-muted-foreground border-b border-border">
            <th className="pb-2">Symbol</th>
            <th className="pb-2 text-right">Qty</th>
            <th className="pb-2 text-right">Price</th>
            <th className="pb-2 text-right">Value</th>
          </tr>
        </thead>
        <tbody>
          {positionList.map((event, i) => {
            const data = event.data as { symbol: string; quantity: number; price: number; value: number };
            return (
              <tr key={i} className="border-b border-border/50">
                <td className="py-2 text-primary">{data.symbol}</td>
                <td className="py-2 text-right text-muted-foreground">{data.quantity}</td>
                <td className="py-2 text-right text-muted-foreground">${data.price.toLocaleString()}</td>
                <td className="py-2 text-right text-primary">${data.value.toLocaleString()}</td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}

function TradesTable({ trades }: { trades: BotEvent[] }) {
  if (trades.length === 0) {
    return (
      <div className="bg-muted/50 border border-border rounded p-4">
        <h2 className="text-sm font-semibold text-foreground mb-3">Recent Trades</h2>
        <p className="text-muted-foreground text-sm">No trades yet</p>
      </div>
    );
  }

  return (
    <div className="bg-muted/50 border border-border rounded p-4">
      <h2 className="text-sm font-semibold text-foreground mb-3">Recent Trades ({trades.length})</h2>
      <table className="w-full text-sm">
        <thead>
          <tr className="text-left text-muted-foreground border-b border-border">
            <th className="pb-2">Time</th>
            <th className="pb-2">Symbol</th>
            <th className="pb-2">Side</th>
            <th className="pb-2 text-right">Qty</th>
            <th className="pb-2 text-right">Price</th>
            <th className="pb-2 text-right">Total</th>
          </tr>
        </thead>
        <tbody>
          {trades.slice().reverse().map((event, i) => {
            const data = event.data as { symbol: string; side: string; quantity: number; price: number; total: number };
            const isBuy = data.side === "buy";
            return (
              <tr key={i} className="border-b border-border/50">
                <td className="py-2 text-muted-foreground">{event.timestamp.toLocaleTimeString()}</td>
                <td className="py-2 text-foreground">{data.symbol}</td>
                <td className={`py-2 ${isBuy ? "text-green-600" : "text-red-600"}`}>
                  {data.side.toUpperCase()}
                </td>
                <td className="py-2 text-right text-muted-foreground">{data.quantity}</td>
                <td className="py-2 text-right text-muted-foreground">${data.price.toLocaleString()}</td>
                <td className="py-2 text-right text-primary">${data.total.toLocaleString()}</td>
              </tr>
            );
          })}
        </tbody>
      </table>
    </div>
  );
}

function RunSummary({ runs }: { runs: BotEvent[][] }) {
  if (runs.length === 0) {
    return null;
  }

  return (
    <div className="bg-muted/50 border border-border rounded p-4">
      <h2 className="text-sm font-semibold text-foreground mb-3">Execution Runs ({runs.length})</h2>
      <div className="text-sm text-muted-foreground">
        {runs.length} scheduled runs detected. Each run contains {runs[0]?.length || 0} to {runs[runs.length - 1]?.length || 0} events.
      </div>
    </div>
  );
}
