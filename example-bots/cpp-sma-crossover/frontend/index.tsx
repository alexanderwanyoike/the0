/**
 * SMA Crossover Dashboard
 * =======================
 * Custom frontend for the C++ SMA crossover bot.
 */

import React from "react";
import { useThe0Events, BotEvent } from "@alexanderwanyoike/the0-react";

export default function Dashboard() {
  const { events, utils, loading, error } = useThe0Events();

  if (loading) {
    return (
      <div className="flex items-center justify-center h-64">
        <div className="text-green-400 font-mono">Connecting to bot...</div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="p-4 bg-red-950/30 border border-red-800 rounded">
        <p className="text-red-400">Error loading events: {error}</p>
      </div>
    );
  }

  const prices = utils.filterByType("price");
  const smas = utils.filterByType("sma");
  const signals = utils.filterByType("signal");
  const latestPrice = utils.latest("price");
  const latestSma = utils.latest("sma");

  return (
    <div className="p-4 space-y-6 font-mono">
      <div className="border-b border-green-900/50 pb-4">
        <h1 className="text-xl font-bold text-green-400">SMA Crossover Strategy (C++)</h1>
        <p className="text-gray-500 text-sm">
          {prices.length} price updates, {signals.length} signals generated
        </p>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-2 gap-6">
        <PriceCard price={latestPrice} />
        <SMACard sma={latestSma} />
      </div>

      <SignalsList signals={signals} />
      <PriceHistory prices={prices} smas={smas} />
    </div>
  );
}

function PriceCard({ price }: { price: BotEvent | null }) {
  if (!price) {
    return (
      <div className="bg-gray-900 border border-gray-800 rounded p-6">
        <p className="text-gray-500">Waiting for price data...</p>
      </div>
    );
  }

  const data = price.data as { symbol: string; value: number; change_pct: number };
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
      <div className="flex items-baseline gap-4 mb-2">
        <span className="text-4xl font-bold text-green-400">${data.value.toLocaleString()}</span>
        <span className={`text-lg ${isPositive ? "text-green-500" : "text-red-500"}`}>
          {isPositive ? "+" : ""}{data.change_pct.toFixed(3)}%
        </span>
      </div>
      <div className="text-xs text-gray-600 mt-4">
        Updated: {price.timestamp.toLocaleTimeString()}
      </div>
    </div>
  );
}

function SMACard({ sma }: { sma: BotEvent | null }) {
  if (!sma) {
    return (
      <div className="bg-gray-900 border border-gray-800 rounded p-6">
        <p className="text-gray-500">Waiting for SMA data...</p>
      </div>
    );
  }

  const data = sma.data as {
    symbol: string; short_sma: number; long_sma: number;
    short_period: number; long_period: number;
  };
  const isBullish = data.short_sma > data.long_sma;

  return (
    <div className="bg-gray-900 border border-blue-900/50 rounded p-6">
      <div className="text-sm text-gray-500 mb-4">Moving Averages</div>
      <div className="space-y-4">
        <div className="flex justify-between items-center">
          <span className="text-gray-400">SMA {data.short_period} (Fast)</span>
          <span className="text-xl font-bold text-yellow-400">${data.short_sma.toLocaleString()}</span>
        </div>
        <div className="flex justify-between items-center">
          <span className="text-gray-400">SMA {data.long_period} (Slow)</span>
          <span className="text-xl font-bold text-blue-400">${data.long_sma.toLocaleString()}</span>
        </div>
      </div>
      <div className={`mt-4 p-2 rounded text-center ${isBullish ? "bg-green-950/30 border border-green-800" : "bg-red-950/30 border border-red-800"}`}>
        <span className={`text-sm ${isBullish ? "text-green-400" : "text-red-400"}`}>
          {isBullish ? "Bullish Trend" : "Bearish Trend"}
        </span>
      </div>
    </div>
  );
}

function SignalsList({ signals }: { signals: BotEvent[] }) {
  return (
    <div className="bg-gray-900 border border-gray-800 rounded p-4">
      <h2 className="text-sm font-semibold text-gray-400 mb-3">Trading Signals ({signals.length})</h2>
      {signals.length === 0 ? (
        <p className="text-gray-500 text-sm">No signals yet - waiting for SMA crossover</p>
      ) : (
        <div className="space-y-3">
          {signals.slice(-5).reverse().map((signal, i) => {
            const data = signal.data as {
              type: string; symbol: string; price: number; confidence: number; reason: string;
            };
            const isBuy = data.type === "BUY";
            return (
              <div key={i} className={`border-l-4 ${isBuy ? "border-green-500 bg-green-950/20" : "border-red-500 bg-red-950/20"} p-3 rounded-r`}>
                <div className="flex justify-between items-start">
                  <div>
                    <span className={`text-lg font-bold ${isBuy ? "text-green-400" : "text-red-400"}`}>
                      {data.type} {data.symbol}
                    </span>
                    <p className="text-gray-400 text-sm mt-1">@ ${data.price.toLocaleString()}</p>
                    <p className="text-gray-500 text-sm mt-1">{data.reason}</p>
                    <div className="mt-2">
                      <span className="text-xs text-gray-500">Confidence: </span>
                      <span className="text-xs text-gray-400">{(data.confidence * 100).toFixed(0)}%</span>
                      <div className="w-24 h-1 bg-gray-700 rounded mt-1">
                        <div className={`h-1 rounded ${isBuy ? "bg-green-500" : "bg-red-500"}`} style={{ width: `${data.confidence * 100}%` }} />
                      </div>
                    </div>
                  </div>
                  <span className="text-xs text-gray-500">{signal.timestamp.toLocaleTimeString()}</span>
                </div>
              </div>
            );
          })}
        </div>
      )}
    </div>
  );
}

function PriceHistory({ prices, smas }: { prices: BotEvent[]; smas: BotEvent[] }) {
  if (prices.length === 0) {
    return (
      <div className="bg-gray-900 border border-gray-800 rounded p-4">
        <h2 className="text-sm font-semibold text-gray-400 mb-3">Price History</h2>
        <p className="text-gray-500 text-sm">No price history yet</p>
      </div>
    );
  }

  return (
    <div className="bg-gray-900 border border-gray-800 rounded p-4">
      <h2 className="text-sm font-semibold text-gray-400 mb-3">Price & SMA History (Last {Math.min(prices.length, 20)})</h2>
      <div className="overflow-x-auto">
        <table className="w-full text-sm">
          <thead>
            <tr className="text-left text-gray-500 border-b border-gray-800">
              <th className="pb-2">Time</th>
              <th className="pb-2 text-right">Price</th>
              <th className="pb-2 text-right">Change</th>
              <th className="pb-2 text-right">SMA (Fast)</th>
              <th className="pb-2 text-right">SMA (Slow)</th>
            </tr>
          </thead>
          <tbody>
            {prices.slice(-20).reverse().map((event, i) => {
              const data = event.data as { value: number; change_pct: number };
              const isPositive = data.change_pct >= 0;
              const smaData = smas[smas.length - 1 - i]?.data as { short_sma?: number; long_sma?: number } | undefined;
              return (
                <tr key={i} className="border-b border-gray-800/50">
                  <td className="py-2 text-gray-500">{event.timestamp.toLocaleTimeString()}</td>
                  <td className="py-2 text-right text-green-400">${data.value.toLocaleString()}</td>
                  <td className={`py-2 text-right ${isPositive ? "text-green-500" : "text-red-500"}`}>
                    {isPositive ? "+" : ""}{data.change_pct.toFixed(3)}%
                  </td>
                  <td className="py-2 text-right text-yellow-400">
                    {smaData?.short_sma ? `$${smaData.short_sma.toLocaleString()}` : "-"}
                  </td>
                  <td className="py-2 text-right text-blue-400">
                    {smaData?.long_sma ? `$${smaData.long_sma.toLocaleString()}` : "-"}
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
