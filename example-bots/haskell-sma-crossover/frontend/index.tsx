/**
 * SMA Crossover Dashboard
 * =======================
 * Custom frontend for the Haskell SMA crossover bot.
 * This is a SCHEDULED bot - shows results from each run.
 */

import React from "react";
import { useThe0Events, BotEvent } from "@alexanderwanyoike/the0-react";

export default function Dashboard() {
  const { events, utils, loading, error } = useThe0Events();

  if (loading) {
    return (
      <div className="flex items-center justify-center h-64">
        <div className="text-green-600 dark:text-green-400 font-mono">Connecting to bot...</div>
      </div>
    );
  }

  if (error) {
    return (
      <div className="p-4 bg-red-50 dark:bg-red-950/30 border border-red-300 dark:border-red-800 rounded">
        <p className="text-red-600 dark:text-red-400">Error loading events: {error}</p>
      </div>
    );
  }

  const prices = utils.filterByType("price");
  const smas = utils.filterByType("sma");
  const signals = utils.filterByType("signal");
  const latestPrice = utils.latest("price");
  const latestSma = utils.latest("sma");
  const latestSignal = utils.latest("signal");
  const runs = utils.groupByRun();

  return (
    <div className="p-4 space-y-6 font-mono">
      <div className="border-b border-gray-200 dark:border-gray-800 pb-4">
        <h1 className="text-xl font-bold text-green-600 dark:text-green-400">SMA Crossover Strategy (Haskell)</h1>
        <p className="text-gray-500 dark:text-gray-400 text-sm">
          Scheduled Bot - {runs.length} runs, {signals.length} signals generated
        </p>
      </div>

      <div className="grid grid-cols-1 md:grid-cols-3 gap-6">
        <PriceCard price={latestPrice} />
        <SMACard sma={latestSma} />
        <SignalCard signal={latestSignal} />
      </div>

      <RunHistory runs={runs} />
    </div>
  );
}

function PriceCard({ price }: { price: BotEvent | null }) {
  if (!price) {
    return (
      <div className="bg-gray-50 dark:bg-gray-900 border border-gray-200 dark:border-gray-800 rounded p-6">
        <p className="text-gray-500">Waiting for price data...</p>
      </div>
    );
  }

  const data = price.data as { symbol: string; value: number; change_pct: number };
  const isPositive = data.change_pct >= 0;

  return (
    <div className="bg-gray-50 dark:bg-gray-900 border border-gray-200 dark:border-gray-800 rounded p-6">
      <div className="text-sm text-gray-600 dark:text-gray-400 mb-2">Latest Price</div>
      <div className="text-3xl font-bold text-green-600 dark:text-green-400">${data.value.toLocaleString()}</div>
      <div className={`text-sm ${isPositive ? "text-green-600 dark:text-green-500" : "text-red-600 dark:text-red-500"}`}>
        {isPositive ? "+" : ""}{data.change_pct.toFixed(3)}%
      </div>
      <div className="text-xs text-gray-500 mt-2">{data.symbol}</div>
    </div>
  );
}

function SMACard({ sma }: { sma: BotEvent | null }) {
  if (!sma) {
    return (
      <div className="bg-gray-50 dark:bg-gray-900 border border-gray-200 dark:border-gray-800 rounded p-6">
        <p className="text-gray-500">Waiting for SMA data...</p>
      </div>
    );
  }

  const data = sma.data as {
    short_sma: number; long_sma: number;
    short_period: number; long_period: number;
  };
  const isBullish = data.short_sma > data.long_sma;

  return (
    <div className="bg-gray-50 dark:bg-gray-900 border border-gray-200 dark:border-gray-800 rounded p-6">
      <div className="text-sm text-gray-600 dark:text-gray-400 mb-2">Moving Averages</div>
      <div className="space-y-2">
        <div className="flex justify-between">
          <span className="text-gray-600 dark:text-gray-400 text-sm">SMA {data.short_period}</span>
          <span className="text-yellow-600 dark:text-yellow-400">${data.short_sma.toLocaleString()}</span>
        </div>
        <div className="flex justify-between">
          <span className="text-gray-600 dark:text-gray-400 text-sm">SMA {data.long_period}</span>
          <span className="text-blue-600 dark:text-blue-400">${data.long_sma.toLocaleString()}</span>
        </div>
      </div>
      <div className={`mt-3 text-center text-sm font-medium ${isBullish ? "text-green-700 dark:text-green-400" : "text-red-700 dark:text-red-400"}`}>
        {isBullish ? "Bullish" : "Bearish"}
      </div>
    </div>
  );
}

function SignalCard({ signal }: { signal: BotEvent | null }) {
  if (!signal) {
    return (
      <div className="bg-gray-50 dark:bg-gray-900 border border-gray-200 dark:border-gray-800 rounded p-6">
        <p className="text-gray-500">Waiting for signal...</p>
      </div>
    );
  }

  const data = signal.data as {
    type: string; symbol: string; price: number; confidence: number; reason: string;
  };
  const isBuy = data.type === "BUY";

  return (
    <div className={`bg-gray-50 dark:bg-gray-900 border rounded p-6 ${isBuy ? "border-green-400 dark:border-green-500" : "border-red-400 dark:border-red-500"}`}>
      <div className="text-sm text-gray-600 dark:text-gray-400 mb-2">Current Signal</div>
      <div className={`text-3xl font-bold ${isBuy ? "text-green-700 dark:text-green-400" : "text-red-700 dark:text-red-400"}`}>
        {data.type}
      </div>
      <div className="text-gray-600 dark:text-gray-400 text-sm mt-1">@ ${data.price.toLocaleString()}</div>
      <div className="mt-2">
        <span className="text-xs text-gray-500">Confidence: </span>
        <span className="text-xs text-gray-600 dark:text-gray-400">{(data.confidence * 100).toFixed(0)}%</span>
      </div>
    </div>
  );
}

function RunHistory({ runs }: { runs: BotEvent[][] }) {
  if (runs.length === 0) {
    return (
      <div className="bg-gray-50 dark:bg-gray-900 border border-gray-200 dark:border-gray-800 rounded p-4">
        <h2 className="text-sm font-semibold text-gray-700 dark:text-gray-300 mb-3">Run History</h2>
        <p className="text-gray-500 text-sm">No runs yet</p>
      </div>
    );
  }

  return (
    <div className="bg-gray-50 dark:bg-gray-900 border border-gray-200 dark:border-gray-800 rounded p-4">
      <h2 className="text-sm font-semibold text-gray-700 dark:text-gray-300 mb-3">
        Run History (Last {Math.min(runs.length, 10)})
      </h2>
      <div className="space-y-3">
        {runs.slice(-10).reverse().map((runEvents, i) => {
          const priceEvent = runEvents.find(e => (e.data as any)._metric === "price" || (e.data as any).value);
          const signalEvent = runEvents.find(e => (e.data as any)._metric === "signal" || (e.data as any).type);

          const priceData = priceEvent?.data as { value?: number; symbol?: string } | undefined;
          const signalData = signalEvent?.data as { type?: string; confidence?: number } | undefined;

          return (
            <div key={i} className="flex items-center justify-between p-2 bg-gray-100 dark:bg-gray-800/50 rounded">
              <div className="flex items-center gap-4">
                <span className="text-xs text-gray-500">
                  {runEvents[0]?.timestamp.toLocaleString()}
                </span>
                <span className="text-green-600 dark:text-green-400">
                  {priceData?.symbol}: ${priceData?.value?.toLocaleString() ?? "-"}
                </span>
              </div>
              {signalData && (
                <span className={`text-sm font-bold ${signalData.type === "BUY" ? "text-green-700 dark:text-green-400" : "text-red-700 dark:text-red-400"}`}>
                  {signalData.type} ({((signalData.confidence ?? 0) * 100).toFixed(0)}%)
                </span>
              )}
            </div>
          );
        })}
      </div>
    </div>
  );
}
