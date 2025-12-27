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
        <div className="text-primary font-mono">Connecting to bot...</div>
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

  const prices = utils.filterByType("price");
  const smas = utils.filterByType("sma");
  const signals = utils.filterByType("signal");
  const latestPrice = utils.latest("price");
  const latestSma = utils.latest("sma");
  const latestSignal = utils.latest("signal");
  const runs = utils.groupByRun();

  return (
    <div className="p-4 space-y-6 font-mono">
      <div className="border-b border-border pb-4">
        <h1 className="text-xl font-bold text-primary">SMA Crossover Strategy (Haskell)</h1>
        <p className="text-muted-foreground text-sm">
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
      <div className="bg-muted/50 border border-border rounded p-6">
        <p className="text-muted-foreground">Waiting for price data...</p>
      </div>
    );
  }

  const data = price.data as { symbol: string; value: number; change_pct: number };
  const isPositive = data.change_pct >= 0;

  return (
    <div className="bg-muted/50 border border-border rounded p-6">
      <div className="text-sm text-muted-foreground mb-2">Latest Price</div>
      <div className="text-3xl font-bold text-primary">${data.value.toLocaleString()}</div>
      <div className={`text-sm ${isPositive ? "text-green-600" : "text-red-600"}`}>
        {isPositive ? "+" : ""}{data.change_pct.toFixed(3)}%
      </div>
      <div className="text-xs text-muted-foreground mt-2">{data.symbol}</div>
    </div>
  );
}

function SMACard({ sma }: { sma: BotEvent | null }) {
  if (!sma) {
    return (
      <div className="bg-muted/50 border border-border rounded p-6">
        <p className="text-muted-foreground">Waiting for SMA data...</p>
      </div>
    );
  }

  const data = sma.data as {
    short_sma: number; long_sma: number;
    short_period: number; long_period: number;
  };
  const isBullish = data.short_sma > data.long_sma;

  return (
    <div className="bg-muted/50 border border-border rounded p-6">
      <div className="text-sm text-muted-foreground mb-2">Moving Averages</div>
      <div className="space-y-2">
        <div className="flex justify-between">
          <span className="text-muted-foreground text-sm">SMA {data.short_period}</span>
          <span className="text-yellow-600">${data.short_sma.toLocaleString()}</span>
        </div>
        <div className="flex justify-between">
          <span className="text-muted-foreground text-sm">SMA {data.long_period}</span>
          <span className="text-blue-600">${data.long_sma.toLocaleString()}</span>
        </div>
      </div>
      <div className={`mt-3 text-center text-sm font-medium ${isBullish ? "text-green-600" : "text-red-600"}`}>
        {isBullish ? "Bullish" : "Bearish"}
      </div>
    </div>
  );
}

function SignalCard({ signal }: { signal: BotEvent | null }) {
  if (!signal) {
    return (
      <div className="bg-muted/50 border border-border rounded p-6">
        <p className="text-muted-foreground">Waiting for signal...</p>
      </div>
    );
  }

  const data = signal.data as {
    type: string; symbol: string; price: number; confidence: number; reason: string;
  };
  const isBuy = data.type === "BUY";

  return (
    <div className={`bg-muted/50 border rounded p-6 ${isBuy ? "border-green-500" : "border-red-500"}`}>
      <div className="text-sm text-muted-foreground mb-2">Current Signal</div>
      <div className={`text-3xl font-bold ${isBuy ? "text-green-600" : "text-red-600"}`}>
        {data.type}
      </div>
      <div className="text-muted-foreground text-sm mt-1">@ ${data.price.toLocaleString()}</div>
      <div className="mt-2">
        <span className="text-xs text-muted-foreground">Confidence: </span>
        <span className="text-xs text-foreground">{(data.confidence * 100).toFixed(0)}%</span>
      </div>
    </div>
  );
}

function RunHistory({ runs }: { runs: BotEvent[][] }) {
  if (runs.length === 0) {
    return (
      <div className="bg-muted/50 border border-border rounded p-4">
        <h2 className="text-sm font-semibold text-foreground mb-3">Run History</h2>
        <p className="text-muted-foreground text-sm">No runs yet</p>
      </div>
    );
  }

  return (
    <div className="bg-muted/50 border border-border rounded p-4">
      <h2 className="text-sm font-semibold text-foreground mb-3">
        Run History (Last {Math.min(runs.length, 10)})
      </h2>
      <div className="space-y-3">
        {runs.slice(-10).reverse().map((runEvents, i) => {
          const priceEvent = runEvents.find(e => (e.data as any)._metric === "price" || (e.data as any).value);
          const signalEvent = runEvents.find(e => (e.data as any)._metric === "signal" || (e.data as any).type);

          const priceData = priceEvent?.data as { value?: number; symbol?: string } | undefined;
          const signalData = signalEvent?.data as { type?: string; confidence?: number } | undefined;

          return (
            <div key={i} className="flex items-center justify-between p-2 bg-muted/30 rounded">
              <div className="flex items-center gap-4">
                <span className="text-xs text-muted-foreground">
                  {runEvents[0]?.timestamp.toLocaleString()}
                </span>
                <span className="text-primary">
                  {priceData?.symbol}: ${priceData?.value?.toLocaleString() ?? "-"}
                </span>
              </div>
              {signalData && (
                <span className={`text-sm font-bold ${signalData.type === "BUY" ? "text-green-600" : "text-red-600"}`}>
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
