/**
 * Time utilities for filtering and aggregating bot events.
 */

import { BotEvent, isMetricEvent } from "./event-parser";

/**
 * Parse duration string into milliseconds.
 * Supports: '30s', '5m', '1h', '1d', '1w'
 */
function parseDuration(duration: string): number {
  const match = duration.match(/^(\d+)(s|m|h|d|w)$/);
  if (!match) {
    throw new Error(
      `Invalid duration format: ${duration}. Use format like '30s', '5m', '1h', '1d', '1w'`,
    );
  }

  const value = parseInt(match[1], 10);
  const unit = match[2];

  const multipliers: Record<string, number> = {
    s: 1000,
    m: 60 * 1000,
    h: 60 * 60 * 1000,
    d: 24 * 60 * 60 * 1000,
    w: 7 * 24 * 60 * 60 * 1000,
  };

  return value * multipliers[unit];
}

/**
 * Filter events from the last N duration.
 * @example since(events, '1h') // Events from last hour
 */
export function since(events: BotEvent[], duration: string): BotEvent[] {
  const ms = parseDuration(duration);
  const cutoff = new Date(Date.now() - ms);
  return events.filter((e) => e.timestamp >= cutoff);
}

/**
 * Filter events between two dates.
 */
export function between(
  events: BotEvent[],
  start: Date,
  end: Date,
): BotEvent[] {
  return events.filter((e) => e.timestamp >= start && e.timestamp <= end);
}

/**
 * Filter events by metric type.
 * @example filterByType(events, 'trade') // Only trade metrics
 */
export function filterByType(
  events: BotEvent[],
  metricType: string,
): BotEvent[] {
  return events.filter(
    (e) => e.type === "metric" && e.metricType === metricType,
  );
}

/**
 * Get only log events (non-metrics).
 */
export function logs(events: BotEvent[]): BotEvent[] {
  return events.filter((e) => e.type === "log");
}

/**
 * Get only metric events.
 */
export function metrics(events: BotEvent[]): BotEvent[] {
  return events.filter((e) => e.type === "metric");
}

/**
 * Get the latest event of a specific metric type.
 */
export function latest(
  events: BotEvent[],
  metricType: string,
): BotEvent | null {
  const filtered = filterByType(events, metricType);
  if (filtered.length === 0) return null;
  return filtered[filtered.length - 1];
}

/**
 * Get all metric types present in events.
 */
export function getMetricTypes(events: BotEvent[]): string[] {
  const types = new Set<string>();
  for (const event of events) {
    if (isMetricEvent(event) && event.metricType) {
      types.add(event.metricType);
    }
  }
  return Array.from(types);
}

/**
 * Group events by metric type.
 */
export function groupByMetricType(
  events: BotEvent[],
): Record<string, BotEvent[]> {
  const groups: Record<string, BotEvent[]> = {};

  for (const event of events) {
    if (event.type === "metric" && event.metricType) {
      if (!groups[event.metricType]) {
        groups[event.metricType] = [];
      }
      groups[event.metricType].push(event);
    }
  }

  return groups;
}

type TimeWindow = "1m" | "5m" | "15m" | "1h" | "4h" | "1d";

/**
 * Group events by time window.
 * Returns array of event arrays, one per window.
 */
export function groupByTimeWindow(
  events: BotEvent[],
  window: TimeWindow,
): BotEvent[][] {
  if (events.length === 0) return [];

  const windowMs: Record<TimeWindow, number> = {
    "1m": 60 * 1000,
    "5m": 5 * 60 * 1000,
    "15m": 15 * 60 * 1000,
    "1h": 60 * 60 * 1000,
    "4h": 4 * 60 * 60 * 1000,
    "1d": 24 * 60 * 60 * 1000,
  };

  const ms = windowMs[window];
  const groups: Map<number, BotEvent[]> = new Map();

  for (const event of events) {
    const windowStart = Math.floor(event.timestamp.getTime() / ms) * ms;
    if (!groups.has(windowStart)) {
      groups.set(windowStart, []);
    }
    groups.get(windowStart)!.push(event);
  }

  // Sort by window start time and return values
  const sortedKeys = Array.from(groups.keys()).sort((a, b) => a - b);
  return sortedKeys.map((key) => groups.get(key)!);
}

/**
 * Group events by execution run (for scheduled bots).
 * Uses time gaps to detect run boundaries.
 * @param gapThreshold - Minimum gap in ms to consider a new run (default: 5 minutes)
 */
export function groupByRun(
  events: BotEvent[],
  gapThreshold: number = 5 * 60 * 1000,
): BotEvent[][] {
  if (events.length === 0) return [];

  // Sort by timestamp
  const sorted = [...events].sort(
    (a, b) => a.timestamp.getTime() - b.timestamp.getTime(),
  );

  const runs: BotEvent[][] = [];
  let currentRun: BotEvent[] = [sorted[0]];

  for (let i = 1; i < sorted.length; i++) {
    const gap =
      sorted[i].timestamp.getTime() - sorted[i - 1].timestamp.getTime();

    if (gap > gapThreshold) {
      // New run detected
      runs.push(currentRun);
      currentRun = [sorted[i]];
    } else {
      currentRun.push(sorted[i]);
    }
  }

  // Don't forget the last run
  runs.push(currentRun);

  return runs;
}

/**
 * Get the latest value for each metric type.
 * Useful for dashboard snapshots.
 */
export function latestByType(events: BotEvent[]): Record<string, BotEvent> {
  const result: Record<string, BotEvent> = {};
  const types = getMetricTypes(events);

  for (const type of types) {
    const event = latest(events, type);
    if (event) {
      result[type] = event;
    }
  }

  return result;
}

/**
 * Count events by metric type.
 */
export function countByType(events: BotEvent[]): Record<string, number> {
  const counts: Record<string, number> = {};

  for (const event of events) {
    if (event.type === "metric" && event.metricType) {
      counts[event.metricType] = (counts[event.metricType] || 0) + 1;
    }
  }

  return counts;
}

/**
 * Extract numeric values from metric events.
 * Useful for charting time series data.
 */
export function extractTimeSeries(
  events: BotEvent[],
  metricType: string,
  valueKey: string = "value",
): { timestamp: Date; value: number }[] {
  return filterByType(events, metricType)
    .filter((e) => {
      const data = e.data as Record<string, unknown>;
      return typeof data[valueKey] === "number";
    })
    .map((e) => {
      const data = e.data as Record<string, unknown>;
      return {
        timestamp: e.timestamp,
        value: data[valueKey] as number,
      };
    });
}
