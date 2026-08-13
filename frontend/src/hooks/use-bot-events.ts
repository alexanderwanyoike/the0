"use client";

import { useMemo, useCallback, useEffect, useRef } from "react";
import { useBotLogs, DEFAULT_LOOKBACK_DAYS } from "./use-bot-logs";
import {
  BotEvent,
  parseEvents,
  isMetricEvent,
  isLogEvent,
} from "@/lib/events/event-parser";
import * as eventUtils from "@/lib/events/event-utils";

interface UseBotEventsOptions {
  botId: string;
  /** Stream events live via SSE (realtime bots) instead of polling */
  streaming?: boolean;
  autoRefresh?: boolean;
  refreshInterval?: number;
  dateRange?: { start: string; end: string };
  /** Latest mode: fetch the newest metrics across a lookback window instead
   *  of a fixed date window (scheduled bots that don't run every day).
   *  Takes precedence over dateRange. */
  latest?: boolean;
}

interface BotEventUtils {
  /** Filter events from the last N duration (e.g., '1h', '30m', '1d') */
  since: (duration: string) => BotEvent[];
  /** Filter events between two dates */
  between: (start: Date, end: Date) => BotEvent[];
  /** Filter by metric type */
  filterByType: (metricType: string) => BotEvent[];
  /** Get only log events */
  logs: () => BotEvent[];
  /** Get only metric events */
  metrics: () => BotEvent[];
  /** Get latest event of a metric type */
  latest: (metricType: string) => BotEvent | null;
  /** Get all metric types present */
  getMetricTypes: () => string[];
  /** Group events by metric type */
  groupByMetricType: () => Record<string, BotEvent[]>;
  /** Group events by time window */
  groupByTimeWindow: (
    window: "1m" | "5m" | "15m" | "1h" | "4h" | "1d",
  ) => BotEvent[][];
  /** Group events by execution run (for scheduled bots) */
  groupByRun: (gapThreshold?: number) => BotEvent[][];
  /** Get latest value for each metric type */
  latestByType: () => Record<string, BotEvent>;
  /** Count events by metric type */
  countByType: () => Record<string, number>;
  /** Extract time series data for charting */
  extractTimeSeries: (
    metricType: string,
    valueKey?: string,
  ) => { timestamp: Date; value: number }[];
}

interface UseBotEventsReturn {
  /** Parsed bot events */
  events: BotEvent[];
  /** True only while there is nothing to render yet (initial load) */
  loading: boolean;
  /** True while a background refresh is in flight */
  isFetching: boolean;
  /** SSE connection state: true = live stream active, false = degraded to
   *  polling or not yet connected, undefined = not streaming */
  connected?: boolean;
  /** When data last arrived (REST or SSE) */
  lastUpdate: Date | null;
  /** Error message if any */
  error: string | null;
  /** Event utilities bound to current events */
  utils: BotEventUtils;
  /** Refresh events */
  refresh: () => void;
  /** Update date filter */
  setDateFilter: (date: string | null) => void;
  /** Update date range filter */
  setDateRangeFilter: (startDate: string, endDate: string) => void;
  /** Export logs as text file */
  exportLogs: () => void;
  /** Raw log entries (for fallback) */
  rawLogs: { date: string; content: string }[];
}

/**
 * Hook for consuming bot events with parsing and utilities.
 * Wraps useBotLogs and adds event parsing + time utilities.
 */
export function useBotEvents({
  botId,
  streaming = false,
  autoRefresh = false,
  refreshInterval = 30000,
  dateRange,
  latest = false,
}: UseBotEventsOptions): UseBotEventsReturn {
  // Build initial query; always request metrics type. Latest mode asks for
  // the newest metrics across a lookback window so scheduled bots show their
  // last known state even when they haven't run inside any fixed window.
  const initialQuery = latest
    ? {
        limit: 1000,
        offset: 0,
        type: "metrics" as const,
        sort: "desc" as const,
        lookbackDays: DEFAULT_LOOKBACK_DAYS,
      }
    : dateRange
      ? {
          dateRange: dateRange.start.includes("T")
            ? `${dateRange.start}--${dateRange.end}`
            : `${dateRange.start}-${dateRange.end}`,
          limit: 1000,
          offset: 0,
          type: "metrics" as const,
        }
      : { limit: 1000, offset: 0, type: "metrics" as const };

  // With streaming, metric history still loads via REST (type=metrics) and
  // new lines append live over SSE; non-metric lines are filtered out by
  // parseEvents below. With a dateRange active the hook stays REST-only.
  const {
    logs: rawLogs,
    loading,
    isFetching,
    connected,
    lastUpdate,
    error,
    refresh,
    setDateFilter,
    setDateRangeFilter,
    setLatestFilter,
    exportLogs,
  } = useBotLogs({
    botId,
    streaming,
    autoRefresh,
    refreshInterval,
    initialQuery,
  });

  // Refetch when the query mode changes after mount: latest <-> range, or a
  // different range. A signature comparison covers every transition,
  // including range selected on a bot that had no range before.
  const querySignature = latest
    ? "latest"
    : dateRange
      ? `range:${dateRange.start}--${dateRange.end}`
      : "none";
  const prevSignature = useRef(querySignature);
  useEffect(() => {
    if (prevSignature.current === querySignature) return;
    prevSignature.current = querySignature;
    if (latest) {
      setLatestFilter();
    } else if (dateRange) {
      setDateRangeFilter(dateRange.start, dateRange.end);
    } else {
      // Neither mode active (e.g. back to live): clear any stale filter so
      // streaming/default fetching resumes
      setDateFilter(null);
    }
  }, [
    querySignature,
    latest,
    dateRange,
    setLatestFilter,
    setDateRangeFilter,
    setDateFilter,
  ]);

  // Per-entry parse cache keyed by log entry identity. Appends (SSE lines,
  // pagination) preserve existing entry objects, so only new entries are
  // parsed and already-parsed events keep their identity - without this,
  // every arriving line re-parsed the whole buffer (up to 10k entries) and
  // handed consumers all-new event objects. Replace fetches create fresh
  // entry objects, which correctly miss the cache; WeakMap lets the old
  // generation be GC'd.
  const parseCacheRef = useRef(
    new WeakMap<object, (BotEvent & { timestamp: Date })[]>(),
  );

  // Parse raw logs into events
  // Ensure timestamps are always Date objects for SDK compatibility
  const events = useMemo(() => {
    const cache = parseCacheRef.current;
    const all: (BotEvent & { timestamp: Date })[] = [];

    for (const log of rawLogs) {
      let parsed = cache.get(log);
      if (!parsed) {
        // Fallback-stamp events without timestamps at parse time (SDK expects
        // Date, not null); caching also keeps that fallback stable instead of
        // drifting to "now" on every re-render
        parsed = parseEvents([
          { date: log.date, content: log.content, timestamp: log.timestamp },
        ]).map((event) => ({
          ...event,
          timestamp: event.timestamp ?? new Date(),
        }));
        cache.set(log, parsed);
      }
      all.push(...parsed);
    }

    // The API returns newest-first for desc queries but every event util
    // (latest, extractTimeSeries, groupByRun) assumes chronological order.
    // Stable-sort here so utils are correct regardless of fetch order;
    // fallback-stamped events share their parse time and keep relative order.
    return all.sort((a, b) => a.timestamp.getTime() - b.timestamp.getTime());
  }, [rawLogs]);

  // Create bound utilities
  const utils = useMemo<BotEventUtils>(
    () => ({
      since: (duration: string) => eventUtils.since(events, duration),
      between: (start: Date, end: Date) =>
        eventUtils.between(events, start, end),
      filterByType: (metricType: string) =>
        eventUtils.filterByType(events, metricType),
      logs: () => eventUtils.logs(events),
      metrics: () => eventUtils.metrics(events),
      latest: (metricType: string) => eventUtils.latest(events, metricType),
      getMetricTypes: () => eventUtils.getMetricTypes(events),
      groupByMetricType: () => eventUtils.groupByMetricType(events),
      groupByTimeWindow: (window) =>
        eventUtils.groupByTimeWindow(events, window),
      groupByRun: (gapThreshold?: number) =>
        eventUtils.groupByRun(events, gapThreshold),
      latestByType: () => eventUtils.latestByType(events),
      countByType: () => eventUtils.countByType(events),
      extractTimeSeries: (metricType: string, valueKey?: string) =>
        eventUtils.extractTimeSeries(events, metricType, valueKey),
    }),
    [events],
  );

  return {
    events,
    loading,
    isFetching,
    // Only meaningful in streaming mode; undefined keeps indicators hidden
    connected: streaming ? connected : undefined,
    lastUpdate,
    error,
    utils,
    refresh,
    setDateFilter,
    setDateRangeFilter,
    exportLogs,
    rawLogs,
  };
}

// Re-export types and utilities for convenience
export type { BotEvent, BotEventUtils };
export { isMetricEvent, isLogEvent };
