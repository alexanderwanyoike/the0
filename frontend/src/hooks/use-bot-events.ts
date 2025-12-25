"use client";

import { useMemo, useCallback } from "react";
import { useBotLogs } from "./use-bot-logs";
import {
  BotEvent,
  parseEvents,
  isMetricEvent,
  isLogEvent,
} from "@/lib/events/event-parser";
import * as eventUtils from "@/lib/events/event-utils";

interface UseBotEventsOptions {
  botId: string;
  autoRefresh?: boolean;
  refreshInterval?: number;
  dateRange?: { start: string; end: string };
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
  /** Loading state */
  loading: boolean;
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
  autoRefresh = false,
  refreshInterval = 30000,
  dateRange,
}: UseBotEventsOptions): UseBotEventsReturn {
  // Build initial query from dateRange
  const initialQuery = dateRange
    ? {
        dateRange: `${dateRange.start}-${dateRange.end}`,
        limit: 100,
        offset: 0,
      }
    : { limit: 100, offset: 0 };

  const {
    logs: rawLogs,
    loading,
    error,
    refresh,
    setDateFilter,
    setDateRangeFilter,
    exportLogs,
  } = useBotLogs({
    botId,
    autoRefresh,
    refreshInterval,
    initialQuery,
  });

  // Parse raw logs into events
  const events = useMemo(() => {
    // Convert raw logs to the format expected by parseEvents
    const logEntries = rawLogs.map((log) => ({
      date: log.date,
      content: log.content,
    }));
    return parseEvents(logEntries);
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
