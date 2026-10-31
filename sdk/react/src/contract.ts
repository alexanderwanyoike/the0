/**
 * The contract between the0 platform (the host page) and custom dashboard
 * bundles. Bundles built with older SDK versions keep running on newer
 * platforms, so every change here must be additive.
 */

import type { Context } from "react";

/**
 * Window globals the platform sets before it loads a dashboard bundle.
 * Bundles read React from them so that host and dashboard share one React.
 */
export const THE0_GLOBALS = {
  eventsContext: "__THE0_EVENTS_CONTEXT__",
  react: "__THE0_REACT__",
  reactDom: "__THE0_REACT_DOM__",
  jsxRuntime: "__THE0_REACT_JSX__",
} as const;

declare global {
  interface Window {
    __THE0_EVENTS_CONTEXT__?: Context<The0EventsContextValue | null>;
  }
}

export interface BotEvent {
  timestamp: Date;
  type: "log" | "metric";
  data: string | Record<string, unknown>;
  metricType?: string;
  level?: "DEBUG" | "INFO" | "WARN" | "ERROR";
  raw: string;
}

export interface BotEventUtils {
  /** Get events from the last duration (e.g., "1h", "24h", "7d") */
  since: (duration: string) => BotEvent[];
  /** Get events between two dates */
  between: (start: Date, end: Date) => BotEvent[];
  /** Filter events by metric type */
  filterByType: (metricType: string) => BotEvent[];
  /** Get only metric events */
  metrics: () => BotEvent[];
  /** Get only log events */
  logs: () => BotEvent[];
  /** Get the latest event of a specific metric type */
  latest: (metricType: string) => BotEvent | null;
  /** Group events by metric type */
  groupByMetricType: () => Record<string, BotEvent[]>;
  /** Group events by execution run (for scheduled bots) */
  groupByRun: (gapThresholdMs?: number) => BotEvent[][];
  /** Extract time series data for charting */
  extractTimeSeries: (
    metricType: string,
    valueKey?: string
  ) => { timestamp: Date; value: number }[];
}

export interface The0EventsContextValue {
  events: BotEvent[];
  /** True only while there is nothing to render yet (initial load). The
   *  platform refreshes data in the background without flipping this, so
   *  guard full-screen loading states with `events.length === 0`. */
  loading: boolean;
  /** True while a background refresh is in flight. Optional: older platform
   *  versions may not provide it. */
  isFetching?: boolean;
  /** Live stream state: true = SSE connected, false = degraded to polling,
   *  undefined = not streaming. Optional: older platform versions may not
   *  provide it. Use for a live/updating indicator. */
  connected?: boolean;
  /** When data last arrived (REST or SSE). Optional: older platform
   *  versions may not provide it. */
  lastUpdate?: Date | null;
  /** Set only when there is nothing to render (initial load failed), so a
   *  plain `if (error)` early return is safe: background refresh failures
   *  never blank a dashboard that has data on screen. */
  error: string | null;
  utils: BotEventUtils;
  refresh: () => void;
}
