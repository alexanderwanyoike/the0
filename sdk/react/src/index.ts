/**
 * @the0/react - React SDK for bot custom frontends
 *
 * This SDK provides hooks and utilities for building custom dashboards
 * that visualize bot metrics and events.
 */

import { useContext, createContext } from "react";

// ============================================================================
// Types
// ============================================================================

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
  loading: boolean;
  error: string | null;
  utils: BotEventUtils;
  refresh: () => void;
}

// ============================================================================
// Context (shared with main app via global)
// ============================================================================

declare global {
  interface Window {
    __THE0_EVENTS_CONTEXT__?: React.Context<The0EventsContextValue | null>;
  }
}

// Get or create the shared context
function getSharedContext(): React.Context<The0EventsContextValue | null> {
  if (typeof window !== "undefined" && window.__THE0_EVENTS_CONTEXT__) {
    return window.__THE0_EVENTS_CONTEXT__;
  }

  const context = createContext<The0EventsContextValue | null>(null);

  if (typeof window !== "undefined") {
    window.__THE0_EVENTS_CONTEXT__ = context;
  }

  return context;
}

export const The0EventsContext = getSharedContext();

// ============================================================================
// Hooks
// ============================================================================

/**
 * Hook to access bot events and utilities in custom dashboards.
 *
 * @example
 * ```tsx
 * import { useThe0Events } from '@the0/react';
 *
 * export default function Dashboard() {
 *   const { events, utils, loading } = useThe0Events();
 *
 *   const trades = utils.filterByType('trade');
 *   const latestPortfolio = utils.latest('portfolio_value');
 *
 *   return (
 *     <div>
 *       <h1>Portfolio: ${latestPortfolio?.data.value}</h1>
 *       <TradeList trades={trades} />
 *     </div>
 *   );
 * }
 * ```
 */
export function useThe0Events(): The0EventsContextValue {
  const context = useContext(The0EventsContext);

  if (!context) {
    throw new Error(
      "useThe0Events must be used within a the0 dashboard context. " +
        "This hook only works in custom bot frontends loaded by the0 platform."
    );
  }

  return context;
}

// Alias for convenience
export const useBotEvents = useThe0Events;

// ============================================================================
// Re-exports for convenience
// ============================================================================

export { The0EventsContext as BotEventsContext };
