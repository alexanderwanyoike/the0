"use client";

import React, { createContext, useContext, useMemo, ReactNode } from "react";
import { useBotEvents, BotEvent, BotEventUtils } from "@/hooks/use-bot-events";

interface BotEventsContextValue {
  /** Parsed bot events */
  events: BotEvent[];
  /** True only while there is nothing to render yet. Background refreshes
   *  keep this false so dashboards don't unmount into a loading screen. */
  loading: boolean;
  /** True while a background refresh is in flight (subtle indicators) */
  isFetching: boolean;
  /** SSE connection state: true = live, false = degraded to polling,
   *  undefined = not streaming */
  connected?: boolean;
  /** When data last arrived (REST or SSE) */
  lastUpdate?: Date | null;
  /** Set only when there is nothing to render (initial load failed).
   *  Background refresh failures keep this null so a naive `if (error)`
   *  early-return can't blank a dashboard that has data on screen; they
   *  surface through toasts and the connection indicator instead. */
  error: string | null;
  /** Event utilities bound to current events */
  utils: BotEventUtils;
  /** Refresh events */
  refresh: () => void;
  /** Bot ID */
  botId: string;
}

// Shared context - also accessible via @the0/react SDK
declare global {
  interface Window {
    __THE0_EVENTS_CONTEXT__?: React.Context<BotEventsContextValue | null>;
  }
}

function getSharedContext(): React.Context<BotEventsContextValue | null> {
  if (typeof window !== "undefined" && window.__THE0_EVENTS_CONTEXT__) {
    return window.__THE0_EVENTS_CONTEXT__;
  }

  const context = createContext<BotEventsContextValue | null>(null);

  if (typeof window !== "undefined") {
    window.__THE0_EVENTS_CONTEXT__ = context;
  }

  return context;
}

const BotEventsContext = getSharedContext();

interface BotEventsProviderProps {
  children: ReactNode;
  botId: string;
  /** Stream events live via SSE (realtime bots) instead of polling */
  streaming?: boolean;
  /** Latest mode: newest metrics across a lookback window (scheduled bots) */
  latest?: boolean;
  autoRefresh?: boolean;
  refreshInterval?: number;
  dateRange?: { start: string; end: string };
}

/**
 * Provider component that supplies bot events to custom dashboards.
 * Wrap bot frontend components with this provider.
 */
export function BotEventsProvider({
  children,
  botId,
  streaming = false,
  latest = false,
  autoRefresh = true,
  refreshInterval = 30000,
  dateRange,
}: BotEventsProviderProps) {
  const { events, loading, isFetching, connected, lastUpdate, error, utils, refresh } =
    useBotEvents({
      botId,
      streaming,
      latest,
      autoRefresh,
      refreshInterval,
      dateRange,
    });

  // Errors only reach bot frontends when there is nothing to render: this is
  // what makes the naive `if (error) return <Error/>` pattern safe to write
  const displayError = events.length === 0 ? error : null;

  // Memoized so a provider re-render with unchanged hook output doesn't hand
  // every consumer (each chart in a bot dashboard) a new value object
  const value = useMemo(
    () => ({
      events,
      loading,
      isFetching,
      connected,
      lastUpdate,
      error: displayError,
      utils,
      refresh,
      botId,
    }),
    [events, loading, isFetching, connected, lastUpdate, displayError, utils, refresh, botId],
  );

  return (
    <BotEventsContext.Provider value={value}>
      {children}
    </BotEventsContext.Provider>
  );
}

/**
 * Hook to access bot events from context.
 * Must be used within a BotEventsProvider.
 */
export function useBotEventsContext(): BotEventsContextValue {
  const context = useContext(BotEventsContext);

  if (!context) {
    throw new Error(
      "useBotEventsContext must be used within a BotEventsProvider",
    );
  }

  return context;
}

// Alias for the0 SDK naming convention
export const useThe0Events = useBotEventsContext;
