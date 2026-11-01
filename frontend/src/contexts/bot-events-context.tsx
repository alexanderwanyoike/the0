"use client";

import React, { useContext, useMemo, ReactNode } from "react";
import { The0EventsContext, The0EventsContextValue } from "the0-react";
import { useBotEvents } from "@/hooks/use-bot-events";

// the0-react registers this context on window, where dashboard bundles,
// which carry their own copy of the SDK, find it.
const BotEventsContext = The0EventsContext;

// the0-react 0.2.1 predates botId and the extra utilities this host provides.
type HostEventsContextValue = The0EventsContextValue & {
  botId: string;
  utils: ReturnType<typeof useBotEvents>["utils"];
};

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
  const {
    events,
    loading,
    isFetching,
    connected,
    lastUpdate,
    error,
    utils,
    refresh,
  } = useBotEvents({
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
  const value = useMemo<HostEventsContextValue>(
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
    [
      events,
      loading,
      isFetching,
      connected,
      lastUpdate,
      displayError,
      utils,
      refresh,
      botId,
    ],
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
export function useBotEventsContext(): HostEventsContextValue {
  // BotEventsProvider is the only writer of this context in the host
  const context = useContext(BotEventsContext) as HostEventsContextValue | null;

  if (!context) {
    throw new Error(
      "useBotEventsContext must be used within a BotEventsProvider",
    );
  }

  return context;
}

// Alias for the0 SDK naming convention
export const useThe0Events = useBotEventsContext;
