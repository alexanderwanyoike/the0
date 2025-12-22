"use client";

import React, { createContext, useContext, ReactNode } from "react";
import { useBotEvents, BotEvent, BotEventUtils } from "@/hooks/use-bot-events";

interface BotEventsContextValue {
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
  autoRefresh = true,
  refreshInterval = 30000,
  dateRange,
}: BotEventsProviderProps) {
  const { events, loading, error, utils, refresh } = useBotEvents({
    botId,
    autoRefresh,
    refreshInterval,
    dateRange,
  });

  return (
    <BotEventsContext.Provider
      value={{
        events,
        loading,
        error,
        utils,
        refresh,
        botId,
      }}
    >
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
