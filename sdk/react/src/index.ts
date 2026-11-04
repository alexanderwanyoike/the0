/**
 * the0-react - React SDK for bot custom frontends
 *
 * This SDK provides hooks and utilities for building custom dashboards
 * that visualize bot metrics and events.
 */

import { useContext, createContext } from "react";
import { THE0_GLOBALS, The0EventsContextValue } from "./contract";

export { THE0_GLOBALS } from "./contract";
export type {
  BotEvent,
  BotEventUtils,
  The0EventsContextValue,
} from "./contract";

// ============================================================================
// Context (shared with main app via global)
// ============================================================================

// Get or create the shared context
function getSharedContext(): React.Context<The0EventsContextValue | null> {
  const existing =
    typeof window !== "undefined"
      ? window[THE0_GLOBALS.eventsContext]
      : undefined;
  if (existing) {
    return existing;
  }

  const context = createContext<The0EventsContextValue | null>(null);

  if (typeof window !== "undefined") {
    window[THE0_GLOBALS.eventsContext] = context;
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
 * import { useThe0Events } from 'the0-react';
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
