/**
 * useBotLogs - Unified bot log hook (REST history + optional SSE live updates)
 *
 * One hook serves both the console and custom dashboards:
 * - History ALWAYS comes from REST (GET /api/logs/:botId). This keeps a single
 *   code path for history and lets consumers filter server-side (type=metrics).
 * - With `streaming: true` and no date filter active, an SSE connection
 *   (GET /api/logs/:botId/stream) appends new lines live. The SSE "history"
 *   event is intentionally ignored (REST is the source of history); it still
 *   serves as the server-side ownership check for the stream.
 * - Setting a date/dateRange filter suspends SSE; clearing it reconnects.
 * - If SSE fails, the hook falls back to REST polling at `refreshInterval`.
 * - With `streaming: false`, plain REST with optional `autoRefresh` polling.
 *
 * Abort semantics: intentional aborts (unmount, bot switch, filter change) are
 * detected via `controller.signal.aborted`, NOT the error name. Browsers may
 * reject an aborted fetch/body-read with TypeError("Failed to fetch") instead
 * of AbortError; checking only the name surfaced phantom errors and leaked
 * fallback polling intervals after unmount.
 */

"use client";

import { useState, useEffect, useCallback, useRef } from "react";
import { useToast } from "@/hooks/use-toast";
import type { LogEntry, LogsQuery, PageMerge } from "@/types/logs";
import {
  MAX_LOG_ENTRIES,
  formatLogsForExport,
  isHistoricalQuery,
  mergeUniqueEntries,
  parseLiveUpdate,
  toDisplayOrder,
} from "@/lib/log-utils";
import { useAuth } from "@/contexts/auth-context";
import { fetchLogsPage, LogsPage } from "./bot-logs/fetch-logs-page";
import {
  useAutoRefreshPolling,
  useLogPolling,
} from "./bot-logs/use-log-polling";
import { useLiveLogStream } from "./bot-logs/use-live-log-stream";

interface UseBotLogsProps {
  botId: string;
  /** Use SSE live updates (realtime bots). False = REST only. */
  streaming?: boolean;
  /** Poll via REST at refreshInterval (non-streaming mode only). */
  autoRefresh?: boolean;
  refreshInterval?: number;
  initialQuery?: LogsQuery;
}

export interface UseBotLogsReturn {
  logs: LogEntry[];
  /** True only while there is nothing to render yet (initial load / bot
   *  switch). Background refetches keep this false so consumers can keep
   *  showing stale data instead of unmounting into a loading screen. */
  loading: boolean;
  /** True while any replace fetch is in flight, including background
   *  refreshes. Use for subtle "updating" indicators. */
  isFetching: boolean;
  error: string | null;
  hasMore: boolean;
  total: number;
  query: LogsQuery;
  connected: boolean;
  lastUpdate: Date | null;
  hasEarlierLogs: boolean;
  loadingEarlier: boolean;
  loadingMore: boolean;
  refresh: () => void;
  loadMore: () => Promise<void>;
  loadEarlierLogs: () => Promise<void>;
  updateQuery: (params: Partial<LogsQuery>) => void;
  setDateFilter: (date: string | null) => void;
  setDateRangeFilter: (start: string, end: string) => void;
  setLatestFilter: () => void;
  exportLogs: () => void;
}

const DEFAULT_QUERY: LogsQuery = { limit: 1000, offset: 0, sort: "desc" };
/** How far back latest mode is willing to scan for a bot's most recent
 *  output. Scheduled bots that run weekly stay well inside this window. */
export const DEFAULT_LOOKBACK_DAYS = 30;

export const useBotLogs = ({
  botId,
  streaming = false,
  autoRefresh = false,
  refreshInterval = 30000,
  initialQuery = DEFAULT_QUERY,
}: UseBotLogsProps): UseBotLogsReturn => {
  const [logs, setLogs] = useState<LogEntry[]>([]);
  const [loading, setLoading] = useState(false);
  const [isFetching, setIsFetching] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [query, setQuery] = useState<LogsQuery>(initialQuery);
  const [hasMore, setHasMore] = useState(false);
  const [total, setTotal] = useState(0);
  const [lastUpdate, setLastUpdate] = useState<Date | null>(null);
  const [hasEarlierLogs, setHasEarlierLogs] = useState(false);
  const [loadingEarlier, setLoadingEarlier] = useState(false);
  const [loadingMore, setLoadingMore] = useState(false);
  // Bumped by refresh() to force an SSE reconnect in live mode
  const [sseNonce, setSseNonce] = useState(0);

  const { user } = useAuth();
  const { toast } = useToast();

  const restAbortRef = useRef<AbortController | null>(null);
  // True once a replace fetch has delivered data for the current bot. While
  // set, replace fetches are background refreshes: they must not flip
  // `loading` (which would unmount dashboards into their loading screen).
  const hasDataRef = useRef(false);
  // Latest fetchLogs so the polling interval never calls a stale closure
  const fetchLogsRef = useRef<
    (q?: LogsQuery, m?: PageMerge) => Promise<boolean>
  >(null!);
  // Skip polling overwrites while the user has explicitly paginated
  const paginatedRef = useRef(false);
  const loadingMoreRef = useRef(false);
  const loadingEarlierRef = useRef(false);
  // Live mode: SSE updates arriving before the REST history resolves are
  // buffered here, then merged (deduped) once history lands.
  const historyLoadedRef = useRef(false);
  const pendingUpdatesRef = useRef<LogEntry[]>([]);
  // Offset for backfilling earlier logs in live mode
  const earlierOffsetRef = useRef(0);
  // Prop refs so effects/callbacks read latest values without re-running
  const streamingRef = useRef(streaming);
  streamingRef.current = streaming;
  const initialQueryRef = useRef(initialQuery);
  initialQueryRef.current = initialQuery;
  const queryRef = useRef(query);
  queryRef.current = query;

  const liveMode = streaming && !isHistoricalQuery(query);

  const { startPolling, stopPolling, isPolling } = useLogPolling(
    refreshInterval,
    () => {
      // Skip the tick while the user has paginated or a pagination fetch
      // is in flight - fetchLogs aborts the previous request, so firing
      // here would silently cancel their load-more/load-earlier click.
      if (
        !paginatedRef.current &&
        !loadingMoreRef.current &&
        !loadingEarlierRef.current
      ) {
        fetchLogsRef.current();
      }
    },
  );

  // -- Pending live updates (SSE lines buffered while history is in flight) --

  const flushPendingUpdates = useCallback(() => {
    if (pendingUpdatesRef.current.length === 0) return;
    const pending = pendingUpdatesRef.current;
    pendingUpdatesRef.current = [];
    setLogs((prev) => mergeUniqueEntries(prev, pending, "append"));
  }, []);

  // -- REST fetch (history, filters, pagination, polling) --

  const applyPage = useCallback(
    (
      page: LogsPage,
      queryParams: LogsQuery,
      merge: PageMerge,
      live: boolean,
    ) => {
      if (merge) {
        setLogs((prev) => mergeUniqueEntries(prev, page.entries, merge));
        if (merge === "append") setHasMore(page.hasMore);
        else setHasEarlierLogs(page.hasMore);
      } else {
        historyLoadedRef.current = true;
        hasDataRef.current = true;
        earlierOffsetRef.current = queryParams.limit || 100;
        setLogs(page.entries.slice(-MAX_LOG_ENTRIES));
        // Merge in any live updates that arrived while the fetch was in
        // flight (React applies the updater after the set above).
        flushPendingUpdates();
        // Live history extends backwards (earlier logs); REST pages forwards
        setHasEarlierLogs(live ? page.hasMore : false);
        setHasMore(live ? false : page.hasMore);
      }
      setTotal(page.total);
      setLastUpdate(new Date());
    },
    [flushPendingUpdates],
  );

  const handleFetchFailure = useCallback(
    (err: any, merge: PageMerge, wasHistoryLoaded: boolean) => {
      if (!merge) {
        // A genuine failure means no newer fetch superseded us (it would
        // have aborted this one), so restore buffering state and release
        // any updates captured while the fetch was in flight.
        historyLoadedRef.current = wasHistoryLoaded;
        if (wasHistoryLoaded) flushPendingUpdates();
      }

      const errorMessage = err?.message || "Failed to fetch logs";
      setError(errorMessage);
      if (!merge) {
        toast({
          title: "Error",
          description: errorMessage,
          variant: "destructive",
        });
      }
    },
    [toast, flushPendingUpdates],
  );

  const fetchLogs = useCallback(
    async (
      queryParams: LogsQuery = query,
      merge: PageMerge = false,
    ): Promise<boolean> => {
      if (!botId) return false;

      restAbortRef.current?.abort();
      const controller = new AbortController();
      restAbortRef.current = controller;

      // Buffer live updates while a replace fetch is in flight: an SSE line
      // that arrives after the server snapshots the response would otherwise
      // be appended and then overwritten when the fetch resolves.
      const wasHistoryLoaded = historyLoadedRef.current;
      if (!merge) historyLoadedRef.current = false;

      try {
        if (!merge) {
          setIsFetching(true);
          // Full loading state only when there is nothing to render yet;
          // once data exists, refetches are stale-while-revalidate
          if (!hasDataRef.current) setLoading(true);
        }
        setError(null);
        if (!user) {
          throw new Error("User not authenticated");
        }

        const page = await fetchLogsPage(botId, queryParams, controller.signal);
        const live = streamingRef.current && !isHistoricalQuery(queryParams);
        applyPage(
          { ...page, entries: toDisplayOrder(page.entries, live, merge) },
          queryParams,
          merge,
          live,
        );
        return true;
      } catch (err: any) {
        // Intentional aborts (unmount, bot switch, filter change) are silent.
        // Check signal.aborted first: browsers may reject an aborted request
        // with TypeError("Failed to fetch") rather than AbortError.
        // historyLoadedRef is deliberately NOT restored on abort: whatever
        // superseded this fetch (a newer replace, or an unmount reset) now
        // owns that state.
        if (controller.signal.aborted || err?.name === "AbortError") {
          return false;
        }
        handleFetchFailure(err, merge, wasHistoryLoaded);
        return false;
      } finally {
        if (!controller.signal.aborted) {
          setLoading(false);
          setIsFetching(false);
          setLoadingEarlier(false);
        }
      }
    },
    [botId, query, user, applyPage, handleFetchFailure],
  );
  fetchLogsRef.current = fetchLogs;

  // -- SSE live updates --

  const handleUpdateEvent = useCallback((data: string) => {
    try {
      const entries = parseLiveUpdate(data, queryRef.current.type);
      if (entries.length === 0) return;
      if (!historyLoadedRef.current) {
        pendingUpdatesRef.current.push(...entries);
        return;
      }
      setLogs((prev) => [...prev, ...entries].slice(-MAX_LOG_ENTRIES));
      setTotal((prev) => prev + entries.length);
      setLastUpdate(new Date());
    } catch (err) {
      console.error("Failed to parse update SSE event:", err);
    }
  }, []);

  // -- Lifecycle: reset + initial fetch on bot/user/mode change --

  useEffect(() => {
    if (!botId || !user) {
      setLoading(false);
      return;
    }

    const startQuery = initialQueryRef.current;
    setLogs([]);
    setError(null);
    setQuery(startQuery);
    setHasMore(false);
    setTotal(0);
    setLastUpdate(null);
    setHasEarlierLogs(false);
    setLoadingEarlier(false);
    setLoading(true);
    paginatedRef.current = false;
    historyLoadedRef.current = false;
    hasDataRef.current = false;
    pendingUpdatesRef.current = [];

    fetchLogsRef.current(startQuery);

    return () => {
      restAbortRef.current?.abort();
    };
  }, [botId, user, streaming]);

  useAutoRefreshPolling({
    botId,
    user,
    streaming,
    autoRefresh,
    refreshInterval,
    startPolling,
    stopPolling,
    isPolling,
  });

  const connected = useLiveLogStream({
    botId,
    user,
    liveMode,
    reconnectKey: sseNonce,
    onUpdate: handleUpdateEvent,
    startPolling,
    stopPolling,
  });

  // -- Query operations --

  const updateQuery = useCallback(
    (params: Partial<LogsQuery>) => {
      const updatedQuery = { ...query, ...params, offset: 0 };
      setQuery(updatedQuery);
      paginatedRef.current = false;
      fetchLogs(updatedQuery, false);
    },
    [query, fetchLogs],
  );

  const setDateFilter = useCallback(
    (date: string | null) => {
      updateQuery({
        date: date || undefined,
        dateRange: undefined,
        lookbackDays: undefined,
      });
    },
    [updateQuery],
  );

  const setDateRangeFilter = useCallback(
    (startDate: string, endDate: string) => {
      // Use -- separator for ISO datetime ranges, - for YYYYMMDD date ranges
      const separator = startDate.includes("T") ? "--" : "-";
      updateQuery({
        dateRange: `${startDate}${separator}${endDate}`,
        date: undefined,
        lookbackDays: undefined,
      });
    },
    [updateQuery],
  );

  const setLatestFilter = useCallback(() => {
    updateQuery({
      date: undefined,
      dateRange: undefined,
      lookbackDays: DEFAULT_LOOKBACK_DAYS,
    });
  }, [updateQuery]);

  const refresh = useCallback(() => {
    const refreshQuery = { ...query, offset: 0 };
    setQuery(refreshQuery);
    paginatedRef.current = false;
    fetchLogs(refreshQuery, false);
    // Re-establish SSE in live mode (no-op otherwise)
    setSseNonce((n) => n + 1);
  }, [query, fetchLogs]);

  const loadMore = useCallback(async () => {
    if (loadingMoreRef.current || !hasMore) return;
    loadingMoreRef.current = true;
    setLoadingMore(true);

    const nextQuery = {
      ...query,
      offset: (query.offset || 0) + (query.limit || 100),
    };

    try {
      const ok = await fetchLogs(nextQuery, "append");
      if (ok) {
        setQuery(nextQuery);
        paginatedRef.current = true;
      }
    } finally {
      loadingMoreRef.current = false;
      setLoadingMore(false);
    }
  }, [fetchLogs, hasMore, query]);

  const loadEarlierLogs = useCallback(async () => {
    if (!botId || !user || loadingEarlier) return;
    loadingEarlierRef.current = true;
    setLoadingEarlier(true);

    try {
      const limit = query.limit || 100;
      const offset = earlierOffsetRef.current || limit;
      const ok = await fetchLogs({ ...query, offset }, "prepend");
      if (ok) {
        earlierOffsetRef.current = offset + limit;
      }
    } finally {
      loadingEarlierRef.current = false;
      setLoadingEarlier(false);
    }
  }, [botId, user, loadingEarlier, fetchLogs, query]);

  const exportLogs = useCallback(() => {
    if (logs.length === 0) {
      toast({
        title: "No logs to export",
        description: "There are no logs available to export.",
        variant: "destructive",
      });
      return;
    }

    const blob = new Blob([formatLogsForExport(logs)], { type: "text/plain" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = `bot-${botId}-logs-${new Date().toISOString().split("T")[0]}.txt`;
    a.click();
    URL.revokeObjectURL(url);
  }, [logs, botId, toast]);

  return {
    logs,
    loading,
    isFetching,
    error,
    hasMore,
    total,
    query,
    connected,
    lastUpdate,
    hasEarlierLogs,
    loadingEarlier,
    loadingMore,
    refresh,
    loadMore,
    loadEarlierLogs,
    updateQuery,
    setDateFilter,
    setDateRangeFilter,
    setLatestFilter,
    exportLogs,
  };
};
