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
import { LogEntry } from "@/components/bot/console-interface";
import { expandLogEntries } from "@/lib/log-utils";
import { useAuth } from "@/contexts/auth-context";
import { authFetch } from "@/lib/auth-fetch";
import { validateSSEAuth } from "@/lib/sse/sse-auth";

export interface LogsQuery {
  date?: string;
  dateRange?: string;
  limit?: number;
  offset?: number;
  type?: string;
  sort?: "asc" | "desc";
}

interface LogsResponse {
  data: LogEntry[];
  total: number;
  hasMore: boolean;
}

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
  loading: boolean;
  error: string | null;
  hasMore: boolean;
  total: number;
  query: LogsQuery;
  connected: boolean;
  lastUpdate: Date | null;
  hasEarlierLogs: boolean;
  loadingEarlier: boolean;
  refresh: () => void;
  loadMore: () => Promise<void>;
  loadEarlierLogs: () => Promise<void>;
  updateQuery: (params: Partial<LogsQuery>) => void;
  setDateFilter: (date: string | null) => void;
  setDateRangeFilter: (start: string, end: string) => void;
  exportLogs: () => void;
}

const MAX_LOG_ENTRIES = 10000;
const DEFAULT_QUERY: LogsQuery = { limit: 1000, offset: 0, sort: "desc" };

const entryKey = (l: LogEntry) => `${l.date}|${l.content}`;

/** Parse a raw SSE message block into event type and data. */
function parseSSEMessage(msg: string): { eventType: string; data: string } {
  let eventType = "message";
  let data = "";
  for (const line of msg.split("\n")) {
    if (line.startsWith("event: ")) eventType = line.slice(7).trim();
    else if (line.startsWith("data: ")) data += line.slice(6);
    else if (line.startsWith("data:")) data += line.slice(5);
  }
  return { eventType, data };
}

export const useBotLogs = ({
  botId,
  streaming = false,
  autoRefresh = false,
  refreshInterval = 30000,
  initialQuery = DEFAULT_QUERY,
}: UseBotLogsProps): UseBotLogsReturn => {
  const [logs, setLogs] = useState<LogEntry[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [query, setQuery] = useState<LogsQuery>(initialQuery);
  const [hasMore, setHasMore] = useState(false);
  const [total, setTotal] = useState(0);
  const [connected, setConnected] = useState(false);
  const [lastUpdate, setLastUpdate] = useState<Date | null>(null);
  const [hasEarlierLogs, setHasEarlierLogs] = useState(false);
  const [loadingEarlier, setLoadingEarlier] = useState(false);
  // Bumped by refresh() to force an SSE reconnect in live mode
  const [sseNonce, setSseNonce] = useState(0);

  const { user } = useAuth();
  const { toast } = useToast();

  const restAbortRef = useRef<AbortController | null>(null);
  const pollingRef = useRef<NodeJS.Timeout | null>(null);
  // Latest fetchLogs so the polling interval never calls a stale closure
  const fetchLogsRef = useRef<(q?: LogsQuery, m?: false | "append" | "prepend") => Promise<boolean>>(null!);
  // Skip polling overwrites while the user has explicitly paginated
  const paginatedRef = useRef(false);
  const loadingMoreRef = useRef(false);
  // Live mode: SSE updates arriving before the REST history resolves are
  // buffered here, then merged (deduped) once history lands.
  const historyLoadedRef = useRef(false);
  const pendingUpdatesRef = useRef<LogEntry[]>([]);
  // Offset for backfilling earlier logs in live mode
  const earlierOffsetRef = useRef(0);
  // Prop refs so effects/callbacks read latest values without re-running
  const refreshIntervalRef = useRef(refreshInterval);
  refreshIntervalRef.current = refreshInterval;
  const streamingRef = useRef(streaming);
  streamingRef.current = streaming;
  const initialQueryRef = useRef(initialQuery);
  initialQueryRef.current = initialQuery;
  const queryRef = useRef(query);
  queryRef.current = query;

  // A date/dateRange filter suspends live streaming (historical views are REST)
  const filterActive = !!(query.date || query.dateRange);
  const liveMode = streaming && !filterActive;

  // -- Polling (single owner for interval lifecycle) --

  const stopPolling = useCallback(() => {
    if (pollingRef.current) {
      clearInterval(pollingRef.current);
      pollingRef.current = null;
    }
  }, []);

  const startPolling = useCallback(() => {
    stopPolling();
    if (refreshIntervalRef.current > 0) {
      pollingRef.current = setInterval(() => {
        if (!paginatedRef.current) fetchLogsRef.current();
      }, refreshIntervalRef.current);
    }
  }, [stopPolling]);

  // -- REST fetch (history, filters, pagination, polling) --

  const fetchLogs = useCallback(
    async (
      queryParams: LogsQuery = query,
      merge: false | "append" | "prepend" = false,
    ): Promise<boolean> => {
      if (!botId) return false;

      restAbortRef.current?.abort();
      const controller = new AbortController();
      restAbortRef.current = controller;

      try {
        if (!merge) setLoading(true);
        setError(null);
        if (!user) {
          throw new Error("User not authenticated");
        }

        const searchParams = new URLSearchParams();
        if (queryParams.dateRange) {
          searchParams.set("dateRange", queryParams.dateRange);
        } else if (queryParams.date) {
          searchParams.set("date", queryParams.date);
        } else {
          searchParams.set(
            "date",
            new Date().toISOString().slice(0, 10).replace(/-/g, ""),
          );
        }
        if (queryParams.limit)
          searchParams.set("limit", queryParams.limit.toString());
        if (queryParams.offset)
          searchParams.set("offset", queryParams.offset.toString());
        if (queryParams.type) searchParams.set("type", queryParams.type);
        if (queryParams.sort) searchParams.set("sort", queryParams.sort);

        const response = await authFetch(
          `/api/logs/${encodeURIComponent(botId)}?${searchParams.toString()}`,
          { signal: controller.signal },
        );

        if (!response.ok) {
          throw new Error(`Failed to fetch logs: ${response.statusText}`);
        }

        const result: LogsResponse = await response.json();
        let expanded = expandLogEntries(result.data);

        // Live mode fetches sort=desc (latest N) but displays chronologically
        // so SSE appends land at the bottom.
        const live =
          streamingRef.current && !queryParams.date && !queryParams.dateRange;
        if (live && merge !== "append") {
          expanded = [...expanded].reverse();
        }

        if (merge === "append") {
          setLogs((prev) => {
            const existing = new Set(prev.map(entryKey));
            const unique = expanded.filter((l) => !existing.has(entryKey(l)));
            return [...prev, ...unique].slice(-MAX_LOG_ENTRIES);
          });
          setHasMore(result.hasMore);
        } else if (merge === "prepend") {
          setLogs((prev) => {
            const existing = new Set(prev.map(entryKey));
            const unique = expanded.filter((l) => !existing.has(entryKey(l)));
            // Keep the front (earlier logs) when trimming a full buffer
            return [...unique, ...prev].slice(0, MAX_LOG_ENTRIES);
          });
          setHasEarlierLogs(result.hasMore);
        } else {
          historyLoadedRef.current = true;
          earlierOffsetRef.current = queryParams.limit || 100;
          let next = expanded.slice(-MAX_LOG_ENTRIES);
          // Merge in any live updates that arrived while history was in flight
          if (pendingUpdatesRef.current.length > 0) {
            const existing = new Set(next.map(entryKey));
            const fresh = pendingUpdatesRef.current.filter(
              (l) => !existing.has(entryKey(l)),
            );
            pendingUpdatesRef.current = [];
            next = [...next, ...fresh].slice(-MAX_LOG_ENTRIES);
          }
          setLogs(next);
          if (live) {
            setHasEarlierLogs(result.hasMore);
            setHasMore(false);
          } else {
            setHasMore(result.hasMore);
            setHasEarlierLogs(false);
          }
        }

        setTotal(result.total);
        setLastUpdate(new Date());
        return true;
      } catch (err: any) {
        // Intentional aborts (unmount, bot switch, filter change) are silent.
        // Check signal.aborted first: browsers may reject an aborted request
        // with TypeError("Failed to fetch") rather than AbortError.
        if (controller.signal.aborted || err?.name === "AbortError") {
          return false;
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
        return false;
      } finally {
        if (!controller.signal.aborted) {
          setLoading(false);
          setLoadingEarlier(false);
        }
      }
    },
    [botId, query, user, toast],
  );
  fetchLogsRef.current = fetchLogs;

  // -- SSE live updates --

  const handleUpdateEvent = useCallback((data: string) => {
    try {
      const parsed: { content: string; timestamp: string } = JSON.parse(data);
      let entries = expandLogEntries([
        {
          date: parsed.timestamp,
          content: parsed.content,
          timestamp: parsed.timestamp,
        },
      ]);
      // Mirror the server-side type=metrics filter (logs.service) for live
      // lines: history is filtered by the API, appends must match.
      if (queryRef.current.type === "metrics") {
        entries = entries.filter((entry) => {
          try {
            return !!JSON.parse(entry.content)?._metric;
          } catch {
            return false;
          }
        });
      }
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
    pendingUpdatesRef.current = [];

    fetchLogsRef.current(startQuery);

    return () => {
      restAbortRef.current?.abort();
    };
  }, [botId, user, streaming]);

  // -- Polling owner: non-streaming autoRefresh + cadence reconfiguration --

  useEffect(() => {
    if (!botId || !user) return;

    if (!streaming) {
      if (autoRefresh && refreshInterval > 0) {
        startPolling();
        return () => stopPolling();
      }
      stopPolling();
      return;
    }

    // Streaming mode: polling only exists as an SSE-failure fallback (owned
    // by the SSE effect). If it's running, restart it at the new cadence.
    if (pollingRef.current) {
      startPolling();
    }
  }, [botId, user, streaming, autoRefresh, refreshInterval, startPolling, stopPolling]);

  // -- SSE connection lifecycle (live mode only) --

  useEffect(() => {
    if (!liveMode || !botId || !user) return;

    const authResult = validateSSEAuth();
    if (!authResult.success) {
      // Can't stream without auth; keep data fresh via polling instead
      startPolling();
      return () => stopPolling();
    }

    const controller = new AbortController();

    authFetch(`/api/logs/${encodeURIComponent(botId)}/stream`, {
      signal: controller.signal,
    })
      .then(async (response) => {
        if (!response.ok || !response.body) {
          throw new Error(`Stream response: ${response.status}`);
        }

        setConnected(true);
        stopPolling();

        const reader = response.body.getReader();
        const decoder = new TextDecoder();
        let buffer = "";

        while (true) {
          const { done, value } = await reader.read();
          if (done) break;

          buffer += decoder.decode(value, { stream: true });
          const messages = buffer.split("\n\n");
          buffer = messages.pop() || "";

          for (const msg of messages) {
            if (!msg.trim()) continue;
            const { eventType, data } = parseSSEMessage(msg);
            if (!data) continue;
            // "history" events are intentionally ignored - REST is the
            // single source of history (see module doc).
            if (eventType === "update") {
              handleUpdateEvent(data);
            }
          }
        }

        // Stream ended cleanly (server closed it, e.g. access denied)
        setConnected(false);
      })
      .catch((err) => {
        // Intentional teardown (unmount, filter change, refresh) must not
        // trigger fallback polling - that's how intervals used to leak.
        // signal.aborted is checked first because browsers may reject an
        // aborted body read with TypeError, not AbortError.
        if (controller.signal.aborted || err?.name === "AbortError") return;

        setConnected(false);
        startPolling();
      });

    return () => {
      controller.abort();
      setConnected(false);
      // Fallback polling belongs to this SSE session
      stopPolling();
    };
  }, [botId, user, liveMode, sseNonce, handleUpdateEvent, startPolling, stopPolling]);

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
      });
    },
    [updateQuery],
  );

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
    }
  }, [fetchLogs, hasMore, query]);

  const loadEarlierLogs = useCallback(async () => {
    if (!botId || !user || loadingEarlier) return;
    setLoadingEarlier(true);

    const limit = query.limit || 100;
    const offset = earlierOffsetRef.current || limit;
    const ok = await fetchLogs({ ...query, offset }, "prepend");
    if (ok) {
      earlierOffsetRef.current = offset + limit;
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

    const logText = logs.map((log) => `[${log.date}] ${log.content}`).join("\n");
    const blob = new Blob([logText], { type: "text/plain" });
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
    error,
    hasMore,
    total,
    query,
    connected,
    lastUpdate,
    hasEarlierLogs,
    loadingEarlier,
    refresh,
    loadMore,
    loadEarlierLogs,
    updateQuery,
    setDateFilter,
    setDateRangeFilter,
    exportLogs,
  };
};
