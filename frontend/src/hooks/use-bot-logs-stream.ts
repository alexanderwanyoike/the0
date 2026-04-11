/**
 * useBotLogsStream - Real-time log streaming hook
 *
 * SSE connection with 4-second fallback to REST polling when SSE is unavailable.
 *
 * Behaviors:
 * - SSE active: disables polling, appends new log entries in real-time
 * - Date filter set: disconnects SSE, fetches historical data via REST
 * - Date filter cleared: reconnects SSE for live streaming
 * - SSE error: falls back to REST polling
 *
 * Cross-service contract:
 * - GET /api/logs/:botId - REST endpoint returning { data: LogEntry[], total, hasMore }
 * - GET /api/logs/:botId/stream - SSE endpoint emitting "history" and "update" events
 */

"use client";

import { useState, useEffect, useCallback, useRef } from "react";
import { useAuth } from "@/contexts/auth-context";
import { authFetch } from "@/lib/auth-fetch";
import { useToast } from "@/hooks/use-toast";
import { LogEntry } from "@/components/bot/console-interface";
import { expandLogEntries } from "@/lib/log-utils";
import { validateSSEAuth } from "@/lib/sse/sse-auth";

interface LogsQuery {
  date?: string;
  dateRange?: string;
  limit?: number;
  offset?: number;
  sort?: "asc" | "desc";
}

interface LogsResponse {
  data: LogEntry[];
  total: number;
  hasMore: boolean;
}

interface UseBotLogsStreamProps {
  botId: string;
  refreshInterval?: number;
  initialQuery?: LogsQuery;
}

export interface UseBotLogsStreamReturn {
  logs: LogEntry[];
  loading: boolean;
  error: string | null;
  hasMore: boolean;
  total: number;
  refresh: () => void;
  exportLogs: () => void;
  setDateFilter: (date: string | null) => void;
  setDateRangeFilter: (start: string, end: string) => void;
  connected: boolean;
  lastUpdate: Date | null;
  hasEarlierLogs: boolean;
  loadingEarlier: boolean;
  loadEarlierLogs: () => void;
  /** Load more paginated results (only available in REST mode, i.e. when a date filter is active) */
  loadMore?: () => Promise<void>;
  updateQuery: (params: Partial<{ date?: string; dateRange?: string; limit?: number; offset?: number; sort?: "asc" | "desc" }>) => void;
}

const MAX_LOG_ENTRIES = 10000;


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

export const useBotLogsStream = ({
  botId,
  refreshInterval = 30000,
  initialQuery = { limit: 100, offset: 0, sort: "desc" },
}: UseBotLogsStreamProps): UseBotLogsStreamReturn => {
  const [logs, setLogs] = useState<LogEntry[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [connected, setConnected] = useState(false);
  const [lastUpdate, setLastUpdate] = useState<Date | null>(null);
  const [hasEarlierLogs, setHasEarlierLogs] = useState(false);
  const [loadingEarlier, setLoadingEarlier] = useState(false);
  const [hasMore, setHasMore] = useState(false);
  const [query, setQuery] = useState<LogsQuery>(initialQuery);
  const [totalSeen, setTotalSeen] = useState(0);

  const { user } = useAuth();
  const { toast } = useToast();

  const sseAbortRef = useRef<AbortController | null>(null);
  const restAbortRef = useRef<AbortController | null>(null);
  const pollingRef = useRef<NodeJS.Timeout | null>(null);
  const dateFilterActiveRef = useRef(false);
  const historyReceivedRef = useRef(false);
  const loadingMoreRef = useRef(false);
  // Ref so SSE connection and polling can read the latest refreshInterval
  // without causing the lifecycle effect to tear down and reconnect SSE.
  const refreshIntervalRef = useRef(refreshInterval);
  refreshIntervalRef.current = refreshInterval;

  // -- Cleanup helpers --

  const stopPolling = useCallback(() => {
    if (pollingRef.current) {
      clearInterval(pollingRef.current);
      pollingRef.current = null;
    }
  }, []);

  const abortAll = useCallback(() => {
    sseAbortRef.current?.abort();
    sseAbortRef.current = null;
    restAbortRef.current?.abort();
    restAbortRef.current = null;
    stopPolling();
  }, [stopPolling]);

  // -- REST fetch (for fallback, date filters, load-earlier) --

  const fetchLogs = useCallback(
    async (params?: Partial<LogsQuery>, merge: false | "prepend" | "append" = false) => {
      if (!botId || !user) return;

      restAbortRef.current?.abort();
      const controller = new AbortController();
      restAbortRef.current = controller;

      const effectiveQuery = { ...query, ...params };

      try {
        if (!merge) setLoading(true);
        setError(null);

        const searchParams = new URLSearchParams();
        if (effectiveQuery.date) {
          searchParams.set("date", effectiveQuery.date);
        } else if (!effectiveQuery.dateRange) {
          searchParams.set(
            "date",
            new Date().toISOString().slice(0, 10).replace(/-/g, ""),
          );
        }
        if (effectiveQuery.dateRange)
          searchParams.set("dateRange", effectiveQuery.dateRange);
        if (effectiveQuery.limit != null)
          searchParams.set("limit", effectiveQuery.limit.toString());
        if (effectiveQuery.offset != null)
          searchParams.set("offset", effectiveQuery.offset.toString());
        if (effectiveQuery.sort)
          searchParams.set("sort", effectiveQuery.sort);

        const response = await authFetch(
          `/api/logs/${encodeURIComponent(botId)}?${searchParams.toString()}`,
          { signal: controller.signal },
        );

        if (!response.ok) {
          throw new Error(`Failed to fetch logs: ${response.statusText}`);
        }

        const result: LogsResponse = await response.json();
        const expanded = expandLogEntries(result.data);

        if (merge) {
          setLogs((prev) => {
            const existingKeys = new Set(
              prev.map((l) => `${l.date}|${l.content}`),
            );
            const unique = expanded.filter(
              (l) => !existingKeys.has(`${l.date}|${l.content}`),
            );
            const combined = merge === "prepend"
              ? [...unique, ...prev]
              : [...prev, ...unique];
            // Prepend keeps the front (earlier logs), append keeps the tail (latest)
            return merge === "prepend"
              ? combined.slice(0, MAX_LOG_ENTRIES)
              : combined.slice(-MAX_LOG_ENTRIES);
          });
        } else {
          setLogs(expanded.slice(-MAX_LOG_ENTRIES));
          setHasEarlierLogs(expanded.length >= MAX_LOG_ENTRIES);
        }
        // Only update forward pagination state on non-prepend fetches.
        // Prepend (loadEarlierLogs) has its own hasMore semantics.
        if (merge !== "prepend") {
          setHasMore(result.hasMore);
        }
        setTotalSeen(result.total);
        setLastUpdate(new Date());
      } catch (err: any) {
        if (err.name === "AbortError") return;
        const msg = err.message || "Failed to fetch logs";
        setError(msg);
        toast({ title: "Error", description: msg, variant: "destructive" });
      } finally {
        if (!controller.signal.aborted) {
          setLoading(false);
          setLoadingEarlier(false);
        }
      }
    },
    [botId, query, user, toast],
  );

  // -- SSE connection --

  const connectSSE = useCallback(() => {
    if (!user || !botId) return;

    const authResult = validateSSEAuth();
    if (!authResult.success) return;

    sseAbortRef.current?.abort();
    const controller = new AbortController();
    sseAbortRef.current = controller;

    authFetch(`/api/logs/${encodeURIComponent(botId)}/stream`, {
      signal: controller.signal,
    })
      .then(async (response) => {
        if (!response.ok || !response.body) {
          throw new Error(`Stream response: ${response.status}`);
        }

        setConnected(true);
        setError(null);
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

            if (eventType === "history") {
              try {
                const entries: LogEntry[] = JSON.parse(data);
                const expanded = expandLogEntries(entries);
                setHasEarlierLogs(expanded.length >= MAX_LOG_ENTRIES);
                setLogs(expanded.slice(-MAX_LOG_ENTRIES));
                setTotalSeen(expanded.length);
                setLastUpdate(new Date());
                setLoading(false);
                historyReceivedRef.current = true;
              } catch (err) {
                console.error("Failed to parse history SSE event:", err);
              }
            } else if (eventType === "update") {
              try {
                const parsed: { content: string; timestamp: string } =
                  JSON.parse(data);
                const entries = expandLogEntries([
                  { date: parsed.timestamp, content: parsed.content, timestamp: parsed.timestamp },
                ]);
                setLogs((prev) =>
                  [...prev, ...entries].slice(-MAX_LOG_ENTRIES),
                );
                setTotalSeen((prev) => prev + entries.length);
                setLastUpdate(new Date());
              } catch (err) {
                console.error("Failed to parse update SSE event:", err);
              }
            }
          }
        }

        // Stream ended cleanly
        setConnected(false);
      })
      .catch((err) => {
        if (err.name === "AbortError") return;
        setConnected(false);

        // Fall back to REST polling
        fetchLogs();
        if (refreshIntervalRef.current > 0) {
          pollingRef.current = setInterval(() => fetchLogs(), refreshIntervalRef.current);
        }
      });
  }, [botId, user, fetchLogs, stopPolling]);

  // -- Lifecycle: connect on mount/botId change, cleanup on unmount --

  useEffect(() => {
    if (!user || !botId) {
      setLoading(false);
      return;
    }

    // Reset state for new bot
    setLogs([]);
    setTotalSeen(0);
    setLoading(true);
    setConnected(false);
    setError(null);
    setLastUpdate(null);
    setHasEarlierLogs(false);
    setLoadingEarlier(false);
    setHasMore(false);
    setQuery(initialQuery);
    dateFilterActiveRef.current = false;
    historyReceivedRef.current = false;

    connectSSE();

    // 4s fallback: if SSE hasn't delivered history yet, switch to REST polling
    const fallbackTimer = setTimeout(() => {
      if (!historyReceivedRef.current) {
        fetchLogs();
        if (refreshIntervalRef.current > 0) {
          pollingRef.current = setInterval(() => fetchLogs(), refreshIntervalRef.current);
        }
      }
    }, 4000);

    return () => {
      clearTimeout(fallbackTimer);
      abortAll();
    };
    // refreshInterval intentionally excluded - uses ref to avoid SSE reconnection
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [botId, user]);

  // Reconfigure REST polling interval when refreshInterval changes
  // without tearing down the SSE connection.
  useEffect(() => {
    if (!pollingRef.current) return; // No active polling, nothing to reconfigure
    stopPolling();
    if (refreshInterval > 0) {
      pollingRef.current = setInterval(() => fetchLogs(), refreshInterval);
    }
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [refreshInterval]);

  // -- Date filter: disconnect SSE, fetch REST, reconnect on clear --

  const setDateFilter = useCallback(
    (date: string | null) => {
      dateFilterActiveRef.current = !!date;
      const updatedQuery = {
        ...query,
        date: date || undefined,
        dateRange: undefined,
        offset: 0,
      };
      setQuery(updatedQuery);

      if (date) {
        sseAbortRef.current?.abort();
        sseAbortRef.current = null;
        stopPolling();
        setConnected(false);
        fetchLogs(updatedQuery);
      } else {
        // Reconnecting to SSE - abort any in-flight REST request and reset
        restAbortRef.current?.abort();
        restAbortRef.current = null;
        setHasMore(false);
        connectSSE();
      }
    },
    [query, fetchLogs, connectSSE, stopPolling],
  );

  const setDateRangeFilter = useCallback(
    (start: string, end: string) => {
      dateFilterActiveRef.current = true;
      // Use -- separator for ISO datetime ranges, - for YYYYMMDD date ranges
      const separator = start.includes("T") ? "--" : "-";
      const updatedQuery = {
        ...query,
        date: undefined,
        dateRange: `${start}${separator}${end}`,
        offset: 0,
      };
      setQuery(updatedQuery);

      sseAbortRef.current?.abort();
      sseAbortRef.current = null;
      stopPolling();
      setConnected(false);
      fetchLogs(updatedQuery);
    },
    [query, fetchLogs, stopPolling],
  );

  // -- Load earlier logs (prepend to current buffer) --

  const loadEarlierLogs = useCallback(async () => {
    if (!botId || !user || loadingEarlier) return;
    setLoadingEarlier(true);

    // Use the active date filter if set, otherwise default to today
    const date =
      query.date ||
      new Date().toISOString().slice(0, 10).replace(/-/g, "");
    await fetchLogs({ date, limit: 1000, offset: 0 }, "prepend");
    setHasEarlierLogs(false);
  }, [botId, user, loadingEarlier, fetchLogs, query.date]);

  // -- Load more (pagination for REST date-range fetches) --

  const loadMore = useCallback(async () => {
    if (loadingMoreRef.current || !hasMore || !botId || !user) return;
    loadingMoreRef.current = true;

    const nextQuery = {
      ...query,
      offset: (query.offset || 0) + (query.limit || 100),
    };

    try {
      await fetchLogs(nextQuery, "append");
      // Only advance offset after successful fetch
      setQuery(nextQuery);
    } finally {
      loadingMoreRef.current = false;
    }
  }, [hasMore, botId, user, query, fetchLogs]);

  // -- Refresh: reconnect or re-fetch depending on mode --

  const refresh = useCallback(() => {
    if (dateFilterActiveRef.current) {
      fetchLogs();
    } else {
      abortAll();
      connectSSE();
    }
  }, [fetchLogs, abortAll, connectSSE]);

  // -- Update query (for sort changes, etc.) --

  const updateQuery = useCallback(
    (params: Partial<LogsQuery>) => {
      const updatedQuery = { ...query, ...params, offset: 0 };
      setQuery(updatedQuery);
      if (dateFilterActiveRef.current) {
        fetchLogs(updatedQuery);
      }
    },
    [query, fetchLogs],
  );

  // -- Export logs as downloadable file --

  const exportLogs = useCallback(() => {
    if (logs.length === 0) {
      toast({
        title: "No logs",
        description: "No logs available to export",
        variant: "destructive",
      });
      return;
    }

    const content = logs.map((l) => `${l.date} ${l.content}`).join("\n");
    const blob = new Blob([content], { type: "text/plain" });
    const url = URL.createObjectURL(blob);
    const a = document.createElement("a");
    a.href = url;
    a.download = `${botId}-logs-${new Date().toISOString().slice(0, 10)}.txt`;
    a.click();
    URL.revokeObjectURL(url);
  }, [logs, botId, toast]);

  return {
    logs,
    loading,
    error,
    hasMore,
    total: totalSeen,
    refresh,
    exportLogs,
    setDateFilter,
    setDateRangeFilter,
    connected,
    lastUpdate,
    hasEarlierLogs,
    loadingEarlier,
    loadEarlierLogs,
    loadMore: hasMore ? loadMore : undefined,
    updateQuery,
  };
};
