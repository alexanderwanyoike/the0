/**
 * useBotLogsStream - Real-time log streaming hook
 *
 * Provides the same data interface as `useBotLogs` but with real-time SSE streaming.
 * Follows the `useCustomBotSSE` pattern: SSE connection with 4-second fallback to
 * REST polling when SSE is unavailable.
 *
 * Key behaviors:
 * - SSE active: disables polling, appends new log entries in real-time
 * - Date filter set: disconnects SSE, fetches historical data via REST
 * - Date filter cleared: reconnects SSE for live streaming
 * - SSE error before initial load: falls back to REST polling
 * - SSE error after initial load: reconnects with exponential backoff
 *
 * Cross-service contract: expects the API to provide:
 * - GET /api/logs/:botId - REST endpoint returning { data: LogEntry[], total, hasMore }
 * - GET /api/logs/:botId/stream - SSE endpoint emitting "history" and "update" events
 *   - "history" event data: LogEntry[] (initial batch)
 *   - "update" event data: { content: string, timestamp: string } (individual new entry)
 */

"use client";

import { useState, useEffect, useCallback, useRef } from "react";
import { useAuth } from "@/contexts/auth-context";
import { authFetch } from "@/lib/auth-fetch";
import { useToast } from "@/hooks/use-toast";
import { LogEntry } from "@/components/bot/console-interface";
import {
  createAuthenticatedSSEUrl,
  validateSSEAuth,
} from "@/lib/sse/sse-auth";

interface LogsQuery {
  date?: string;
  dateRange?: string;
  limit?: number;
  offset?: number;
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
}

/**
 * Expand raw log entries by splitting multi-line content into individual LogEntry items.
 * Each non-empty line becomes its own entry, preserving the original timestamp.
 *
 * The API may return log entries where a single entry's `content` field contains
 * multiple newline-separated lines (e.g. multi-line print output from a bot).
 * This function normalizes them into one LogEntry per line for consistent rendering.
 */
function expandLogEntries(entries: LogEntry[]): LogEntry[] {
  const expanded: LogEntry[] = [];
  entries.forEach((entry) => {
    const lines = entry.content.split("\n").filter((line) => line.trim() !== "");
    lines.forEach((line) => {
      expanded.push({
        date: entry.date,
        content: line.trim(),
      });
    });
  });
  return expanded;
}

export const useBotLogsStream = ({
  botId,
  refreshInterval = 30000,
  initialQuery = { limit: 100, offset: 0 },
}: UseBotLogsStreamProps): UseBotLogsStreamReturn => {
  const [logs, setLogs] = useState<LogEntry[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [hasMore, setHasMore] = useState(false);
  const [total, setTotal] = useState(0);
  const [connected, setConnected] = useState(false);
  const [lastUpdate, setLastUpdate] = useState<Date | null>(null);
  const [query, setQuery] = useState<LogsQuery>(initialQuery);

  const { user } = useAuth();
  const { toast } = useToast();

  const eventSourceRef = useRef<EventSource | null>(null);
  const pollingIntervalRef = useRef<NodeJS.Timeout | null>(null);
  const fallbackTimeoutRef = useRef<NodeJS.Timeout | null>(null);
  const abortControllerRef = useRef<AbortController | null>(null);
  const initialLoadCompleteRef = useRef(false);
  const isUsingDateFilterRef = useRef(false);
  const reconnectAttemptsRef = useRef(0);

  // ---- REST fetch (same pattern as useBotLogs) ----

  const fetchLogs = useCallback(
    async (queryParams: LogsQuery = query) => {
      if (!botId) return;

      if (abortControllerRef.current) {
        abortControllerRef.current.abort();
      }

      const controller = new AbortController();
      abortControllerRef.current = controller;

      try {
        setLoading(true);
        setError(null);

        if (!user) {
          throw new Error("User not authenticated");
        }

        const searchParams = new URLSearchParams();
        if (queryParams.date) {
          searchParams.set("date", queryParams.date);
        } else if (!queryParams.dateRange) {
          searchParams.set(
            "date",
            new Date().toISOString().slice(0, 10).replace(/-/g, ""),
          );
        }
        if (queryParams.dateRange)
          searchParams.set("dateRange", queryParams.dateRange);
        if (queryParams.limit != null)
          searchParams.set("limit", queryParams.limit.toString());
        if (queryParams.offset != null)
          searchParams.set("offset", queryParams.offset.toString());

        const response = await authFetch(
          `/api/logs/${encodeURIComponent(botId)}?${searchParams.toString()}`,
          { signal: controller.signal },
        );

        if (!response.ok) {
          throw new Error(`Failed to fetch logs: ${response.statusText}`);
        }

        const result: LogsResponse = await response.json();
        const expandedLogs = expandLogEntries(result.data);

        setLogs(expandedLogs);
        setHasMore(result.hasMore);
        setTotal(result.total);
        setLastUpdate(new Date());
        initialLoadCompleteRef.current = true;
      } catch (err: any) {
        if (err.name === "AbortError") return;

        const errorMessage = err.message || "Failed to fetch logs";
        setError(errorMessage);
        toast({
          title: "Error",
          description: errorMessage,
          variant: "destructive",
        });
      } finally {
        setLoading(false);
        abortControllerRef.current = null;
      }
    },
    [botId, query, toast, user],
  );

  const fetchLogsRef = useRef(fetchLogs);
  fetchLogsRef.current = fetchLogs;

  // ---- SSE connection ----

  const connectSSE = useCallback(() => {
    if (!user || !botId) return;

    const authResult = validateSSEAuth();
    if (!authResult.success) return;

    const urlResult = createAuthenticatedSSEUrl(
      `/api/logs/${encodeURIComponent(botId)}/stream`,
    );
    if (!urlResult.success) return;

    // Clean up any existing EventSource and pending timers to avoid duplicates
    if (eventSourceRef.current) {
      eventSourceRef.current.close();
      eventSourceRef.current = null;
    }
    if (fallbackTimeoutRef.current) {
      clearTimeout(fallbackTimeoutRef.current);
      fallbackTimeoutRef.current = null;
    }

    const es = new EventSource(urlResult.data.url, {
      withCredentials: true,
    });

    es.onopen = () => {
      setConnected(true);
      setError(null);
      reconnectAttemptsRef.current = 0;

      // Clear fallback timer since SSE connected
      if (fallbackTimeoutRef.current) {
        clearTimeout(fallbackTimeoutRef.current);
        fallbackTimeoutRef.current = null;
      }

      // Stop polling when SSE is active
      if (pollingIntervalRef.current) {
        clearInterval(pollingIntervalRef.current);
        pollingIntervalRef.current = null;
      }
    };

    es.addEventListener("history", (event: MessageEvent) => {
      try {
        const entries: LogEntry[] = JSON.parse(event.data);
        const expanded = expandLogEntries(entries);
        setLogs(expanded);
        setTotal(expanded.length);
        setLastUpdate(new Date());
        initialLoadCompleteRef.current = true;
        setLoading(false);
      } catch (err) {
        console.error("Failed to parse history SSE event:", err);
      }
    });

    es.addEventListener("update", (event: MessageEvent) => {
      try {
        const data: { content: string; timestamp: string } = JSON.parse(
          event.data,
        );
        const newEntries = expandLogEntries([
          { date: data.timestamp, content: data.content },
        ]);
        setLogs((prev) => [...prev, ...newEntries]);
        setTotal((prev) => prev + newEntries.length);
        setLastUpdate(new Date());
      } catch (err) {
        console.error("Failed to parse update SSE event:", err);
      }
    });

    es.onerror = () => {
      setConnected(false);
      // Close the errored EventSource immediately to free the browser
      // connection slot during the backoff delay before reconnecting.
      es.close();
      eventSourceRef.current = null;

      if (!initialLoadCompleteRef.current) {
        // Never loaded data -- fall back to REST polling
        fetchLogsRef.current();
        if (!pollingIntervalRef.current) {
          pollingIntervalRef.current = setInterval(() => {
            fetchLogsRef.current();
          }, refreshInterval);
        }
      } else if (!isUsingDateFilterRef.current) {
        // SSE dropped after initial load -- try to reconnect with backoff
        // Only reconnect when not using date filter
        const delay = Math.min(
          1000 * Math.pow(2, reconnectAttemptsRef.current),
          30000,
        );
        reconnectAttemptsRef.current++;
        fallbackTimeoutRef.current = setTimeout(() => {
          connectSSE();
        }, delay);
      }
    };

    eventSourceRef.current = es;
  }, [user, botId, refreshInterval]);

  // ---- Cleanup helper ----

  const cleanupSSE = useCallback(() => {
    if (eventSourceRef.current) {
      eventSourceRef.current.close();
      eventSourceRef.current = null;
    }
    setConnected(false);
  }, []);

  // ---- Establish SSE on mount with 4s fallback to REST polling ----

  useEffect(() => {
    if (!user || !botId) {
      setLoading(false);
      return;
    }

    // Only connect SSE when there is no date filter active
    if (!isUsingDateFilterRef.current) {
      setLoading(true);
      connectSSE();

      // If SSE doesn't connect within 4 seconds, fall back to REST polling
      fallbackTimeoutRef.current = setTimeout(() => {
        if (!initialLoadCompleteRef.current) {
          cleanupSSE();
          fetchLogsRef.current();

          // Start polling as fallback
          pollingIntervalRef.current = setInterval(() => {
            fetchLogsRef.current();
          }, refreshInterval);
        }
      }, 4000);
    }

    return () => {
      if (fallbackTimeoutRef.current) {
        clearTimeout(fallbackTimeoutRef.current);
        fallbackTimeoutRef.current = null;
      }
      if (pollingIntervalRef.current) {
        clearInterval(pollingIntervalRef.current);
        pollingIntervalRef.current = null;
      }
      cleanupSSE();
    };
  }, [user, botId, connectSSE, cleanupSSE, refreshInterval]);

  // ---- Cleanup everything on unmount ----

  useEffect(() => {
    return () => {
      if (eventSourceRef.current) {
        eventSourceRef.current.close();
        eventSourceRef.current = null;
      }
      if (pollingIntervalRef.current) {
        clearInterval(pollingIntervalRef.current);
        pollingIntervalRef.current = null;
      }
      if (fallbackTimeoutRef.current) {
        clearTimeout(fallbackTimeoutRef.current);
        fallbackTimeoutRef.current = null;
      }
      if (abortControllerRef.current) {
        abortControllerRef.current.abort();
        abortControllerRef.current = null;
      }
    };
  }, []);

  // ---- Public API ----

  // Refresh always uses REST to get a full consistent snapshot of current logs.
  // This is intentional even when SSE is connected -- the user is explicitly
  // requesting a reload, and REST gives a reliable full page of results.
  const refresh = useCallback(() => {
    const refreshQuery = { ...query, offset: 0 };
    setQuery(refreshQuery);
    fetchLogsRef.current(refreshQuery);
  }, [query]);

  const setDateFilter = useCallback(
    (date: string | null) => {
      isUsingDateFilterRef.current = !!date;

      if (date) {
        // Date filter active -- switch to REST
        cleanupSSE();
        if (pollingIntervalRef.current) {
          clearInterval(pollingIntervalRef.current);
          pollingIntervalRef.current = null;
        }
        if (fallbackTimeoutRef.current) {
          clearTimeout(fallbackTimeoutRef.current);
          fallbackTimeoutRef.current = null;
        }
      }

      const updatedQuery: LogsQuery = {
        ...query,
        date: date || undefined,
        dateRange: undefined,
        offset: 0,
      };
      setQuery(updatedQuery);
      fetchLogsRef.current(updatedQuery);

      // Reconnect SSE when clearing date filter, with a fallback timeout
      // in case the new SSE connection never establishes
      if (!date) {
        connectSSE();

        fallbackTimeoutRef.current = setTimeout(() => {
          if (!eventSourceRef.current) {
            // SSE didn't reconnect within 4s -- start polling as fallback
            if (!pollingIntervalRef.current) {
              pollingIntervalRef.current = setInterval(() => {
                fetchLogsRef.current();
              }, refreshInterval);
            }
          }
        }, 4000);
      }
    },
    [query, cleanupSSE, connectSSE, refreshInterval],
  );

  const setDateRangeFilter = useCallback(
    (start: string, end: string) => {
      // Date range filter means historical data -- switch to REST
      isUsingDateFilterRef.current = true;
      cleanupSSE();
      if (pollingIntervalRef.current) {
        clearInterval(pollingIntervalRef.current);
        pollingIntervalRef.current = null;
      }
      if (fallbackTimeoutRef.current) {
        clearTimeout(fallbackTimeoutRef.current);
        fallbackTimeoutRef.current = null;
      }

      const updatedQuery: LogsQuery = {
        ...query,
        dateRange: `${start}-${end}`,
        date: undefined,
        offset: 0,
      };
      setQuery(updatedQuery);
      fetchLogsRef.current(updatedQuery);
    },
    [query, cleanupSSE],
  );

  const exportLogs = useCallback(() => {
    if (logs.length === 0) {
      toast({
        title: "No logs to export",
        description: "There are no logs available to export.",
        variant: "destructive",
      });
      return;
    }

    const logText = logs
      .map((log) => `[${log.date}] ${log.content}`)
      .join("\n");

    const blob = new Blob([logText], { type: "text/plain" });
    const url = URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.href = url;
    link.download = `bot-${encodeURIComponent(botId)}-logs-${new Date().toISOString().split("T")[0]}.txt`;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    URL.revokeObjectURL(url);

    toast({
      description: "Logs exported successfully",
    });
  }, [logs, botId, toast]);

  return {
    logs,
    loading,
    error,
    hasMore,
    total,
    refresh,
    exportLogs,
    setDateFilter,
    setDateRangeFilter,
    connected,
    lastUpdate,
  };
};
