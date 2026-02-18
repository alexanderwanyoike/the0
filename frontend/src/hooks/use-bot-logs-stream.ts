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
import { validateSSEAuth } from "@/lib/sse/sse-auth";

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
  hasEarlierLogs: boolean;
  loadingEarlier: boolean;
  loadEarlierLogs: () => void;
}

const MAX_LOG_ENTRIES = 2000;

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
        content: line, // preserve original indentation (trim only used for blank-line filter)
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
  // totalSeen: cumulative count of all logs received this session, including entries
  // trimmed from the buffer when it exceeds MAX_LOG_ENTRIES. The UI uses logs.length
  // for the visible entry count badge; totalSeen tracks how many we've seen overall.
  const [totalSeen, setTotalSeen] = useState(0);
  const [connected, setConnected] = useState(false);
  const [lastUpdate, setLastUpdate] = useState<Date | null>(null);
  const [query, setQuery] = useState<LogsQuery>(initialQuery);
  const [hasEarlierLogs, setHasEarlierLogs] = useState(false);
  const [loadingEarlier, setLoadingEarlier] = useState(false);

  const { user } = useAuth();
  const { toast } = useToast();

  const sseAbortRef = useRef<AbortController | null>(null);
  const pollingIntervalRef = useRef<NodeJS.Timeout | null>(null);
  const fallbackTimeoutRef = useRef<NodeJS.Timeout | null>(null);
  const abortControllerRef = useRef<AbortController | null>(null);
  const initialLoadCompleteRef = useRef(false);
  const isUsingDateFilterRef = useRef(false);
  const reconnectAttemptsRef = useRef(0);
  const trimmedCountRef = useRef(0);

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
        setTotalSeen(result.total);
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

    // Clean up any existing SSE stream and pending timers to avoid duplicates
    if (sseAbortRef.current) {
      sseAbortRef.current.abort();
      sseAbortRef.current = null;
    }
    if (fallbackTimeoutRef.current) {
      clearTimeout(fallbackTimeoutRef.current);
      fallbackTimeoutRef.current = null;
    }

    const controller = new AbortController();
    sseAbortRef.current = controller;

    // Use authFetch to send Authorization header (EventSource can't do this)
    authFetch(`/api/logs/${encodeURIComponent(botId)}/stream`, {
      signal: controller.signal,
    })
      .then(async (response) => {
        if (!response.ok || !response.body) {
          throw new Error(`Stream response: ${response.status}`);
        }

        setConnected(true);
        setError(null);
        reconnectAttemptsRef.current = 0;
        trimmedCountRef.current = 0;
        setHasEarlierLogs(false);

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

        // Read the SSE stream and parse events
        const reader = response.body.getReader();
        const decoder = new TextDecoder();
        let buffer = "";

        while (true) {
          const { done, value } = await reader.read();
          if (done) break;

          buffer += decoder.decode(value, { stream: true });

          // Parse complete SSE messages (separated by double newline)
          const messages = buffer.split("\n\n");
          // Keep the last chunk which may be incomplete
          buffer = messages.pop() || "";

          for (const msg of messages) {
            if (!msg.trim()) continue;

            let eventType = "message";
            let data = "";

            for (const line of msg.split("\n")) {
              if (line.startsWith("event: ")) {
                eventType = line.slice(7).trim();
              } else if (line.startsWith("data: ")) {
                data += line.slice(6);
              } else if (line.startsWith("data:")) {
                data += line.slice(5);
              }
            }

            if (!data) continue;

            if (eventType === "history") {
              try {
                const entries: LogEntry[] = JSON.parse(data);
                const expanded = expandLogEntries(entries);
                if (expanded.length > MAX_LOG_ENTRIES) {
                  const trimmed = expanded.length - MAX_LOG_ENTRIES;
                  trimmedCountRef.current = trimmed;
                  setHasEarlierLogs(true);
                  setLogs(expanded.slice(trimmed));
                } else {
                  trimmedCountRef.current = 0;
                  setHasEarlierLogs(false);
                  setLogs(expanded);
                }
                setTotalSeen(expanded.length);
                setLastUpdate(new Date());
                initialLoadCompleteRef.current = true;
                setLoading(false);
              } catch (err) {
                console.error("Failed to parse history SSE event:", err);
              }
            } else if (eventType === "update") {
              try {
                const parsed: { content: string; timestamp: string } =
                  JSON.parse(data);
                const newEntries = expandLogEntries([
                  { date: parsed.timestamp, content: parsed.content },
                ]);
                setLogs((prev) => {
                  const combined = [...prev, ...newEntries];
                  if (combined.length > MAX_LOG_ENTRIES) {
                    const excess = combined.length - MAX_LOG_ENTRIES;
                    trimmedCountRef.current += excess;
                    setHasEarlierLogs(true);
                    return combined.slice(excess);
                  }
                  return combined;
                });
                setTotalSeen((prev) => prev + newEntries.length);
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
        sseAbortRef.current = null;

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
          const delay = Math.min(
            1000 * Math.pow(2, reconnectAttemptsRef.current),
            30000,
          );
          reconnectAttemptsRef.current++;
          fallbackTimeoutRef.current = setTimeout(() => {
            connectSSE();
          }, delay);
        }
      });
  }, [user, botId, refreshInterval]);

  // ---- Cleanup helper ----

  const cleanupSSE = useCallback(() => {
    if (sseAbortRef.current) {
      sseAbortRef.current.abort();
      sseAbortRef.current = null;
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
      if (sseAbortRef.current) {
        sseAbortRef.current.abort();
        sseAbortRef.current = null;
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
          if (!sseAbortRef.current) {
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

  const loadEarlierLogs = useCallback(async () => {
    if (!botId || !user) return;

    setLoadingEarlier(true);
    try {
      const today = new Date().toISOString().slice(0, 10).replace(/-/g, "");
      const searchParams = new URLSearchParams();
      searchParams.set("date", today);

      const response = await authFetch(
        `/api/logs/${encodeURIComponent(botId)}?${searchParams.toString()}`,
      );

      if (!response.ok) {
        throw new Error(`Failed to fetch earlier logs: ${response.statusText}`);
      }

      const result: LogsResponse = await response.json();
      const allExpanded = expandLogEntries(result.data);

      setLogs((currentLogs) => {
        // Find where the current buffer starts in the full log
        // Prepend entries that are before the current buffer
        const currentFirst = currentLogs[0];
        if (!currentFirst || allExpanded.length === 0) {
          return currentLogs;
        }

        // Find the index of the first current log in the full set
        let matchIndex = -1;
        for (let i = 0; i < allExpanded.length; i++) {
          if (
            allExpanded[i].content === currentFirst.content &&
            allExpanded[i].date === currentFirst.date
          ) {
            matchIndex = i;
            break;
          }
        }

        if (matchIndex <= 0) {
          // No earlier entries found or already at start
          setHasEarlierLogs(false);
          return currentLogs;
        }

        const earlier = allExpanded.slice(0, matchIndex);
        const combined = [...earlier, ...currentLogs];

        if (combined.length > MAX_LOG_ENTRIES) {
          const excess = combined.length - MAX_LOG_ENTRIES;
          trimmedCountRef.current = excess;
          setHasEarlierLogs(true);
          return combined.slice(excess);
        }

        trimmedCountRef.current = 0;
        setHasEarlierLogs(false);
        return combined;
      });
    } catch (err: any) {
      if (err.name === "AbortError") return;
      toast({
        title: "Error",
        description: err.message || "Failed to load earlier logs",
        variant: "destructive",
      });
    } finally {
      setLoadingEarlier(false);
    }
  }, [botId, user, toast]);

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
  };
};
