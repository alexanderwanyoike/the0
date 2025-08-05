"use client";

import { useState, useEffect, useCallback, useRef } from "react";
import { useToast } from "@/hooks/use-toast";
import { LogEntry } from "@/components/bot/console-interface";
import { useAuth } from "@/contexts/auth-context";
import { authFetch } from "@/lib/auth-fetch";

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

interface UseBotLogsProps {
  botId: string;
  autoRefresh?: boolean;
  refreshInterval?: number;
  initialQuery?: LogsQuery;
}

export const useBotLogs = ({
  botId,
  autoRefresh = false,
  refreshInterval = 30000, // 30 seconds
  initialQuery = { limit: 100, offset: 0 },
}: UseBotLogsProps) => {
  const [logs, setLogs] = useState<LogEntry[]>([]);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const [query, setQuery] = useState<LogsQuery>(initialQuery);
  const [hasMore, setHasMore] = useState(false);
  const [total, setTotal] = useState(0);
  const { user } = useAuth();

  const { toast } = useToast();
  const intervalRef = useRef<NodeJS.Timeout | null>(null);
  const abortControllerRef = useRef<AbortController | null>(null);

  const fetchLogs = useCallback(
    async (queryParams: LogsQuery = query, append: boolean = false) => {
      if (!botId) return;

      // Cancel previous request
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
        } else {
          searchParams.set(
            "date",
            new Date().toISOString().slice(0, 10).replace(/-/g, ""),
          );
        }
        if (queryParams.dateRange)
          searchParams.set("dateRange", queryParams.dateRange);
        if (queryParams.limit)
          searchParams.set("limit", queryParams.limit.toString());
        if (queryParams.offset)
          searchParams.set("offset", queryParams.offset.toString());

        const response = await authFetch(
          `/api/logs/${botId}?${searchParams.toString()}`,
          {
            signal: controller.signal,
          },
        );

        if (!response.ok) {
          throw new Error(`Failed to fetch logs: ${response.statusText}`);
        }

        const result: LogsResponse = await response.json();

        // Split each log entry's content into individual lines
        const expandedLogs: LogEntry[] = [];
        result.data.forEach((logEntry) => {
          // Split content by newlines and create individual entries
          const lines = logEntry.content
            .split("\n")
            .filter((line) => line.trim() !== "");
          lines.forEach((line) => {
            expandedLogs.push({
              date: logEntry.date,
              content: line.trim(),
            });
          });
        });

        if (append) {
          setLogs((prev) => [...prev, ...expandedLogs]);
        } else {
          setLogs(expandedLogs);
        }

        setHasMore(result.hasMore);
        setTotal(result.total);
      } catch (err: any) {
        if (err.name === "AbortError") {
          return; // Request was cancelled, don't show error
        }

        const errorMessage = err.message || "Failed to fetch logs";
        setError(errorMessage);

        if (!append) {
          // Only show toast for initial fetch errors
          toast({
            title: "Error",
            description: errorMessage,
            variant: "destructive",
          });
        }
      } finally {
        setLoading(false);
        abortControllerRef.current = null;
      }
    },
    [botId, query, toast, user],
  );

  // Load more logs (pagination)
  const loadMore = useCallback(async () => {
    if (loading || !hasMore) return;

    const nextQuery = {
      ...query,
      offset: (query.offset || 0) + (query.limit || 100),
    };

    await fetchLogs(nextQuery, true);
  }, [fetchLogs, loading, hasMore, query]);

  // Refresh logs
  const refresh = useCallback(() => {
    const refreshQuery = { ...query, offset: 0 };
    setQuery(refreshQuery);
    fetchLogs(refreshQuery, false);
  }, [fetchLogs, query]);

  // Update query filters
  const updateQuery = useCallback(
    (newQuery: Partial<LogsQuery>) => {
      const updatedQuery = {
        ...query,
        ...newQuery,
        offset: 0, // Reset offset when changing filters
      };
      setQuery(updatedQuery);
      fetchLogs(updatedQuery, false);
    },
    [query, fetchLogs],
  );

  // Set single date filter
  const setDateFilter = useCallback(
    (date: string | null) => {
      updateQuery({
        date: date || undefined,
        dateRange: undefined, // Clear date range when single date is set
      });
    },
    [updateQuery],
  );

  // Set date range filter
  const setDateRangeFilter = useCallback(
    (startDate: string, endDate: string) => {
      updateQuery({
        dateRange: `${startDate}-${endDate}`,
        date: undefined, // Clear single date when range is set
      });
    },
    [updateQuery],
  );

  // Export logs as text
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
      .map((log) => {
        return `[${log.date}] ${log.content}`;
      })
      .join("\n");

    const blob = new Blob([logText], { type: "text/plain" });
    const url = URL.createObjectURL(blob);
    const link = document.createElement("a");
    link.href = url;
    link.download = `bot-${botId}-logs-${new Date().toISOString().split("T")[0]}.txt`;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    URL.revokeObjectURL(url);

    toast({
      description: "Logs exported successfully",
    });
  }, [logs, botId, toast]);

  // Setup auto-refresh
  useEffect(() => {
    if (autoRefresh && refreshInterval > 0) {
      intervalRef.current = setInterval(refresh, refreshInterval);
      return () => {
        if (intervalRef.current) {
          clearInterval(intervalRef.current);
        }
      };
    }
  }, [autoRefresh, refreshInterval, refresh]);

  // Initial fetch
  useEffect(() => {
    fetchLogs();
  }, [fetchLogs]);

  // Cleanup on unmount
  useEffect(() => {
    return () => {
      if (intervalRef.current) {
        clearInterval(intervalRef.current);
      }
      if (abortControllerRef.current) {
        abortControllerRef.current.abort();
      }
    };
  }, []);

  return {
    logs,
    loading,
    error,
    hasMore,
    total,
    query,
    refresh,
    loadMore,
    updateQuery,
    setDateFilter,
    setDateRangeFilter,
    exportLogs,
  };
};
