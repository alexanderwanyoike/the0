"use client";

import { useState, useEffect, useCallback, useRef } from "react";
import { useToast } from "@/hooks/use-toast";
import { LogEntry } from "@/components/bot/console-interface";
import { expandLogEntries } from "@/lib/log-utils";
import { useAuth } from "@/contexts/auth-context";
import { authFetch } from "@/lib/auth-fetch";

interface LogsQuery {
  date?: string;
  dateRange?: string;
  limit?: number;
  offset?: number;
  type?: string;
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

const MAX_LOG_ENTRIES = 2000;

export const useBotLogs = ({
  botId,
  autoRefresh = false,
  refreshInterval = 30000, // 30 seconds
  initialQuery = { limit: 2000, offset: 0 },
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
  // Ref to always hold the latest fetchLogs so the polling interval
  // never calls a stale closure after query/filter changes.
  const fetchLogsRef = useRef<typeof fetchLogs>(null!);

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

        const response = await authFetch(
          `/api/logs/${encodeURIComponent(botId)}?${searchParams.toString()}`,
          {
            signal: controller.signal,
          },
        );

        if (!response.ok) {
          throw new Error(`Failed to fetch logs: ${response.statusText}`);
        }

        const result: LogsResponse = await response.json();

        const expandedLogs = expandLogEntries(result.data);

        if (append) {
          setLogs((prev) => {
            const combined = [...prev, ...expandedLogs];
            return combined.length > MAX_LOG_ENTRIES
              ? combined.slice(combined.length - MAX_LOG_ENTRIES)
              : combined;
          });
        } else {
          setLogs(
            expandedLogs.length > MAX_LOG_ENTRIES
              ? expandedLogs.slice(expandedLogs.length - MAX_LOG_ENTRIES)
              : expandedLogs,
          );
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
        if (!controller.signal.aborted) {
          setLoading(false);
        }
        abortControllerRef.current = null;
      }
    },
    [botId, query, toast, user],
  );
  fetchLogsRef.current = fetchLogs;

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
      // Use -- separator for ISO datetime ranges, - for YYYYMMDD date ranges
      const separator = startDate.includes("T") ? "--" : "-";
      updateQuery({
        dateRange: `${startDate}${separator}${endDate}`,
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
    const a = document.createElement("a");
    a.href = url;
    a.download = `bot-${botId}-logs-${new Date().toISOString().split("T")[0]}.txt`;
    a.click();
    URL.revokeObjectURL(url);
  }, [logs, botId, toast]);

  // Lifecycle: reset state, fetch, and set up polling when botId changes
  useEffect(() => {
    if (!botId || !user) {
      setLoading(false);
      return;
    }

    setLogs([]);
    setError(null);
    setQuery(initialQuery);
    setHasMore(false);
    setTotal(0);
    setLoading(true);

    fetchLogs(initialQuery);

    let interval: NodeJS.Timeout | null = null;
    if (autoRefresh && refreshInterval > 0) {
      // Use the ref so the interval always calls the latest fetchLogs
      // without needing to recreate the timer on every query change.
      interval = setInterval(() => fetchLogsRef.current(), refreshInterval);
    }

    return () => {
      if (interval) clearInterval(interval);
      abortControllerRef.current?.abort();
    };
  }, [botId, user, autoRefresh, refreshInterval]);

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
