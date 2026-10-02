"use client";

import { useCallback, useEffect, useRef, useState } from "react";
import { authFetch } from "@/lib/auth-fetch";
import { buildLogsSearchParams, expandLogEntries } from "@/lib/log-utils";
import { validateSSEAuth } from "@/lib/sse/sse-auth";
import { readSSEEvents } from "@/lib/sse/sse-stream-reader";
import type { LogEntry, LogsQuery } from "@/types/logs";

// -- REST pages --

interface LogsResponse {
  data: LogEntry[];
  total: number;
  hasMore: boolean;
}

export interface LogsPage {
  /** One entry per non-empty line, in API order. */
  entries: LogEntry[];
  total: number;
  hasMore: boolean;
}

export async function fetchLogsPage(
  botId: string,
  query: LogsQuery,
  signal: AbortSignal,
): Promise<LogsPage> {
  const response = await authFetch(
    `/api/logs/${encodeURIComponent(botId)}?${buildLogsSearchParams(query).toString()}`,
    { signal },
  );

  if (!response.ok) {
    throw new Error(`Failed to fetch logs: ${response.statusText}`);
  }

  const result: LogsResponse = await response.json();
  return {
    entries: expandLogEntries(result.data),
    total: result.total,
    hasMore: result.hasMore,
  };
}

// -- Polling --

/**
 * Single owner of the REST polling interval. The interval and tick are read
 * through refs so a running interval always calls the latest fetch and a
 * cadence change only applies on the next (re)start.
 */
export function useLogPolling(refreshInterval: number, onTick: () => void) {
  const pollingRef = useRef<NodeJS.Timeout | null>(null);
  const refreshIntervalRef = useRef(refreshInterval);
  refreshIntervalRef.current = refreshInterval;
  const onTickRef = useRef(onTick);
  onTickRef.current = onTick;

  const stopPolling = useCallback(() => {
    if (pollingRef.current) {
      clearInterval(pollingRef.current);
      pollingRef.current = null;
    }
  }, []);

  const startPolling = useCallback(() => {
    stopPolling();
    if (refreshIntervalRef.current > 0) {
      pollingRef.current = setInterval(
        () => onTickRef.current(),
        refreshIntervalRef.current,
      );
    }
  }, [stopPolling]);

  const isPolling = useCallback(() => pollingRef.current !== null, []);

  return { startPolling, stopPolling, isPolling };
}

interface AutoRefreshOptions {
  botId: string;
  user: unknown;
  streaming: boolean;
  autoRefresh: boolean;
  refreshInterval: number;
  startPolling: () => void;
  stopPolling: () => void;
  isPolling: () => boolean;
}

/** Non-streaming autoRefresh polling, plus cadence changes for a running
 *  SSE-failure fallback. */
export function useAutoRefreshPolling({
  botId,
  user,
  streaming,
  autoRefresh,
  refreshInterval,
  startPolling,
  stopPolling,
  isPolling,
}: AutoRefreshOptions) {
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
    // by the stream effect). If it's running, restart it at the new cadence.
    if (isPolling()) {
      startPolling();
    }
  }, [
    botId,
    user,
    streaming,
    autoRefresh,
    refreshInterval,
    startPolling,
    stopPolling,
    isPolling,
  ]);
}

// -- SSE live stream --

interface LiveLogStreamOptions {
  botId: string;
  user: unknown;
  liveMode: boolean;
  /** Changing this tears the stream down and reconnects. */
  reconnectKey: number;
  onUpdate: (data: string) => void;
  startPolling: () => void;
  stopPolling: () => void;
}

/**
 * SSE connection lifecycle for live mode. Returns whether the stream is
 * currently connected; any stream failure falls back to REST polling.
 */
export function useLiveLogStream({
  botId,
  user,
  liveMode,
  reconnectKey,
  onUpdate,
  startPolling,
  stopPolling,
}: LiveLogStreamOptions): boolean {
  const [connected, setConnected] = useState(false);

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

        await readSSEEvents(response.body, (eventType, data) => {
          // "history" events are intentionally ignored - REST is the
          // single source of history (see useBotLogs module doc).
          if (eventType === "update") onUpdate(data);
        });

        // Stream ended cleanly (server restart, proxy recycle, access
        // denied). Live updates are gone either way, so unless this was our
        // own teardown, fall back to polling to keep data flowing.
        setConnected(false);
        if (!controller.signal.aborted) {
          startPolling();
        }
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
  }, [
    botId,
    user,
    liveMode,
    reconnectKey,
    onUpdate,
    startPolling,
    stopPolling,
  ]);

  return connected;
}
