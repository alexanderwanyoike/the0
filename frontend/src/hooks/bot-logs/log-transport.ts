"use client";

import { useCallback, useEffect, useRef } from "react";
import { authFetch } from "@/lib/auth-fetch";
import { buildLogsSearchParams } from "@/lib/log-utils";
import type { LogEntry, LogsQuery } from "@/types/logs";
import type { LogStream } from "./bot-log-stream";

// -- REST pages --

interface LogsResponse {
  data: LogEntry[];
  hasMore: boolean;
  nextCursor: string | null;
}

export interface LogsPage {
  /** In API order. */
  entries: LogEntry[];
  hasMore: boolean;
  /** `before` for the next older page; null when there is none. */
  nextCursor: string | null;
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
    entries: result.data,
    hasMore: result.hasMore,
    nextCursor: result.nextCursor,
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

// -- Live lines --

interface LiveLogsOptions {
  stream: LogStream;
  /** Subscribe to the stream (live mode with a bot and a user). */
  live: boolean;
  onEntries: (entries: LogEntry[]) => void;
  /** The stream has just opened: history read before then may miss lines
   *  written while it was not open. */
  onOpen: () => void;
  startPolling: () => void;
  stopPolling: () => void;
}

/**
 * Live entries from the bot's log stream while `live`. Whenever the stream
 * is down this view falls back to REST polling. Returns whether the stream
 * is connected.
 */
export function useLiveLogs({
  stream,
  live,
  onEntries,
  onOpen,
  startPolling,
  stopPolling,
}: LiveLogsOptions): boolean {
  const { status, subscribe } = stream;
  const previousStatusRef = useRef(status);

  useEffect(() => {
    if (!live) return;
    return subscribe(onEntries);
  }, [live, subscribe, onEntries]);

  useEffect(() => {
    const opened = status === "open" && previousStatusRef.current !== "open";
    previousStatusRef.current = status;
    if (!live) return;
    if (status === "down") {
      startPolling();
      return () => stopPolling();
    }
    if (status === "open") {
      stopPolling();
      if (opened) onOpen();
    }
  }, [live, status, onOpen, startPolling, stopPolling]);

  return live && status === "open";
}
