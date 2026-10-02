"use client";

import { useCallback, useEffect, useRef } from "react";

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
