"use client";

import { useEffect, useState } from "react";
import { authFetch } from "@/lib/auth-fetch";
import { validateSSEAuth } from "@/lib/sse/sse-auth";
import { readSSEEvents } from "@/lib/sse/sse-stream-reader";

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
