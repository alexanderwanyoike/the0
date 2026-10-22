"use client";

import React, {
  createContext,
  ReactNode,
  useCallback,
  useContext,
  useEffect,
  useMemo,
  useRef,
  useState,
} from "react";
import { useAuth } from "@/contexts/auth-context";
import { authFetch } from "@/lib/auth-fetch";
import { parseLiveUpdate } from "@/lib/log-utils";
import { validateSSEAuth } from "@/lib/sse/sse-auth";
import { readSSEEvents } from "@/lib/sse/sse-stream-reader";
import type { LogEntry } from "@/types/logs";

/**
 * "off" while no view is subscribed; "down" once the stream failed or ended,
 * until reconnect().
 */
export type LogStreamStatus = "off" | "connecting" | "open" | "down";

type LiveListener = (entries: LogEntry[]) => void;

export interface LogStream {
  status: LogStreamStatus;
  /** Hands the listener the entries of each live chunk. The connection is
   *  open only while at least one listener is subscribed. */
  subscribe: (listener: LiveListener) => () => void;
  reconnect: () => void;
}

/**
 * The SSE connection behind a log stream. Views read history over REST, so
 * the stream asks the api to skip its history read.
 */
function useLogStreamConnection(botId: string): LogStream {
  const { user } = useAuth();
  const listenersRef = useRef(new Set<LiveListener>());
  const [subscribers, setSubscribers] = useState(0);
  const [status, setStatus] = useState<LogStreamStatus>("off");
  const [nonce, setNonce] = useState(0);
  const active = subscribers > 0;

  const subscribe = useCallback((listener: LiveListener) => {
    listenersRef.current.add(listener);
    setSubscribers((count) => count + 1);
    return () => {
      listenersRef.current.delete(listener);
      setSubscribers((count) => count - 1);
    };
  }, []);

  const reconnect = useCallback(() => setNonce((n) => n + 1), []);

  useEffect(() => {
    if (!active || !botId || !user) {
      setStatus("off");
      return;
    }
    if (!validateSSEAuth().success) {
      setStatus("down");
      return;
    }

    const controller = new AbortController();
    setStatus("connecting");

    authFetch(`/api/logs/${encodeURIComponent(botId)}/stream?history=false`, {
      signal: controller.signal,
    })
      .then(async (response) => {
        if (!response.ok || !response.body) {
          throw new Error(`Stream response: ${response.status}`);
        }
        setStatus("open");

        await readSSEEvents(response.body, (eventType, data) => {
          if (eventType !== "update") return;
          let entries: LogEntry[];
          try {
            entries = parseLiveUpdate(data);
          } catch (err) {
            console.error("Failed to parse update SSE event:", err);
            return;
          }
          listenersRef.current.forEach((listener) => listener(entries));
        });

        // Ended cleanly (server restart, proxy recycle, access denied): live
        // lines are gone either way unless this was our own teardown.
        if (!controller.signal.aborted) setStatus("down");
      })
      .catch((err) => {
        // signal.aborted first: browsers may reject an aborted body read
        // with TypeError, not AbortError.
        if (controller.signal.aborted || err?.name === "AbortError") return;
        setStatus("down");
      });

    return () => controller.abort();
  }, [botId, user, active, nonce]);

  return useMemo(
    () => ({ status, subscribe, reconnect }),
    [status, subscribe, reconnect],
  );
}

const BotLogStreamContext = createContext<{
  botId: string;
  stream: LogStream;
} | null>(null);

/**
 * One log stream for every view of a bot page, so the console and the
 * dashboard share a single connection.
 */
export function BotLogStreamProvider({
  botId,
  children,
}: {
  botId: string;
  children: ReactNode;
}) {
  const stream = useLogStreamConnection(botId);
  const value = useMemo(() => ({ botId, stream }), [botId, stream]);
  return (
    <BotLogStreamContext.Provider value={value}>
      {children}
    </BotLogStreamContext.Provider>
  );
}

/** The bot page's stream when this view is on that bot's page, else null. */
export function useSharedBotLogStream(botId: string): LogStream | null {
  const shared = useContext(BotLogStreamContext);
  return shared && botId && shared.botId === botId ? shared.stream : null;
}

/** The bot page's stream, or one of this view's own outside a bot page. */
export function useBotLogStream(botId: string): LogStream {
  const shared = useSharedBotLogStream(botId);
  const own = useLogStreamConnection(shared ? "" : botId);
  return shared ?? own;
}
