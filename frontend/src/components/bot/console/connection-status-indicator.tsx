"use client";

import React from "react";
import { cn } from "@/lib/utils";

const STATUS = {
  connected: {
    dot: "bg-green-500",
    text: "text-green-600 dark:text-green-400",
    label: "Live",
  },
  disconnected: {
    dot: "bg-gray-400",
    text: "text-gray-500",
    label: "Polling",
  },
  connecting: {
    dot: "bg-yellow-500 animate-pulse",
    text: "text-yellow-600 dark:text-yellow-400",
    label: "Connecting...",
  },
} as const;

/**
 * Connection status indicator for the console toolbar.
 *
 * Three states derived from the useBotLogs hook (streaming mode):
 * - connected=true              -> Green dot + "Live"        (SSE active)
 * - connected=false, lastUpdate -> Gray dot  + "Polling"     (fell back to REST)
 * - connected=false, no update  -> Yellow dot + "Connecting" (SSE hasn't connected yet)
 *
 * When `connected` is undefined the indicator is hidden (backwards compatible).
 */
export const ConnectionStatusIndicator: React.FC<{
  connected?: boolean;
  lastUpdate?: Date | null;
}> = ({ connected, lastUpdate }) => {
  if (connected === undefined) return null;

  const statusLabel = connected
    ? "connected"
    : lastUpdate
      ? "disconnected"
      : "connecting";
  const status = STATUS[statusLabel];

  return (
    <span
      className="flex items-center gap-1 text-xs"
      role="status"
      aria-live="polite"
      aria-atomic="true"
      aria-label={`Connection status: ${statusLabel}`}
    >
      <span
        aria-hidden="true"
        className={cn("h-1.5 w-1.5 rounded-full", status.dot)}
      />
      <span className={cn("text-[10px]", status.text)}>{status.label}</span>
    </span>
  );
};
