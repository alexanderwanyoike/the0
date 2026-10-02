"use client";

import { useEffect, useMemo, useRef, useState } from "react";
import {
  IntervalValue,
  LIVE_INTERVAL,
  LATEST_INTERVAL,
} from "@/components/bot/interval-picker";
import { useBotLogs, DEFAULT_LOOKBACK_DAYS } from "@/hooks/use-bot-logs";
import { shouldUseLogStreaming, isScheduledBot } from "@/lib/bot-utils";
import type { Bot } from "@/lib/api/api-client";

// All loaded bots stream over SSE; polling only exists as an automatic
// fallback inside useBotLogs at this fixed cadence.
const FALLBACK_REFRESH_INTERVAL = 30000;

/**
 * The interval picker, console sort order and log stream shared by the
 * dashboard and console of one bot. Streaming-only fields are undefined for
 * bots that do not stream so the console hides their controls.
 */
export function useBotDetailLogs(botId: string, bot: Bot | null) {
  const [sortOrder, setSortOrder] = useState<"asc" | "desc">("desc");

  const useStreaming = shouldUseLogStreaming(bot);
  const scheduled = isScheduledBot(bot);

  // Realtime bots default to live mode; scheduled bots default to latest
  // mode (their last run is rarely inside any fixed window).
  // Note: bot is null at mount, so the flags resolve once it loads; the
  // useEffect below corrects the interval then.
  const [interval, setInterval_] = useState<IntervalValue>(
    scheduled ? LATEST_INTERVAL : LIVE_INTERVAL,
  );
  const streamingInitialized = useRef(false);

  useEffect(() => {
    streamingInitialized.current = false;
  }, [botId]);

  useEffect(() => {
    if (!streamingInitialized.current && bot) {
      streamingInitialized.current = true;
      setInterval_(scheduled ? LATEST_INTERVAL : LIVE_INTERVAL);
    }
  }, [scheduled, bot]);

  // While bot is null the hook stays inert; once the bot loads, botId and
  // streaming resolve in the same render, so the hook mounts its transport
  // exactly once (no polling-then-streaming flip).
  const hookBotId = bot !== null ? botId : "";

  // Stable identity: an inline literal here defeats React.memo on
  // BotDashboardLoader and remounts the dashboard on every panel render
  const dashboardDateRange = useMemo(
    () =>
      interval.type === "range"
        ? { start: interval.start, end: interval.end }
        : undefined,
    [interval],
  );

  const logsHook = useBotLogs({
    botId: hookBotId,
    streaming: useStreaming,
    autoRefresh: false,
    refreshInterval: FALLBACK_REFRESH_INTERVAL,
    // Scheduled bots start in latest mode: newest lines across the lookback
    // window instead of today's (possibly empty) file. SSE appends land on
    // top of that history when a run executes. Realtime bots keep the
    // hook's default query (today, streamed).
    initialQuery: scheduled
      ? {
          limit: 1000,
          offset: 0,
          sort: "desc" as const,
          lookbackDays: DEFAULT_LOOKBACK_DAYS,
        }
      : undefined,
  });

  const handleIntervalChange = (val: IntervalValue) => {
    setInterval_(val);
    if (val.type === "live") {
      // Clear date filter so SSE streams all logs
      logsHook.setDateFilter(null);
    } else if (val.type === "latest") {
      logsHook.setLatestFilter();
    } else {
      logsHook.setDateRangeFilter(val.start, val.end);
    }
  };

  const handleSortChange = (s: "asc" | "desc") => {
    setSortOrder(s);
    logsHook.updateQuery({ sort: s });
  };

  return {
    useStreaming,
    scheduled,
    interval,
    dashboardDateRange,
    handleIntervalChange,
    sortOrder,
    handleSortChange,
    logs: logsHook.logs,
    logsLoading: logsHook.loading,
    refreshLogs: logsHook.refresh,
    setDateFilter: logsHook.setDateFilter,
    setDateRangeFilter: logsHook.setDateRangeFilter,
    exportLogs: logsHook.exportLogs,
    connected: useStreaming ? logsHook.connected : undefined,
    lastUpdate: useStreaming ? logsHook.lastUpdate : undefined,
    hasEarlierLogs: useStreaming ? logsHook.hasEarlierLogs : undefined,
    loadingEarlier: useStreaming ? logsHook.loadingEarlier : undefined,
    loadEarlierLogs: useStreaming ? logsHook.loadEarlierLogs : undefined,
    // Pagination applies to REST results (scheduled bots or date-filtered views)
    hasMoreLogs: logsHook.hasMore || undefined,
    loadMoreLogs: logsHook.hasMore ? logsHook.loadMore : undefined,
    loadingMore: logsHook.loadingMore,
  };
}
