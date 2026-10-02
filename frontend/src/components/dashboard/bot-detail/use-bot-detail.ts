"use client";

import { useEffect, useMemo, useRef, useState } from "react";
import { useRouter } from "next/navigation";
import { useAuth } from "@/contexts/auth-context";
import { useToast } from "@/hooks/use-toast";
import { useDashboardBots } from "@/contexts/dashboard-bots-context";
import { BotService, Bot as ApiBotType } from "@/lib/api/api-client";
import { getErrorMessage } from "@/lib/axios";
import { maskSensitiveConfig } from "@/lib/mask-config";
import {
  IntervalValue,
  LIVE_INTERVAL,
  LATEST_INTERVAL,
} from "@/components/bot/interval-picker";
import { useBotLogs, DEFAULT_LOOKBACK_DAYS } from "@/hooks/use-bot-logs";
import { shouldUseLogStreaming, isScheduledBot } from "@/lib/bot-utils";

export interface DetailBot extends ApiBotType {
  userId?: string;
  user_id?: string;
  name?: string;
  version?: string;
  customBotId?: string;
}

async function loadOwnedBot(botId: string, userId: string): Promise<DetailBot> {
  const result = await BotService.getBot(botId);
  if (!result.success) {
    throw new Error(result.error.message || "Failed to fetch bot");
  }
  const botData = result.data as DetailBot;
  const botUserId = botData.userId || botData.user_id;
  if (botUserId !== userId) throw new Error("Unauthorized access");
  return botData;
}

const isGoneOrForbidden = (error: unknown) =>
  error instanceof Error &&
  (error.message === "Bot not found" ||
    error.message === "Unauthorized access");

/** Loads the bot for the signed-in owner and exposes its detail actions. */
export function useOwnedBot(botId: string) {
  const [bot, setBot] = useState<DetailBot | null>(null);
  const [loading, setLoading] = useState(true);
  const [isDeleting, setIsDeleting] = useState(false);
  const [isUpdatingEnabled, setIsUpdatingEnabled] = useState(false);
  const router = useRouter();
  const { toast } = useToast();
  const { user } = useAuth();
  const { removeBotFromList, bots } = useDashboardBots();

  useEffect(() => {
    const fetchBot = async () => {
      if (!botId || !user) return;
      setLoading(true);
      try {
        setBot(await loadOwnedBot(botId, user.id));
      } catch (error) {
        console.error("Error fetching bot:", error);
        toast({
          title: "Error",
          description: `Failed to load bot: ${error instanceof Error ? error.message : "Unknown error"}`,
          variant: "destructive",
        });
        if (isGoneOrForbidden(error)) {
          setTimeout(() => router.push("/dashboard"), 2000);
        }
      } finally {
        setLoading(false);
      }
    };
    fetchBot();
  }, [botId, user, router, toast]);

  // Memoized so the deep copy does not run on every render
  const maskedConfig = useMemo(
    () => (bot ? maskSensitiveConfig(bot.config) : null),
    [bot?.config],
  );

  const copyConfig = () => {
    if (!bot) return;
    navigator.clipboard.writeText(
      JSON.stringify(maskSensitiveConfig(bot.config), null, 2),
    );
    toast({
      description: "Bot configuration copied to clipboard",
      duration: 2000,
    });
  };

  const deleteBot = async () => {
    if (!bot) return;
    setIsDeleting(true);
    try {
      const result = await BotService.deleteBot(botId);
      if (!result.success) {
        throw new Error(result.error.message || "Failed to delete bot");
      }
      toast({ description: "Bot deleted successfully", duration: 2000 });
      removeBotFromList(botId);
      const remaining = bots.filter((b) => b.id !== botId);
      router.replace(
        remaining.length > 0 ? `/dashboard/${remaining[0].id}` : "/dashboard",
      );
    } catch (error) {
      console.error("Error deleting bot:", error);
      toast({
        title: "Delete Failed",
        description:
          error instanceof Error ? error.message : "Failed to delete bot",
        variant: "destructive",
      });
      setIsDeleting(false);
    }
  };

  const toggleEnabled = async (enabled: boolean) => {
    if (!bot) return;
    setIsUpdatingEnabled(true);
    try {
      const updatedConfig = { ...bot.config, enabled };
      const result = await BotService.updateBot(botId, updatedConfig);
      if (!result.success) throw new Error(result.error.message);
      setBot((prev) => (prev ? { ...prev, config: updatedConfig } : null));
      toast({
        description: `Bot ${enabled ? "enabled" : "disabled"} successfully. It may take a few moments to reflect the change.`,
        duration: 2000,
      });
    } catch (error) {
      console.error("Error updating bot enabled status:", error);
      toast({
        title: "Update Failed",
        description: getErrorMessage(error),
        variant: "destructive",
      });
    } finally {
      setIsUpdatingEnabled(false);
    }
  };

  return {
    bot,
    loading,
    maskedConfig,
    isDeleting,
    isUpdatingEnabled,
    copyConfig,
    deleteBot,
    toggleEnabled,
  };
}

// All loaded bots stream over SSE; polling only exists as an automatic
// fallback inside useBotLogs at this fixed cadence.
const FALLBACK_REFRESH_INTERVAL = 30000;

/**
 * The interval picker, console sort order and log stream shared by the
 * dashboard and console of one bot. Streaming-only fields are undefined for
 * bots that do not stream so the console hides their controls.
 */
export function useBotDetailLogs(botId: string, bot: DetailBot | null) {
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
