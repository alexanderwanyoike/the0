import { useState, useCallback, useMemo } from "react";
import { Bot } from "@/lib/api/api-client";

export type BotTypeFilter = "all" | "scheduled" | "realtime";
export type BotStatusFilter = "all" | "enabled" | "disabled";

export function useBotFilters() {
  const [search, setSearch] = useState("");
  const [type, setTypeState] = useState<BotTypeFilter>("all");
  const [status, setStatusState] = useState<BotStatusFilter>("all");

  const setType = useCallback((value: BotTypeFilter) => {
    setTypeState((prev) => (prev === value ? "all" : value));
  }, []);

  const setStatus = useCallback((value: BotStatusFilter) => {
    setStatusState((prev) => (prev === value ? "all" : value));
  }, []);

  const hasActiveFilters =
    type !== "all" || status !== "all" || search.trim() !== "";
  const activeCount = (type !== "all" ? 1 : 0) + (status !== "all" ? 1 : 0);

  const filterBots = useCallback(
    (bots: Bot[]) => {
      return bots.filter((bot) => {
        const config = bot.config as Record<string, any>;

        // Text search
        const trimmed = search.trim();
        if (trimmed) {
          const q = trimmed.toLowerCase();
          const name = (config?.name || bot.id).toLowerCase();
          const symbol = (config?.symbol || "").toLowerCase();
          if (!name.includes(q) && !symbol.includes(q)) return false;
        }

        // Type filter
        if (type !== "all") {
          const hasSchedule = !!config?.schedule;
          if (type === "scheduled" && !hasSchedule) return false;
          if (type === "realtime" && hasSchedule) return false;
        }

        // Status filter
        if (status !== "all") {
          const enabled = config?.enabled ?? true;
          if (status === "enabled" && !enabled) return false;
          if (status === "disabled" && enabled) return false;
        }

        return true;
      });
    },
    [search, type, status],
  );

  return {
    search,
    setSearch,
    type,
    setType,
    status,
    setStatus,
    hasActiveFilters,
    activeCount,
    filterBots,
  };
}
