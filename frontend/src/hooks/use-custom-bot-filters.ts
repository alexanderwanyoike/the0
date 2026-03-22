import { useState, useCallback } from "react";
import { CustomBotWithVersions } from "@/types/custom-bots";

export type CustomBotTypeFilter = "all" | "scheduled" | "realtime";

export function useCustomBotFilters() {
  const [search, setSearch] = useState("");
  const [type, setTypeState] = useState<CustomBotTypeFilter>("all");

  const setType = useCallback((value: CustomBotTypeFilter) => {
    setTypeState((prev) => (prev === value ? "all" : value));
  }, []);

  const hasActiveFilters = type !== "all" || search.trim() !== "";
  const activeCount = type !== "all" ? 1 : 0;

  const filterBots = useCallback(
    (bots: CustomBotWithVersions[]) => {
      return bots.filter((bot) => {
        const config = bot.versions[0]?.config;

        // Text search
        const trimmed = search.trim();
        if (trimmed) {
          const q = trimmed.toLowerCase();
          const name = bot.name.toLowerCase();
          const desc = (config?.description || "").toLowerCase();
          if (!name.includes(q) && !desc.includes(q)) return false;
        }

        // Type filter
        if (type !== "all") {
          const botType = (config?.type || "").toLowerCase();
          if (
            type === "realtime" &&
            botType !== "real-time" &&
            botType !== "realtime"
          )
            return false;
          if (type === "scheduled" && botType !== "scheduled") return false;
        }

        return true;
      });
    },
    [search, type],
  );

  return {
    search,
    setSearch,
    type,
    setType,
    hasActiveFilters,
    activeCount,
    filterBots,
  };
}
