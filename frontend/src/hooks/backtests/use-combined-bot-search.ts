import { useState, useEffect, useCallback, useMemo } from "react";
import { useCustomBots } from "@/hooks/custom-bots/use-custom-bots";
import { useUserBots } from "@/hooks/use-user-bots";
import { BotSearchResult } from "@/types/backtest";
// Marketplace search functionality removed

export function useCombinedBotSearch() {
  const [query, setQuery] = useState("");
  const [suggestions, setSuggestions] = useState<BotSearchResult[]>([]);
  const [loading, setLoading] = useState(false);

  // Marketplace search removed - using empty arrays
  const marketplaceResults: BotSearchResult[] = [];
  const marketplaceLoading = false;
  const { bots: customBots, loading: customBotsLoading } = useCustomBots();
  const { userBots, loading: userBotsLoading } = useUserBots();

  const search = useCallback(async (searchQuery: string) => {
    setQuery(searchQuery);
    setLoading(true);

    // Marketplace search removed
  }, []);

  // Memoize the filtered results to prevent infinite loops
  const filteredResults = useMemo(() => {
    if (query.length < 2) {
      return [];
    }

    const combined: BotSearchResult[] = [];

    // Add marketplace suggestions
    marketplaceResults.forEach((suggestion) => {
      combined.push({
        id: suggestion.name,
        name: suggestion.name,
        resultType: "marketplace",
        approved: true,
      });
    });

    // Add custom bots that match the search query and are approved
    if (customBots && Array.isArray(customBots)) {
      customBots
        .filter(
          (bot) =>
            bot.name.toLowerCase().includes(query.toLowerCase()) &&
            bot.versions?.[0]?.status === "approved",
        )
        .forEach((bot) => {
          combined.push({
            id: bot.id,
            name: bot.name,
            resultType: "custom",
            approved: true,
          });
        });
    }

    // Add user bots that match the search query
    if (userBots && Array.isArray(userBots)) {
      userBots
        .filter((bot) =>
          bot.customBotName.toLowerCase().includes(query.toLowerCase()),
        )
        .forEach((bot) => {
          // Check if this bot is not already in the list from custom bots
          const existingBot = combined.find(
            (existing) => existing.name === bot.customBotName,
          );

          if (!existingBot) {
            combined.push({
              id: bot.id,
              name: bot.customBotName,
              resultType: "user",
              approved: true,
            });
          }
        });
    }

    return combined;
  }, [query, customBots, userBots]);

  // Update suggestions when filtered results change
  useEffect(() => {
    if (query.length < 2) {
      setSuggestions([]);
      setLoading(false);
      return;
    }

    const isLoading =
      marketplaceLoading || customBotsLoading || userBotsLoading;
    setLoading(isLoading);

    if (!isLoading) {
      setSuggestions(filteredResults);
      setLoading(false);
    }
  }, [
    filteredResults,
    marketplaceLoading,
    customBotsLoading,
    userBotsLoading,
    query,
  ]);

  const clearSuggestions = useCallback(() => {
    setQuery("");
    setSuggestions([]);
  }, []);

  return {
    search,
    suggestions,
    query,
    loading,
    clearSuggestions,
  };
}
