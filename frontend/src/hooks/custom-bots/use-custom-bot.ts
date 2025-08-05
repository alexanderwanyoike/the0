"use client";
import { useState, useEffect, useCallback } from "react";
import { useAuth } from "@/contexts/auth-context";
import { CustomBotVersion, CustomBotWithVersions } from "@/types/custom-bots";
import { CustomBotService } from "@/lib/api/custom-bots.service";

export interface UseCustomBotReturn {
  bot: CustomBotWithVersions | null;
  loading: boolean;
  error: string | null;
  refetch: () => Promise<void>;
}

export const useCustomBot = (botName: string): UseCustomBotReturn => {
  const [bot, setBot] = useState<CustomBotWithVersions | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const { user } = useAuth();

  const fetchCustomBot = useCallback(async () => {
    if (!user?.id || !botName) {
      setBot(null);
      setError("Missing user or bot name");
      setLoading(false);
      return;
    }

    try {
      setLoading(true);
      setError(null);

      const result = await CustomBotService.getCustomBot(botName);

      if (result.success) {
        setBot(result.data);
      } else {
        setBot(null);
        setError(result.error.message);

        // Handle specific error cases
        if (result.error.statusCode === 404) {
          setError("Bot not found");
        } else if (result.error.statusCode === 401) {
          console.warn("User unauthorized for custom bot");
        } else if (result.error.statusCode === 403) {
          console.warn("User forbidden from accessing custom bot");
        }
      }
    } catch (err: any) {
      console.error("Error fetching custom bot:", err);
      setError(err.message || "Failed to load bot details");
      setBot(null);
    } finally {
      setLoading(false);
    }
  }, [user?.id, botName]);

  useEffect(() => {
    fetchCustomBot();
  }, [fetchCustomBot]);

  return {
    bot,
    loading,
    error,
    refetch: fetchCustomBot, // Provide manual refresh capability
  };
};
