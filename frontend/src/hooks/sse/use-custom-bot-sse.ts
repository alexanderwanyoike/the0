/**
 * Specific Custom Bot SSE Hook
 *
 * Replaces the Firebase real-time pattern from use-custom-bot.ts with
 * Server-Sent Events for real-time updates of individual bot development.
 * Maintains the same interface while providing faster updates for development workflow.
 *
 * Features:
 * - Real-time updates via SSE connection (2-second intervals)
 * - Fallback to manual refresh when SSE unavailable
 * - Consistent interface with existing hook
 * - Bot-specific error handling (not found, unauthorized, etc.)
 * - Connection state monitoring for development feedback
 */

"use client";

import { useCallback, useEffect, useState } from "react";
import { useAuth } from "@/contexts/auth-context";
import { CustomBotWithVersions } from "@/types/custom-bots";
import { CustomBotService } from "@/lib/api/custom-bots.service";
import { UseCustomBotReturn } from "@/hooks/custom-bots/use-custom-bot";
import { useSSEClient } from "./use-sse-client";

export interface UseCustomBotSSEReturn {
  bot: CustomBotWithVersions | null;
  loading: boolean;
  error: string | null;
  refetch: () => Promise<void>;
  connected: boolean; // Additional SSE connection state
  lastUpdate: Date | null; // When last update was received
}

export const useCustomBotSSE = (botName: string): UseCustomBotSSEReturn => {
  const [bot, setBot] = useState<CustomBotWithVersions | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [initialLoadComplete, setInitialLoadComplete] = useState(false);
  const { user } = useAuth();

  // Handle SSE messages
  const handleSSEMessage = useCallback(
    (data: any) => {
      console.log(`Custom bot SSE update received for ${botName}:`, data);

      // The data might be wrapped in a response format, extract the actual bot data
      const botData = data.data || data;

      if (botData && (botData.name === botName || !botData.name)) {
        setBot(botData);
        setError(null);

        if (!initialLoadComplete) {
          setLoading(false);
          setInitialLoadComplete(true);
        }
      }
    },
    [botName, initialLoadComplete],
  );

  // Handle SSE connection errors
  const handleSSEError = useCallback(
    (error: Event) => {
      console.warn(`Custom bot SSE connection error for ${botName}:`, error);

      if (!initialLoadComplete) {
        // If we haven't loaded initial data and SSE fails, fall back to manual fetch
        console.log(
          `SSE failed during initial load for ${botName}, falling back to manual fetch`,
        );
        fetchCustomBot();
      } else {
        // If we already have data, just show a non-intrusive connection error
        setError("Real-time updates temporarily unavailable");
      }
    },
    [botName, initialLoadComplete],
  );

  // Handle SSE connection established
  const handleSSEConnected = useCallback(() => {
    console.log(`Custom bot SSE connected for ${botName}`);
    if (error === "Real-time updates temporarily unavailable") {
      setError(null);
    }
  }, [botName, error]);

  // Handle SSE connection lost
  const handleSSEDisconnected = useCallback(() => {
    console.log(`Custom bot SSE disconnected for ${botName}`);
  }, [botName]);

  // Set up SSE connection (only if we have a valid bot name)
  const sseState = useSSEClient<any>(
    `/api/custom-bots/${encodeURIComponent(botName)}/stream`,
    {
      onMessage: handleSSEMessage,
      onError: handleSSEError,
      onConnected: handleSSEConnected,
      onDisconnected: handleSSEDisconnected,
      enabled: !!user?.id && !!botName,
      autoConnect: true,
      maxReconnectAttempts: 5,
      reconnectDelay: 2000, // Faster reconnection for development workflow
    },
  );

  // Manual fetch function (fallback and refresh capability)
  const fetchCustomBot = useCallback(async () => {
    if (!user?.id || !botName) {
      setBot(null);
      setError(!user?.id ? "Authentication required" : "Bot name is required");
      setLoading(false);
      setInitialLoadComplete(true);
      return;
    }

    try {
      setLoading(true);
      setError(null);

      const result = await CustomBotService.getCustomBot(botName);

      if (result.success) {
        setBot(result.data);
        setInitialLoadComplete(true);
      } else {
        setBot(null);
        setError(result.error.message);

        // Handle specific error cases
        if (result.error.statusCode === 404) {
          setError(`Bot '${botName}' not found`);
        } else if (result.error.statusCode === 401) {
          console.warn("User unauthorized for custom bot");
          setError("Unauthorized access to bot");
        } else if (result.error.statusCode === 403) {
          console.warn("User forbidden from accessing custom bot");
          setError("Access forbidden for this bot");
        }
      }
    } catch (err: any) {
      console.error(`Error fetching custom bot ${botName}:`, err);
      setError(err.message || "Failed to load bot details");
      setBot(null);
    } finally {
      setLoading(false);
    }
  }, [user?.id, botName]);

  // Initial data fetch if SSE is not connected quickly enough
  useEffect(() => {
    if (!user?.id || !botName) {
      setBot(null);
      setError(!user?.id ? "Authentication required" : "Bot name is required");
      setLoading(false);
      setInitialLoadComplete(true);
      return;
    }

    // If SSE doesn't connect and provide data within 4 seconds, fall back to manual fetch
    // Shorter timeout for individual bot since development needs faster feedback
    const fallbackTimeout = setTimeout(() => {
      if (!initialLoadComplete && !sseState.connected) {
        console.log(
          `SSE connection taking too long for ${botName}, falling back to manual fetch`,
        );
        fetchCustomBot();
      }
    }, 4000);

    return () => clearTimeout(fallbackTimeout);
  }, [
    user?.id,
    botName,
    initialLoadComplete,
    sseState.connected,
    fetchCustomBot,
  ]);

  // Manual refetch function for user-initiated refresh
  const refetch = useCallback(async () => {
    console.log(`Manual refetch requested for custom bot ${botName}`);
    await fetchCustomBot();
  }, [fetchCustomBot]);

  return {
    bot,
    loading,
    error,
    refetch,
    connected: sseState.connected,
    lastUpdate: sseState.lastUpdate,
  };
};

/**
 * Compatibility hook that maintains the exact same interface as the original
 * useCustomBot hook for seamless replacement in existing components.
 */
export const useCustomBot = (botName: string): UseCustomBotReturn => {
  const sseResult = useCustomBotSSE(botName);

  return {
    bot: sseResult.bot,
    loading: sseResult.loading,
    error: sseResult.error,
    refetch: sseResult.refetch,
  };
};

// Re-export the return type for compatibility
export type { UseCustomBotReturn } from "../custom-bots/use-custom-bot";
