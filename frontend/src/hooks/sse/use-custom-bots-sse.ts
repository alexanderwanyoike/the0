/**
 * Custom Bots SSE Hook
 *
 * Replaces the Firebase real-time pattern from use-custom-bots.ts with
 * Server-Sent Events for real-time updates. Maintains the same interface
 * and behavior while providing improved scalability and backend integration.
 *
 * Features:
 * - Real-time updates via SSE connection
 * - Fallback to manual refresh when SSE unavailable
 * - Consistent interface with existing hook
 * - Proper data transformation and error handling
 * - Connection state monitoring
 */

import { useCallback, useEffect, useState } from "react";
import { useAuth } from "@/contexts/auth-context";
import { CustomBotWithVersions } from "@/types/custom-bots";
import { CustomBotService } from "@/lib/api/custom-bots.service";
import { UseCustomBotsReturn } from "@/hooks/custom-bots/use-custom-bots";
import { useSSEClient } from "./use-sse-client";

export interface UseCustomBotsSSEReturn {
  bots: CustomBotWithVersions[];
  loading: boolean;
  error: string | null;
  refetch: () => Promise<void>;
  connected: boolean; // Additional SSE connection state
  lastUpdate: Date | null; // When last update was received
}

export const useCustomBotsSSE = (): UseCustomBotsSSEReturn => {
  const [bots, setBots] = useState<CustomBotWithVersions[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [initialLoadComplete, setInitialLoadComplete] = useState(false);
  const { user } = useAuth();

  // Handle SSE messages
  const handleSSEMessage = useCallback(
    (data: CustomBotWithVersions[]) => {
      console.log("Custom bots SSE update received:", data.length, "bots");
      setBots(data);
      setError(null);

      if (!initialLoadComplete) {
        setLoading(false);
        setInitialLoadComplete(true);
      }
    },
    [initialLoadComplete],
  );

  // Handle SSE connection errors
  const handleSSEError = useCallback(
    (error: Event) => {
      console.warn("Custom bots SSE connection error:", error);

      if (!initialLoadComplete) {
        // If we haven't loaded initial data and SSE fails, fall back to manual fetch
        console.log(
          "SSE failed during initial load, falling back to manual fetch",
        );
        fetchCustomBots();
      } else {
        // If we already have data, just show a non-intrusive connection error
        setError("Real-time updates temporarily unavailable");
      }
    },
    [initialLoadComplete],
  );

  // Handle SSE connection established
  const handleSSEConnected = useCallback(() => {
    console.log("Custom bots SSE connected");
    if (error === "Real-time updates temporarily unavailable") {
      setError(null);
    }
  }, [error]);

  // Handle SSE connection lost
  const handleSSEDisconnected = useCallback(() => {
    console.log("Custom bots SSE disconnected");
  }, []);

  // Set up SSE connection
  const sseState = useSSEClient<CustomBotWithVersions[]>(
    "/api/custom-bots/stream",
    {
      onMessage: handleSSEMessage,
      onError: handleSSEError,
      onConnected: handleSSEConnected,
      onDisconnected: handleSSEDisconnected,
      enabled: !!user?.id,
      autoConnect: true,
      maxReconnectAttempts: 5,
      reconnectDelay: 3000,
    },
  );

  // Manual fetch function (fallback and refresh capability)
  const fetchCustomBots = useCallback(async () => {
    if (!user?.id) {
      setBots([]);
      setLoading(false);
      setInitialLoadComplete(true);
      return;
    }

    try {
      setLoading(true);
      setError(null);

      const result = await CustomBotService.getCustomBots();

      if (result.success) {
        setBots(result.data);
        setInitialLoadComplete(true);
      } else {
        setError(result.error.message);
        setBots([]);

        // Handle specific error cases
        if (result.error.statusCode === 401) {
          console.warn("User unauthorized for custom bots");
        } else if (result.error.statusCode === 403) {
          console.warn("User forbidden from accessing custom bots");
        }
      }
    } catch (err: any) {
      console.error("Error fetching custom bots:", err);
      setError(err.message || "Failed to load custom bots");
      setBots([]);
    } finally {
      setLoading(false);
    }
  }, [user?.id]);

  // Initial data fetch if SSE is not connected quickly enough
  useEffect(() => {
    if (!user?.id) {
      setBots([]);
      setLoading(false);
      setInitialLoadComplete(true);
      return;
    }

    // If SSE doesn't connect and provide data within 5 seconds, fall back to manual fetch
    const fallbackTimeout = setTimeout(() => {
      if (!initialLoadComplete && !sseState.connected) {
        console.log(
          "SSE connection taking too long, falling back to manual fetch",
        );
        fetchCustomBots();
      }
    }, 5000);

    return () => clearTimeout(fallbackTimeout);
  }, [user?.id, initialLoadComplete, sseState.connected, fetchCustomBots]);

  // Manual refetch function for user-initiated refresh
  const refetch = useCallback(async () => {
    console.log("Manual refetch requested for custom bots");
    await fetchCustomBots();
  }, [fetchCustomBots]);

  return {
    bots,
    loading,
    error,
    refetch,
    connected: sseState.connected,
    lastUpdate: sseState.lastUpdate,
  };
};

/**
 * Compatibility hook that maintains the exact same interface as the original
 * useCustomBots hook for seamless replacement in existing components.
 */
export const useCustomBots = (): UseCustomBotsReturn => {
  const sseResult = useCustomBotsSSE();

  return {
    bots: sseResult.bots,
    loading: sseResult.loading,
    error: sseResult.error,
    refetch: sseResult.refetch,
  };
};

// Re-export the return type for compatibility
export type { UseCustomBotsReturn } from "../custom-bots/use-custom-bots";
