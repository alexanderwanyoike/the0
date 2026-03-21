"use client";

import React, {
  createContext,
  useContext,
  useEffect,
  useState,
  useCallback,
} from "react";
import { BotService, Bot } from "@/lib/api/api-client";
import { useAuth } from "@/contexts/auth-context";

interface DashboardBotsContextValue {
  bots: Bot[];
  loading: boolean;
  error: string | null;
  refetchBots: () => Promise<void>;
  removeBotFromList: (id: string) => void;
}

const DashboardBotsContext = createContext<DashboardBotsContextValue | null>(
  null,
);

export function DashboardBotsProvider({
  children,
}: {
  children: React.ReactNode;
}) {
  const [bots, setBots] = useState<Bot[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const { user } = useAuth();

  const fetchBots = useCallback(async () => {
    if (!user) {
      setLoading(false);
      return;
    }
    setLoading(true);
    setError(null);
    try {
      const result = await BotService.getBots();
      if (result.success) {
        setBots(result.data);
      } else {
        setError(result.error.message);
      }
    } catch (err) {
      setError(err instanceof Error ? err.message : "Failed to fetch bots");
    } finally {
      setLoading(false);
    }
  }, [user]);

  useEffect(() => {
    fetchBots();
  }, [fetchBots]);

  const removeBotFromList = useCallback((id: string) => {
    setBots((prev) => prev.filter((b) => b.id !== id));
  }, []);

  return (
    <DashboardBotsContext.Provider
      value={{
        bots,
        loading,
        error,
        refetchBots: fetchBots,
        removeBotFromList,
      }}
    >
      {children}
    </DashboardBotsContext.Provider>
  );
}

export function useDashboardBots() {
  const ctx = useContext(DashboardBotsContext);
  if (!ctx) {
    throw new Error(
      "useDashboardBots must be used within DashboardBotsProvider",
    );
  }
  return ctx;
}
