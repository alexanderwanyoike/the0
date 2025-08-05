import { useState, useEffect, useCallback } from "react";
import { useAuth } from "@/contexts/auth-context";
import { authFetch } from "@/lib/auth-fetch";
import { Backtest } from "@/types/backtest";

interface UseBacktestsReturn {
  backtests: Backtest[];
  loading: boolean;
  error: string | null;
  refetch: () => void;
}

export function useBacktests(): UseBacktestsReturn {
  const [backtests, setBacktests] = useState<Backtest[]>([]);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const { user } = useAuth();

  const fetchBacktests = useCallback(async () => {
    if (!user) {
      setBacktests([]);
      setLoading(false);
      return;
    }

    try {
      setLoading(true);
      setError(null);

      const response = await authFetch("/api/backtests");

      if (!response.ok) {
        throw new Error("Failed to fetch backtests");
      }

      const data = await response.json();

      // Transform dates from ISO strings to Date objects
      const transformedBacktests = data.map((backtest: any) => ({
        ...backtest,
        createdAt: new Date(backtest.createdAt),
        updatedAt: new Date(backtest.updatedAt),
        analysis: backtest.analysis ? JSON.parse(backtest.analysis) : null,
      }));

      setBacktests(transformedBacktests);
    } catch (err: any) {
      console.error("Error fetching backtests:", err);
      setError(err.message);
      setBacktests([]);
    } finally {
      setLoading(false);
    }
  }, [user]);

  useEffect(() => {
    fetchBacktests();
  }, [fetchBacktests]);

  const refetch = useCallback(() => {
    fetchBacktests();
  }, [fetchBacktests]);

  return {
    backtests,
    loading,
    error,
    refetch,
  };
}
