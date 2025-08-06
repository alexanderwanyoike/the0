import { useState, useEffect } from "react";
import { useAuth } from "@/contexts/auth-context";
import { authFetch } from "@/lib/auth-fetch";
import { Backtest } from "@/types/backtest";

interface UseBacktestReturn {
  backtest: Backtest | null;
  loading: boolean;
  error: string | null;
  refetch: () => void;
}

export function useBacktest(id: string): UseBacktestReturn {
  const [backtest, setBacktest] = useState<Backtest | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const { user } = useAuth();

  const fetchBacktest = async () => {
    if (!user || !id) return;

    try {
      setLoading(true);
      setError(null);

      const response = await authFetch(`/api/backtests/${id}`);
      if (!response.ok) {
        throw new Error(
          response.status === 404
            ? "Backtest not found"
            : "Failed to fetch backtest",
        );
      }

      const data = await response.json();
      setBacktest({
        ...data,
        createdAt: new Date(data.createdAt),
        updatedAt: new Date(data.updatedAt),
        analysis: data.analysis
          ? typeof data.analysis === "string"
            ? JSON.parse(data.analysis)
            : data.analysis
          : null,
      });
    } catch (err: any) {
      setError(err.message);
      setBacktest(null);
    } finally {
      setLoading(false);
    }
  };

  // Initial fetch
  useEffect(() => {
    fetchBacktest();
  }, [user, id]);

  // Poll only if pending or running
  useEffect(() => {
    if (backtest?.status !== "pending" && backtest?.status !== "running")
      return;

    const interval = setInterval(fetchBacktest, 3000);
    return () => clearInterval(interval);
  }, [backtest?.status]);

  return {
    backtest,
    loading,
    error,
    refetch: fetchBacktest,
  };
}
