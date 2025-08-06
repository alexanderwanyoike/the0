import { useState, useCallback } from "react";
import { authFetch } from "@/lib/auth-fetch";
import {
  CreateBacktestRequest,
  CreateBacktestResponse,
} from "@/types/backtest";
import { useToast } from "@/hooks/use-toast";
import { useRouter } from "next/navigation";

interface UseBacktestCreationReturn {
  createBacktest: (
    data: CreateBacktestRequest,
  ) => Promise<CreateBacktestResponse | null>;
  loading: boolean;
  error: string | null;
}

export function useBacktestCreation(): UseBacktestCreationReturn {
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);
  const { toast } = useToast();
  const router = useRouter();

  const createBacktest = useCallback(
    async (
      data: CreateBacktestRequest,
    ): Promise<CreateBacktestResponse | null> => {
      setLoading(true);
      setError(null);

      try {
        console.log(data);
        const response = await authFetch("/api/backtests", {
          method: "POST",
          body: JSON.stringify(data),
        });

        if (!response.ok) {
          const errorData = await response.json();
          throw new Error(
            errorData.error?.message || "Failed to create backtest",
          );
        }

        const result = await response.json();

        toast({
          title: "Backtest Created",
          description: `Backtest "${data.name}" has been created successfully`,
        });

        // Navigate to the backtest detail page
        router.push(`/backtests/${result.id}`);

        return result;
      } catch (err: any) {
        const errorMessage = err.message || "Failed to create backtest";
        setError(errorMessage);

        toast({
          title: "Error",
          description: errorMessage,
          variant: "destructive",
        });

        return null;
      } finally {
        setLoading(false);
      }
    },
    [toast, router],
  );

  return {
    createBacktest,
    loading,
    error,
  };
}
