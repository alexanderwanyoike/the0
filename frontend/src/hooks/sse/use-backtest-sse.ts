/**
 * Backtest Progress SSE Hook
 * 
 * Replaces the hybrid Firebase + polling pattern from use-backtest.ts with
 * Server-Sent Events for real-time backtest progress monitoring. Maintains
 * the same comprehensive interface while providing faster progress updates.
 * 
 * Features:
 * - Real-time progress updates via SSE connection (1-second intervals)
 * - Automatic connection termination on backtest completion
 * - Separate analysis data processing with fallback to API
 * - Progress percentage and current step tracking
 * - Complex data transformation handling (timestamps, JSON parsing)
 * - Consistent interface with existing hook
 */

import { useCallback, useEffect, useState } from 'react';
import { useAuth } from '@/contexts/auth-context';
import { authFetch } from '@/lib/auth-fetch';
import { Backtest, BacktestAnalysis } from '@/types/backtest';
import { useSSEClient } from './use-sse-client';

interface UseBacktestSSEReturn {
  backtest: Backtest | null;
  loading: boolean;
  error: string | null;
  refetch: () => void;
  // Analysis-specific state
  analysisData: BacktestAnalysis | null;
  analysisLoading: boolean;
  analysisError: string | null;
  refetchAnalysis: () => void;
  // Additional SSE-specific state
  connected: boolean;
  progress: number; // Progress percentage (0-100)
  currentStep: string; // Current execution step
  lastUpdate: Date | null;
}

export function useBacktestSSE(id: string): UseBacktestSSEReturn {
  const [backtest, setBacktest] = useState<Backtest | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);
  const [initialLoadComplete, setInitialLoadComplete] = useState(false);

  // Analysis-specific state
  const [analysisData, setAnalysisData] = useState<BacktestAnalysis | null>(null);
  const [analysisLoading, setAnalysisLoading] = useState(false);
  const [analysisError, setAnalysisError] = useState<string | null>(null);

  // Progress tracking state
  const [progress, setProgress] = useState<number>(0);
  const [currentStep, setCurrentStep] = useState<string>('Initializing...');

  const { user } = useAuth();

  // Handle SSE messages
  const handleSSEMessage = useCallback((data: any) => {
    console.log(`Backtest SSE update received for ${id}:`, data);
    
    // Extract backtest data (might be wrapped in different response formats)
    const backtestData = data.data || data;
    
    if (backtestData && backtestData.id === id) {
      // Transform dates from ISO strings to Date objects
      const transformedBacktest: Backtest = {
        ...backtestData,
        createdAt: new Date(backtestData.createdAt),
        updatedAt: new Date(backtestData.updatedAt),
        analysis: backtestData.analysis
          ? typeof backtestData.analysis === 'string'
            ? JSON.parse(backtestData.analysis)
            : backtestData.analysis
          : null,
      };

      setBacktest(transformedBacktest);
      setError(null);

      // Update progress tracking
      if (backtestData.progress !== undefined) {
        setProgress(backtestData.progress);
      }
      if (backtestData.currentStep) {
        setCurrentStep(backtestData.currentStep);
      }

      if (!initialLoadComplete) {
        setLoading(false);
        setInitialLoadComplete(true);
      }

      // Process analysis data if backtest is completed
      if (transformedBacktest.status === 'completed' && transformedBacktest.analysis) {
        processAnalysisData(transformedBacktest);
      }
    }
  }, [id, initialLoadComplete]);

  // Handle SSE connection errors
  const handleSSEError = useCallback((error: Event) => {
    console.warn(`Backtest SSE connection error for ${id}:`, error);
    
    if (!initialLoadComplete) {
      // If we haven't loaded initial data and SSE fails, fall back to manual fetch
      console.log(`SSE failed during initial load for backtest ${id}, falling back to manual fetch`);
      fetchBacktest();
    } else {
      // If we already have data, just show a non-intrusive connection error
      setError('Real-time updates temporarily unavailable');
    }
  }, [id, initialLoadComplete]);

  // Handle SSE connection events
  const handleSSEConnected = useCallback(() => {
    console.log(`Backtest SSE connected for ${id}`);
    if (error === 'Real-time updates temporarily unavailable') {
      setError(null);
    }
  }, [id, error]);

  const handleSSEDisconnected = useCallback(() => {
    console.log(`Backtest SSE disconnected for ${id}`);
    // If backtest is completed, this is expected behavior
    if (backtest?.status && ['completed', 'failed', 'cancelled'].includes(backtest.status)) {
      console.log(`Backtest ${id} reached terminal state, SSE disconnection is expected`);
    }
  }, [id, backtest?.status]);

  // Set up SSE connection (only if we have a valid backtest ID)
  const sseState = useSSEClient<any>(`/api/backtests/${id}/stream`, {
    onMessage: handleSSEMessage,
    onError: handleSSEError,
    onConnected: handleSSEConnected,
    onDisconnected: handleSSEDisconnected,
    enabled: !!user?.id && !!id,
    autoConnect: true,
    maxReconnectAttempts: 3, // Fewer retries for completed backtests
    reconnectDelay: 1000, // Fast reconnection for progress monitoring
  });

  // Manual fetch function (fallback and refresh capability)
  const fetchBacktest = useCallback(async () => {
    if (!user || !id) {
      setBacktest(null);
      setLoading(false);
      setInitialLoadComplete(true);
      return;
    }

    try {
      setLoading(true);
      setError(null);

      const response = await authFetch(`/api/backtests/${id}`);

      if (!response.ok) {
        if (response.status === 404) {
          throw new Error('Backtest not found');
        }
        throw new Error('Failed to fetch backtest');
      }

      const data = await response.json();

      // Transform dates from ISO strings to Date objects
      const transformedBacktest: Backtest = {
        ...data,
        createdAt: new Date(data.createdAt),
        updatedAt: new Date(data.updatedAt),
        analysis: data.analysis
          ? typeof data.analysis === 'string'
            ? JSON.parse(data.analysis)
            : data.analysis
          : null,
      };

      setBacktest(transformedBacktest);
      setInitialLoadComplete(true);

      // Process analysis data if available from API
      if (
        transformedBacktest.status === 'completed' &&
        transformedBacktest.analysis
      ) {
        processAnalysisData(transformedBacktest);
      }
    } catch (err: any) {
      console.error('Error fetching backtest:', err);
      setError(err.message);
      setBacktest(null);
    } finally {
      setLoading(false);
    }
  }, [user, id]);

  // Process analysis data (extracted from original hook logic)
  const processAnalysisData = useCallback((backtestToProcess?: Backtest) => {
    const targetBacktest = backtestToProcess || backtest;
    
    if (
      !targetBacktest ||
      (targetBacktest.status !== 'completed' && targetBacktest.status !== 'failed')
    ) {
      return;
    }

    setAnalysisLoading(true);
    setAnalysisError(null);

    try {
      if (targetBacktest.analysis) {
        // Parse analysis data if it's a string, otherwise use as-is
        let parsedAnalysis =
          typeof targetBacktest.analysis === 'string'
            ? JSON.parse(targetBacktest.analysis)
            : targetBacktest.analysis;

        // If the analysis data is wrapped in a results object, extract it
        // But don't unwrap if it's an error status (error analysis is the top-level object)
        if (
          parsedAnalysis &&
          typeof parsedAnalysis === 'object' &&
          'status' in parsedAnalysis &&
          'results' in parsedAnalysis &&
          parsedAnalysis.status !== 'error'
        ) {
          parsedAnalysis = (parsedAnalysis as any).results;
        }

        setAnalysisData(parsedAnalysis);
      } else {
        setAnalysisData(null);
      }
    } catch (err: any) {
      console.error('Error processing analysis data:', err);
      setAnalysisError('Failed to parse analysis data');
      setAnalysisData(null);
    } finally {
      setAnalysisLoading(false);
    }
  }, [backtest]);

  // Initial data fetch if SSE is not connected quickly enough
  useEffect(() => {
    if (!user?.id || !id) {
      setLoading(false);
      setInitialLoadComplete(true);
      return;
    }

    // If SSE doesn't connect and provide data within 3 seconds, fall back to manual fetch
    // Shorter timeout for backtest monitoring as users expect immediate feedback
    const fallbackTimeout = setTimeout(() => {
      if (!initialLoadComplete && !sseState.connected) {
        console.log(`SSE connection taking too long for backtest ${id}, falling back to manual fetch`);
        fetchBacktest();
      }
    }, 3000);

    return () => clearTimeout(fallbackTimeout);
  }, [user?.id, id, initialLoadComplete, sseState.connected, fetchBacktest]);

  // Trigger analysis processing when backtest reaches completed/failed state
  useEffect(() => {
    if (backtest?.status === 'completed' || backtest?.status === 'failed') {
      // If we don't have analysis data, try to process it
      if (!backtest.analysis && backtest.status === 'completed') {
        // For completed backtests without analysis, fetch from API
        fetchBacktest();
      } else if (backtest.analysis && !analysisData) {
        processAnalysisData();
      }
    }
  }, [
    backtest?.status,
    backtest?.analysis,
    analysisData,
    processAnalysisData,
    fetchBacktest,
  ]);

  // Manual refetch functions
  const refetch = useCallback(() => {
    console.log(`Manual refetch requested for backtest ${id}`);
    fetchBacktest();
  }, [fetchBacktest]);

  const refetchAnalysis = useCallback(() => {
    console.log(`Manual analysis refetch requested for backtest ${id}`);
    processAnalysisData();
  }, [processAnalysisData]);

  return {
    backtest,
    loading,
    error,
    refetch,
    // Analysis-specific state
    analysisData,
    analysisLoading,
    analysisError,
    refetchAnalysis,
    // Additional SSE-specific state
    connected: sseState.connected,
    progress,
    currentStep,
    lastUpdate: sseState.lastUpdate,
  };
}

/**
 * Compatibility hook that maintains the exact same interface as the original
 * useBacktest hook for seamless replacement in existing components.
 */
export function useBacktest(id: string): UseBacktestReturn {
  const sseResult = useBacktestSSE(id);
  
  return {
    backtest: sseResult.backtest,
    loading: sseResult.loading,
    error: sseResult.error,
    refetch: sseResult.refetch,
    analysisData: sseResult.analysisData,
    analysisLoading: sseResult.analysisLoading,
    analysisError: sseResult.analysisError,
    refetchAnalysis: sseResult.refetchAnalysis,
  };
}

// Re-export the original return type for compatibility
interface UseBacktestReturn {
  backtest: Backtest | null;
  loading: boolean;
  error: string | null;
  refetch: () => void;
  // Analysis-specific state
  analysisData: BacktestAnalysis | null;
  analysisLoading: boolean;
  analysisError: string | null;
  refetchAnalysis: () => void;
}