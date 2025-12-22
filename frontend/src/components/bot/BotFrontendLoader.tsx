"use client";

import React, { useState, useEffect, ComponentType } from "react";
import { BotEventsProvider } from "@/contexts/bot-events-context";
import { ConsoleInterface, LogEntry } from "./console-interface";
import { RefreshCw, AlertCircle } from "lucide-react";
import { cn } from "@/lib/utils";

interface BotFrontendLoaderProps {
  botId: string;
  botName: string;
  hasFrontend: boolean;
  /** Fallback props for console view */
  logs: LogEntry[];
  loading: boolean;
  onRefresh: () => void;
  onDateChange: (date: string | null) => void;
  onDateRangeChange: (startDate: string, endDate: string) => void;
  onExport: () => void;
  className?: string;
}

type DashboardComponent = ComponentType<Record<string, never>>;

/**
 * Loads and renders custom bot frontends dynamically.
 * Falls back to console view if no custom frontend or load fails.
 */
export function BotFrontendLoader({
  botId,
  botName,
  hasFrontend,
  logs,
  loading,
  onRefresh,
  onDateChange,
  onDateRangeChange,
  onExport,
  className,
}: BotFrontendLoaderProps) {
  const [BotDashboard, setBotDashboard] = useState<DashboardComponent | null>(
    null,
  );
  const [loadError, setLoadError] = useState<Error | null>(null);
  const [isLoading, setIsLoading] = useState(false);

  useEffect(() => {
    if (!hasFrontend) {
      setBotDashboard(null);
      setLoadError(null);
      return;
    }

    setIsLoading(true);
    setLoadError(null);

    const frontendUrl = `/api/custom-bots/${encodeURIComponent(botName)}/frontend`;

    // Dynamic import of ESM bundle
    import(/* webpackIgnore: true */ frontendUrl)
      .then((mod) => {
        if (mod.default) {
          setBotDashboard(() => mod.default);
        } else {
          throw new Error("Bundle must export a default component");
        }
      })
      .catch((err) => {
        console.error("Failed to load bot frontend:", err);
        setLoadError(err);
      })
      .finally(() => {
        setIsLoading(false);
      });
  }, [botName, hasFrontend]);

  // No custom frontend - show default console
  if (!hasFrontend) {
    return (
      <ConsoleInterface
        botId={botId}
        logs={logs}
        loading={loading}
        onRefresh={onRefresh}
        onDateChange={onDateChange}
        onDateRangeChange={onDateRangeChange}
        onExport={onExport}
        className={className}
      />
    );
  }

  // Loading state
  if (isLoading) {
    return (
      <div
        className={cn(
          "flex flex-col items-center justify-center h-64 bg-black border border-green-900/50",
          className,
        )}
      >
        <RefreshCw className="h-8 w-8 animate-spin text-green-400 mb-4" />
        <p className="text-green-400 font-mono">Loading custom dashboard...</p>
      </div>
    );
  }

  // Error state - fallback to console
  if (loadError) {
    return (
      <div className={cn("flex flex-col", className)}>
        <div className="bg-red-950/30 border border-red-800 p-4 mb-4 flex items-start gap-3">
          <AlertCircle className="h-5 w-5 text-red-400 flex-shrink-0 mt-0.5" />
          <div>
            <p className="text-red-300 font-medium">
              Failed to load custom dashboard
            </p>
            <p className="text-red-400/70 text-sm mt-1">
              {loadError.message}. Showing console view instead.
            </p>
          </div>
        </div>
        <ConsoleInterface
          botId={botId}
          logs={logs}
          loading={loading}
          onRefresh={onRefresh}
          onDateChange={onDateChange}
          onDateRangeChange={onDateRangeChange}
          onExport={onExport}
        />
      </div>
    );
  }

  // Render custom dashboard with event context
  if (BotDashboard) {
    return (
      <BotEventsProvider botId={botId} autoRefresh refreshInterval={30000}>
        <div className={className}>
          <BotDashboard />
        </div>
      </BotEventsProvider>
    );
  }

  // Fallback
  return (
    <ConsoleInterface
      botId={botId}
      logs={logs}
      loading={loading}
      onRefresh={onRefresh}
      onDateChange={onDateChange}
      onDateRangeChange={onDateRangeChange}
      onExport={onExport}
      className={className}
    />
  );
}
