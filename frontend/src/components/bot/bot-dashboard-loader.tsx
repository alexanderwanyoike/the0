"use client";

import React, { useState, useEffect, ComponentType } from "react";
import * as ReactJSXRuntime from "react/jsx-runtime";
import { BotEventsProvider } from "@/contexts/bot-events-context";
import { RefreshCw, AlertCircle } from "lucide-react";
import { cn } from "@/lib/utils";

// Expose React globally for the shared module endpoints to access
if (typeof window !== "undefined") {
  (window as any).__THE0_REACT__ = React;
  (window as any).__THE0_REACT_JSX__ = ReactJSXRuntime;
}

interface BotDashboardLoaderProps {
  botId: string;
  customBotId: string;
  className?: string;
}

type DashboardComponent = ComponentType<Record<string, never>>;

/**
 * Loads and renders custom bot dashboard dynamically.
 * Shows error state if load fails.
 */
export function BotDashboardLoader({
  botId,
  customBotId,
  className,
}: BotDashboardLoaderProps) {
  const [BotDashboard, setBotDashboard] = useState<DashboardComponent | null>(
    null,
  );
  const [loadError, setLoadError] = useState<Error | null>(null);
  const [isLoading, setIsLoading] = useState(true);

  useEffect(() => {
    if (!customBotId) {
      setIsLoading(false);
      return;
    }

    setIsLoading(true);
    setLoadError(null);

    const frontendUrl = `/api/custom-bots/frontend/${encodeURIComponent(customBotId)}`;

    // Dynamic import - import map resolves react to host-served modules
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
  }, [customBotId]);

  // Loading state
  if (isLoading) {
    return (
      <div
        className={cn(
          "flex flex-col items-center justify-center bg-muted/30 rounded-lg",
          className,
        )}
      >
        <RefreshCw className="h-8 w-8 animate-spin text-muted-foreground mb-4" />
        <p className="text-muted-foreground">Loading dashboard...</p>
      </div>
    );
  }

  // Error state
  if (loadError) {
    return (
      <div
        className={cn(
          "flex flex-col items-center justify-center bg-muted/30 rounded-lg p-8",
          className,
        )}
      >
        <div className="bg-red-950/30 border border-red-800 p-4 rounded-lg flex items-start gap-3 max-w-md">
          <AlertCircle className="h-5 w-5 text-red-400 flex-shrink-0 mt-0.5" />
          <div>
            <p className="text-red-300 font-medium">Failed to load dashboard</p>
            <p className="text-red-400/70 text-sm mt-1">{loadError.message}</p>
          </div>
        </div>
      </div>
    );
  }

  // Render custom dashboard with event context
  if (BotDashboard) {
    return (
      <BotEventsProvider botId={botId} autoRefresh refreshInterval={30000}>
        <div className={cn("h-full overflow-auto", className)}>
          <BotDashboard />
        </div>
      </BotEventsProvider>
    );
  }

  // No dashboard loaded
  return (
    <div
      className={cn(
        "flex items-center justify-center text-muted-foreground bg-muted/30 rounded-lg",
        className,
      )}
    >
      <p>No dashboard available</p>
    </div>
  );
}
