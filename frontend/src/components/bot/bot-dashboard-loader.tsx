"use client";

import React, {
  useState,
  useEffect,
  ComponentType,
  Component,
  ErrorInfo,
  ReactNode,
} from "react";
import * as ReactDOM from "react-dom";
import * as ReactJSXRuntime from "react/jsx-runtime";
import {
  BotEventsProvider,
  useBotEventsContext,
} from "@/contexts/bot-events-context";
import { RefreshCw, AlertCircle, RotateCcw } from "lucide-react";
import { cn } from "@/lib/utils";
import { Button } from "@/components/ui/button";

// Expose React globals for custom bot frontends
// The esbuild plugin in bot frontends transforms React imports to use these
if (typeof window !== "undefined") {
  (window as any).__THE0_REACT__ = React;
  (window as any).__THE0_REACT_DOM__ = ReactDOM;
  (window as any).__THE0_REACT_JSX__ = ReactJSXRuntime;
}

/**
 * Error boundary for custom bot dashboards.
 * Catches runtime errors and displays a friendly error message.
 */
interface DashboardErrorBoundaryProps {
  children: ReactNode;
  onReset?: () => void;
}

interface DashboardErrorBoundaryState {
  hasError: boolean;
  error: Error | null;
}

class DashboardErrorBoundary extends Component<
  DashboardErrorBoundaryProps,
  DashboardErrorBoundaryState
> {
  constructor(props: DashboardErrorBoundaryProps) {
    super(props);
    this.state = { hasError: false, error: null };
  }

  static getDerivedStateFromError(error: Error): DashboardErrorBoundaryState {
    return { hasError: true, error };
  }

  componentDidCatch(error: Error, errorInfo: ErrorInfo) {
    console.error("Dashboard error:", error, errorInfo);
  }

  handleReset = () => {
    this.setState({ hasError: false, error: null });
    this.props.onReset?.();
  };

  render() {
    if (this.state.hasError) {
      return (
        <div className="flex flex-col items-center justify-center bg-muted/30 rounded-lg p-8 min-h-[200px]">
          <div className="bg-red-950/30 border border-red-800 p-6 rounded-lg max-w-lg w-full">
            <div className="flex items-start gap-3 mb-4">
              <AlertCircle className="h-5 w-5 text-red-400 flex-shrink-0 mt-0.5" />
              <div className="flex-1 min-w-0">
                <p className="text-red-300 font-medium">Dashboard Error</p>
                <p className="text-red-400/70 text-sm mt-1 break-words">
                  {this.state.error?.message || "An unexpected error occurred"}
                </p>
              </div>
            </div>
            <Button
              variant="outline"
              size="sm"
              onClick={this.handleReset}
              className="w-full border-red-800 text-red-300 hover:bg-red-950/50"
            >
              <RotateCcw className="h-4 w-4 mr-2" />
              Try Again
            </Button>
          </div>
        </div>
      );
    }

    return this.props.children;
  }
}

/**
 * Inner component that watches event loading state from context
 * and calls onEventsReady when the initial load completes.
 */
function EventsLoadingGate({
  children,
  onEventsReady,
  isWaiting,
  className,
}: {
  children: ReactNode;
  onEventsReady: () => void;
  isWaiting: boolean;
  className?: string;
}) {
  const { loading } = useBotEventsContext();

  useEffect(() => {
    if (!loading && isWaiting) {
      onEventsReady();
    }
  }, [loading, isWaiting, onEventsReady]);

  if (isWaiting) {
    return (
      <div
        className={cn(
          "flex flex-col items-center justify-center bg-muted/30 rounded-lg min-h-[200px]",
          className,
        )}
      >
        <RefreshCw className="h-8 w-8 animate-spin text-muted-foreground mb-4" />
        <p className="text-muted-foreground">Loading events...</p>
      </div>
    );
  }

  return <>{children}</>;
}

interface BotDashboardLoaderProps {
  botId: string;
  customBotId: string;
  version?: string;
  dateRange?: { start: string; end: string };
  className?: string;
}

type DashboardComponent = ComponentType<Record<string, never>>;

/**
 * Loads and renders custom bot dashboard dynamically.
 * Shows error state if load fails.
 * Wrapped in React.memo to avoid re-renders from parent state changes.
 */
export const BotDashboardLoader = React.memo(function BotDashboardLoader({
  botId,
  customBotId,
  version,
  dateRange,
  className,
}: BotDashboardLoaderProps) {
  const [BotDashboard, setBotDashboard] = useState<DashboardComponent | null>(
    null,
  );
  const [loadError, setLoadError] = useState<Error | null>(null);
  const [loadPhase, setLoadPhase] = useState<
    "dashboard" | "events" | null
  >(null);

  useEffect(() => {
    if (!customBotId) {
      setLoadPhase(null);
      return;
    }

    setLoadPhase("dashboard");
    setLoadError(null);
    setBotDashboard(null);

    const frontendUrl = `/api/custom-bots/frontend/${encodeURIComponent(customBotId)}`;
    const cacheKey = `the0-bundle-${customBotId}-${version || "latest"}`;

    /**
     * Import the bundle, with localStorage caching.
     * On cache hit: load from a blob URL.
     * On miss: fetch as text, cache, then import via blob URL.
     * Falls back to direct import if localStorage is unavailable.
     */
    const importBundle = async (): Promise<DashboardComponent> => {
      try {
        const cached = localStorage.getItem(cacheKey);
        if (cached) {
          const blob = new Blob([cached], {
            type: "application/javascript",
          });
          const url = URL.createObjectURL(blob);
          try {
            const mod = await import(/* webpackIgnore: true */ url);
            if (!mod.default)
              throw new Error("Bundle must export a default component");
            return mod.default;
          } finally {
            URL.revokeObjectURL(url);
          }
        }
      } catch {
        // localStorage unavailable or blob import failed - continue to fetch
      }

      // Fetch, cache, then import via blob URL
      try {
        const res = await fetch(frontendUrl);
        if (!res.ok)
          throw new Error(`Failed to fetch bundle: ${res.statusText}`);
        const text = await res.text();

        // Best-effort cache
        try {
          localStorage.setItem(cacheKey, text);
        } catch {
          // Quota exceeded or private browsing - ignore
        }

        const blob = new Blob([text], { type: "application/javascript" });
        const url = URL.createObjectURL(blob);
        try {
          const mod = await import(/* webpackIgnore: true */ url);
          if (!mod.default)
            throw new Error("Bundle must export a default component");
          return mod.default;
        } finally {
          URL.revokeObjectURL(url);
        }
      } catch (err) {
        // Last resort: direct import without caching
        const mod = await import(/* webpackIgnore: true */ frontendUrl);
        if (!mod.default)
          throw new Error("Bundle must export a default component");
        return mod.default;
      }
    };

    const loadWithTimeout = async () => {
      const TIMEOUT_MS = 10_000;
      const timeout = new Promise<never>((_, reject) =>
        setTimeout(
          () => reject(new Error("Dashboard load timed out")),
          TIMEOUT_MS,
        ),
      );
      return Promise.race([importBundle(), timeout]);
    };

    loadWithTimeout()
      .then((component) => {
        setBotDashboard(() => component);
        setLoadPhase("events");
      })
      .catch((err) => {
        console.error("Failed to load bot frontend:", err);
        setLoadError(err);
        setLoadPhase(null);
      });
  }, [customBotId, version]);

  // Loading state: show phase
  if (loadPhase === "dashboard") {
    return (
      <div
        className={cn(
          "flex flex-col items-center justify-center bg-muted/30 rounded-lg min-h-[200px]",
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

  // Render custom dashboard with event context and error boundary
  if (BotDashboard) {
    return (
      <BotEventsProvider
        botId={botId}
        autoRefresh
        refreshInterval={30000}
        dateRange={dateRange}
      >
        <DashboardErrorBoundary>
          <EventsLoadingGate
            isWaiting={loadPhase === "events"}
            onEventsReady={() => setLoadPhase(null)}
            className={className}
          >
            <div className={cn("h-full overflow-auto", className)}>
              <BotDashboard />
            </div>
          </EventsLoadingGate>
        </DashboardErrorBoundary>
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
});
