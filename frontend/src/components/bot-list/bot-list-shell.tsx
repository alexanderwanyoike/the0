"use client";

import { ComponentType, ReactNode, useEffect } from "react";
import { useRouter } from "next/navigation";
import { AlertTriangle, Loader2, RefreshCw } from "lucide-react";
import DashboardLayout from "@/components/layouts/dashboard-layout";
import { AuthGate } from "@/components/auth/auth-gate";
import { ResizableSidebarLayout } from "@/components/dashboard/resizable-sidebar-layout";
import { Button } from "@/components/ui/button";
import { useMediaQuery } from "@/hooks/use-media-query";

const DESKTOP_QUERY = "(min-width: 1280px)";

interface BotListLayoutProps {
  provider: ComponentType<{ children: ReactNode }>;
  /** Rendered inside `provider`, and only on desktop. */
  sidebar: ReactNode;
  resizableSidebar?: boolean;
  children: ReactNode;
}

/** Route layout for a section that lists bots beside the selected one. */
export function BotListLayout({
  provider: Provider,
  sidebar,
  resizableSidebar = false,
  children,
}: BotListLayoutProps) {
  return (
    <DashboardLayout>
      <AuthGate>
        <Provider>
          <BotListFrame sidebar={sidebar} resizableSidebar={resizableSidebar}>
            {children}
          </BotListFrame>
        </Provider>
      </AuthGate>
    </DashboardLayout>
  );
}

function BotListFrame({
  sidebar,
  resizableSidebar,
  children,
}: {
  sidebar: ReactNode;
  resizableSidebar: boolean;
  children: ReactNode;
}) {
  const isDesktop = useMediaQuery(DESKTOP_QUERY);

  if (!isDesktop) {
    return (
      <div className="h-[calc(100vh-3rem)]">
        <main className="h-full overflow-auto">{children}</main>
      </div>
    );
  }

  if (resizableSidebar) {
    return (
      <div className="h-[calc(100vh-3rem)]">
        <ResizableSidebarLayout sidebar={sidebar}>
          <main className="h-full overflow-auto">{children}</main>
        </ResizableSidebarLayout>
      </div>
    );
  }

  return (
    <div className="flex h-[calc(100vh-3rem)]">
      <aside className="w-[220px] border-r flex-shrink-0">{sidebar}</aside>
      <main className="flex-1 overflow-auto">{children}</main>
    </div>
  );
}

interface BotListPageProps<T> {
  bots: T[];
  loading: boolean;
  error: string | null;
  onRetry: () => void;
  errorTitle: string;
  emptyState: ReactNode;
  /** Must be stable (module level): the desktop redirect depends on it. */
  botHref: (bot: T) => string;
  /** The mobile list. */
  children: ReactNode;
}

/**
 * Index page of a bot section. On desktop the layout's sidebar already lists
 * the bots, so the page only redirects to the first one.
 */
export function BotListPage<T>({
  bots,
  loading,
  error,
  onRetry,
  errorTitle,
  emptyState,
  botHref,
  children,
}: BotListPageProps<T>) {
  const isDesktop = useMediaQuery(DESKTOP_QUERY);
  const router = useRouter();

  useEffect(() => {
    if (!loading && isDesktop && bots.length > 0) {
      router.replace(botHref(bots[0]));
    }
  }, [loading, isDesktop, bots, botHref, router]);

  if (isDesktop === null) return null;

  if (loading) {
    return (
      <div className="flex items-center justify-center h-full">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  if (error) {
    return <ErrorState title={errorTitle} error={error} onRetry={onRetry} />;
  }

  if (bots.length === 0) return emptyState;

  return isDesktop ? null : children;
}

function ErrorState({
  title,
  error,
  onRetry,
}: {
  title: string;
  error: string;
  onRetry: () => void;
}) {
  return (
    <div className="flex flex-col items-center justify-center py-16 px-4 h-full">
      <div className="w-16 h-16 bg-destructive/10 rounded-full flex items-center justify-center mb-4">
        <AlertTriangle className="w-8 h-8 text-destructive" />
      </div>
      <h3 className="text-lg font-semibold mb-1">{title}</h3>
      <p className="text-sm text-muted-foreground text-center max-w-md mb-4">
        {error}
      </p>
      <Button variant="outline" size="sm" onClick={onRetry} className="gap-2">
        <RefreshCw className="h-4 w-4" />
        Try again
      </Button>
    </div>
  );
}
