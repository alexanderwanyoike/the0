"use client";

import { ComponentType, ReactNode } from "react";
import DashboardLayout from "@/components/layouts/dashboard-layout";
import { AuthGate } from "@/components/auth/auth-gate";
import { useMediaQuery } from "@/hooks/use-media-query";

type SidebarLayout = ComponentType<{ sidebar: ReactNode; children: ReactNode }>;

interface BotListLayoutProps {
  provider: ComponentType<{ children: ReactNode }>;
  /** Rendered inside `provider`, and only on desktop. */
  sidebar: ReactNode;
  /**
   * Injected rather than chosen by a flag so sections without one do not
   * bundle it. Without it the sidebar is a fixed-width aside.
   */
  sidebarLayout?: SidebarLayout;
  children: ReactNode;
}

export function BotListLayout({
  provider: Provider,
  sidebar,
  sidebarLayout,
  children,
}: BotListLayoutProps) {
  return (
    <DashboardLayout>
      <AuthGate>
        <Provider>
          <BotListFrame sidebar={sidebar} sidebarLayout={sidebarLayout}>
            {children}
          </BotListFrame>
        </Provider>
      </AuthGate>
    </DashboardLayout>
  );
}

function BotListFrame({
  sidebar,
  sidebarLayout: SidebarLayout,
  children,
}: {
  sidebar: ReactNode;
  sidebarLayout?: SidebarLayout;
  children: ReactNode;
}) {
  const isDesktop = useMediaQuery("(min-width: 1280px)");

  if (!isDesktop) {
    return (
      <div className="h-[calc(100vh-3rem)]">
        <main className="h-full overflow-auto">{children}</main>
      </div>
    );
  }

  if (SidebarLayout) {
    return (
      <div className="h-[calc(100vh-3rem)]">
        <SidebarLayout sidebar={sidebar}>
          <main className="h-full overflow-auto">{children}</main>
        </SidebarLayout>
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
