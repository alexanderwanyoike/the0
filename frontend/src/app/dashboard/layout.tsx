"use client";

import { ReactNode } from "react";
import { useRouter } from "next/navigation";
import { usePathname } from "next/navigation";
import { AuthGate } from "@/components/auth/auth-gate";
import DashboardLayout from "@/components/layouts/dashboard-layout";
import {
  DashboardBotsProvider,
  useDashboardBots,
} from "@/contexts/dashboard-bots-context";
import { BotListPanel } from "@/components/bot-list/bot-list";
import { BotListItem } from "@/components/dashboard/bot-list-item";
import { useBotFilters } from "@/hooks/use-bot-filters";
import { ResizableSidebarLayout } from "@/components/dashboard/resizable-sidebar-layout";
import { useMediaQuery } from "@/hooks/use-media-query";

function DashboardInner({ children }: { children: ReactNode }) {
  const { bots } = useDashboardBots();
  const router = useRouter();
  const pathname = usePathname();
  const isDesktop = useMediaQuery("(min-width: 1280px)");

  // Extract active bot ID from URL
  const pathParts = pathname.split("/");
  const activeBotId = pathParts.length >= 3 ? pathParts[2] : null;

  const handleSelectBot = (botId: string) => {
    router.push(`/dashboard/${botId}`);
  };

  // Wait for media query to resolve
  if (isDesktop === null) {
    return (
      <div className="h-[calc(100vh-3rem)]">
        <main className="h-full overflow-auto">{children}</main>
      </div>
    );
  }

  // Desktop: resizable side panel + content
  if (isDesktop) {
    return (
      <div className="h-[calc(100vh-3rem)]">
        <ResizableSidebarLayout
          sidebar={
            <BotListPanel
              title="Bots"
              emptyCopy="No bots yet"
              filterLabel="Filter bots"
              bots={bots}
              useFilters={useBotFilters}
              className="h-full"
              renderItem={(bot) => (
                <BotListItem
                  key={bot.id}
                  bot={bot}
                  isActive={bot.id === activeBotId}
                  onClick={() => handleSelectBot(bot.id)}
                />
              )}
            />
          }
        >
          <main className="h-full overflow-auto">{children}</main>
        </ResizableSidebarLayout>
      </div>
    );
  }

  // Non-desktop: children only (list page or detail page handles its own layout)
  return (
    <div className="h-[calc(100vh-3rem)]">
      <main className="h-full overflow-auto">{children}</main>
    </div>
  );
}

export default function Layout({ children }: { children: ReactNode }) {
  return (
    <DashboardLayout>
      <AuthGate>
        <DashboardBotsProvider>
          <DashboardInner>{children}</DashboardInner>
        </DashboardBotsProvider>
      </AuthGate>
    </DashboardLayout>
  );
}
