"use client";

import { ReactNode } from "react";
import { usePathname, useRouter } from "next/navigation";
import {
  DashboardBotsProvider,
  useDashboardBots,
} from "@/contexts/dashboard-bots-context";
import { BotListPanel } from "@/components/bot-list/bot-list-panel";
import { BotListLayout } from "@/components/bot-list/bot-list-layout";
import { ResizableSidebarLayout } from "@/components/dashboard/resizable-sidebar-layout";
import { BotListItem } from "@/components/dashboard/bot-list-item";
import { useBotFilters } from "@/hooks/use-bot-filters";

function DashboardBotsSidebar() {
  const { bots } = useDashboardBots();
  const router = useRouter();
  const activeBotId = usePathname().split("/")[2] ?? null;

  return (
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
          onClick={() => router.push(`/dashboard/${bot.id}`)}
        />
      )}
    />
  );
}

export default function Layout({ children }: { children: ReactNode }) {
  return (
    <BotListLayout
      provider={DashboardBotsProvider}
      sidebar={<DashboardBotsSidebar />}
      sidebarLayout={ResizableSidebarLayout}
    >
      {children}
    </BotListLayout>
  );
}
