"use client";

import { ReactNode } from "react";
import { useRouter } from "next/navigation";
import { usePathname } from "next/navigation";
import { AuthGate } from "@/components/auth/auth-gate";
import DashboardLayout from "@/components/layouts/dashboard-layout";
import {
  CustomBotsProvider,
  useCustomBotsContext,
} from "@/contexts/custom-bots-context";
import { BotListPanel } from "@/components/bot-list/bot-list";
import { CustomBotListItem } from "@/components/custom-bots/custom-bot-list-item";
import { useCustomBotFilters } from "@/hooks/use-custom-bot-filters";
import { useMediaQuery } from "@/hooks/use-media-query";

function CustomBotsInner({ children }: { children: ReactNode }) {
  const { bots } = useCustomBotsContext();
  const router = useRouter();
  const pathname = usePathname();
  const isDesktop = useMediaQuery("(min-width: 1280px)");

  // Extract active bot name from URL
  const pathParts = pathname.split("/");
  const activeBotName =
    pathParts.length >= 3 ? decodeURIComponent(pathParts[2]) : null;

  const handleSelectBot = (name: string) => {
    router.push(`/custom-bots/${encodeURIComponent(name)}`);
  };

  // Wait for media query to resolve
  if (isDesktop === null) {
    return (
      <div className="h-[calc(100vh-3rem)]">
        <main className="h-full overflow-auto">{children}</main>
      </div>
    );
  }

  // Desktop: side panel + content
  if (isDesktop) {
    return (
      <div className="flex h-[calc(100vh-3rem)]">
        <aside className="w-[220px] border-r flex-shrink-0">
          <BotListPanel
            title="Custom Bots"
            emptyCopy="No custom bots yet"
            filterLabel="Filter custom bots"
            bots={bots}
            useFilters={useCustomBotFilters}
            className="h-full"
            renderItem={(bot) => (
              <CustomBotListItem
                key={bot.id}
                bot={bot}
                isActive={bot.name === activeBotName}
                onClick={() => handleSelectBot(bot.name)}
              />
            )}
          />
        </aside>
        <main className="flex-1 overflow-auto">{children}</main>
      </div>
    );
  }

  // Non-desktop: children only
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
        <CustomBotsProvider>
          <CustomBotsInner>{children}</CustomBotsInner>
        </CustomBotsProvider>
      </AuthGate>
    </DashboardLayout>
  );
}
