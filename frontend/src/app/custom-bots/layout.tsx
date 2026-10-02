"use client";

import { ReactNode } from "react";
import { usePathname, useRouter } from "next/navigation";
import {
  CustomBotsProvider,
  useCustomBotsContext,
} from "@/contexts/custom-bots-context";
import { BotListPanel } from "@/components/bot-list/bot-list-panel";
import { BotListLayout } from "@/components/bot-list/bot-list-layout";
import { CustomBotListItem } from "@/components/custom-bots/custom-bot-list-item";
import { useCustomBotFilters } from "@/hooks/use-custom-bot-filters";

function CustomBotsSidebar() {
  const { bots } = useCustomBotsContext();
  const router = useRouter();
  const nameSegment = usePathname().split("/")[2];
  const activeBotName =
    nameSegment === undefined ? null : decodeURIComponent(nameSegment);

  return (
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
          onClick={() =>
            router.push(`/custom-bots/${encodeURIComponent(bot.name)}`)
          }
        />
      )}
    />
  );
}

export default function Layout({ children }: { children: ReactNode }) {
  return (
    <BotListLayout
      provider={CustomBotsProvider}
      sidebar={<CustomBotsSidebar />}
    >
      {children}
    </BotListLayout>
  );
}
