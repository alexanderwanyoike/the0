"use client";

import { useRouter } from "next/navigation";
import { useCustomBotsContext } from "@/contexts/custom-bots-context";
import { useCustomBotFilters } from "@/hooks/use-custom-bot-filters";
import { MobileBotList } from "@/components/bot-list/bot-list";
import { BotListPage } from "@/components/bot-list/bot-list-shell";
import { MobileCustomBotListItem } from "@/components/custom-bots/custom-bot-list-item";
import { EmptyState } from "@/components/custom-bots/empty-state";
import { CustomBotWithVersions } from "@/types/custom-bots";

const botHref = (bot: CustomBotWithVersions) =>
  `/custom-bots/${encodeURIComponent(bot.name)}`;

export default function CustomBotsPage() {
  const { bots, loading, error, refetch } = useCustomBotsContext();
  const router = useRouter();

  return (
    <BotListPage
      bots={bots}
      loading={loading}
      error={error}
      onRetry={refetch}
      errorTitle="Failed to load custom bots"
      emptyState={<EmptyState />}
      botHref={botHref}
    >
      <MobileBotList
        title="Custom Bots"
        bots={bots}
        useFilters={useCustomBotFilters}
        filterLabel="Filter custom bots"
        renderItem={(bot) => (
          <MobileCustomBotListItem
            key={bot.id}
            bot={bot}
            onClick={() => router.push(botHref(bot))}
          />
        )}
      />
    </BotListPage>
  );
}
