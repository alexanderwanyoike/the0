"use client";

import { useRouter } from "next/navigation";
import Link from "next/link";
import { Bot } from "lucide-react";
import { useDashboardBots } from "@/contexts/dashboard-bots-context";
import { useBotFilters } from "@/hooks/use-bot-filters";
import { Button } from "@/components/ui/button";
import { MobileBotList } from "@/components/bot-list/bot-list";
import { BotListPage } from "@/components/bot-list/bot-list-shell";
import { MobileBotListItem } from "@/components/dashboard/bot-list-item";
import { Bot as ApiBotType } from "@/lib/api/api-client";

const botHref = (bot: ApiBotType) => `/dashboard/${bot.id}`;

export default function DashboardPage() {
  const { bots, loading, error, refetchBots } = useDashboardBots();
  const router = useRouter();

  return (
    <BotListPage
      bots={bots}
      loading={loading}
      error={error}
      onRetry={refetchBots}
      errorTitle="Failed to load bots"
      emptyState={<EmptyState />}
      botHref={botHref}
    >
      <MobileBotList
        title="Trading Bots"
        bots={bots}
        useFilters={useBotFilters}
        filterLabel="Filter bots"
        renderItem={(bot) => (
          <MobileBotListItem
            key={bot.id}
            bot={bot}
            onClick={() => router.push(botHref(bot))}
          />
        )}
      />
    </BotListPage>
  );
}

function EmptyState() {
  return (
    <div className="flex flex-col items-center justify-center py-16 px-4 h-full">
      <div className="w-20 h-20 bg-muted rounded-full flex items-center justify-center mb-6">
        <Bot className="w-10 h-10 text-muted-foreground" />
      </div>
      <h3 className="text-xl font-semibold mb-2">No trading bots yet</h3>
      <p className="text-muted-foreground text-center max-w-md mb-6">
        Get started by viewing your custom bots or checking your deployed bots
      </p>
      <div className="flex flex-col sm:flex-row gap-3 w-full max-w-md">
        <Button asChild variant="outline" className="gap-2 flex-1">
          <Link href="/user-bots">
            <Bot className="w-4 h-4" />
            View My Bots
          </Link>
        </Button>
        <Button asChild variant="outline" className="gap-2 flex-1">
          <Link href="/custom-bots">
            <Bot className="w-4 h-4" />
            Custom Bots
          </Link>
        </Button>
      </div>
    </div>
  );
}
