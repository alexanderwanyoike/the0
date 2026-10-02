"use client";

import { useEffect } from "react";
import { useRouter } from "next/navigation";
import { useDashboardBots } from "@/contexts/dashboard-bots-context";
import { useMediaQuery } from "@/hooks/use-media-query";
import { useBotFilters } from "@/hooks/use-bot-filters";
import { Badge } from "@/components/ui/badge";
import { MobileBotCard, MobileBotList } from "@/components/bot-list/bot-list";
import { Bot as ApiBotType } from "@/lib/api/api-client";
import { AlertTriangle, Bot, Clock, RefreshCw } from "lucide-react";
import { Button } from "@/components/ui/button";
import Link from "next/link";
import cronstrue from "cronstrue";
import { Loader2 } from "lucide-react";

export default function DashboardPage() {
  const { bots, loading, error, refetchBots } = useDashboardBots();
  const isDesktop = useMediaQuery("(min-width: 1280px)");
  const router = useRouter();

  // Desktop: auto-redirect to first bot
  useEffect(() => {
    if (!loading && isDesktop && bots.length > 0) {
      router.replace(`/dashboard/${bots[0].id}`);
    }
  }, [loading, isDesktop, bots, router]);

  // Wait for media query to resolve before rendering
  if (isDesktop === null) return null;

  // Desktop: show nothing while redirecting (or empty state if no bots)
  if (isDesktop) {
    if (loading) {
      return (
        <div className="flex items-center justify-center h-full">
          <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
        </div>
      );
    }
    if (error) {
      return <ErrorState error={error} onRetry={refetchBots} />;
    }
    if (bots.length === 0) {
      return <EmptyState />;
    }
    return null;
  }

  // Non-desktop: show bot list
  if (loading) {
    return (
      <div className="flex items-center justify-center h-full">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  if (error) {
    return <ErrorState error={error} onRetry={refetchBots} />;
  }

  if (bots.length === 0) {
    return <EmptyState />;
  }

  return <DashboardBotList bots={bots} />;
}

const botHref = (bot: ApiBotType) => `/dashboard/${bot.id}`;

function DashboardBotList({ bots }: { bots: ApiBotType[] }) {
  const router = useRouter();

  return (
    <MobileBotList
      title="Trading Bots"
      bots={bots}
      useFilters={useBotFilters}
      filterLabel="Filter bots"
      renderItem={(bot) => (
        <DashboardBotCard
          key={bot.id}
          bot={bot}
          onClick={() => router.push(botHref(bot))}
        />
      )}
    />
  );
}

function readableSchedule(schedule: string | undefined) {
  if (!schedule) return "Real-time";
  try {
    return cronstrue.toString(schedule);
  } catch {
    return schedule;
  }
}

function DashboardBotCard({
  bot,
  onClick,
}: {
  bot: ApiBotType;
  onClick: () => void;
}) {
  const config = bot.config as Record<string, any>;
  const name = config?.name || bot.id;
  const symbol = config?.symbol || "";
  const botType = config?.type || "Bot";
  const enabled = config?.enabled ?? true;

  return (
    <MobileBotCard
      statusColor={enabled ? "bg-green-500" : "bg-gray-400"}
      onClick={onClick}
    >
      <div className="min-w-0 flex-1">
        <p className="text-sm font-medium truncate">{name}</p>
        <div className="flex items-center gap-2 mt-1">
          {symbol && (
            <Badge variant="secondary" className="text-xs font-mono">
              {symbol}
            </Badge>
          )}
          <Badge variant="outline" className="text-xs">
            {botType}
          </Badge>
        </div>
      </div>
      <div className="flex items-center gap-1 text-xs text-muted-foreground flex-shrink-0">
        <Clock className="h-3 w-3" />
        <span className="hidden sm:inline">
          {readableSchedule(config?.schedule)}
        </span>
      </div>
    </MobileBotCard>
  );
}

function ErrorState({
  error,
  onRetry,
}: {
  error: string;
  onRetry: () => void;
}) {
  return (
    <div className="flex flex-col items-center justify-center py-16 px-4 h-full">
      <div className="w-16 h-16 bg-destructive/10 rounded-full flex items-center justify-center mb-4">
        <AlertTriangle className="w-8 h-8 text-destructive" />
      </div>
      <h3 className="text-lg font-semibold mb-1">Failed to load bots</h3>
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
