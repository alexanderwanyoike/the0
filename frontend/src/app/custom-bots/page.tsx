"use client";

import { useEffect } from "react";
import { useRouter } from "next/navigation";
import { useCustomBotsContext } from "@/contexts/custom-bots-context";
import { useMediaQuery } from "@/hooks/use-media-query";
import { useCustomBotFilters } from "@/hooks/use-custom-bot-filters";
import { Badge } from "@/components/ui/badge";
import { MobileBotCard, MobileBotList } from "@/components/bot-list/bot-list";
import { AlertTriangle, Loader2, RefreshCw } from "lucide-react";
import { Button } from "@/components/ui/button";
import { EmptyState } from "@/components/custom-bots/empty-state";
import { CustomBotWithVersions } from "@/types/custom-bots";

export default function CustomBotsPage() {
  const { bots, loading, error, refetch } = useCustomBotsContext();
  const isDesktop = useMediaQuery("(min-width: 1280px)");
  const router = useRouter();

  // Desktop: auto-redirect to first bot
  useEffect(() => {
    if (!loading && isDesktop && bots.length > 0) {
      router.replace(`/custom-bots/${encodeURIComponent(bots[0].name)}`);
    }
  }, [loading, isDesktop, bots, router]);

  // Wait for media query to resolve before rendering
  if (isDesktop === null) return null;

  if (isDesktop) {
    if (loading) {
      return (
        <div className="flex items-center justify-center h-full">
          <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
        </div>
      );
    }
    if (error) {
      return <ErrorState error={error} onRetry={refetch} />;
    }
    if (bots.length === 0) {
      return <EmptyState />;
    }
    return null;
  }

  if (loading) {
    return (
      <div className="flex items-center justify-center h-full">
        <Loader2 className="h-8 w-8 animate-spin text-muted-foreground" />
      </div>
    );
  }

  if (error) {
    return <ErrorState error={error} onRetry={refetch} />;
  }

  if (bots.length === 0) {
    return <EmptyState />;
  }

  return <CustomBotList bots={bots} />;
}

const botHref = (bot: CustomBotWithVersions) =>
  `/custom-bots/${encodeURIComponent(bot.name)}`;

function CustomBotList({ bots }: { bots: CustomBotWithVersions[] }) {
  const router = useRouter();

  return (
    <MobileBotList
      title="Custom Bots"
      bots={bots}
      useFilters={useCustomBotFilters}
      filterLabel="Filter custom bots"
      renderItem={(bot) => (
        <CustomBotCard
          key={bot.id}
          bot={bot}
          onClick={() => router.push(botHref(bot))}
        />
      )}
    />
  );
}

function CustomBotCard({
  bot,
  onClick,
}: {
  bot: CustomBotWithVersions;
  onClick: () => void;
}) {
  const config = bot.versions[0]?.config;
  const botType = config?.type || "Bot";
  const description = config?.description || "";
  const status = bot.versions[0]?.status;

  return (
    <MobileBotCard
      statusColor={status === "active" ? "bg-green-500" : "bg-yellow-500"}
      onClick={onClick}
    >
      <div className="min-w-0 flex-1">
        <p className="text-sm font-medium truncate">{bot.name}</p>
        {description && (
          <p className="text-xs text-muted-foreground truncate mt-0.5">
            {description}
          </p>
        )}
        <div className="flex items-center gap-2 mt-1">
          <Badge variant="outline" className="text-xs">
            {botType}
          </Badge>
          <span className="text-xs text-muted-foreground">
            v{bot.latestVersion}
          </span>
        </div>
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
      <h3 className="text-lg font-semibold mb-1">Failed to load custom bots</h3>
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
