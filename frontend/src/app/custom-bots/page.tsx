"use client";

import { useEffect } from "react";
import { useRouter } from "next/navigation";
import { useCustomBotsContext } from "@/contexts/custom-bots-context";
import { useMediaQuery } from "@/hooks/use-media-query";
import { useCustomBotFilters } from "@/hooks/use-custom-bot-filters";
import { Badge } from "@/components/ui/badge";
import { Input } from "@/components/ui/input";
import { CustomBotFilterDropdown } from "@/components/custom-bots/custom-bot-filter-dropdown";
import { Bot, Loader2, Search } from "lucide-react";
import { EmptyState } from "@/components/custom-bots/empty-state";
import { CustomBotWithVersions } from "@/types/custom-bots";

export default function CustomBotsPage() {
  const { bots, loading } = useCustomBotsContext();
  const isDesktop = useMediaQuery("(min-width: 1280px)");
  const router = useRouter();

  // Desktop: auto-redirect to first bot
  useEffect(() => {
    if (!loading && isDesktop && bots.length > 0) {
      router.replace(`/custom-bots/${bots[0].name}`);
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

  if (bots.length === 0) {
    return <EmptyState />;
  }

  return <MobileCustomBotList bots={bots} />;
}

function MobileCustomBotList({ bots }: { bots: CustomBotWithVersions[] }) {
  const {
    search,
    setSearch,
    type,
    setType,
    hasActiveFilters,
    activeCount,
    filterBots,
  } = useCustomBotFilters();
  const router = useRouter();

  const filtered = filterBots(bots);

  return (
    <div className="px-3 py-4">
      <div className="mb-4">
        <h2 className="text-sm font-medium text-muted-foreground">
          Custom Bots
        </h2>
        <p className="text-xl font-semibold">
          {hasActiveFilters
            ? `${filtered.length} / ${bots.length} bots`
            : `${bots.length} ${bots.length === 1 ? "bot" : "bots"}`}
        </p>
      </div>
      <div className="flex gap-2 mb-3">
        <div className="relative flex-1">
          <Search className="absolute left-2.5 top-1/2 -translate-y-1/2 h-4 w-4 text-muted-foreground" />
          <Input
            aria-label="Filter bots"
            placeholder="Filter bots..."
            value={search}
            onChange={(e) => setSearch(e.target.value)}
            className="pl-8"
          />
        </div>
        <CustomBotFilterDropdown
          type={type}
          setType={setType}
          activeCount={activeCount}
        />
      </div>
      <div className="space-y-2">
        {filtered.length === 0 ? (
          <div className="flex flex-col items-center justify-center py-8 text-muted-foreground">
            <Bot className="h-8 w-8 mb-2" />
            <p className="text-sm">No matching bots</p>
          </div>
        ) : (
          filtered.map((bot) => {
            const config = bot.versions[0]?.config;
            const botType = config?.type || "Bot";
            const description = config?.description || "";

            return (
              <button
                key={bot.id}
                onClick={() => router.push(`/custom-bots/${bot.name}`)}
                className="w-full text-left p-3 rounded-lg border bg-card hover:bg-accent/50 transition-colors"
              >
                <div className="flex items-center gap-3">
                  <span className="h-2.5 w-2.5 rounded-full flex-shrink-0 bg-green-500" />
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
                </div>
              </button>
            );
          })
        )}
      </div>
    </div>
  );
}
