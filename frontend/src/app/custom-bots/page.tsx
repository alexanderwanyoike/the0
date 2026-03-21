"use client";

import { useEffect, useState } from "react";
import { useRouter } from "next/navigation";
import { useCustomBotsContext } from "@/contexts/custom-bots-context";
import { useMediaQuery } from "@/hooks/use-media-query";
import { Badge } from "@/components/ui/badge";
import { Input } from "@/components/ui/input";
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

  // Desktop: show nothing while redirecting (or empty state if no bots)
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

  // Non-desktop: show bot list
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
  const [filter, setFilter] = useState("");
  const router = useRouter();

  const filtered = bots.filter((bot) => {
    if (!filter) return true;
    const q = filter.toLowerCase();
    const name = bot.name.toLowerCase();
    const desc = (bot.versions[0]?.config?.description || "").toLowerCase();
    return name.includes(q) || desc.includes(q);
  });

  return (
    <div className="px-3 py-4">
      <div className="mb-4">
        <h2 className="text-sm font-medium text-muted-foreground">
          Custom Bots
        </h2>
        <p className="text-xl font-semibold">
          {bots.length} {bots.length === 1 ? "bot" : "bots"}
        </p>
      </div>
      <div className="relative mb-3">
        <Search className="absolute left-2.5 top-1/2 -translate-y-1/2 h-4 w-4 text-muted-foreground" />
        <Input
          placeholder="Filter bots..."
          value={filter}
          onChange={(e) => setFilter(e.target.value)}
          className="pl-8"
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
            const type = config?.type || "Bot";
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
                        {type}
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
