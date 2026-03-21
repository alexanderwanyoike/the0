"use client";

import { useEffect, useState } from "react";
import { useRouter } from "next/navigation";
import { useDashboardBots } from "@/contexts/dashboard-bots-context";
import { useMediaQuery } from "@/hooks/use-media-query";
import { Badge } from "@/components/ui/badge";
import { Input } from "@/components/ui/input";
import { Bot as ApiBotType } from "@/lib/api/api-client";
import { Bot, Clock, Search } from "lucide-react";
import { Button } from "@/components/ui/button";
import Link from "next/link";
import cronstrue from "cronstrue";
import { Loader2 } from "lucide-react";

export default function DashboardPage() {
  const { bots, loading } = useDashboardBots();
  const isDesktop = useMediaQuery("(min-width: 1280px)");
  const router = useRouter();

  // Desktop: auto-redirect to first bot
  useEffect(() => {
    if (!loading && isDesktop && bots.length > 0) {
      router.replace(`/dashboard/${bots[0].id}`);
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

  return <MobileBotList bots={bots} />;
}

function MobileBotList({ bots }: { bots: ApiBotType[] }) {
  const [filter, setFilter] = useState("");
  const router = useRouter();

  const filtered = bots.filter((bot) => {
    if (!filter) return true;
    const config = bot.config as Record<string, any>;
    const name = (config?.name || bot.id).toLowerCase();
    const symbol = (config?.symbol || "").toLowerCase();
    const q = filter.toLowerCase();
    return name.includes(q) || symbol.includes(q);
  });

  return (
    <div className="px-3 py-4">
      <div className="mb-4">
        <h2 className="text-sm font-medium text-muted-foreground">
          Trading Bots
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
            const config = bot.config as Record<string, any>;
            const name = config?.name || bot.id;
            const symbol = config?.symbol || "";
            const type = config?.type || "Bot";
            const schedule = config?.schedule;
            const enabled = config?.enabled ?? true;

            let readableSchedule = "Real-time";
            if (schedule) {
              try {
                readableSchedule = cronstrue.toString(schedule);
              } catch {
                readableSchedule = schedule;
              }
            }

            return (
              <button
                key={bot.id}
                onClick={() => router.push(`/dashboard/${bot.id}`)}
                className="w-full text-left p-3 rounded-lg border bg-card hover:bg-accent/50 transition-colors"
              >
                <div className="flex items-center gap-3">
                  <span
                    className={`h-2.5 w-2.5 rounded-full flex-shrink-0 ${enabled ? "bg-green-500" : "bg-gray-400"}`}
                  />
                  <div className="min-w-0 flex-1">
                    <p className="text-sm font-medium truncate">{name}</p>
                    <div className="flex items-center gap-2 mt-1">
                      {symbol && (
                        <Badge
                          variant="secondary"
                          className="text-xs font-mono"
                        >
                          {symbol}
                        </Badge>
                      )}
                      <Badge variant="outline" className="text-xs">
                        {type}
                      </Badge>
                    </div>
                  </div>
                  <div className="flex items-center gap-1 text-xs text-muted-foreground flex-shrink-0">
                    <Clock className="h-3 w-3" />
                    <span className="hidden sm:inline">
                      {readableSchedule}
                    </span>
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
