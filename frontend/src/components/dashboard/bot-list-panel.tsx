"use client";

import { Bot } from "@/lib/api/api-client";
import { BotListItem } from "./bot-list-item";
import { BotFilterDropdown } from "./bot-filter-dropdown";
import { ScrollArea } from "@/components/ui/scroll-area";
import { Input } from "@/components/ui/input";
import { Badge } from "@/components/ui/badge";
import { Bot as BotIcon, Search } from "lucide-react";
import { cn } from "@/lib/utils";
import { useBotFilters } from "@/hooks/use-bot-filters";

interface BotListPanelProps {
  bots: Bot[];
  activeBotId: string | null;
  onSelectBot: (botId: string) => void;
  className?: string;
}

export function BotListPanel({
  bots,
  activeBotId,
  onSelectBot,
  className,
}: BotListPanelProps) {
  const {
    search,
    setSearch,
    type,
    setType,
    status,
    setStatus,
    hasActiveFilters,
    activeCount,
    filterBots,
  } = useBotFilters();

  const filtered = filterBots(bots);

  return (
    <div className={cn("flex flex-col", className)}>
      {/* Header */}
      <div className="px-3 py-3 border-b flex-shrink-0">
        <div className="flex items-center justify-between mb-2">
          <span className="text-sm font-medium">Bots</span>
          <Badge variant="secondary" className="text-xs">
            {hasActiveFilters
              ? `${filtered.length} / ${bots.length}`
              : bots.length}
          </Badge>
        </div>
        <div className="flex gap-2">
          <div className="relative flex-1">
            <Search className="absolute left-2 top-1/2 -translate-y-1/2 h-3.5 w-3.5 text-muted-foreground" />
            <Input
              placeholder="Filter bots..."
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              className="h-8 pl-7 text-sm"
            />
          </div>
          <BotFilterDropdown
            type={type}
            setType={setType}
            status={status}
            setStatus={setStatus}
            activeCount={activeCount}
          />
        </div>
      </div>

      {/* List */}
      <ScrollArea className="flex-1">
        <div className="p-1.5 space-y-0.5">
          {filtered.length === 0 ? (
            <div className="flex flex-col items-center justify-center py-8 text-muted-foreground">
              <BotIcon className="h-8 w-8 mb-2" />
              <p className="text-sm">
                {bots.length === 0 ? "No bots yet" : "No matching bots"}
              </p>
            </div>
          ) : (
            filtered.map((bot) => (
              <BotListItem
                key={bot.id}
                bot={bot}
                isActive={bot.id === activeBotId}
                onClick={() => onSelectBot(bot.id)}
              />
            ))
          )}
        </div>
      </ScrollArea>
    </div>
  );
}
