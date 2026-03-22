"use client";

import { CustomBotWithVersions } from "@/types/custom-bots";
import { CustomBotListItem } from "./custom-bot-list-item";
import { CustomBotFilterDropdown } from "./custom-bot-filter-dropdown";
import { ScrollArea } from "@/components/ui/scroll-area";
import { Input } from "@/components/ui/input";
import { Badge } from "@/components/ui/badge";
import { Bot, Search } from "lucide-react";
import { cn } from "@/lib/utils";
import { useCustomBotFilters } from "@/hooks/use-custom-bot-filters";

interface CustomBotListPanelProps {
  bots: CustomBotWithVersions[];
  activeBotName: string | null;
  onSelectBot: (name: string) => void;
  className?: string;
}

export function CustomBotListPanel({
  bots,
  activeBotName,
  onSelectBot,
  className,
}: CustomBotListPanelProps) {
  const {
    search,
    setSearch,
    type,
    setType,
    hasActiveFilters,
    activeCount,
    filterBots,
  } = useCustomBotFilters();

  const filtered = filterBots(bots);

  return (
    <div className={cn("flex flex-col", className)}>
      {/* Header */}
      <div className="px-3 py-3 border-b flex-shrink-0">
        <div className="flex items-center justify-between mb-2">
          <span className="text-sm font-medium">Custom Bots</span>
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
              aria-label="Filter bots"
              placeholder="Filter bots..."
              value={search}
              onChange={(e) => setSearch(e.target.value)}
              className="h-8 pl-7 text-sm"
            />
          </div>
          <CustomBotFilterDropdown
            type={type}
            setType={setType}
            activeCount={activeCount}
          />
        </div>
      </div>

      {/* List */}
      <ScrollArea className="flex-1">
        <div className="p-1.5 space-y-0.5">
          {filtered.length === 0 ? (
            <div className="flex flex-col items-center justify-center py-8 text-muted-foreground">
              <Bot className="h-8 w-8 mb-2" />
              <p className="text-sm">
                {bots.length === 0 ? "No custom bots yet" : "No matching bots"}
              </p>
            </div>
          ) : (
            filtered.map((bot) => (
              <CustomBotListItem
                key={bot.id}
                bot={bot}
                isActive={bot.name === activeBotName}
                onClick={() => onSelectBot(bot.name)}
              />
            ))
          )}
        </div>
      </ScrollArea>
    </div>
  );
}
