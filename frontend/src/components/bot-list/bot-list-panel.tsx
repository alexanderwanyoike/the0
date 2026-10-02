"use client";

import { Badge } from "@/components/ui/badge";
import { ScrollArea } from "@/components/ui/scroll-area";
import { cn } from "@/lib/utils";
import {
  BotItems,
  BotListProps,
  FilterBar,
  NO_MATCHES,
} from "@/components/bot-list/bot-list";

interface BotListPanelProps<T> extends BotListProps<T> {
  /** Shown instead of the list when there are no bots at all. */
  emptyCopy: string;
  className?: string;
}

export function BotListPanel<T>({
  title,
  bots,
  useFilters,
  filterLabel,
  renderItem,
  emptyCopy,
  className,
}: BotListPanelProps<T>) {
  const filters = useFilters();
  const filtered = filters.filterBots(bots);

  return (
    <div className={cn("flex flex-col", className)}>
      <div className="px-3 py-3 border-b flex-shrink-0">
        <div className="flex items-center justify-between mb-2">
          <span className="text-sm font-medium">{title}</span>
          <Badge variant="secondary" className="text-xs">
            {filters.hasActiveFilters
              ? `${filtered.length} / ${bots.length}`
              : bots.length}
          </Badge>
        </div>
        <FilterBar filters={filters} label={filterLabel} compact />
      </div>

      <ScrollArea className="flex-1">
        <div className="p-1.5 space-y-0.5">
          <BotItems
            bots={filtered}
            renderItem={renderItem}
            emptyCopy={bots.length === 0 ? emptyCopy : NO_MATCHES}
          />
        </div>
      </ScrollArea>
    </div>
  );
}
