"use client";

import { Fragment, ReactNode } from "react";
import { Bot, Filter, Search } from "lucide-react";
import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { ScrollArea } from "@/components/ui/scroll-area";
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuLabel,
  DropdownMenuRadioGroup,
  DropdownMenuRadioItem,
  DropdownMenuSeparator,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu";
import { cn } from "@/lib/utils";
import type { BotListFilters, FilterFacet } from "@/hooks/bot-list-filters";

const NO_MATCHES = "No matching bots";

interface BotListProps<T> {
  title: string;
  bots: T[];
  /**
   * Called as a hook so every mounted list owns its filter state; pass a
   * module-level hook, never an inline function.
   */
  useFilters: () => BotListFilters<T>;
  /** Accessible name of the filter menu button. */
  filterLabel: string;
  renderItem: (bot: T) => ReactNode;
}

interface BotListPanelProps<T> extends BotListProps<T> {
  /** Shown instead of the list when there are no bots at all. */
  emptyCopy: string;
  className?: string;
}

/** Desktop sidebar list of bots with search and filters. */
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

/** Full-width list of bots shown in place of the sidebar below desktop. */
export function MobileBotList<T>({
  title,
  bots,
  useFilters,
  filterLabel,
  renderItem,
}: BotListProps<T>) {
  const filters = useFilters();
  const filtered = filters.filterBots(bots);

  return (
    <div className="px-3 py-4">
      <div className="mb-4">
        <h2 className="text-sm font-medium text-muted-foreground">{title}</h2>
        <p className="text-xl font-semibold">
          {filters.hasActiveFilters
            ? `${filtered.length} / ${bots.length} bots`
            : `${bots.length} ${bots.length === 1 ? "bot" : "bots"}`}
        </p>
      </div>
      <FilterBar filters={filters} label={filterLabel} className="mb-3" />
      <div className="space-y-2">
        <BotItems
          bots={filtered}
          renderItem={renderItem}
          emptyCopy={NO_MATCHES}
        />
      </div>
    </div>
  );
}

export function MobileBotCard({
  statusColor,
  onClick,
  children,
}: {
  statusColor: string;
  onClick: () => void;
  children: ReactNode;
}) {
  return (
    <button
      onClick={onClick}
      className="w-full text-left p-3 rounded-lg border bg-card hover:bg-accent/50 transition-colors"
    >
      <div className="flex items-center gap-3">
        <span
          className={`h-2.5 w-2.5 rounded-full flex-shrink-0 ${statusColor}`}
        />
        {children}
      </div>
    </button>
  );
}

function BotItems<T>({
  bots,
  renderItem,
  emptyCopy,
}: {
  bots: T[];
  renderItem: (bot: T) => ReactNode;
  emptyCopy: string;
}) {
  if (bots.length > 0) return bots.map((bot) => renderItem(bot));

  return (
    <div className="flex flex-col items-center justify-center py-8 text-muted-foreground">
      <Bot className="h-8 w-8 mb-2" />
      <p className="text-sm">{emptyCopy}</p>
    </div>
  );
}

function FilterBar<T>({
  filters,
  label,
  compact = false,
  className,
}: {
  filters: BotListFilters<T>;
  label: string;
  compact?: boolean;
  className?: string;
}) {
  return (
    <div className={cn("flex gap-2", className)}>
      <div className="relative flex-1">
        <Search
          className={
            compact
              ? "absolute left-2 top-1/2 -translate-y-1/2 h-3.5 w-3.5 text-muted-foreground"
              : "absolute left-2.5 top-1/2 -translate-y-1/2 h-4 w-4 text-muted-foreground"
          }
        />
        <Input
          aria-label="Filter bots"
          placeholder="Filter bots..."
          value={filters.search}
          onChange={(e) => filters.setSearch(e.target.value)}
          className={compact ? "h-8 pl-7 text-sm" : "pl-8"}
        />
      </div>
      <FilterMenu
        label={label}
        activeCount={filters.activeCount}
        facets={filters.facets}
      />
    </div>
  );
}

function FilterMenu({
  label,
  activeCount,
  facets,
}: {
  label: string;
  activeCount: number;
  facets: FilterFacet[];
}) {
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <Button
          variant="outline"
          size="icon"
          aria-label={`${label}${activeCount > 0 ? ` (${activeCount} active)` : ""}`}
          className="h-8 w-8 flex-shrink-0 relative"
        >
          <Filter className="h-3.5 w-3.5" />
          {activeCount > 0 && (
            <Badge
              variant="secondary"
              className="absolute -top-1.5 -right-1.5 h-4 w-4 p-0 flex items-center justify-center text-[10px]"
            >
              {activeCount}
            </Badge>
          )}
        </Button>
      </DropdownMenuTrigger>
      <DropdownMenuContent align="start" className="w-44">
        {facets.map((facet, index) => (
          <Fragment key={facet.label}>
            {index > 0 && <DropdownMenuSeparator />}
            <DropdownMenuLabel className="text-xs">
              {facet.label}
            </DropdownMenuLabel>
            <DropdownMenuRadioGroup
              value={facet.value}
              onValueChange={facet.onChange}
            >
              {facet.options.map((option) => (
                <DropdownMenuRadioItem key={option.value} value={option.value}>
                  {option.label}
                </DropdownMenuRadioItem>
              ))}
            </DropdownMenuRadioGroup>
          </Fragment>
        ))}
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
