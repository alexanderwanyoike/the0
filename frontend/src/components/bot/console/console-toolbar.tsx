"use client";

import React from "react";
import {
  ArrowDownUp,
  Download,
  Filter,
  Pause,
  Play,
  RefreshCw,
  ScrollText,
  Search,
  X,
} from "lucide-react";
import { Badge } from "@/components/ui/badge";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { cn } from "@/lib/utils";
import type { ConsoleFilters } from "./use-console-filters";

const TOOLBAR_BUTTON =
  "text-green-600 dark:text-green-400 hover:bg-green-100 dark:hover:bg-green-950/30 hover:text-green-700 dark:hover:text-green-300";
const COMPACT_BUTTON = "h-7 w-7 p-0";

interface ConsoleToolbarProps {
  compact: boolean;
  entryCount: number;
  connected?: boolean;
  lastUpdate?: Date | null;
  searchQuery: string;
  onSearchChange: (value: string) => void;
  onToggleFilters: () => void;
  loading: boolean;
  onRefresh: () => void;
  sort: "asc" | "desc";
  onSortChange?: (sort: "asc" | "desc") => void;
  autoScroll: boolean;
  onToggleAutoScroll: () => void;
  onExport: () => void;
}

const ConsoleTitle = ({
  entryCount,
  connected,
  lastUpdate,
}: Pick<ConsoleToolbarProps, "entryCount" | "connected" | "lastUpdate">) => (
  <div className="flex items-center gap-2">
    <ScrollText className="h-5 w-5 text-green-600 dark:text-green-400" />
    <h3 className="font-medium text-green-600 dark:text-green-400 font-mono">
      CONSOLE
    </h3>
    <Badge
      variant="outline"
      className="text-xs bg-green-100 dark:bg-green-950/50 border-green-400 dark:border-green-800 text-green-600 dark:text-green-400"
    >
      {entryCount} entries
    </Badge>
    <span className="ml-2">
      <ConnectionStatusIndicator
        connected={connected}
        lastUpdate={lastUpdate}
      />
    </span>
  </div>
);

const CompactStatusAndSearch = ({
  connected,
  lastUpdate,
  searchQuery,
  onSearchChange,
}: Pick<
  ConsoleToolbarProps,
  "connected" | "lastUpdate" | "searchQuery" | "onSearchChange"
>) => (
  <>
    <span className="mr-1">
      <ConnectionStatusIndicator
        connected={connected}
        lastUpdate={lastUpdate}
      />
    </span>
    <div className="flex-1 mr-2">
      <Input
        placeholder="Search..."
        value={searchQuery}
        onChange={(e) => onSearchChange(e.target.value)}
        className="h-7 text-xs"
      />
    </div>
  </>
);

export const ConsoleToolbar: React.FC<ConsoleToolbarProps> = (props) => {
  const { compact, loading, sort, autoScroll } = props;

  return (
    <div className="flex items-center justify-between">
      {!compact && <ConsoleTitle {...props} />}

      <div
        className={cn(
          "flex items-center gap-1",
          compact && "w-full justify-end",
        )}
      >
        {compact && <CompactStatusAndSearch {...props} />}
        {!compact && (
          <Button
            variant="ghost"
            size="sm"
            onClick={props.onToggleFilters}
            className={TOOLBAR_BUTTON}
          >
            <Filter className="h-4 w-4" />
          </Button>
        )}
        <Button
          variant="ghost"
          size="sm"
          onClick={props.onRefresh}
          disabled={loading}
          className={cn(TOOLBAR_BUTTON, compact && COMPACT_BUTTON)}
        >
          <RefreshCw className={cn("h-4 w-4", loading && "animate-spin")} />
        </Button>
        <Button
          variant="ghost"
          size="sm"
          onClick={() => props.onSortChange?.(sort === "desc" ? "asc" : "desc")}
          title={sort === "desc" ? "Newest first" : "Oldest first"}
          className={cn(TOOLBAR_BUTTON, compact && COMPACT_BUTTON)}
        >
          <ArrowDownUp
            className={cn("h-4 w-4", sort === "desc" && "rotate-180")}
          />
        </Button>
        <Button
          variant="ghost"
          size="sm"
          onClick={props.onToggleAutoScroll}
          className={cn(
            TOOLBAR_BUTTON,
            autoScroll
              ? "bg-green-200 dark:bg-green-950/50 text-green-700 dark:text-green-300"
              : "",
            compact && COMPACT_BUTTON,
          )}
        >
          {autoScroll ? (
            <Pause className="h-4 w-4" />
          ) : (
            <Play className="h-4 w-4" />
          )}
        </Button>
        <Button
          variant="ghost"
          size="sm"
          onClick={props.onExport}
          className={cn(TOOLBAR_BUTTON, compact && COMPACT_BUTTON)}
        >
          <Download className="h-4 w-4" />
        </Button>
      </div>
    </div>
  );
};

const DateField = ({
  label,
  value,
  onChange,
}: {
  label: string;
  value: string;
  onChange: (value: string) => void;
}) => (
  <div>
    <label className="text-xs text-muted-foreground mb-1 block">{label}</label>
    <Input
      type="date"
      value={value}
      onChange={(e) => onChange(e.target.value)}
    />
  </div>
);

export const ConsoleFilterPanel: React.FC<{ filters: ConsoleFilters }> = ({
  filters,
}) => (
  <div className="space-y-3 pt-3 border-t">
    <div className="relative">
      <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-muted-foreground h-4 w-4" />
      <Input
        placeholder="Search logs..."
        value={filters.searchQuery}
        onChange={(e) => filters.setSearchQuery(e.target.value)}
        className="pl-10"
      />
    </div>

    <div className="grid grid-cols-1 md:grid-cols-3 gap-3">
      <DateField
        label="Single Date"
        value={filters.selectedDate}
        onChange={filters.handleDateChange}
      />

      <div className="md:col-span-2 grid grid-cols-2 gap-2">
        <DateField
          label="Date Range Start"
          value={filters.dateRange.start}
          onChange={filters.handleRangeStartChange}
        />
        <DateField
          label="Date Range End"
          value={filters.dateRange.end}
          onChange={filters.handleRangeEndChange}
        />
      </div>
    </div>

    {filters.hasActiveFilters && (
      <div className="flex justify-end">
        <Button
          variant="ghost"
          size="sm"
          onClick={filters.clearFilters}
          className="text-xs h-7"
        >
          <X className="h-3 w-3 mr-1" />
          Clear
        </Button>
      </div>
    )}
  </div>
);

const STATUS = {
  connected: {
    dot: "bg-green-500",
    text: "text-green-600 dark:text-green-400",
    label: "Live",
  },
  disconnected: {
    dot: "bg-gray-400",
    text: "text-gray-500",
    label: "Polling",
  },
  connecting: {
    dot: "bg-yellow-500 animate-pulse",
    text: "text-yellow-600 dark:text-yellow-400",
    label: "Connecting...",
  },
} as const;

/**
 * Connection status indicator for the console toolbar.
 *
 * Three states derived from the useBotLogs hook (streaming mode):
 * - connected=true              -> Green dot + "Live"        (SSE active)
 * - connected=false, lastUpdate -> Gray dot  + "Polling"     (fell back to REST)
 * - connected=false, no update  -> Yellow dot + "Connecting" (SSE hasn't connected yet)
 *
 * When `connected` is undefined the indicator is hidden (backwards compatible).
 */
export const ConnectionStatusIndicator: React.FC<{
  connected?: boolean;
  lastUpdate?: Date | null;
}> = ({ connected, lastUpdate }) => {
  if (connected === undefined) return null;

  const statusLabel = connected
    ? "connected"
    : lastUpdate
      ? "disconnected"
      : "connecting";
  const status = STATUS[statusLabel];

  return (
    <span
      className="flex items-center gap-1 text-xs"
      role="status"
      aria-live="polite"
      aria-atomic="true"
      aria-label={`Connection status: ${statusLabel}`}
    >
      <span
        aria-hidden="true"
        className={cn("h-1.5 w-1.5 rounded-full", status.dot)}
      />
      <span className={cn("text-[10px]", status.text)}>{status.label}</span>
    </span>
  );
};
