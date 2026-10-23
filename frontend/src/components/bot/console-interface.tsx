"use client";

import React, { useState, useRef, useMemo, useEffect } from "react";
import { Virtuoso, VirtuosoHandle } from "react-virtuoso";
import { Button } from "@/components/ui/button";
import { RefreshCw, ScrollText } from "lucide-react";
import { cn } from "@/lib/utils";
import type { LogEntry } from "@/types/logs";
import { SmartLogEntry } from "./console/log-entries";
import { displayText } from "./console/log-format";
import { ConsoleFilterPanel, ConsoleToolbar } from "./console/console-toolbar";
import { useConsoleFilters } from "./console/use-console-filters";

export { ConnectionStatusIndicator } from "./console/console-toolbar";

interface ConsoleInterfaceProps {
  botId: string;
  logs: LogEntry[];
  loading: boolean;
  onRefresh: () => void;
  onDateChange: (date: string | null) => void;
  onDateRangeChange: (startDate: string, endDate: string) => void;
  onExport: () => void;
  className?: string;
  /** When true, hides the header (title, badges) - useful when embedded in a parent with its own header */
  compact?: boolean;
  /** Whether the streaming connection is active */
  connected?: boolean;
  /** Timestamp of the last received update */
  lastUpdate?: Date | null;
  /** Whether there are earlier logs that were trimmed from the buffer */
  hasEarlierLogs?: boolean;
  /** Whether earlier logs are currently being fetched */
  loadingEarlier?: boolean;
  /** Callback to load earlier logs that were trimmed from the buffer */
  onLoadEarlier?: () => void;
  /** Whether there are more paginated logs available from the API */
  hasMore?: boolean;
  /** Callback to load more paginated logs */
  loadMore?: () => void;
  /** Whether more logs are currently being loaded */
  loadingMore?: boolean;
  /** Current sort order from the API */
  sort?: "asc" | "desc";
  /** Callback when user toggles sort order */
  onSortChange?: (sort: "asc" | "desc") => void;
  /** Names the window of logs shown (such as the interval); a new one opens
   *  at its newest line. */
  viewKey?: string;
  /** Whether the logs are being read again (such as for a new window). */
  fetching?: boolean;
}

const PagerButton = ({
  edge,
  onClick,
  loading,
  children,
}: {
  edge: "top" | "bottom";
  onClick: () => void;
  loading?: boolean;
  children: React.ReactNode;
}) => (
  <div
    className={cn(
      "flex justify-center py-2",
      edge === "top"
        ? "border-b border-gray-200 dark:border-gray-800"
        : "border-t border-gray-200 dark:border-gray-800",
    )}
  >
    <Button
      variant="ghost"
      size="sm"
      onClick={onClick}
      disabled={loading}
      className="text-xs text-gray-500 dark:text-gray-400 hover:text-gray-700 dark:hover:text-gray-200"
    >
      {loading ? (
        <>
          <RefreshCw className="h-3 w-3 mr-1 animate-spin" />
          Loading...
        </>
      ) : (
        children
      )}
    </Button>
  </div>
);

type ConsoleLogViewProps = Pick<
  ConsoleInterfaceProps,
  | "logs"
  | "loading"
  | "connected"
  | "hasEarlierLogs"
  | "loadingEarlier"
  | "onLoadEarlier"
  | "hasMore"
  | "loadMore"
  | "loadingMore"
> & {
  displayLogs: LogEntry[];
  searchQuery: string;
  sort: "asc" | "desc";
  autoScroll: boolean;
  view: string;
  fetching: boolean;
};

const ConsoleLogView: React.FC<ConsoleLogViewProps> = ({
  logs,
  displayLogs,
  loading,
  connected,
  sort,
  autoScroll,
  searchQuery,
  hasEarlierLogs,
  loadingEarlier,
  onLoadEarlier,
  hasMore,
  loadMore,
  loadingMore,
  view,
  fetching,
}) => {
  const [, setIsUserAtBottom] = useState(true);
  const virtuosoRef = useRef<VirtuosoHandle>(null);
  const newestIndex = sort === "asc" ? displayLogs.length - 1 : 0;

  // The old window's lines stay up until the new window's arrive, so the
  // jump to the newest line waits for them. Earlier pages and live lines
  // keep the reader's place.
  const viewRef = useRef(view);
  const jumpPendingRef = useRef(false);
  useEffect(() => {
    if (viewRef.current === view) return;
    viewRef.current = view;
    jumpPendingRef.current = true;
  }, [view]);
  useEffect(() => {
    if (!jumpPendingRef.current) return;
    jumpPendingRef.current = false;
    if (displayLogs.length === 0) return;
    virtuosoRef.current?.scrollToIndex({
      index: newestIndex,
      align: sort === "asc" ? "end" : "start",
    });
    // Only new lines end the wait; a sort flip remounts the list instead
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [displayLogs]);
  // A read that ends with no new lines (it failed) ends the wait too, or a
  // later search or live line would jump the reader away
  const wasFetchingRef = useRef(fetching);
  useEffect(() => {
    if (wasFetchingRef.current && !fetching) jumpPendingRef.current = false;
    wasFetchingRef.current = fetching;
  }, [fetching]);

  // Background refreshes keep showing the current lines; only an empty
  // console shows the spinner.
  if (loading && logs.length === 0) {
    return (
      <div className="flex items-center justify-center h-32">
        <RefreshCw className="h-6 w-6 animate-spin text-gray-500 dark:text-green-500" />
      </div>
    );
  }

  if (displayLogs.length === 0) {
    return (
      <div className="flex items-center justify-center h-32 text-gray-500 dark:text-green-600 font-mono text-sm">
        {logs.length === 0 ? "> Waiting for logs..." : "> No logs match filter"}
      </div>
    );
  }

  return (
    <Virtuoso
      key={sort}
      ref={virtuosoRef}
      data={displayLogs}
      initialTopMostItemIndex={newestIndex}
      overscan={50}
      followOutput={
        connected && sort === "asc" && autoScroll ? "smooth" : false
      }
      atBottomStateChange={(atBottom) => setIsUserAtBottom(atBottom)}
      itemContent={(index, log) => <SmartLogEntry log={log} index={index} />}
      endReached={undefined}
      components={{
        Header:
          hasEarlierLogs && onLoadEarlier && !searchQuery
            ? () => (
                <PagerButton
                  edge="top"
                  onClick={onLoadEarlier}
                  loading={loadingEarlier}
                >
                  <ScrollText className="h-3 w-3 mr-1" />
                  Load earlier logs
                </PagerButton>
              )
            : undefined,
        Footer:
          hasMore && loadMore
            ? () => (
                <PagerButton
                  edge="bottom"
                  onClick={loadMore}
                  loading={loadingMore}
                >
                  Load more
                </PagerButton>
              )
            : undefined,
      }}
      className="h-full"
    />
  );
};

export const ConsoleInterface: React.FC<ConsoleInterfaceProps> = ({
  logs,
  loading,
  onRefresh,
  onDateChange,
  onDateRangeChange,
  onExport,
  className,
  compact = false,
  connected,
  lastUpdate,
  sort = "desc",
  onSortChange,
  viewKey = "",
  fetching = false,
  ...pagination
}) => {
  const filters = useConsoleFilters(onDateChange, onDateRangeChange);
  const view = [
    viewKey,
    filters.selectedDate,
    filters.dateRange.start,
    filters.dateRange.end,
  ].join("|");
  const { searchQuery } = filters;
  const [autoScroll, setAutoScroll] = useState(true);
  const [showFilters, setShowFilters] = useState(false);

  // No reversal needed - the API returns data in the requested sort order
  const displayLogs = useMemo(() => {
    if (searchQuery === "") return logs;
    const needle = searchQuery.toLowerCase();
    return logs.filter((log) =>
      displayText(log).toLowerCase().includes(needle),
    );
  }, [logs, searchQuery]);

  return (
    <div
      className={cn(
        "flex flex-col h-full bg-gray-100 dark:bg-gray-950 text-gray-800 dark:text-green-400",
        !compact && "border border-gray-300 dark:border-green-900/50",
        className,
      )}
    >
      <div
        className={cn(
          "border-b border-gray-300 dark:border-green-900/50 bg-gray-50 dark:bg-gray-900",
          compact ? "px-2 py-1" : "p-4 space-y-3",
        )}
      >
        <ConsoleToolbar
          compact={compact}
          entryCount={displayLogs.length}
          connected={connected}
          lastUpdate={lastUpdate}
          searchQuery={searchQuery}
          onSearchChange={filters.setSearchQuery}
          onToggleFilters={() => setShowFilters(!showFilters)}
          loading={loading}
          onRefresh={onRefresh}
          sort={sort}
          onSortChange={onSortChange}
          autoScroll={autoScroll}
          onToggleAutoScroll={() => setAutoScroll(!autoScroll)}
          onExport={onExport}
        />

        {!compact && showFilters && <ConsoleFilterPanel filters={filters} />}
      </div>

      <div className="flex-1 relative overflow-hidden bg-gray-100 dark:bg-gray-950">
        <ConsoleLogView
          {...pagination}
          logs={logs}
          displayLogs={displayLogs}
          loading={loading}
          connected={connected}
          sort={sort}
          autoScroll={autoScroll}
          searchQuery={searchQuery}
          view={view}
          fetching={fetching}
        />
      </div>
    </div>
  );
};
