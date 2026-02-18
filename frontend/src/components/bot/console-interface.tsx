"use client";

import React, { useState, useRef, useEffect } from "react";
import { Button } from "@/components/ui/button";
import { Input } from "@/components/ui/input";
import { Badge } from "@/components/ui/badge";
import {
  Search,
  Calendar,
  Download,
  Play,
  Pause,
  ScrollText,
  RefreshCw,
  Filter,
  X,
  BarChart3,
  Copy,
  Check,
  ChevronRight,
  Info,
  AlertTriangle,
  XCircle,
  Bug,
} from "lucide-react";
import { cn } from "@/lib/utils";
import { parseLogLine, isMetricEvent } from "@/lib/events/event-parser";

export interface LogEntry {
  date: string;
  content: string;
}

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
}

const LOG_LEVEL_COLORS = {
  ERROR: "text-red-400 bg-red-950/50 border-red-800",
  WARN: "text-yellow-400 bg-yellow-950/50 border-yellow-800",
  INFO: "text-blue-400 bg-blue-950/50 border-blue-800",
  DEBUG: "text-gray-400 bg-gray-950/50 border-gray-800",
};

/**
 * Format a Date to compact display format.
 * Returns null for null input or invalid Date objects.
 */
function formatTimestamp(date: Date | null): { date: string; time: string } | null {
  if (!date || isNaN(date.getTime())) return null;
  const month = String(date.getMonth() + 1).padStart(2, "0");
  const day = String(date.getDate()).padStart(2, "0");
  const hours = String(date.getHours()).padStart(2, "0");
  const minutes = String(date.getMinutes()).padStart(2, "0");
  const seconds = String(date.getSeconds()).padStart(2, "0");
  return {
    date: `${month}-${day}`,
    time: `${hours}:${minutes}:${seconds}`,
  };
}

const LogEntryComponent: React.FC<{ log: LogEntry; index: number }> = React.memo(({
  log,
}) => {
  const [copied, setCopied] = useState(false);
  const [expanded, setExpanded] = useState(false);

  // Use the event parser to get structured data
  const event = parseLogLine(log.content);
  const message = typeof event.data === "string" ? event.data : log.content;
  const level = event.level || "INFO";
  const ts = formatTimestamp(event.timestamp);

  const handleCopy = async (e: React.MouseEvent) => {
    e.stopPropagation();
    try {
      await navigator.clipboard.writeText(message);
      setCopied(true);
      setTimeout(() => setCopied(false), 2000);
    } catch {
      // Clipboard API may fail in some contexts
    }
  };

  const getLevelStyle = (lvl: string) => {
    switch (lvl) {
      case "ERROR": return "bg-red-100 dark:bg-red-500/20 text-red-600 dark:text-red-400";
      case "WARN": return "bg-yellow-100 dark:bg-yellow-500/20 text-yellow-700 dark:text-yellow-400";
      case "INFO": return "bg-green-100 dark:bg-green-500/20 text-green-700 dark:text-green-400";
      case "DEBUG": return "bg-gray-100 dark:bg-gray-500/20 text-gray-600 dark:text-gray-500";
      default: return "bg-green-100 dark:bg-green-500/20 text-green-700 dark:text-green-400";
    }
  };

  const LevelIcon = ({ lvl }: { lvl: string }) => {
    const iconClass = "h-2.5 w-2.5";
    switch (lvl) {
      case "ERROR": return <XCircle className={iconClass} />;
      case "WARN": return <AlertTriangle className={iconClass} />;
      case "INFO": return <Info className={iconClass} />;
      case "DEBUG": return <Bug className={iconClass} />;
      default: return <Info className={iconClass} />;
    }
  };

  return (
    <div
      className={cn(
        "group font-mono text-[11px] leading-tight cursor-pointer",
        "hover:bg-gray-200 dark:hover:bg-gray-800/50",
        expanded && "bg-gray-100 dark:bg-gray-800/30",
      )}
      onClick={() => setExpanded(!expanded)}
    >
      <div className="flex items-center gap-1.5 py-0.5 px-2">
        <ChevronRight
          className={cn(
            "h-3 w-3 text-gray-500 dark:text-gray-600 flex-shrink-0 transition-transform duration-100",
            expanded && "rotate-90 text-green-600 dark:text-green-400",
          )}
        />
        {ts && (
          <span className="text-gray-600 dark:text-gray-500 flex-shrink-0 select-none tabular-nums">
            <span className="text-[9px]">{ts.date}</span>
            {" "}
            <span>{ts.time}</span>
          </span>
        )}
        <span className={cn("flex-shrink-0 select-none text-[9px] font-medium px-1 rounded inline-flex items-center gap-0.5", getLevelStyle(level))}>
          <LevelIcon lvl={level} />
          {level}
        </span>
        <span className="text-gray-900 dark:text-gray-300 flex-1 min-w-0 truncate">{message}</span>
        <button
          onClick={handleCopy}
          className="opacity-0 group-hover:opacity-100 p-0.5 hover:bg-gray-300 dark:hover:bg-gray-700 rounded flex-shrink-0"
          title="Copy"
        >
          {copied ? (
            <Check className="h-2.5 w-2.5 text-green-500" />
          ) : (
            <Copy className="h-2.5 w-2.5 text-gray-400 dark:text-gray-500" />
          )}
        </button>
      </div>
      {expanded && (
        <div className="ml-6 mr-2 my-1 px-2 py-1.5 bg-gray-50 dark:bg-gray-900 rounded border border-gray-300 dark:border-gray-700 text-[11px]">
          <pre className="text-gray-900 dark:text-gray-300 whitespace-pre-wrap break-words">{message}</pre>
        </div>
      )}
    </div>
  );
});
LogEntryComponent.displayName = "LogEntryComponent";

/**
 * Component for rendering metric entries with visual distinction.
 */
const MetricEntryComponent: React.FC<{ log: LogEntry; index: number }> = React.memo(({
  log,
}) => {
  const [copied, setCopied] = useState(false);
  const [expanded, setExpanded] = useState(false);

  const event = parseLogLine(log.content);

  if (!isMetricEvent(event)) {
    return <LogEntryComponent log={log} index={0} />;
  }

  const metricData = event.data as Record<string, unknown>;
  const metricType = event.metricType || "metric";

  // Try to get timestamp from metric data if not already parsed
  let ts = formatTimestamp(event.timestamp);
  if (!ts && metricData.timestamp) {
    const rawTs = metricData.timestamp;
    if (typeof rawTs === "number") {
      ts = formatTimestamp(new Date(rawTs < 10_000_000_000 ? rawTs * 1000 : rawTs));
    } else if (typeof rawTs === "string") {
      const numMatch = rawTs.match(/^(\d+)Z?$/);
      if (numMatch) {
        const ms = parseInt(numMatch[1], 10);
        ts = formatTimestamp(new Date(ms < 10_000_000_000 ? ms * 1000 : ms));
      } else {
        ts = formatTimestamp(new Date(rawTs));
      }
    }
  }

  // Get display values (exclude _metric and timestamp keys for cleaner display)
  const displayData = Object.entries(metricData)
    .filter(([key]) => key !== "_metric" && key !== "timestamp")
    .map(([key, value]) => ({
      key,
      value: typeof value === "object" ? JSON.stringify(value) : String(value),
    }));

  const summaryContent = displayData
    .map(({ key, value }) => `${key}: ${value}`)
    .join(" | ");

  const handleCopy = async (e: React.MouseEvent) => {
    e.stopPropagation();
    try {
      const fullContent = displayData.map(({ key, value }) => `${key}: ${value}`).join("\n");
      await navigator.clipboard.writeText(fullContent);
      setCopied(true);
      setTimeout(() => setCopied(false), 2000);
    } catch {
      // Clipboard API may fail in some contexts
    }
  };

  return (
    <div
      className={cn(
        "group font-mono text-[11px] leading-tight cursor-pointer border-l-2 border-l-blue-500",
        "hover:bg-blue-100 dark:hover:bg-blue-950/30",
        expanded && "bg-blue-50 dark:bg-blue-950/20",
      )}
      onClick={() => setExpanded(!expanded)}
    >
      <div className="flex items-center gap-1.5 py-0.5 px-2">
        <ChevronRight
          className={cn(
            "h-3 w-3 text-blue-500 dark:text-blue-600 flex-shrink-0 transition-transform duration-100",
            expanded && "rotate-90 text-blue-600 dark:text-blue-400",
          )}
        />
        {ts && (
          <span className="text-blue-700 dark:text-blue-500 flex-shrink-0 select-none tabular-nums">
            <span className="text-[9px]">{ts.date}</span>
            {" "}
            <span>{ts.time}</span>
          </span>
        )}
        <BarChart3 className="h-2.5 w-2.5 text-blue-600 dark:text-blue-400 flex-shrink-0" />
        <span className="flex-shrink-0 select-none text-[9px] font-medium px-1 rounded bg-blue-100 dark:bg-blue-500/30 text-blue-700 dark:text-blue-300">
          {metricType}
        </span>
        <span className="text-blue-800 dark:text-blue-200 flex-1 min-w-0 truncate">{summaryContent}</span>
        <button
          onClick={handleCopy}
          className="opacity-0 group-hover:opacity-100 p-0.5 hover:bg-blue-200 dark:hover:bg-blue-800 rounded flex-shrink-0"
          title="Copy"
        >
          {copied ? (
            <Check className="h-2.5 w-2.5 text-green-500" />
          ) : (
            <Copy className="h-2.5 w-2.5 text-blue-500 dark:text-blue-400" />
          )}
        </button>
      </div>
      {expanded && (
        <div className="ml-6 mr-2 my-1 px-2 py-1.5 bg-blue-50 dark:bg-blue-950/50 rounded border border-blue-200 dark:border-blue-800 text-[11px]">
          <div className="grid gap-0.5">
            {displayData.map(({ key, value }) => (
              <div key={key} className="flex gap-2">
                <span className="text-blue-600 dark:text-blue-400 min-w-[80px]">{key}:</span>
                <span className="text-blue-800 dark:text-blue-100 break-all">{value}</span>
              </div>
            ))}
          </div>
        </div>
      )}
    </div>
  );
});
MetricEntryComponent.displayName = "MetricEntryComponent";

/**
 * Smart entry component that detects metrics vs logs.
 */
const SmartLogEntry: React.FC<{ log: LogEntry; index: number }> = React.memo(({
  log,
  index,
}) => {
  // Check if this is a metric by trying to parse
  const event = parseLogLine(log.content);

  if (isMetricEvent(event)) {
    return <MetricEntryComponent log={log} index={index} />;
  }

  return <LogEntryComponent log={log} index={index} />;
}, (prevProps, nextProps) =>
  prevProps.log.content === nextProps.log.content &&
  prevProps.log.date === nextProps.log.date
);
SmartLogEntry.displayName = "SmartLogEntry";

/**
 * Connection status indicator for the console toolbar.
 *
 * Three states derived from the useBotLogsStream hook:
 * - connected=true              -> Green dot + "Live"        (SSE active)
 * - connected=false, lastUpdate -> Gray dot  + "Polling"     (fell back to REST)
 * - connected=false, no update  -> Yellow dot + "Connecting" (SSE hasn't connected yet)
 *
 * When `connected` is undefined the indicator is hidden (backwards compatible).
 */
const ConnectionStatusIndicator: React.FC<{
  connected?: boolean;
  lastUpdate?: Date | null;
}> = ({ connected, lastUpdate }) => {
  if (connected === undefined) return null;

  const statusLabel = connected
    ? "connected"
    : lastUpdate
      ? "disconnected"
      : "connecting";

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
        className={cn(
          "h-1.5 w-1.5 rounded-full",
          connected
            ? "bg-green-500"
            : lastUpdate
              ? "bg-gray-400"
              : "bg-yellow-500 animate-pulse"
        )}
      />
      <span className={cn(
        "text-[10px]",
        connected
          ? "text-green-600 dark:text-green-400"
          : lastUpdate
            ? "text-gray-500"
            : "text-yellow-600 dark:text-yellow-400"
      )}>
        {connected ? "Live" : lastUpdate ? "Polling" : "Connecting..."}
      </span>
    </span>
  );
};

export const ConsoleInterface: React.FC<ConsoleInterfaceProps> = ({
  botId,
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
  hasEarlierLogs,
  loadingEarlier,
  onLoadEarlier,
}) => {
  const [searchQuery, setSearchQuery] = useState("");
  const [autoScroll, setAutoScroll] = useState(false);
  const [selectedDate, setSelectedDate] = useState<string>("");
  const [dateRange, setDateRange] = useState<{ start: string; end: string }>({
    start: "",
    end: "",
  });
  const [showFilters, setShowFilters] = useState(false);
  const [isUserAtBottom, setIsUserAtBottom] = useState(false);

  const scrollContainerRef = useRef<HTMLDivElement>(null);
  const bottomRef = useRef<HTMLDivElement>(null);

  // Track if user is at the bottom of the scroll
  const handleScroll = () => {
    const container = scrollContainerRef.current;
    if (!container) return;

    const threshold = 50; // pixels from bottom
    const isAtBottom =
      container.scrollHeight - container.scrollTop - container.clientHeight <
      threshold;
    setIsUserAtBottom(isAtBottom);
  };

  // Only auto-scroll if user is already at bottom or autoScroll is manually enabled
  useEffect(() => {
    if (
      (autoScroll || isUserAtBottom) &&
      bottomRef.current &&
      scrollContainerRef.current
    ) {
      // Use scrollTop instead of scrollIntoView to avoid page-level scroll issues
      scrollContainerRef.current.scrollTop =
        scrollContainerRef.current.scrollHeight;
    }
  }, [logs, autoScroll, isUserAtBottom]);

  // Filter and reverse logs so newest is first
  const filteredLogs = logs
    .filter(
      (log) =>
        searchQuery === "" ||
        log.content.toLowerCase().includes(searchQuery.toLowerCase()),
    )
    .slice()
    .reverse();

  const handleDateChange = (value: string) => {
    setSelectedDate(value);
    if (value) {
      onDateChange(value.replace(/-/g, ""));
      setDateRange({ start: "", end: "" });
    } else {
      onDateChange(null);
    }
  };

  const handleDateRangeChange = () => {
    if (dateRange.start && dateRange.end) {
      onDateRangeChange(
        dateRange.start.replace(/-/g, ""),
        dateRange.end.replace(/-/g, ""),
      );
      setSelectedDate("");
    }
  };

  const clearFilters = () => {
    setSearchQuery("");
    setSelectedDate("");
    setDateRange({ start: "", end: "" });
    onDateChange(null);
  };

  return (
    <div
      className={cn(
        "flex flex-col h-full bg-gray-100 dark:bg-gray-950 text-gray-800 dark:text-green-400",
        !compact && "border border-gray-300 dark:border-green-900/50",
        className,
      )}
    >
      {/* Toolbar - always visible, but simplified in compact mode */}
      <div
        className={cn(
          "border-b border-gray-300 dark:border-green-900/50 bg-gray-50 dark:bg-gray-900",
          compact ? "px-2 py-1" : "p-4 space-y-3",
        )}
      >
        <div className="flex items-center justify-between">
          {!compact && (
            <div className="flex items-center gap-2">
              <ScrollText className="h-5 w-5 text-green-600 dark:text-green-400" />
              <h3 className="font-medium text-green-600 dark:text-green-400 font-mono">
                CONSOLE
              </h3>
              <Badge
                variant="outline"
                className="text-xs bg-green-100 dark:bg-green-950/50 border-green-400 dark:border-green-800 text-green-600 dark:text-green-400"
              >
                {filteredLogs.length} entries
              </Badge>
              <span className="ml-2">
                <ConnectionStatusIndicator connected={connected} lastUpdate={lastUpdate} />
              </span>
            </div>
          )}

          <div
            className={cn(
              "flex items-center gap-1",
              compact && "w-full justify-end",
            )}
          >
            {compact && (
              <>
                <span className="mr-1">
                  <ConnectionStatusIndicator connected={connected} lastUpdate={lastUpdate} />
                </span>
                <div className="flex-1 mr-2">
                  <Input
                    placeholder="Search..."
                    value={searchQuery}
                    onChange={(e) => setSearchQuery(e.target.value)}
                    className="h-7 text-xs"
                  />
                </div>
              </>
            )}
            {!compact && (
              <Button
                variant="ghost"
                size="sm"
                onClick={() => setShowFilters(!showFilters)}
                className="text-green-600 dark:text-green-400 hover:bg-green-100 dark:hover:bg-green-950/30 hover:text-green-700 dark:hover:text-green-300"
              >
                <Filter className="h-4 w-4" />
              </Button>
            )}
            <Button
              variant="ghost"
              size="sm"
              onClick={onRefresh}
              disabled={loading}
              className={cn(
                "text-green-600 dark:text-green-400 hover:bg-green-100 dark:hover:bg-green-950/30 hover:text-green-700 dark:hover:text-green-300",
                compact && "h-7 w-7 p-0",
              )}
            >
              <RefreshCw className={cn("h-4 w-4", loading && "animate-spin")} />
            </Button>
            <Button
              variant="ghost"
              size="sm"
              onClick={() => setAutoScroll(!autoScroll)}
              className={cn(
                "text-green-600 dark:text-green-400 hover:bg-green-100 dark:hover:bg-green-950/30 hover:text-green-700 dark:hover:text-green-300",
                autoScroll
                  ? "bg-green-200 dark:bg-green-950/50 text-green-700 dark:text-green-300"
                  : "",
                compact && "h-7 w-7 p-0",
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
              onClick={onExport}
              className={cn(
                "text-green-600 dark:text-green-400 hover:bg-green-100 dark:hover:bg-green-950/30 hover:text-green-700 dark:hover:text-green-300",
                compact && "h-7 w-7 p-0",
              )}
            >
              <Download className="h-4 w-4" />
            </Button>
          </div>
        </div>

        {!compact && showFilters && (
          <div className="space-y-3 pt-3 border-t">
            <div className="relative">
              <Search className="absolute left-3 top-1/2 transform -translate-y-1/2 text-muted-foreground h-4 w-4" />
              <Input
                placeholder="Search logs..."
                value={searchQuery}
                onChange={(e) => setSearchQuery(e.target.value)}
                className="pl-10"
              />
            </div>

            <div className="grid grid-cols-1 md:grid-cols-3 gap-3">
              <div>
                <label className="text-xs text-muted-foreground mb-1 block">
                  Single Date
                </label>
                <Input
                  type="date"
                  value={selectedDate}
                  onChange={(e) => handleDateChange(e.target.value)}
                />
              </div>

              <div className="md:col-span-2 grid grid-cols-2 gap-2">
                <div>
                  <label className="text-xs text-muted-foreground mb-1 block">
                    Date Range Start
                  </label>
                  <Input
                    type="date"
                    value={dateRange.start}
                    onChange={(e) =>
                      setDateRange({ ...dateRange, start: e.target.value })
                    }
                  />
                </div>
                <div>
                  <label className="text-xs text-muted-foreground mb-1 block">
                    Date Range End
                  </label>
                  <Input
                    type="date"
                    value={dateRange.end}
                    onChange={(e) => {
                      setDateRange({ ...dateRange, end: e.target.value });
                      if (e.target.value && dateRange.start) {
                        handleDateRangeChange();
                      }
                    }}
                  />
                </div>
              </div>
            </div>

            {(searchQuery || selectedDate || dateRange.start) && (
              <div className="flex justify-end">
                <Button
                  variant="ghost"
                  size="sm"
                  onClick={clearFilters}
                  className="text-xs h-7"
                >
                  <X className="h-3 w-3 mr-1" />
                  Clear
                </Button>
              </div>
            )}
          </div>
        )}
      </div>

      <div className="flex-1 relative overflow-hidden">
        <div
          ref={scrollContainerRef}
          onScroll={handleScroll}
          className="h-full overflow-auto bg-gray-100 dark:bg-gray-950"
        >
          {loading && logs.length === 0 ? (
            <div className="flex items-center justify-center h-32">
              <RefreshCw className="h-6 w-6 animate-spin text-gray-500 dark:text-green-500" />
            </div>
          ) : filteredLogs.length === 0 ? (
            <div className="flex items-center justify-center h-32 text-gray-500 dark:text-green-600 font-mono text-sm">
              {logs.length === 0
                ? "> Waiting for logs..."
                : "> No logs match filter"}
            </div>
          ) : (
            <>
              {filteredLogs.map((log, index) => (
                <SmartLogEntry
                  key={`log-${index}-${log.content.slice(0, 20)}`}
                  log={log}
                  index={index}
                />
              ))}
              {hasEarlierLogs && onLoadEarlier && !searchQuery && (
                <div className="flex justify-center py-2 border-t border-gray-200 dark:border-gray-800">
                  <Button
                    variant="ghost"
                    size="sm"
                    onClick={onLoadEarlier}
                    disabled={loadingEarlier}
                    className="text-xs text-gray-500 dark:text-gray-400 hover:text-gray-700 dark:hover:text-gray-200"
                  >
                    {loadingEarlier ? (
                      <>
                        <RefreshCw className="h-3 w-3 mr-1 animate-spin" />
                        Loading...
                      </>
                    ) : (
                      <>
                        <ScrollText className="h-3 w-3 mr-1" />
                        Load earlier logs
                      </>
                    )}
                  </Button>
                </div>
              )}
              <div ref={bottomRef} />
            </>
          )}
        </div>
      </div>
    </div>
  );
};
