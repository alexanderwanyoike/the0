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
}

const LOG_LEVEL_COLORS = {
  ERROR: "text-red-400 bg-red-950/50 border-red-800",
  WARN: "text-yellow-400 bg-yellow-950/50 border-yellow-800",
  INFO: "text-blue-400 bg-blue-950/50 border-blue-800",
  DEBUG: "text-gray-400 bg-gray-950/50 border-gray-800",
};

const LogEntryComponent: React.FC<{ log: LogEntry; index: number }> = ({
  log,
  index,
}) => {
  const parseLogContent = (content: string) => {
    // Extract timestamp in various formats
    const timestampMatch = content.match(
      /\[?(\d{4}-\d{2}-\d{2}[T\s]\d{2}:\d{2}:\d{2})/,
    );
    const levelMatch = content.match(/\b(ERROR|WARN|WARNING|INFO|DEBUG)\b/);

    let timestamp = "";
    if (timestampMatch) {
      // Convert to date and time format (MM-DD HH:MM:SS)
      const fullTimestamp = timestampMatch[1];
      const date = new Date(fullTimestamp);
      const month = String(date.getMonth() + 1).padStart(2, "0");
      const day = String(date.getDate()).padStart(2, "0");
      const hours = String(date.getHours()).padStart(2, "0");
      const minutes = String(date.getMinutes()).padStart(2, "0");
      const seconds = String(date.getSeconds()).padStart(2, "0");
      timestamp = `${month}-${day} ${hours}:${minutes}:${seconds}`;
    } else {
      const now = new Date();
      const month = String(now.getMonth() + 1).padStart(2, "0");
      const day = String(now.getDate()).padStart(2, "0");
      const hours = String(now.getHours()).padStart(2, "0");
      const minutes = String(now.getMinutes()).padStart(2, "0");
      const seconds = String(now.getSeconds()).padStart(2, "0");
      timestamp = `${month}-${day} ${hours}:${minutes}:${seconds}`;
    }

    const level = levelMatch ? levelMatch[1] : "INFO";

    // Clean message by removing timestamp and level prefixes
    let message = content
      .replace(/^\[.*?\]\s*/, "")
      .replace(/^\d{4}-\d{2}-\d{2}[T\s]\d{2}:\d{2}:\d{2}[^\s]*\s*/, "")
      .replace(/^(ERROR|WARN|WARNING|INFO|DEBUG):?\s*/, "")
      .trim();

    if (!message) {
      message = content; // Fallback to original content if parsing fails
    }

    return { timestamp, level, message };
  };

  const parsed = parseLogContent(log.content);

  const getStatusIndicator = (level: string) => {
    switch (level) {
      case "ERROR":
        return "●"; // Red dot
      case "WARN":
      case "WARNING":
        return "●"; // Yellow dot
      case "INFO":
        return "●"; // Green dot
      case "DEBUG":
        return "●"; // Gray dot
      default:
        return "●"; // Default green dot
    }
  };

  const getStatusColor = (level: string) => {
    switch (level) {
      case "ERROR":
        return "text-red-500 dark:text-red-400";
      case "WARN":
      case "WARNING":
        return "text-yellow-500 dark:text-yellow-400";
      case "INFO":
        return "text-green-500 dark:text-green-400";
      case "DEBUG":
        return "text-gray-500 dark:text-gray-400";
      default:
        return "text-green-500 dark:text-green-400";
    }
  };

  return (
    <div
      className="group flex items-center gap-3 py-1 px-3 hover:bg-gray-200 dark:hover:bg-green-950/30 font-mono text-sm cursor-default"
      title={log.content}
    >
      <span className="text-gray-500 dark:text-green-600 flex-shrink-0 text-xs">
        {parsed.timestamp}
      </span>
      <span className={`flex-shrink-0 ${getStatusColor(parsed.level)}`}>
        {getStatusIndicator(parsed.level)}
      </span>
      <span className="text-gray-700 dark:text-green-300 truncate flex-1">
        {parsed.message}
      </span>
    </div>
  );
};

/**
 * Component for rendering metric entries with visual distinction.
 */
const MetricEntryComponent: React.FC<{ log: LogEntry; index: number }> = ({
  log,
}) => {
  const event = parseLogLine(log.content);

  if (!isMetricEvent(event)) {
    return <LogEntryComponent log={log} index={0} />;
  }

  const metricData = event.data as Record<string, unknown>;
  const metricType = event.metricType || "metric";

  // Format timestamp
  const timestamp = event.timestamp;
  const month = String(timestamp.getMonth() + 1).padStart(2, "0");
  const day = String(timestamp.getDate()).padStart(2, "0");
  const hours = String(timestamp.getHours()).padStart(2, "0");
  const minutes = String(timestamp.getMinutes()).padStart(2, "0");
  const seconds = String(timestamp.getSeconds()).padStart(2, "0");
  const formattedTime = `${month}-${day} ${hours}:${minutes}:${seconds}`;

  // Get display values (exclude _metric key)
  const displayData = Object.entries(metricData)
    .filter(([key]) => key !== "_metric")
    .map(([key, value]) => ({
      key,
      value: typeof value === "object" ? JSON.stringify(value) : String(value),
    }));

  const fullContent = displayData
    .map(({ key, value }) => `${key}: ${value}`)
    .join(" | ");

  return (
    <div
      className="group flex items-center gap-3 py-1 px-3 bg-blue-50 dark:bg-blue-950/20 hover:bg-blue-100 dark:hover:bg-blue-950/40 border-l-2 border-blue-500 font-mono text-sm cursor-default"
      title={`[${metricType}] ${fullContent}`}
    >
      <span className="text-blue-600 dark:text-blue-400 flex-shrink-0 text-xs">
        {formattedTime}
      </span>
      <BarChart3 className="h-3.5 w-3.5 text-blue-500 dark:text-blue-400 flex-shrink-0" />
      <Badge
        variant="outline"
        className="text-[10px] py-0 px-1.5 bg-blue-100 dark:bg-blue-900/50 border-blue-300 dark:border-blue-700 text-blue-600 dark:text-blue-300"
      >
        {metricType}
      </Badge>
      <span className="text-blue-700 dark:text-blue-200 truncate flex-1">
        {fullContent}
      </span>
    </div>
  );
};

/**
 * Smart entry component that detects metrics vs logs.
 */
const SmartLogEntry: React.FC<{ log: LogEntry; index: number }> = ({
  log,
  index,
}) => {
  // Check if this is a metric by trying to parse
  const event = parseLogLine(log.content);

  if (isMetricEvent(event)) {
    return <MetricEntryComponent log={log} index={index} />;
  }

  return <LogEntryComponent log={log} index={index} />;
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
            </div>
          )}

          <div
            className={cn(
              "flex items-center gap-1",
              compact && "w-full justify-end",
            )}
          >
            {compact && (
              <div className="flex-1 mr-2">
                <Input
                  placeholder="Search..."
                  value={searchQuery}
                  onChange={(e) => setSearchQuery(e.target.value)}
                  className="h-7 text-xs"
                />
              </div>
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
              <div ref={bottomRef} />
            </>
          )}
        </div>
      </div>
    </div>
  );
};
