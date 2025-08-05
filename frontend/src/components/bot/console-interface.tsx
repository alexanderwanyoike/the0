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
} from "lucide-react";
import { cn } from "@/lib/utils";

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
    <div className="flex items-start gap-3 py-0.5 px-3 hover:bg-green-950/10 dark:hover:bg-green-950/10 hover:bg-green-100/20 font-mono text-sm transition-colors">
      <span className="text-green-600 dark:text-green-400 text-sm min-w-[110px] flex-shrink-0">
        {parsed.timestamp}
      </span>
      <span
        className={`text-sm leading-none ${getStatusColor(parsed.level)} flex-shrink-0`}
      >
        {getStatusIndicator(parsed.level)}
      </span>
      <span className="flex-1 text-gray-800 dark:text-green-300 leading-tight">
        {parsed.message}
      </span>
    </div>
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
}) => {
  const [searchQuery, setSearchQuery] = useState("");
  const [autoScroll, setAutoScroll] = useState(true);
  const [selectedDate, setSelectedDate] = useState<string>("");
  const [dateRange, setDateRange] = useState<{ start: string; end: string }>({
    start: "",
    end: "",
  });
  const [showFilters, setShowFilters] = useState(false);

  const scrollContainerRef = useRef<HTMLDivElement>(null);
  const bottomRef = useRef<HTMLDivElement>(null);

  useEffect(() => {
    if (autoScroll && bottomRef.current) {
      bottomRef.current.scrollIntoView({ behavior: "smooth" });
    }
  }, [logs, autoScroll]);

  const filteredLogs = logs.filter(
    (log) =>
      searchQuery === "" ||
      log.content.toLowerCase().includes(searchQuery.toLowerCase()),
  );

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
        "flex flex-col h-full bg-white dark:bg-black border border-gray-300 dark:border-green-900/50",
        className,
      )}
    >
      <div className="border-b border-gray-300 dark:border-green-900/50 bg-gray-50 dark:bg-black p-4 space-y-3">
        <div className="flex items-center justify-between">
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

          <div className="flex items-center gap-2">
            <Button
              variant="ghost"
              size="sm"
              onClick={() => setShowFilters(!showFilters)}
              className="text-green-600 dark:text-green-400 hover:bg-green-100 dark:hover:bg-green-950/30 hover:text-green-700 dark:hover:text-green-300"
            >
              <Filter className="h-4 w-4" />
            </Button>
            <Button
              variant="ghost"
              size="sm"
              onClick={onRefresh}
              disabled={loading}
              className="text-green-600 dark:text-green-400 hover:bg-green-100 dark:hover:bg-green-950/30 hover:text-green-700 dark:hover:text-green-300"
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
              className="text-green-600 dark:text-green-400 hover:bg-green-100 dark:hover:bg-green-950/30 hover:text-green-700 dark:hover:text-green-300"
            >
              <Download className="h-4 w-4" />
            </Button>
          </div>
        </div>

        {showFilters && (
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
          className="h-full overflow-auto bg-white dark:bg-black"
        >
          {loading && logs.length === 0 ? (
            <div className="flex items-center justify-center h-32">
              <RefreshCw className="h-6 w-6 animate-spin text-green-600 dark:text-green-400" />
            </div>
          ) : filteredLogs.length === 0 ? (
            <div className="flex items-center justify-center h-32 text-gray-600 dark:text-green-600 font-mono">
              {logs.length === 0
                ? "> No logs available"
                : "> No logs match your filters"}
            </div>
          ) : (
            <>
              {filteredLogs.map((log, index) => (
                <LogEntryComponent
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
