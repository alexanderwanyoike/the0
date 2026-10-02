"use client";

import React, { useState } from "react";
import {
  AlertTriangle,
  BarChart3,
  Bug,
  Check,
  ChevronRight,
  Copy,
  Info,
  XCircle,
} from "lucide-react";
import { cn } from "@/lib/utils";
import { isMetricEvent } from "@/lib/events/event-parser";
import type { LogEntry } from "@/types/logs";
import {
  DisplayTimestamp,
  cachedParseLogLine,
  formatTimestamp,
  levelStyle,
  metricDisplayData,
  metricTimestamp,
} from "./log-format";

type EntryProps = { log: LogEntry; index: number };

function useCopyFeedback() {
  const [copied, setCopied] = useState(false);

  const copy = async (e: React.MouseEvent, text: () => string) => {
    e.stopPropagation();
    try {
      await navigator.clipboard.writeText(text());
      setCopied(true);
      setTimeout(() => setCopied(false), 2000);
    } catch {
      // Clipboard API may fail in some contexts
    }
  };

  return { copied, copy };
}

const LEVEL_ICONS = new Map<
  string,
  React.ComponentType<{ className?: string }>
>([
  ["ERROR", XCircle],
  ["WARN", AlertTriangle],
  ["INFO", Info],
  ["DEBUG", Bug],
]);

const LevelIcon = ({ lvl }: { lvl: string }) => {
  const Icon = LEVEL_ICONS.get(lvl) ?? Info;
  return <Icon className="h-2.5 w-2.5" />;
};

const EntryTimestamp = ({
  ts,
  className,
}: {
  ts: DisplayTimestamp | null;
  className: string;
}) =>
  ts && (
    <span className={cn(className, "flex-shrink-0 select-none tabular-nums")}>
      <span className="text-[9px]">{ts.date}</span> <span>{ts.time}</span>
    </span>
  );

const LogEntryComponent: React.FC<EntryProps> = React.memo(({ log }) => {
  const { copied, copy } = useCopyFeedback();
  const [expanded, setExpanded] = useState(false);

  const event = cachedParseLogLine(log.content, log.timestamp);
  const message = typeof event.data === "string" ? event.data : log.content;
  const level = event.level || "INFO";
  const ts = formatTimestamp(event.timestamp);

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
        <EntryTimestamp ts={ts} className="text-gray-600 dark:text-gray-500" />
        <span
          className={cn(
            "flex-shrink-0 select-none text-[9px] font-medium px-1 rounded inline-flex items-center gap-0.5",
            levelStyle(level),
          )}
        >
          <LevelIcon lvl={level} />
          {level}
        </span>
        <span className="text-gray-900 dark:text-gray-300 flex-1 min-w-0 truncate">
          {message}
        </span>
        <button
          onClick={(e) => copy(e, () => message)}
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
          <pre className="text-gray-900 dark:text-gray-300 whitespace-pre-wrap break-words">
            {message}
          </pre>
        </div>
      )}
    </div>
  );
});
LogEntryComponent.displayName = "LogEntryComponent";

/**
 * Component for rendering metric entries with visual distinction.
 */
const MetricEntryComponent: React.FC<EntryProps> = React.memo(({ log }) => {
  const { copied, copy } = useCopyFeedback();
  const [expanded, setExpanded] = useState(false);

  const event = cachedParseLogLine(log.content, log.timestamp);

  if (!isMetricEvent(event)) {
    return <LogEntryComponent log={log} index={0} />;
  }

  const metricData = event.data as Record<string, unknown>;
  const metricType = event.metricType || "metric";
  const ts = metricTimestamp(event.timestamp, metricData);
  const displayData = metricDisplayData(metricData);
  const summaryContent = displayData
    .map(({ key, value }) => `${key}: ${value}`)
    .join(" | ");

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
        <EntryTimestamp ts={ts} className="text-blue-700 dark:text-blue-500" />
        <BarChart3 className="h-2.5 w-2.5 text-blue-600 dark:text-blue-400 flex-shrink-0" />
        <span className="flex-shrink-0 select-none text-[9px] font-medium px-1 rounded bg-blue-100 dark:bg-blue-500/30 text-blue-700 dark:text-blue-300">
          {metricType}
        </span>
        <span className="text-blue-800 dark:text-blue-200 flex-1 min-w-0 truncate">
          {summaryContent}
        </span>
        <button
          onClick={(e) =>
            copy(e, () =>
              displayData
                .map(({ key, value }) => `${key}: ${value}`)
                .join("\n"),
            )
          }
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
                <span className="text-blue-600 dark:text-blue-400 min-w-[80px]">
                  {key}:
                </span>
                <span className="text-blue-800 dark:text-blue-100 break-all">
                  {value}
                </span>
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
export const SmartLogEntry: React.FC<EntryProps> = React.memo(
  ({ log, index }) => {
    const event = cachedParseLogLine(log.content, log.timestamp);

    if (isMetricEvent(event)) {
      return <MetricEntryComponent log={log} index={index} />;
    }

    return <LogEntryComponent log={log} index={index} />;
  },
  (prevProps, nextProps) =>
    prevProps.log.content === nextProps.log.content &&
    prevProps.log.date === nextProps.log.date,
);
SmartLogEntry.displayName = "SmartLogEntry";
