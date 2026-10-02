import { parseLogLine } from "@/lib/events/event-parser";

type ParsedLogLine = ReturnType<typeof parseLogLine>;

/**
 * Module-level cache for parseLogLine results.
 * Avoids re-parsing the same log content string on every render.
 * Capped at 5000 entries to prevent unbounded memory growth.
 */
const parseLogLineCache = new Map<string, ParsedLogLine>();
export function cachedParseLogLine(
  content: string,
  timestamp?: string | null,
): ParsedLogLine {
  const cacheKey = timestamp ? `${timestamp}\0${content}` : content;
  let result = parseLogLineCache.get(cacheKey);
  if (result === undefined) {
    result = parseLogLine(content, timestamp);
    parseLogLineCache.set(cacheKey, result);
    if (parseLogLineCache.size > 5000) {
      const firstKey = parseLogLineCache.keys().next().value;
      if (firstKey !== undefined) parseLogLineCache.delete(firstKey);
    }
  }
  return result;
}

export type DisplayTimestamp = { date: string; time: string };

/**
 * Format a Date to compact display format.
 * Returns null for null input or invalid Date objects.
 */
export function formatTimestamp(date: Date | null): DisplayTimestamp | null {
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

// Epoch values below this are seconds, not milliseconds
const EPOCH_SECONDS_LIMIT = 10_000_000_000;

const epochToDate = (n: number) =>
  new Date(n < EPOCH_SECONDS_LIMIT ? n * 1000 : n);

/** A metric's own `timestamp` field: epoch seconds/ms (number or digit
 *  string, optional trailing Z) or any Date-parsable string. */
function metricDataTimestamp(raw: unknown): DisplayTimestamp | null {
  if (typeof raw === "number") return formatTimestamp(epochToDate(raw));
  if (typeof raw !== "string") return null;
  const numMatch = raw.match(/^(\d+)Z?$/);
  return formatTimestamp(
    numMatch ? epochToDate(parseInt(numMatch[1], 10)) : new Date(raw),
  );
}

export function metricTimestamp(
  parsed: Date | null,
  metricData: Record<string, unknown>,
): DisplayTimestamp | null {
  const ts = formatTimestamp(parsed);
  if (ts || !metricData.timestamp) return ts;
  return metricDataTimestamp(metricData.timestamp);
}

/** Metric fields worth showing: everything except the _metric marker and
 *  the timestamp, with objects JSON-encoded. */
export function metricDisplayData(
  metricData: Record<string, unknown>,
): { key: string; value: string }[] {
  return Object.entries(metricData)
    .filter(([key]) => key !== "_metric" && key !== "timestamp")
    .map(([key, value]) => ({
      key,
      value: typeof value === "object" ? JSON.stringify(value) : String(value),
    }));
}

const INFO_STYLE =
  "bg-green-100 dark:bg-green-500/20 text-green-700 dark:text-green-400";

const LEVEL_STYLES = new Map<string, string>([
  ["ERROR", "bg-red-100 dark:bg-red-500/20 text-red-600 dark:text-red-400"],
  [
    "WARN",
    "bg-yellow-100 dark:bg-yellow-500/20 text-yellow-700 dark:text-yellow-400",
  ],
  ["INFO", INFO_STYLE],
  ["DEBUG", "bg-gray-100 dark:bg-gray-500/20 text-gray-600 dark:text-gray-500"],
]);

export function levelStyle(level: string): string {
  return LEVEL_STYLES.get(level) ?? INFO_STYLE;
}
