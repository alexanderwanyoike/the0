import type { LogEntry, LogsQuery, PageMerge } from "@/types/logs";

export const MAX_LOG_ENTRIES = 10000;

/** A date or date range pins the view to history, which live streaming
 *  cannot extend. */
export function isHistoricalQuery(query: LogsQuery): boolean {
  return !!(query.date || query.dateRange);
}

export function buildLogsSearchParams(query: LogsQuery): URLSearchParams {
  const searchParams = new URLSearchParams();
  if (query.dateRange) {
    searchParams.set("dateRange", query.dateRange);
  } else if (query.date) {
    searchParams.set("date", query.date);
  } else if (query.lookbackDays) {
    searchParams.set("lookbackDays", query.lookbackDays.toString());
  } else {
    searchParams.set(
      "date",
      new Date().toISOString().slice(0, 10).replace(/-/g, ""),
    );
  }
  if (query.limit) searchParams.set("limit", query.limit.toString());
  if (query.before) searchParams.set("before", query.before);
  if (query.type) searchParams.set("type", query.type);
  if (query.sort) searchParams.set("sort", query.sort);
  return searchParams;
}

/**
 * Live mode fetches sort=desc (latest N) but displays chronologically so SSE
 * appends land at the bottom. Appended pages are already in display order.
 */
export function toDisplayOrder(
  entries: LogEntry[],
  live: boolean,
  merge: PageMerge,
): LogEntry[] {
  return live && merge !== "append" ? [...entries].reverse() : entries;
}

/**
 * Join a page onto existing logs, skipping entries already present. Trimming
 * a full buffer keeps the side the page was added to.
 */
export function mergeUniqueEntries(
  prev: LogEntry[],
  page: LogEntry[],
  direction: "append" | "prepend",
): LogEntry[] {
  const existing = new Set(prev.map((l) => l.key));
  const unique = page.filter((l) => !existing.has(l.key));
  return direction === "append"
    ? [...prev, ...unique].slice(-MAX_LOG_ENTRIES)
    : [...unique, ...prev].slice(0, MAX_LOG_ENTRIES);
}

/** The api's type=metrics rule (logs.service isMetricLine), which history
 *  is filtered by, so live lines are filtered the same way. */
function isMetricLine(content: string): boolean {
  try {
    return Boolean(JSON.parse(content)._metric);
  } catch {
    return false;
  }
}

/**
 * Turn an SSE "update" payload (the entries of one live chunk) into log
 * entries. Throws on payloads that are not a list of entries, so the caller
 * decides how to report them.
 */
export function parseLiveUpdate(data: string, type?: string): LogEntry[] {
  const entries: unknown = JSON.parse(data);
  if (!Array.isArray(entries)) {
    throw new TypeError("Live update is not a list of log entries");
  }
  return type === "metrics"
    ? entries.filter((entry: LogEntry) => isMetricLine(entry.content))
    : entries;
}

export function formatLogsForExport(logs: LogEntry[]): string {
  return logs.map((log) => `[${log.date}] ${log.content}`).join("\n");
}
