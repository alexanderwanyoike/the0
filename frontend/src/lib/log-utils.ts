import type { LogEntry, LogsQuery, PageMerge } from "@/types/logs";

export const MAX_LOG_ENTRIES = 10000;

const entryKey = (l: LogEntry) => `${l.date}|${l.content}`;

/**
 * Split multi-line log entries into individual LogEntry items.
 * Preserves date, content, and timestamp on each expanded line.
 */
export function expandLogEntries(entries: LogEntry[]): LogEntry[] {
  const expanded: LogEntry[] = [];
  for (const entry of entries) {
    const lines = entry.content.split("\n").filter((l) => l.trim() !== "");
    for (const line of lines) {
      expanded.push({
        date: entry.date,
        content: line,
        timestamp: entry.timestamp,
      });
    }
  }
  return expanded;
}

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
  if (query.offset) searchParams.set("offset", query.offset.toString());
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
  const existing = new Set(prev.map(entryKey));
  const unique = page.filter((l) => !existing.has(entryKey(l)));
  return direction === "append"
    ? [...prev, ...unique].slice(-MAX_LOG_ENTRIES)
    : [...unique, ...prev].slice(0, MAX_LOG_ENTRIES);
}

/**
 * Turn an SSE "update" payload into log entries. Throws on payloads that are
 * not a log line, so the caller decides how to report them.
 */
export function parseLiveUpdate(data: string, type?: string): LogEntry[] {
  const parsed: { content: string; timestamp: string } = JSON.parse(data);
  const entries = expandLogEntries([
    {
      date: parsed.timestamp,
      content: parsed.content,
      timestamp: parsed.timestamp,
    },
  ]);
  // Mirror the server-side type=metrics filter (logs.service) for live
  // lines: history is filtered by the API, appends must match.
  if (type !== "metrics") return entries;
  return entries.filter((entry) => {
    try {
      return !!JSON.parse(entry.content)?._metric;
    } catch {
      return false;
    }
  });
}

export function formatLogsForExport(logs: LogEntry[]): string {
  return logs.map((log) => `[${log.date}] ${log.content}`).join("\n");
}
