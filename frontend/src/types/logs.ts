/** One log line, in the same shape from REST and from the live stream. */
export interface LogEntry {
  /** Stable across REST and SSE: two entries with one key are one line. */
  key: string;
  /** YYYYMMDD, the UTC day of the daily log file. */
  date: string;
  /** The line's own timestamp, null when it has none. */
  timestamp: string | null;
  /** The stored line, verbatim. */
  content: string;
}

export interface LogsQuery {
  date?: string;
  dateRange?: string;
  /** Latest mode: newest entries across the last N days, regardless of when
   *  the bot last ran. Ignored when date/dateRange is set. */
  lookbackDays?: number;
  limit?: number;
  /** Entry key: only entries older than it. */
  before?: string;
  type?: string;
  sort?: "asc" | "desc";
}

/** How a fetched page joins the logs already held: replace them, or extend
 *  them at the end (next page) or the front (earlier logs). */
export type PageMerge = false | "append" | "prepend";
