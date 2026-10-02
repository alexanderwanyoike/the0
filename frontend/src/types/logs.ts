export interface LogEntry {
  date: string;
  content: string;
  /** API-extracted timestamp from NDJSON. Null/undefined for old-format logs. */
  timestamp?: string | null;
}

export interface LogsQuery {
  date?: string;
  dateRange?: string;
  /** Latest mode: newest entries across the last N days, regardless of when
   *  the bot last ran. Ignored when date/dateRange is set. */
  lookbackDays?: number;
  limit?: number;
  offset?: number;
  type?: string;
  sort?: "asc" | "desc";
}

/** How a fetched page joins the logs already held: replace them, or extend
 *  them at the end (next page) or the front (earlier logs). */
export type PageMerge = false | "append" | "prepend";
