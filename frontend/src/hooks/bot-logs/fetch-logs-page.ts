import { authFetch } from "@/lib/auth-fetch";
import { buildLogsSearchParams, expandLogEntries } from "@/lib/log-utils";
import type { LogEntry, LogsQuery } from "@/types/logs";

interface LogsResponse {
  data: LogEntry[];
  total: number;
  hasMore: boolean;
}

export interface LogsPage {
  /** One entry per non-empty line, in API order. */
  entries: LogEntry[];
  total: number;
  hasMore: boolean;
}

export async function fetchLogsPage(
  botId: string,
  query: LogsQuery,
  signal: AbortSignal,
): Promise<LogsPage> {
  const response = await authFetch(
    `/api/logs/${encodeURIComponent(botId)}?${buildLogsSearchParams(query).toString()}`,
    { signal },
  );

  if (!response.ok) {
    throw new Error(`Failed to fetch logs: ${response.statusText}`);
  }

  const result: LogsResponse = await response.json();
  return {
    entries: expandLogEntries(result.data),
    total: result.total,
    hasMore: result.hasMore,
  };
}
