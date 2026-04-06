import { LogEntry } from "@/components/bot/console-interface";

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
