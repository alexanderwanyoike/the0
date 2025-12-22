/**
 * Event parser for bot logs.
 * Parses log lines into typed events (log or metric).
 */

export interface RawLogEntry {
  date: string;
  content: string;
}

export interface BotEvent {
  timestamp: Date;
  type: "log" | "metric";
  data: string | Record<string, unknown>;
  metricType?: string;
  level?: "DEBUG" | "INFO" | "WARN" | "ERROR";
  raw: string;
}

export interface MetricPayload {
  _metric: string;
  [key: string]: unknown;
}

/**
 * Extract timestamp from log line.
 * Expected format: [2006-01-02 15:04:05] content
 */
function extractTimestamp(content: string): Date {
  const match = content.match(/^\[(\d{4}-\d{2}-\d{2}\s\d{2}:\d{2}:\d{2})\]/);
  if (match) {
    return new Date(match[1].replace(" ", "T"));
  }
  return new Date();
}

/**
 * Strip timestamp prefix from log line.
 */
function stripTimestamp(content: string): string {
  return content.replace(/^\[\d{4}-\d{2}-\d{2}\s\d{2}:\d{2}:\d{2}\]\s*/, "");
}

/**
 * Extract log level from content.
 */
function extractLevel(
  content: string,
): "DEBUG" | "INFO" | "WARN" | "ERROR" | undefined {
  const match = content.match(/^(DEBUG|INFO|WARN(?:ING)?|ERROR):/i);
  if (match) {
    const level = match[1].toUpperCase();
    if (level === "WARNING") return "WARN";
    return level as "DEBUG" | "INFO" | "WARN" | "ERROR";
  }
  return undefined;
}

/**
 * Try to parse content as JSON metric.
 */
function tryParseMetric(content: string): MetricPayload | null {
  // Remove log level prefix if present
  const cleaned = content.replace(/^(DEBUG|INFO|WARN(?:ING)?|ERROR):\s*/i, "");

  // Try to find JSON in the content
  const jsonMatch = cleaned.match(/^(\{.*\})$/);
  if (!jsonMatch) return null;

  try {
    const parsed = JSON.parse(jsonMatch[1]);
    if (parsed && typeof parsed === "object" && "_metric" in parsed) {
      return parsed as MetricPayload;
    }
  } catch {
    // Not valid JSON
  }
  return null;
}

/**
 * Parse a single log line into a BotEvent.
 */
export function parseLogLine(content: string): BotEvent {
  const timestamp = extractTimestamp(content);
  const stripped = stripTimestamp(content);
  const level = extractLevel(stripped);

  // Try to parse as metric
  const metric = tryParseMetric(stripped);
  if (metric) {
    return {
      timestamp,
      type: "metric",
      metricType: metric._metric,
      data: metric,
      level,
      raw: content,
    };
  }

  // Regular log entry
  return {
    timestamp,
    type: "log",
    data: stripped,
    level: level || "INFO",
    raw: content,
  };
}

/**
 * Parse raw log entries into typed BotEvents.
 * Handles the API response format where content is the entire file.
 */
export function parseEvents(logs: RawLogEntry[]): BotEvent[] {
  const events: BotEvent[] = [];

  for (const entry of logs) {
    // Split content into individual lines
    const lines = entry.content.split("\n").filter((line) => line.trim());

    for (const line of lines) {
      events.push(parseLogLine(line));
    }
  }

  return events;
}

/**
 * Check if an event is a metric event.
 */
export function isMetricEvent(
  event: BotEvent,
): event is BotEvent & { type: "metric"; data: MetricPayload } {
  return event.type === "metric";
}

/**
 * Check if an event is a log event.
 */
export function isLogEvent(
  event: BotEvent,
): event is BotEvent & { type: "log"; data: string } {
  return event.type === "log";
}
