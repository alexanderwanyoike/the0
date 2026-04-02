/**
 * Event parser for bot logs.
 * Parses log lines into typed events (log or metric).
 */

import { parseISO, isValid } from "date-fns";

export interface RawLogEntry {
  date: string;
  content: string;
  /** API-extracted timestamp from NDJSON. Null for old-format logs. */
  timestamp?: string | null;
}

export interface BotEvent {
  timestamp: Date | null;
  type: "log" | "metric";
  data: string | Record<string, unknown>;
  metricType?: string;
  level?: "DEBUG" | "INFO" | "WARN" | "ERROR";
  raw: string;
}

/**
 * Structured log format (pino, winston, Rust tracing, etc.)
 */
interface StructuredLog {
  level?: string;
  message?: string;
  msg?: string; // pino uses 'msg'
  time?: number | string; // pino uses Unix ms or ISO string
  timestamp?: string;
  ts?: number; // Unix timestamp
  fields?: { message?: string; [key: string]: unknown }; // Rust tracing uses fields.message
  target?: string; // Rust tracing target
  [key: string]: unknown;
}

export interface MetricPayload {
  _metric: string;
  [key: string]: unknown;
}

/**
 * Extract timestamp from log line.
 * Expected format: [2006-01-02 15:04:05] content
 * Returns null if no timestamp found.
 */
function extractTimestamp(content: string): Date | null {
  const match = content.match(/^\[(\d{4}-\d{2}-\d{2}\s\d{2}:\d{2}:\d{2})\]/);
  if (match) {
    return new Date(match[1].replace(" ", "T"));
  }
  return null;
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
 * Try to parse content as structured JSON log (pino, winston, Rust tracing, etc.)
 * Returns null if not a structured log or if it's a metric.
 */
function tryParseStructuredLog(content: string): StructuredLog | null {
  // Remove log level prefix if present
  const cleaned = content.replace(/^(DEBUG|INFO|WARN(?:ING)?|ERROR):\s*/i, "");

  // Try to find JSON in the content
  const jsonMatch = cleaned.match(/^(\{.*\})$/);
  if (!jsonMatch) return null;

  try {
    const parsed = JSON.parse(jsonMatch[1]);
    // Check if it's a structured log but NOT a metric
    if (
      parsed &&
      typeof parsed === "object" &&
      !("_metric" in parsed) &&
      ("level" in parsed ||
        "message" in parsed ||
        "msg" in parsed ||
        ("fields" in parsed && parsed.fields?.message)) // Rust tracing format
    ) {
      return parsed as StructuredLog;
    }
  } catch {
    // Not valid JSON
  }
  return null;
}

/**
 * Parse a timestamp value that could be:
 * - Unix ms (number)
 * - Unix seconds (number < 10 billion)
 * - ISO string
 * - Unix ms as string (e.g., "1735312800000" or "1735312800000Z")
 */
function parseTimestampValue(value: unknown): Date | null {
  if (value === undefined || value === null) return null;

  // Handle numbers (Unix timestamp)
  if (typeof value === "number") {
    // If < 10 billion, assume seconds; otherwise ms
    const ms = value < 10_000_000_000 ? value * 1000 : value;
    return new Date(ms);
  }

  // Handle strings
  if (typeof value === "string") {
    // Try numeric string (with optional Z suffix for legacy C++ SDK)
    const numMatch = value.match(/^(\d+)Z?$/);
    if (numMatch) {
      const ms = parseInt(numMatch[1], 10);
      return new Date(ms < 10_000_000_000 ? ms * 1000 : ms);
    }

    // Try ISO format with date-fns
    const parsed = parseISO(value);
    if (isValid(parsed)) {
      return parsed;
    }
  }

  return null;
}

/**
 * Extract timestamp from structured log.
 * Checks common timestamp fields: time, timestamp, ts
 */
function extractTimestampFromStructuredLog(log: StructuredLog): Date | null {
  // Try fields in order of preference
  return (
    parseTimestampValue(log.time) ??
    parseTimestampValue(log.timestamp) ??
    parseTimestampValue(log.ts)
  );
}

/**
 * Map structured log level to our level type.
 * Handles both string levels (e.g., "info") and numeric levels (e.g., 30 from pino).
 */
function mapStructuredLogLevel(
  level: string | number | undefined,
): "DEBUG" | "INFO" | "WARN" | "ERROR" | undefined {
  if (level === undefined || level === null) return undefined;

  // Handle numeric levels (pino style)
  if (typeof level === "number") {
    if (level <= 20) return "DEBUG";
    if (level <= 30) return "INFO";
    if (level <= 40) return "WARN";
    return "ERROR";
  }

  // Handle string levels
  if (typeof level !== "string") return undefined;
  const upper = level.toUpperCase();
  switch (upper) {
    case "TRACE":
    case "DEBUG":
    case "10":
    case "20":
      return "DEBUG";
    case "INFO":
    case "30":
      return "INFO";
    case "WARN":
    case "WARNING":
    case "40":
      return "WARN";
    case "ERROR":
    case "FATAL":
    case "50":
    case "60":
      return "ERROR";
    default:
      return undefined;
  }
}

/**
 * Resolve the effective timestamp for a parsed event.
 * Priority: API timestamp (from NDJSON) > parser-extracted timestamp > null.
 */
function resolveTimestamp(
  apiTimestamp: string | null | undefined,
  extracted: Date | null,
): Date | null {
  if (apiTimestamp) {
    const parsed = parseISO(apiTimestamp);
    if (isValid(parsed)) return parsed;
    // Try Date constructor as fallback for non-ISO formats
    const fallback = new Date(apiTimestamp);
    if (!isNaN(fallback.getTime())) return fallback;
  }
  return extracted;
}

/**
 * Each parser in the chain returns a BotEvent or null if it cannot handle the format.
 */
type LogLineParser = (
  content: string,
  apiTimestamp?: string | null,
) => BotEvent | null;

/**
 * Parse metric JSON: content is a JSON object with a _metric field.
 * Handles optional log-level prefix before the JSON (e.g. "INFO: {...}").
 */
const parseMetricJSON: LogLineParser = (content, apiTimestamp) => {
  const stripped = stripTimestamp(content);
  const level = extractLevel(stripped);
  const metric = tryParseMetric(stripped);
  if (!metric) return null;

  const bracketTs = extractTimestamp(content);
  return {
    timestamp: resolveTimestamp(apiTimestamp, bracketTs),
    type: "metric",
    metricType: metric._metric,
    data: metric,
    level,
    raw: content,
  };
};

/**
 * Parse structured JSON log (pino, winston, Rust tracing, etc.).
 * Content is a JSON object with level/message/msg fields but no _metric.
 */
const parseStructuredJSON: LogLineParser = (content, apiTimestamp) => {
  const stripped = stripTimestamp(content);
  const prefixLevel = extractLevel(stripped);
  const structuredLog = tryParseStructuredLog(stripped);
  if (!structuredLog) return null;

  const bracketTs = extractTimestamp(content);
  const structuredTs = extractTimestampFromStructuredLog(structuredLog);
  // API timestamp > structured log timestamp > bracket timestamp
  const effectiveTs = resolveTimestamp(
    apiTimestamp,
    structuredTs ?? bracketTs,
  );
  const structuredLevel = mapStructuredLogLevel(structuredLog.level);

  const message =
    structuredLog.message ||
    structuredLog.msg ||
    structuredLog.fields?.message ||
    JSON.stringify(structuredLog);

  return {
    timestamp: effectiveTs,
    type: "log",
    data: message,
    level: structuredLevel || prefixLevel || "INFO",
    raw: content,
  };
};

/**
 * Parse bracket-prefixed log lines: [2006-01-02 15:04:05] content
 * Only matches when the content starts with a bracket timestamp.
 */
const parseBracketLog: LogLineParser = (content, apiTimestamp) => {
  const bracketTs = extractTimestamp(content);
  if (!bracketTs) return null;

  const stripped = stripTimestamp(content);
  const level = extractLevel(stripped);

  return {
    timestamp: resolveTimestamp(apiTimestamp, bracketTs),
    type: "log",
    data: stripped,
    level: level || "INFO",
    raw: content,
  };
};

/**
 * Fallback parser for plain text log lines.
 * Always succeeds -- this is the last parser in the chain.
 */
const parsePlainText: LogLineParser = (content, apiTimestamp) => {
  const level = extractLevel(content);

  return {
    timestamp: resolveTimestamp(apiTimestamp, null),
    type: "log",
    data: content,
    level: level || "INFO",
    raw: content,
  };
};

/**
 * Ordered parser chain. The first parser that returns non-null wins.
 */
const parsers: LogLineParser[] = [
  parseMetricJSON,
  parseStructuredJSON,
  parseBracketLog,
  parsePlainText,
];

/**
 * Parse a single log line into a BotEvent.
 *
 * @param content  The log line content (may include bracket timestamps, JSON, etc.)
 * @param timestamp  Optional API-provided timestamp (from NDJSON). When present,
 *                   takes precedence over any timestamp extracted from content.
 *                   Pass null or omit for old-format / backward-compatible usage.
 */
export function parseLogLine(
  content: string,
  timestamp?: string | null,
): BotEvent {
  for (const parser of parsers) {
    const result = parser(content, timestamp);
    if (result) return result;
  }

  // Should never reach here since parsePlainText always succeeds,
  // but TypeScript needs a return.
  return {
    timestamp: resolveTimestamp(timestamp, null),
    type: "log",
    data: content,
    level: "INFO",
    raw: content,
  };
}

/**
 * Parse raw log entries into typed BotEvents.
 * Handles both new NDJSON format (one line per entry with timestamp)
 * and old format (multi-line content without timestamp).
 */
export function parseEvents(logs: RawLogEntry[]): BotEvent[] {
  const events: BotEvent[] = [];

  for (const entry of logs) {
    // Split content into individual lines
    const lines = entry.content.split("\n").filter((line) => line.trim());

    for (const line of lines) {
      events.push(parseLogLine(line, entry.timestamp));
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
