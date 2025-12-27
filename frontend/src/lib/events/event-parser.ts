/**
 * Event parser for bot logs.
 * Parses log lines into typed events (log or metric).
 */

import { parseISO, isValid } from "date-fns";

export interface RawLogEntry {
  date: string;
  content: string;
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
 * Structured log format (pino, winston, etc.)
 */
interface StructuredLog {
  level?: string;
  message?: string;
  msg?: string; // pino uses 'msg'
  time?: number | string; // pino uses Unix ms or ISO string
  timestamp?: string;
  ts?: number; // Unix timestamp
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
 * Try to parse content as structured JSON log (pino, winston, etc.)
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
    // Check if it's a structured log (has level, message, or msg) but NOT a metric
    if (
      parsed &&
      typeof parsed === "object" &&
      !("_metric" in parsed) &&
      ("level" in parsed || "message" in parsed || "msg" in parsed)
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
 */
function mapStructuredLogLevel(
  level: string | undefined,
): "DEBUG" | "INFO" | "WARN" | "ERROR" | undefined {
  if (!level) return undefined;
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
 * Parse a single log line into a BotEvent.
 */
export function parseLogLine(content: string): BotEvent {
  const timestamp = extractTimestamp(content);
  const stripped = stripTimestamp(content);
  const level = extractLevel(stripped);

  // 1. Try to parse as metric (existing behavior - highest priority)
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

  // 2. Try to parse as structured JSON log (pino, winston, etc.)
  const structuredLog = tryParseStructuredLog(stripped);
  if (structuredLog) {
    const structuredTimestamp =
      extractTimestampFromStructuredLog(structuredLog) || timestamp;
    const structuredLevel = mapStructuredLogLevel(structuredLog.level);
    const message =
      structuredLog.message ||
      structuredLog.msg ||
      JSON.stringify(structuredLog);

    return {
      timestamp: structuredTimestamp,
      type: "log",
      data: message,
      level: structuredLevel || level || "INFO",
      raw: content,
    };
  }

  // 3. Regular log entry (fallback)
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
