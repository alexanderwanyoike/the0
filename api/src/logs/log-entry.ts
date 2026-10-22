/**
 * One log line as both REST and SSE return it. `content` is the line exactly
 * as the runtime wrote it; parsing it for display is the client's job.
 */
export interface LogEntry {
  /**
   * Stable across reads and across REST and SSE. The runtime's `_key` when
   * the line has one, else `<YYYYMMDD>:<byte offset of the line>` for lines
   * stored before the runtime stamped keys.
   */
  key: string;
  /** YYYYMMDD of the daily log file the line belongs to. */
  date: string;
  /** The line's own `timestamp` when it is a string, else null. */
  timestamp: string | null;
  content: string;
}

interface ParsedLine {
  key: string | null;
  timestamp: string | null;
}

function parseLine(line: string): ParsedLine {
  try {
    const parsed = JSON.parse(line);
    if (parsed === null || typeof parsed !== "object") {
      return { key: null, timestamp: null };
    }
    return {
      key: typeof parsed._key === "string" ? parsed._key : null,
      timestamp: typeof parsed.timestamp === "string" ? parsed.timestamp : null,
    };
  } catch {
    return { key: null, timestamp: null };
  }
}

export function storedLogEntry(
  line: string,
  date: string,
  offset: number,
): LogEntry {
  const { key, timestamp } = parseLine(line);
  return {
    key: key ?? `${date}:${offset}`,
    date,
    timestamp,
    content: line,
  };
}
