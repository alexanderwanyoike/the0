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

/**
 * The runtime's `<unix ns>-<index>` form; a nanosecond time has at least 16
 * digits. Files from before the runtime stamped keys can hold a `_key` a bot
 * logged itself, which is not unique.
 */
const RUNTIME_KEY = /^(\d{16,})-\d+$/;

function parseLine(line: string): ParsedLine {
  try {
    const parsed = JSON.parse(line);
    if (parsed === null || typeof parsed !== "object") {
      return { key: null, timestamp: null };
    }
    const key = parsed._key;
    return {
      key: typeof key === "string" && RUNTIME_KEY.test(key) ? key : null,
      timestamp: typeof parsed.timestamp === "string" ? parsed.timestamp : null,
    };
  } catch {
    return { key: null, timestamp: null };
  }
}

/** UTC calendar day as YYYYMMDD, the naming of daily log files. */
export const utcDay = (date: Date): string =>
  date.toISOString().slice(0, 10).replace(/-/g, "");

/**
 * The runtime names a chunk's daily file after the UTC day of the chunk time
 * it puts in every `_key` of that chunk, so a live line can name its file.
 */
export function runtimeKeyDay(key: string): string | null {
  const match = RUNTIME_KEY.exec(key);
  if (!match) return null;
  const millis = Number(match[1].slice(0, -6));
  const day = new Date(millis);
  return isNaN(day.getTime()) ? null : utcDay(day);
}

/**
 * Entries for one NATS message: a chunk of newline-terminated lines.
 *
 * Transitional: a runtime from before keyed lines publishes raw chunks, so a
 * line without `_key` gets a key unique to this process and the api's receive
 * time. Those lines can never match a stored entry; the path can go once no
 * such runtime is left.
 */
export function liveLogEntries(
  chunk: string,
  receivedAt: Date,
  chunkId: number,
): LogEntry[] {
  const entries: LogEntry[] = [];
  chunk.split("\n").forEach((line, index) => {
    if (!line.trim()) return;
    const { key, timestamp } = parseLine(line);
    if (key) {
      entries.push({
        key,
        date: runtimeKeyDay(key) ?? utcDay(receivedAt),
        timestamp,
        content: line,
      });
      return;
    }
    entries.push({
      key: `live:${receivedAt.getTime()}-${chunkId}-${index}`,
      date: utcDay(receivedAt),
      timestamp: timestamp ?? receivedAt.toISOString(),
      content: line,
    });
  });
  return entries;
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
