import { Injectable, Scope, Inject } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { REQUEST } from "@nestjs/core";
import { PinoLogger } from "nestjs-pino";
import { BotService } from "@/bot/bot.service";
import { AuthenticatedRequest } from "@/auth/auth.types";
import {
  Result,
  Ok,
  Failure,
  errorMessage,
  hasErrorCode,
} from "@/common/result";
import { MINIO_CLIENT } from "@/minio";
import * as Minio from "minio";
import { LogEntry, runtimeKeyDay, storedLogEntry, utcDay } from "./log-entry";
import { LineSplitter } from "./line-splitter";

export type { LogEntry } from "./log-entry";

export interface LogsQuery {
  date?: string;
  dateRange?: string;
  // Latest mode: no fixed window - walk day files backwards from today
  // (up to lookbackDays) until `limit` entries are found, newest-first.
  lookbackDays?: number;
  limit: number;
  offset: number;
  /** Entry key: only entries older than that entry, within the window. */
  before?: string;
  type?: "all" | "metrics";
  sort?: "asc" | "desc";
  // Parsed datetime bounds (set by parseDateQuery for datetime ranges)
  startTime?: Date;
  endTime?: Date;
}

export interface LogsPage {
  entries: LogEntry[];
  hasMore: boolean;
  /** `before` for the next older page; null when the window holds none. */
  nextCursor: string | null;
}

interface DatedLogsRead {
  entries: LogEntry[];
  metricsHasMore: boolean;
}

interface PageRead {
  entries: LogEntry[];
  hasMore: boolean;
}

/** Entries before this byte position of this daily file are older. */
interface FilePosition {
  date: string;
  offset: number;
}

const POSITION_KEY = /^(\d{8}):(\d+)$/;

const dayLogPath = (botId: string, date: string): string =>
  `logs/${botId}/${date}.log`;

@Injectable({ scope: Scope.REQUEST })
export class LogsService {
  private static readonly TAIL_BYTES = 512 * 1024; // 512KB
  private logBucket: string;
  /** Byte offset of each entry read by this request, for cursors and repeats. */
  private readonly offsets = new WeakMap<LogEntry, number>();

  /** AWS/MinIO use "NoSuchKey", Cloudflare R2 uses "NotFound". */
  private static isNotFoundError(error: unknown): boolean {
    return (
      hasErrorCode(error) &&
      (error.code === "NoSuchKey" || error.code === "NotFound")
    );
  }

  constructor(
    @Inject(REQUEST) private readonly request: AuthenticatedRequest,
    @Inject(MINIO_CLIENT) private readonly minioClient: Minio.Client,
    private readonly configService: ConfigService,
    private readonly botService: BotService,
    private readonly logger: PinoLogger,
  ) {
    this.logBucket = this.configService.get<string>("LOG_BUCKET") || "bot-logs";
  }

  /** Whether the user (else the request's user) may read the bot's logs. */
  async checkAccess(
    botId: string,
    userId?: string,
  ): Promise<Result<true, string>> {
    const uid = userId || this.request?.user?.uid;
    if (!uid) return Failure("Authentication required");
    const botResult = await this.botService.findOneByUserId(uid, botId);
    if (!botResult.success) {
      return Failure("Bot not found or access denied");
    }
    return Ok(true);
  }

  async getLogs(
    botId: string,
    query: LogsQuery,
    userId?: string,
  ): Promise<Result<LogsPage, string>> {
    const access = await this.checkAccess(botId, userId);
    if (!access.success) return Failure(access.error);

    try {
      const read = await this.readPage(botId, query);
      if (!read.success) return Failure(read.error);
      return Ok(this.toPage(read.data, query));
    } catch (error: unknown) {
      this.logger.error(
        { err: error },
        this.isLatestMode(query)
          ? "Error fetching latest logs"
          : "Error fetching logs",
      );
      return Failure(`Failed to fetch logs: ${errorMessage(error)}`);
    }
  }

  private async readPage(
    botId: string,
    query: LogsQuery,
  ): Promise<Result<PageRead, string>> {
    if (query.before !== undefined) return this.readBefore(botId, query);

    // Latest mode: no explicit window - serve the newest entries regardless
    // of when the bot last ran (scheduled bots may not have run today).
    if (this.isLatestMode(query)) {
      const dates = this.generateLookbackDates(query.lookbackDays!);
      return Ok(await this.readNewestFirst(botId, dates, query));
    }

    const dates = this.parseDateQuery(query);
    if (!dates.length) return Failure(LogsService.INVALID_WINDOW);

    if (!this.isSingleDayTail(dates, query)) {
      return Ok(await this.readNewestFirst(botId, [...dates].reverse(), query));
    }
    const { entries } = await this.readDatedLogs(botId, dates, query);
    return Ok({ entries, hasMore: entries.length >= query.limit });
  }

  private static readonly INVALID_WINDOW =
    "Invalid date or dateRange format. Use YYYYMMDD, YYYYMMDD-YYYYMMDD, or ISO datetime range with -- separator";

  private isLatestMode(query: LogsQuery): boolean {
    return !query.date && !query.dateRange && !!query.lookbackDays;
  }

  private isSingleDayTail(dates: string[], query: LogsQuery): boolean {
    return dates.length === 1 && query.type !== "metrics" && !query.startTime;
  }

  /**
   * The `limit` entries just older than the cursor entry, walking the
   * window's days newest-first from the cursor's day.
   */
  private async readBefore(
    botId: string,
    query: LogsQuery,
  ): Promise<Result<PageRead, string>> {
    if (query.offset) {
      return Failure("before cannot be combined with offset");
    }
    const window = this.isLatestMode(query)
      ? this.generateLookbackDates(query.lookbackDays!)
      : this.parseDateQuery(query).reverse();
    if (!window.length) return Failure(LogsService.INVALID_WINDOW);

    const cursor = await this.resolveCursor(botId, query.before!);
    if (!cursor.success) return Failure(cursor.error);

    const dates = window.filter((date) => date <= cursor.data.date);
    return Ok(await this.readNewestFirst(botId, dates, query, cursor.data));
  }

  private async resolveCursor(
    botId: string,
    key: string,
  ): Promise<Result<FilePosition, string>> {
    const position = POSITION_KEY.exec(key);
    if (position && this.isValidDateFormat(position[1])) {
      return Ok({ date: position[1], offset: Number(position[2]) });
    }
    const date = runtimeKeyDay(key);
    if (!date) {
      return Failure("Invalid before cursor: expected a log entry key");
    }
    const offset = await this.findKeyOffset(botId, date, key);
    if (offset === null) {
      return Failure("The before cursor matches no log entry");
    }
    return Ok({ date, offset });
  }

  /** The first copy wins, matching dropRepeatedKeys. */
  private async findKeyOffset(
    botId: string,
    date: string,
    key: string,
  ): Promise<number | null> {
    const stream = await this.openLogStream(dayLogPath(botId, date));
    if (!stream) return null;
    const matches = (line: string) =>
      line.includes(key) && storedLogEntry(line, date, 0).key === key;

    const splitter = new LineSplitter();
    for await (const chunk of stream) {
      for (const { line, offset } of splitter.push(chunk)) {
        if (matches(line)) {
          (
            stream as NodeJS.ReadableStream & { destroy?: () => void }
          ).destroy?.();
          return offset;
        }
      }
    }
    const trailing = splitter.rest();
    return trailing && matches(trailing.line) ? trailing.offset : null;
  }

  private toPage(read: PageRead, query: LogsQuery): LogsPage {
    const entries = this.dropRepeatedKeys(read.entries);
    const oldest =
      query.sort === "asc" ? entries[0] : entries[entries.length - 1];
    const nextCursor =
      read.hasMore && oldest
        ? `${oldest.date}:${this.offsets.get(oldest)}`
        : null;
    return { entries, hasMore: read.hasMore, nextCursor };
  }

  /**
   * A storage append that completed but timed out is retried with the same
   * bytes, so a daily file can hold a runtime chunk twice. The first copy is
   * the entry; later copies are dropped.
   */
  private dropRepeatedKeys(entries: LogEntry[]): LogEntry[] {
    const firstAt = new Map<string, number>();
    for (const entry of entries) {
      const at = this.offsets.get(entry) ?? 0;
      const seen = firstAt.get(entry.key);
      if (seen === undefined || at < seen) firstAt.set(entry.key, at);
    }
    return entries.filter(
      (entry) => firstAt.get(entry.key) === (this.offsets.get(entry) ?? 0),
    );
  }

  private storedEntry(line: string, date: string, offset: number): LogEntry {
    const entry = storedLogEntry(line, date, offset);
    this.offsets.set(entry, offset);
    return entry;
  }

  private async readDatedLogs(
    botId: string,
    dates: string[],
    query: LogsQuery,
  ): Promise<DatedLogsRead> {
    const sortOrder = query.sort || "desc";

    if (this.isSingleDayTail(dates, query)) {
      // Single date, non-metrics: tail for the latest entries (file order)
      const entries: LogEntry[] = [];
      await this.tailFilteredLogs(
        dayLogPath(botId, dates[0]),
        dates[0],
        query,
        entries,
      );
      if (sortOrder === "desc") entries.reverse();
      return { entries, metricsHasMore: false };
    }

    // Multi-date or metrics: the newest entries, read from the window start
    const read = await this.streamDatesNewestFirst(botId, dates, query);
    if (sortOrder === "asc") read.entries.reverse();
    return read;
  }

  /**
   * Reads entries in chronological order, reverses to newest-first, then
   * applies offset and limit. Total reads are capped to prevent memory
   * exhaustion on huge ranges.
   */
  private async streamDatesNewestFirst(
    botId: string,
    dates: string[],
    query: LogsQuery,
  ): Promise<DatedLogsRead> {
    const entries: LogEntry[] = [];
    const maxRead = Math.max(query.offset + query.limit, 10000);
    const cappedQuery = { ...query, limit: maxRead };
    for (const date of dates) {
      await this.streamFilteredLogs(
        dayLogPath(botId, date),
        date,
        cappedQuery,
        entries,
      );
      if (entries.length >= maxRead) break;
    }
    entries.reverse();
    const metricsHasMore = entries.length > query.offset + query.limit;
    entries.splice(0, query.offset);
    if (entries.length > query.limit) entries.length = query.limit;
    return { entries, metricsHasMore };
  }

  /**
   * Walks day files newest-first and collects entries until offset+limit+1
   * are found. The +1 sentinel lets hasMore reflect real data instead of
   * guessing whether unscanned days would have contributed anything.
   * `bound` limits its day to the bytes before a cursor position.
   */
  private async readNewestFirst(
    botId: string,
    dates: string[],
    query: LogsQuery,
    bound?: FilePosition,
  ): Promise<PageRead> {
    const maxNeeded = query.offset + query.limit + 1;

    // Chronological accumulator: each older day is prepended as a block, so
    // reversing at the end yields newest-first without per-entry timestamps
    // (day files are already time-ordered internally).
    let collected: LogEntry[] = [];
    for (const date of dates) {
      const logPath = dayLogPath(botId, date);
      const end = bound?.date === date ? bound.offset : undefined;
      const dayEntries: LogEntry[] = [];
      if (query.type === "metrics") {
        // Metric lines can be anywhere in the day, so read all of it.
        await this.streamNewestLogs(
          logPath,
          date,
          query,
          dayEntries,
          maxNeeded,
          end,
        );
      } else {
        // Raw logs can be huge; tail keeps only the newest lines per day.
        const truncated = await this.tailFilteredLogs(
          logPath,
          date,
          { ...query, limit: maxNeeded },
          dayEntries,
          end,
        );
        if (truncated && dayEntries.length < maxNeeded) {
          // The tail window held fewer lines than needed (very long lines
          // or a deep offset). Skipping the rest of the day would silently
          // splice in older days' entries, so re-read the whole day.
          dayEntries.length = 0;
          await this.streamNewestLogs(
            logPath,
            date,
            query,
            dayEntries,
            maxNeeded,
            end,
          );
        }
      }
      // Keep only the newest maxNeeded lines per day so a huge day (many
      // metrics, or the full-read fallback above) can't grow the working
      // set beyond ~2x maxNeeded.
      if (dayEntries.length > maxNeeded) {
        dayEntries.splice(0, dayEntries.length - maxNeeded);
      }
      collected = dayEntries.concat(collected);
      if (collected.length >= maxNeeded) break;
    }

    const newestFirst = collected.reverse();
    const afterOffset = newestFirst.slice(query.offset);
    const hasMore = afterOffset.length > query.limit;
    const entries = afterOffset.slice(0, query.limit);
    if (query.sort === "asc") entries.reverse();
    return { entries, hasMore };
  }

  /** Newest-first YYYYMMDD list: today back through `days` UTC days. */
  private generateLookbackDates(days: number): string[] {
    const dates: string[] = [];
    const current = new Date();
    for (let i = 0; i < days; i++) {
      dates.push(utcDay(current));
      current.setUTCDate(current.getUTCDate() - 1);
    }
    return dates;
  }

  /** `end` limits the read to the file's bytes before that offset. */
  private async streamFilteredLogs(
    logPath: string,
    logDate: string,
    query: LogsQuery,
    entries: LogEntry[],
    end?: number,
  ): Promise<void> {
    const stream = await this.openLogStream(logPath, end);
    if (!stream) return;

    const splitter = new LineSplitter();
    for await (const chunk of stream) {
      for (const { line, offset } of splitter.push(chunk)) {
        if (!this.collectLine(line, offset, logDate, query, entries)) {
          continue;
        }
        if (query.type !== "metrics" && entries.length >= query.limit) {
          (
            stream as NodeJS.ReadableStream & { destroy?: () => void }
          ).destroy?.();
          return;
        }
      }
    }

    const trailing = splitter.rest();
    if (trailing) {
      this.collectTrailingLine(
        trailing.line,
        trailing.offset,
        logDate,
        query,
        entries,
      );
    }
  }

  /**
   * Reads the whole file (up to `end`) but keeps only its newest `keep`
   * matching entries, so memory stays bounded however long the day is.
   */
  private async streamNewestLogs(
    logPath: string,
    logDate: string,
    query: LogsQuery,
    entries: LogEntry[],
    keep: number,
    end?: number,
  ): Promise<void> {
    const stream = await this.openLogStream(logPath, end);
    if (!stream) return;

    const newest: LogEntry[] = [];
    const take = (line: string, offset: number) => {
      this.collectLine(line, offset, logDate, query, newest);
      // Trimming in batches keeps the splice cost linear overall.
      if (newest.length >= 2 * keep) newest.splice(0, newest.length - keep);
    };
    const splitter = new LineSplitter();
    for await (const chunk of stream) {
      for (const { line, offset } of splitter.push(chunk)) take(line, offset);
    }
    const trailing = splitter.rest();
    if (trailing) take(trailing.line, trailing.offset);

    if (newest.length > keep) newest.splice(0, newest.length - keep);
    entries.push(...newest);
  }

  private async openLogStream(
    logPath: string,
    end?: number,
  ): Promise<NodeJS.ReadableStream | null> {
    if (end === 0) return null;
    try {
      return end === undefined
        ? await this.minioClient.getObject(this.logBucket, logPath)
        : await this.minioClient.getPartialObject(
            this.logBucket,
            logPath,
            0,
            end,
          );
    } catch (error: unknown) {
      if (LogsService.isNotFoundError(error)) return null;
      throw error;
    }
  }

  /** @returns true when the line was appended to `entries`. */
  private collectLine(
    line: string,
    offset: number,
    logDate: string,
    query: LogsQuery,
    entries: LogEntry[],
  ): boolean {
    if (!this.passesLineFilters(line, query)) return false;
    const entry = this.storedEntry(line, logDate, offset);
    if (LogsService.isOutsideTimeWindow(entry, query)) return false;
    entries.push(entry);
    return true;
  }

  /**
   * Complete lines stop the stream once a raw-log read reaches its limit;
   * the content after the last newline has no stream left to stop, so the
   * limit is checked before taking it instead.
   */
  private collectTrailingLine(
    leftover: string,
    offset: number,
    logDate: string,
    query: LogsQuery,
    entries: LogEntry[],
  ): void {
    if (!this.passesLineFilters(leftover, query)) return;
    if (query.type !== "metrics" && entries.length >= query.limit) return;
    const entry = this.storedEntry(leftover, logDate, offset);
    if (LogsService.isOutsideTimeWindow(entry, query)) return;
    entries.push(entry);
  }

  private passesLineFilters(line: string, query: LogsQuery): boolean {
    if (!line.trim()) return false;
    return query.type !== "metrics" || LogsService.isMetricLine(line);
  }

  /** Metric detection parses JSON instead of string-matching "_metric",
   *  which also appears in ordinary log messages. */
  private static isMetricLine(line: string): boolean {
    try {
      return Boolean(JSON.parse(line)._metric);
    } catch {
      return false;
    }
  }

  /** Lines without a timestamp are kept: they cannot be placed in a window. */
  private static isOutsideTimeWindow(
    entry: LogEntry,
    query: LogsQuery,
  ): boolean {
    if (!query.startTime || !query.endTime) return false;
    const entryTime = entry.timestamp ? new Date(entry.timestamp) : null;
    return (
      entryTime !== null &&
      (entryTime < query.startTime || entryTime > query.endTime)
    );
  }

  /** `end` limits the read to the file's bytes before that offset.
   *  @returns true when the file was larger than the tail window, i.e. the
   *  read may have skipped earlier lines from this day. */
  private async tailFilteredLogs(
    logPath: string,
    logDate: string,
    query: LogsQuery,
    entries: LogEntry[],
    end?: number,
  ): Promise<boolean> {
    let stat: { size: number };
    try {
      stat = await this.minioClient.statObject(this.logBucket, logPath);
    } catch (error: unknown) {
      if (LogsService.isNotFoundError(error)) return false;
      throw error;
    }

    const fileSize = Math.min(stat.size ?? 0, end ?? Infinity);
    const start = Math.max(0, fileSize - LogsService.TAIL_BYTES);
    if (end !== undefined && fileSize === 0) return false;

    let stream: NodeJS.ReadableStream;
    try {
      if (end !== undefined) {
        stream = await this.minioClient.getPartialObject(
          this.logBucket,
          logPath,
          start,
          fileSize - start,
        );
      } else {
        stream =
          start > 0
            ? await this.minioClient.getPartialObject(
                this.logBucket,
                logPath,
                start,
              )
            : await this.minioClient.getObject(this.logBucket, logPath);
      }
    } catch (error: unknown) {
      // File may have been deleted between stat and read
      if (LogsService.isNotFoundError(error)) return false;
      throw error;
    }

    const splitter = new LineSplitter(start);
    const lines = [];
    for await (const chunk of stream) {
      lines.push(...splitter.push(chunk));
    }
    const trailing = splitter.rest();
    if (trailing) lines.push(trailing);
    // Drop first line if reading from mid-file (likely partial)
    if (start > 0) lines.shift();

    for (const { line, offset } of lines) {
      if (!line.trim()) continue;
      const entry = this.storedEntry(line, logDate, offset);
      if (LogsService.isOutsideTimeWindow(entry, query)) continue;
      entries.push(entry);
    }

    // Keep only the last `limit` entries (skip for metrics - return all)
    if (query.type !== "metrics" && entries.length > query.limit) {
      entries.splice(0, entries.length - query.limit);
    }

    return start > 0;
  }

  private parseDateQuery(query: LogsQuery): string[] {
    if (query.date) {
      if (this.isValidDateFormat(query.date)) {
        return [query.date];
      }
      return [];
    }

    if (query.dateRange) {
      // Datetime range: contains T and uses -- separator
      if (query.dateRange.includes("T")) {
        const parts = query.dateRange.split("--");
        if (parts.length !== 2) return [];
        const startTime = new Date(parts[0]);
        const endTime = new Date(parts[1]);
        if (isNaN(startTime.getTime()) || isNaN(endTime.getTime())) return [];

        // Store parsed times for line-level filtering
        query.startTime = startTime;
        query.endTime = endTime;

        return this.generateDateRange(utcDay(startTime), utcDay(endTime));
      }

      // Legacy YYYYMMDD-YYYYMMDD format
      const [startDate, endDate] = query.dateRange.split("-");
      if (
        !startDate ||
        !endDate ||
        !this.isValidDateFormat(startDate) ||
        !this.isValidDateFormat(endDate)
      ) {
        return [];
      }

      return this.generateDateRange(startDate, endDate);
    }

    return [];
  }

  private isValidDateFormat(date: string): boolean {
    const day = LogsService.parseDay(date);
    return day !== null && utcDay(day) === date;
  }

  /** Midnight UTC of a YYYYMMDD day; out-of-range parts roll over. */
  private static parseDay(date: string): Date | null {
    if (!/^\d{8}$/.test(date)) return null;
    return new Date(
      Date.UTC(
        parseInt(date.substring(0, 4)),
        parseInt(date.substring(4, 6)) - 1,
        parseInt(date.substring(6, 8)),
      ),
    );
  }

  private generateDateRange(startDate: string, endDate: string): string[] {
    const dates: string[] = [];
    const start = LogsService.parseDay(startDate);
    const end = LogsService.parseDay(endDate);
    if (!start || !end || start > end) {
      return [];
    }

    const current = new Date(start);
    while (current <= end) {
      dates.push(utcDay(current));
      current.setUTCDate(current.getUTCDate() + 1);
    }

    return dates;
  }
}
