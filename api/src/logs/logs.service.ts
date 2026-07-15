import { Injectable, Scope, Inject } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { REQUEST } from "@nestjs/core";
import { PinoLogger } from "nestjs-pino";
import { BotService } from "@/bot/bot.service";
import { AuthenticatedRequest } from "@/auth/auth.types";
import { Result, Ok, Failure, errorMessage, hasErrorCode } from "@/common/result";
import { MINIO_CLIENT } from "@/minio";
import * as Minio from "minio";

export interface LogsQuery {
  date?: string;
  dateRange?: string;
  // Latest mode: no fixed window - walk day files backwards from today
  // (up to lookbackDays) until `limit` entries are found, newest-first.
  lookbackDays?: number;
  limit: number;
  offset: number;
  type?: "all" | "metrics";
  sort?: "asc" | "desc";
  // Parsed datetime bounds (set by parseDateQuery for datetime ranges)
  startTime?: Date;
  endTime?: Date;
}

export interface LogEntry {
  date: string;
  content: string;
  timestamp: string | null;
}

@Injectable({ scope: Scope.REQUEST })
export class LogsService {
  private static readonly TAIL_BYTES = 512 * 1024; // 512KB
  private logBucket: string;

  /** AWS/MinIO use "NoSuchKey", Cloudflare R2 uses "NotFound". */
  private static isNotFoundError(error: unknown): boolean {
    return hasErrorCode(error) && (error.code === "NoSuchKey" || error.code === "NotFound");
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

  async getLogs(
    botId: string,
    query: LogsQuery,
    userId?: string,
  ): Promise<Result<{ entries: LogEntry[]; hasMore: boolean }, string>> {
    // Verify bot ownership
    const uid = userId || this.request?.user?.uid;
    if (!uid) return Failure("Authentication required");
    const botResult = await this.botService.findOneByUserId(uid, botId);
    if (!botResult.success) {
      return Failure("Bot not found or access denied");
    }

    // Latest mode: no explicit window - serve the newest entries regardless
    // of when the bot last ran (scheduled bots may not have run today).
    if (!query.date && !query.dateRange && query.lookbackDays) {
      try {
        return Ok(await this.getLatestLogs(botId, query));
      } catch (error: unknown) {
        this.logger.error({ err: error }, "Error fetching latest logs");
        return Failure(`Failed to fetch logs: ${errorMessage(error)}`);
      }
    }

    // Generate date list from query
    const dates = this.parseDateQuery(query);
    if (!dates.length) {
      return Failure(
        "Invalid date or dateRange format. Use YYYYMMDD, YYYYMMDD-YYYYMMDD, or ISO datetime range with -- separator",
      );
    }

    try {
      const entries: LogEntry[] = [];

      const sortOrder = query.sort || "desc";

      if (dates.length === 1 && query.type !== "metrics" && !query.startTime) {
        // Single date, non-metrics: tail for latest entries (returns newest-first)
        const logPath = `logs/${botId}/${dates[0]}.log`;
        await this.tailFilteredLogs(logPath, dates[0], query, entries);
        if (sortOrder === "asc") entries.reverse();
      } else {
        // Multi-date or metrics: stream from start (returns oldest-first)
        const skipped = { count: 0 };
        if (sortOrder === "desc" && query.type !== "metrics") {
          // For desc on the stream path, read entries in chronological order,
          // reverse to newest-first, then apply offset and limit.
          // Cap total reads to prevent memory exhaustion on huge ranges.
          const maxRead = Math.max(query.offset + query.limit, 10000);
          const cappedQuery = { ...query, offset: 0, limit: maxRead };
          for (const date of dates) {
            const logPath = `logs/${botId}/${date}.log`;
            await this.streamFilteredLogs(logPath, date, cappedQuery, entries, skipped);
            if (entries.length >= maxRead) break;
          }
          entries.reverse();
          entries.splice(0, query.offset);
          if (entries.length > query.limit) entries.length = query.limit;
        } else {
          for (const date of dates) {
            const logPath = `logs/${botId}/${date}.log`;
            await this.streamFilteredLogs(logPath, date, query, entries, skipped);
            if (query.type !== "metrics" && entries.length >= query.limit) break;
          }
          if (sortOrder === "desc") entries.reverse();
        }
      }

      const hasMore = query.type !== "metrics" && entries.length >= query.limit;
      return Ok({ entries, hasMore });
    } catch (error: unknown) {
      this.logger.error({ err: error }, "Error fetching logs");
      return Failure(`Failed to fetch logs: ${errorMessage(error)}`);
    }
  }

  /**
   * Latest mode: walk day files newest-first (today back through
   * lookbackDays) and collect entries until offset+limit+1 are found. The
   * +1 sentinel lets hasMore reflect real data instead of guessing whether
   * unscanned days would have contributed anything.
   */
  private async getLatestLogs(
    botId: string,
    query: LogsQuery,
  ): Promise<{ entries: LogEntry[]; hasMore: boolean }> {
    const dates = this.generateLookbackDates(query.lookbackDays!);
    const maxNeeded = query.offset + query.limit + 1;

    // Chronological accumulator: each older day is prepended as a block, so
    // reversing at the end yields newest-first without per-entry timestamps
    // (day files are already time-ordered internally).
    let collected: LogEntry[] = [];
    for (const date of dates) {
      const logPath = `logs/${botId}/${date}.log`;
      const dayEntries: LogEntry[] = [];
      if (query.type === "metrics") {
        // Metric lines are sparse; stream the whole day file. Offset is
        // deliberately zeroed - pagination applies to the flattened result.
        await this.streamFilteredLogs(
          logPath,
          date,
          { ...query, offset: 0 },
          dayEntries,
          { count: 0 },
        );
      } else {
        // Raw logs can be huge; tail keeps only the newest lines per day.
        const truncated = await this.tailFilteredLogs(
          logPath,
          date,
          { ...query, offset: 0, limit: maxNeeded },
          dayEntries,
        );
        if (truncated && dayEntries.length < maxNeeded) {
          // The tail window held fewer lines than needed (very long lines
          // or a deep offset). Skipping the rest of the day would silently
          // splice in older days' entries, so re-read the whole day capped
          // the same way the dateRange desc path caps its reads.
          dayEntries.length = 0;
          await this.streamFilteredLogs(
            logPath,
            date,
            { ...query, offset: 0, limit: Math.max(maxNeeded, 10000) },
            dayEntries,
            { count: 0 },
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

  /** Newest-first YYYYMMDD list: today back through `days` days (local time,
   *  matching the runtime's day bucketing of log files). */
  private generateLookbackDates(days: number): string[] {
    const dates: string[] = [];
    const current = new Date();
    for (let i = 0; i < days; i++) {
      dates.push(
        current.getFullYear().toString() +
          (current.getMonth() + 1).toString().padStart(2, "0") +
          current.getDate().toString().padStart(2, "0"),
      );
      current.setDate(current.getDate() - 1);
    }
    return dates;
  }

  private async streamFilteredLogs(
    logPath: string,
    logDate: string,
    query: LogsQuery,
    entries: LogEntry[],
    skipped: { count: number },
  ): Promise<void> {
    let stream: NodeJS.ReadableStream;
    try {
      stream = await this.minioClient.getObject(this.logBucket, logPath);
    } catch (error: unknown) {
      if (LogsService.isNotFoundError(error)) return;
      throw error;
    }

    let leftover = "";
    for await (const chunk of stream) {
      const text = leftover + chunk.toString("utf-8");
      const lines = text.split("\n");
      leftover = lines.pop() || "";

      for (const line of lines) {
        if (!line.trim()) continue;

        // Filter by type using JSON parsing
        if (query.type === "metrics") {
          try {
            const parsed = JSON.parse(line);
            if (!parsed._metric) continue;
          } catch {
            continue;
          }
        }

        // Handle offset
        if (skipped.count < query.offset) {
          skipped.count++;
          continue;
        }

        const normalized = this.normalizeLine(line, logDate);

        // Filter by datetime window if set
        if (query.startTime && query.endTime) {
          const entryTime = normalized.timestamp
            ? new Date(normalized.timestamp)
            : null;
          if (
            entryTime &&
            (entryTime < query.startTime || entryTime > query.endTime)
          ) {
            continue; // Outside time window
          }
          // Lines without timestamps are included (can't filter, don't exclude)
        }

        entries.push(normalized);

        if (query.type !== "metrics" && entries.length >= query.limit) {
          (stream as NodeJS.ReadableStream & { destroy?: () => void }).destroy?.();
          return;
        }
      }
    }

    // Handle leftover line (content after last newline)
    if (leftover.trim()) {
      let includeLeftover = true;
      if (query.type === "metrics") {
        try {
          const parsed = JSON.parse(leftover);
          if (!parsed._metric) includeLeftover = false;
        } catch {
          includeLeftover = false;
        }
      }

      if (includeLeftover) {
        if (skipped.count < query.offset) {
          skipped.count++;
        } else if (query.type === "metrics" || entries.length < query.limit) {
          const normalized = this.normalizeLine(leftover, logDate);

          // Filter by datetime window if set
          if (query.startTime && query.endTime) {
            const entryTime = normalized.timestamp
              ? new Date(normalized.timestamp)
              : null;
            if (
              entryTime &&
              (entryTime < query.startTime || entryTime > query.endTime)
            ) {
              // Outside time window - skip
            } else {
              entries.push(normalized);
            }
          } else {
            entries.push(normalized);
          }
        }
      }
    }
  }

  /** @returns true when the file was larger than the tail window, i.e. the
   *  read may have skipped earlier lines from this day. */
  private async tailFilteredLogs(
    logPath: string,
    logDate: string,
    query: LogsQuery,
    entries: LogEntry[],
  ): Promise<boolean> {
    let stat: { size: number };
    try {
      stat = await this.minioClient.statObject(this.logBucket, logPath);
    } catch (error: unknown) {
      if (LogsService.isNotFoundError(error)) return false;
      throw error;
    }

    const fileSize = stat.size;
    const start = Math.max(0, fileSize - LogsService.TAIL_BYTES);

    let stream: NodeJS.ReadableStream;
    try {
      stream =
        start > 0
          ? await this.minioClient.getPartialObject(
              this.logBucket,
              logPath,
              start,
            )
          : await this.minioClient.getObject(this.logBucket, logPath);
    } catch (error: unknown) {
      // File may have been deleted between stat and read
      if (LogsService.isNotFoundError(error)) return false;
      throw error;
    }

    const chunks: Buffer[] = [];
    for await (const chunk of stream) {
      chunks.push(Buffer.isBuffer(chunk) ? chunk : Buffer.from(chunk));
    }
    const content = Buffer.concat(chunks).toString("utf-8");

    const lines = content.split("\n");
    // Drop first line if reading from mid-file (likely partial)
    if (start > 0) lines.shift();

    for (const line of lines) {
      if (!line.trim()) continue;
      const normalized = this.normalizeLine(line, logDate);

      // Filter by datetime window if set
      if (query.startTime && query.endTime) {
        const entryTime = normalized.timestamp
          ? new Date(normalized.timestamp)
          : null;
        if (
          entryTime &&
          (entryTime < query.startTime || entryTime > query.endTime)
        ) {
          continue; // Outside time window
        }
        // Lines without timestamps are included (can't filter, don't exclude)
      }

      entries.push(normalized);
    }

    // Keep only the last `limit` entries (skip for metrics - return all)
    if (query.type !== "metrics" && entries.length > query.limit) {
      entries.splice(0, entries.length - query.limit);
    }

    return start > 0;
  }

  private normalizeLine(line: string, logDate: string): LogEntry {
    try {
      const parsed = JSON.parse(line);
      const timestamp = parsed.timestamp || null;

      if (parsed._metric) {
        // Metric line - keep full JSON as content for frontend metric rendering
        return { date: logDate, content: line, timestamp };
      }

      // NDJSON wrapped text - extract message for clean display
      const content = parsed.message || line;
      return { date: logDate, content, timestamp };
    } catch {
      // Old format or plain text - not JSON
      return { date: logDate, content: line, timestamp: null };
    }
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

        // Generate date list from the datetime range
        const startDate = parts[0].slice(0, 10).replace(/-/g, "");
        const endDate = parts[1].slice(0, 10).replace(/-/g, "");
        return this.generateDateRange(startDate, endDate);
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
    const dateRegex = /^\d{8}$/;
    if (!dateRegex.test(date)) {
      return false;
    }

    const year = parseInt(date.substring(0, 4));
    const month = parseInt(date.substring(4, 6));
    const day = parseInt(date.substring(6, 8));

    const dateObj = new Date(year, month - 1, day);
    return (
      dateObj.getFullYear() === year &&
      dateObj.getMonth() === month - 1 &&
      dateObj.getDate() === day
    );
  }

  private generateDateRange(startDate: string, endDate: string): string[] {
    const dates: string[] = [];
    const start = new Date(
      parseInt(startDate.substring(0, 4)),
      parseInt(startDate.substring(4, 6)) - 1,
      parseInt(startDate.substring(6, 8)),
    );
    const end = new Date(
      parseInt(endDate.substring(0, 4)),
      parseInt(endDate.substring(4, 6)) - 1,
      parseInt(endDate.substring(6, 8)),
    );

    if (start > end) {
      return [];
    }

    const current = new Date(start);
    while (current <= end) {
      const dateStr =
        current.getFullYear().toString() +
        (current.getMonth() + 1).toString().padStart(2, "0") +
        current.getDate().toString().padStart(2, "0");
      dates.push(dateStr);
      current.setDate(current.getDate() + 1);
    }

    return dates;
  }
}
