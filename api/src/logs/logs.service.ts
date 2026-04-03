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
  limit: number;
  offset: number;
  type?: "all" | "metrics";
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

    // Generate date list from query
    const dates = this.parseDateQuery(query);
    if (!dates.length) {
      return Failure(
        "Invalid date or dateRange format. Use YYYYMMDD, YYYYMMDD-YYYYMMDD, or ISO datetime range with -- separator",
      );
    }

    try {
      const entries: LogEntry[] = [];

      if (dates.length === 1 && query.type !== "metrics" && !query.startTime) {
        // Single date, non-metrics: tail for latest entries
        const logPath = `logs/${botId}/${dates[0]}.log`;
        await this.tailFilteredLogs(logPath, dates[0], query, entries);
      } else {
        // Multi-date or metrics: stream from start, chronological
        const skipped = { count: 0 };
        for (const date of dates) {
          const logPath = `logs/${botId}/${date}.log`;
          await this.streamFilteredLogs(logPath, date, query, entries, skipped);
          if (query.type !== "metrics" && entries.length >= query.limit) break;
        }
      }

      const hasMore = query.type !== "metrics" && entries.length >= query.limit;
      return Ok({ entries, hasMore });
    } catch (error: unknown) {
      this.logger.error({ err: error }, "Error fetching logs");
      return Failure(`Failed to fetch logs: ${errorMessage(error)}`);
    }
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
      if (hasErrorCode(error) && error.code === "NoSuchKey") return;
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

  private async tailFilteredLogs(
    logPath: string,
    logDate: string,
    query: LogsQuery,
    entries: LogEntry[],
  ): Promise<void> {
    let stat: { size: number };
    try {
      stat = await this.minioClient.statObject(this.logBucket, logPath);
    } catch (error: unknown) {
      if (hasErrorCode(error) && error.code === "NoSuchKey") return;
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
      if (hasErrorCode(error) && error.code === "NoSuchKey") return;
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
