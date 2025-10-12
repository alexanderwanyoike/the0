import { Injectable, Scope } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { BotService } from "@/bot/bot.service";
import { Result, Ok, Failure } from "@/common/result";
import * as Minio from "minio";

export interface LogsQuery {
  date?: string;
  dateRange?: string;
  limit: number;
  offset: number;
}

export interface LogEntry {
  date: string;
  content: string;
}

@Injectable({ scope: Scope.REQUEST })
export class LogsService {
  private minioClient: Minio.Client;
  private logBucket: string;

  constructor(
    private readonly configService: ConfigService,
    private readonly botService: BotService,
  ) {
    this.minioClient = new Minio.Client({
      endPoint: this.configService.get<string>("MINIO_ENDPOINT") || "localhost",
      port: parseInt(this.configService.get<string>("MINIO_PORT") || "9000"),
      useSSL: this.configService.get<string>("MINIO_USE_SSL") === "true",
      accessKey:
        this.configService.get<string>("MINIO_ACCESS_KEY") || "minioadmin",
      secretKey:
        this.configService.get<string>("MINIO_SECRET_KEY") || "minioadmin",
    });
    this.logBucket = this.configService.get<string>("LOG_BUCKET") || "bot-logs";
    console.log(
      `🪣 LogsService initialized with bucket: ${this.logBucket} (LOG_BUCKET env: ${this.configService.get<string>("LOG_BUCKET")})`,
    );
  }

  async getLogs(
    botId: string,
    query: LogsQuery,
  ): Promise<Result<LogEntry[], string>> {
    console.log(
      `🔍 LogsService.getLogs called with botId: ${botId}, query:`,
      query,
    );

    // Verify bot ownership
    const botResult = await this.botService.findOne(botId);
    if (!botResult.success) {
      console.log(`❌ Bot ownership verification failed for botId: ${botId}`);
      return Failure("Bot not found or access denied");
    }
    console.log(`✅ Bot ownership verified for botId: ${botId}`);

    // Generate date list from query
    const dates = this.parseDateQuery(query);
    console.log(`📅 Parsed dates from query:`, dates);
    if (!dates.length) {
      console.log(`❌ No valid dates found in query`);
      return Failure(
        "Invalid date or dateRange format. Use YYYYMMDD or YYYYMMDD-YYYYMMDD",
      );
    }

    try {
      const logEntries: LogEntry[] = [];

      for (const date of dates) {
        const logPath = `logs/${botId}/${date}.log`;
        console.log(
          `📁 Attempting to fetch log from path: ${logPath} in bucket: ${this.logBucket}`,
        );
        const logContent = await this.getLogContent(logPath);

        if (logContent.success && logContent.data) {
          console.log(
            `✅ Found log content for ${date}, length: ${logContent.data.length}`,
          );
          logEntries.push({
            date,
            content: logContent.data,
          });
        } else {
          console.log(
            `❌ No log content found for ${date}, error: ${logContent.error || "No data"}`,
          );
        }
      }

      // Apply pagination
      const paginatedLogs = logEntries.slice(
        query.offset,
        query.offset + query.limit,
      );

      console.log(
        `📄 Returning ${paginatedLogs.length} log entries (total found: ${logEntries.length})`,
      );
      return Ok(paginatedLogs);
    } catch (error: any) {
      console.error("Error fetching logs:", error);
      return Failure(`Failed to fetch logs: ${error.message}`);
    }
  }

  private async getLogContent(
    logPath: string,
  ): Promise<Result<string, string>> {
    try {
      // Check if object exists
      try {
        await this.minioClient.statObject(this.logBucket, logPath);
      } catch (error: any) {
        if (error.code === "NotFound") {
          return Ok(""); // Return empty content if log file doesn't exist
        }
        throw error;
      }

      // Get the object
      const stream = await this.minioClient.getObject(this.logBucket, logPath);

      // Convert stream to string
      const chunks: Buffer[] = [];
      for await (const chunk of stream) {
        chunks.push(chunk);
      }
      const content = Buffer.concat(chunks).toString("utf-8");

      return Ok(content);
    } catch (error: any) {
      console.error(`Error downloading log file ${logPath}:`, error);
      return Failure(`Failed to download log file: ${error.message}`);
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
