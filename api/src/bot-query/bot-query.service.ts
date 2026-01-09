import { Injectable, Scope } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { PinoLogger } from "nestjs-pino";
import { BotService } from "@/bot/bot.service";
import { Result, Ok, Failure } from "@/common/result";

export enum BotQueryErrorCode {
  BOT_NOT_FOUND = "BOT_NOT_FOUND",
  QUERY_FAILED = "QUERY_FAILED",
  RUNTIME_UNAVAILABLE = "RUNTIME_UNAVAILABLE",
  TIMEOUT = "TIMEOUT",
  INVALID_RESPONSE = "INVALID_RESPONSE",
}

export interface BotQueryError {
  code: BotQueryErrorCode;
  message: string;
}

export interface QueryResult {
  status: string;
  data?: unknown;
  error?: string;
  duration: number;
  timestamp: string;
}

export interface QueryRequest {
  queryPath: string;
  params?: Record<string, unknown>;
  timeoutSec?: number;
}

@Injectable({ scope: Scope.REQUEST })
export class BotQueryService {
  private runtimeUrl: string;

  constructor(
    private readonly configService: ConfigService,
    private readonly botService: BotService,
    private readonly logger: PinoLogger,
  ) {
    // Runtime query server URL - defaults to localhost:9477 for local dev
    this.runtimeUrl =
      this.configService.get<string>("RUNTIME_QUERY_URL") ||
      "http://localhost:9477";
  }

  /**
   * Execute a query against a bot.
   */
  async executeQuery(
    botId: string,
    request: QueryRequest,
  ): Promise<Result<QueryResult, BotQueryError>> {
    // Verify bot exists and user has access
    const botResult = await this.botService.findOne(botId);
    if (!botResult.success) {
      return Failure({
        code: BotQueryErrorCode.BOT_NOT_FOUND,
        message: "Bot not found or access denied",
      });
    }

    try {
      const timeoutMs = (request.timeoutSec || 30) * 1000;
      const controller = new AbortController();
      const timeoutId = setTimeout(() => controller.abort(), timeoutMs);

      const response = await fetch(`${this.runtimeUrl}/query`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify({
          bot_id: botId,
          query_path: request.queryPath,
          params: request.params || {},
          timeout_sec: request.timeoutSec || 30,
        }),
        signal: controller.signal,
      });

      clearTimeout(timeoutId);

      if (!response.ok) {
        const errorText = await response.text();
        this.logger.error(
          { botId, queryPath: request.queryPath, status: response.status, error: errorText },
          "Query request failed",
        );

        if (response.status === 404) {
          return Failure({
            code: BotQueryErrorCode.BOT_NOT_FOUND,
            message: "Bot not found in runtime",
          });
        }

        return Failure({
          code: BotQueryErrorCode.QUERY_FAILED,
          message: `Query failed: ${errorText}`,
        });
      }

      const result = await response.json();

      return Ok({
        status: result.status || "ok",
        data: result.data,
        error: result.error,
        duration: result.duration || 0,
        timestamp: result.timestamp || new Date().toISOString(),
      });
    } catch (error: any) {
      if (error.name === "AbortError") {
        return Failure({
          code: BotQueryErrorCode.TIMEOUT,
          message: `Query timed out after ${request.timeoutSec || 30} seconds`,
        });
      }

      if (error.code === "ECONNREFUSED" || error.cause?.code === "ECONNREFUSED") {
        this.logger.error(
          { botId, queryPath: request.queryPath, error: error.message },
          "Runtime unavailable",
        );
        return Failure({
          code: BotQueryErrorCode.RUNTIME_UNAVAILABLE,
          message: "Bot runtime is not available. Ensure the bot-runner service is running.",
        });
      }

      this.logger.error(
        { botId, queryPath: request.queryPath, error: error.message },
        "Query execution error",
      );
      return Failure({
        code: BotQueryErrorCode.QUERY_FAILED,
        message: `Query failed: ${error.message}`,
      });
    }
  }
}
