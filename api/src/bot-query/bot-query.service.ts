import { Injectable } from "@nestjs/common";
import { ConfigService } from "@nestjs/config";
import { PinoLogger } from "nestjs-pino";
import { Actor } from "@/auth/actor";
import { BotService } from "@/bot/bot.service";
import { appError, AppError } from "@/common/app-error";
import { Result, Ok, Failure, errorMessage } from "@/common/result";

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

@Injectable()
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
    actor: Actor,
    botId: string,
    request: QueryRequest,
  ): Promise<Result<QueryResult, AppError>> {
    // Verify bot exists and user has access
    const botResult = await this.botService.findOne(actor, botId);
    if (!botResult.success) {
      return Failure(appError("not_found", "Bot not found or access denied"));
    }

    try {
      const response = await this.postQuery(botId, request);
      if (!response.ok) {
        return await this.runtimeErrorResponse(botId, request, response);
      }
      return Ok(toQueryResult(await response.json()));
    } catch (error: unknown) {
      return this.queryFailure(botId, request, error);
    }
  }

  private async postQuery(
    botId: string,
    request: QueryRequest,
  ): Promise<Response> {
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
    return response;
  }

  private async runtimeErrorResponse(
    botId: string,
    request: QueryRequest,
    response: Response,
  ): Promise<Result<QueryResult, AppError>> {
    const errorText = await response.text();
    this.logger.error(
      {
        botId,
        queryPath: request.queryPath,
        status: response.status,
        error: errorText,
      },
      "Query request failed",
    );

    if (response.status === 404) {
      return Failure(appError("not_found", "Bot not found in runtime"));
    }

    if (response.status === 422) {
      return Failure(
        appError(
          "unprocessable",
          "This bot defines no queries. Add a query entrypoint (entrypoints.query in bot-config.yaml) to answer them.",
        ),
      );
    }

    // The runtime's 500 covers both a bot refusing the query with an error
    // result and a bot that is not running; only the body tells them apart.
    return Failure(appError("validation", `Query failed: ${errorText}`));
  }

  private queryFailure(
    botId: string,
    request: QueryRequest,
    error: unknown,
  ): Result<QueryResult, AppError> {
    const err = error as {
      name?: string;
      code?: string;
      cause?: { code?: string };
    };
    if (err.name === "AbortError") {
      return Failure(
        appError(
          "timeout",
          `Query timed out after ${request.timeoutSec || 30} seconds`,
        ),
      );
    }

    if (err.code === "ECONNREFUSED" || err.cause?.code === "ECONNREFUSED") {
      this.logger.error(
        { botId, queryPath: request.queryPath, error: errorMessage(error) },
        "Runtime unavailable",
      );
      return Failure(
        appError(
          "unavailable",
          "Bot runtime is not available. Ensure the bot-runner service is running.",
        ),
      );
    }

    this.logger.error(
      { botId, queryPath: request.queryPath, error: errorMessage(error) },
      "Query execution error",
    );
    return Failure(
      appError("internal", `Query failed: ${errorMessage(error)}`),
    );
  }
}

const toQueryResult = (result: {
  status?: string;
  data?: unknown;
  error?: string;
  duration?: number;
  timestamp?: string;
}): QueryResult => ({
  status: result.status || "ok",
  data: result.data,
  error: result.error,
  duration: result.duration || 0,
  timestamp: result.timestamp || new Date().toISOString(),
});
