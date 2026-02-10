import {
  Controller,
  Get,
  Param,
  Query,
  Req,
  Res,
  UseGuards,
  BadRequestException,
  NotFoundException,
} from "@nestjs/common";
import { Request, Response } from "express";
import { PinoLogger } from "nestjs-pino";
import { LogsService } from "./logs.service";
import { GetLogsQueryDto } from "./dto/get-logs-query.dto";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";
import { NatsService } from "@/nats/nats.service";
import { BOT_LOG_TOPICS } from "@/bot/bot.constants";

const VALID_BOT_ID = /^[A-Za-z0-9_-]+$/;

interface BotSubscription {
  unsubscribe: () => void;
  clients: Set<Response>;
}

// Shared subscription map - static so it persists across request-scoped instances
const activeSubscriptions = new Map<string, BotSubscription>();

/** @internal Exposed for testing only */
export function _clearActiveSubscriptions() {
  for (const [, sub] of activeSubscriptions) {
    sub.unsubscribe();
  }
  activeSubscriptions.clear();
}

@Controller("logs")
@UseGuards(AuthCombinedGuard)
export class LogsController {
  constructor(
    private readonly logsService: LogsService,
    private readonly natsService: NatsService,
    private readonly logger: PinoLogger,
  ) {}

  @Get(":botId")
  async getLogs(
    @Param("botId") botId: string,
    @Query() query: GetLogsQueryDto,
  ) {
    const result = await this.logsService.getLogs(botId, {
      date: query.date,
      dateRange: query.dateRange,
      limit: query.limit || 100,
      offset: query.offset || 0,
    });

    if (!result.success) {
      if (
        result.error?.includes("not found") ||
        result.error?.includes("access denied")
      ) {
        throw new NotFoundException(result.error);
      }
      throw new BadRequestException(result.error);
    }

    return {
      success: true,
      data: result.data,
      message: "Logs retrieved successfully",
    };
  }

  @Get(":botId/stream")
  async streamLogs(
    @Param("botId") botId: string,
    @Req() req: Request,
    @Res() res: Response,
  ) {
    // Validate botId before using as NATS subject
    if (!VALID_BOT_ID.test(botId)) {
      res.setHeader("Content-Type", "text/event-stream");
      res.setHeader("Cache-Control", "no-cache");
      res.flushHeaders();
      res.write(`event: error\ndata: ${JSON.stringify({ message: "Invalid bot ID" })}\n\n`);
      res.end();
      return;
    }

    // Set SSE headers
    res.setHeader("Content-Type", "text/event-stream");
    res.setHeader("Cache-Control", "no-cache");
    res.setHeader("Connection", "keep-alive");
    res.setHeader("X-Accel-Buffering", "no");
    res.flushHeaders();

    // Send historical logs
    const today = new Date();
    const todayStr =
      today.getUTCFullYear().toString() +
      (today.getUTCMonth() + 1).toString().padStart(2, "0") +
      today.getUTCDate().toString().padStart(2, "0");

    const historyResult = await this.logsService.getLogs(botId, {
      date: todayStr,
      limit: 1000,
      offset: 0,
    });

    if (historyResult.success && historyResult.data) {
      res.write(
        `event: history\ndata: ${JSON.stringify(historyResult.data)}\n\n`,
      );
    }

    // Subscribe to NATS or join existing fan-out
    let subscription = activeSubscriptions.get(botId);

    if (!subscription) {
      const clients = new Set<Response>();
      const natsSubject = BOT_LOG_TOPICS.forBot(botId);

      const subscribeResult = this.natsService.subscribe(
        natsSubject,
        (msg: string) => {
          const data = JSON.stringify({ content: msg, timestamp: new Date().toISOString() });
          const event = `event: update\ndata: ${data}\n\n`;
          for (const client of clients) {
            try {
              if (!client.writableEnded) {
                client.write(event);
              }
            } catch {
              clients.delete(client);
            }
          }
        },
      );

      if (!subscribeResult.success) {
        this.logger.error({ botId, error: subscribeResult.error }, "Failed to subscribe to NATS for bot logs");
      }

      const unsubscribe = subscribeResult.data ?? (() => {});
      subscription = { unsubscribe, clients };
      activeSubscriptions.set(botId, subscription);
    }

    subscription.clients.add(res);

    // Keepalive every 15 seconds
    const keepalive = setInterval(() => {
      res.write(":\n\n");
    }, 15000);

    // Cleanup on disconnect
    const cleanup = () => {
      clearInterval(keepalive);

      const sub = activeSubscriptions.get(botId);
      if (sub) {
        sub.clients.delete(res);
        if (sub.clients.size === 0) {
          sub.unsubscribe();
          activeSubscriptions.delete(botId);
        }
      }
    };

    req.on("close", cleanup);
  }
}
