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

// Must stay in sync with runtime's sanitizeBotID (runtime/internal/daemon/nats_publisher.go)
const VALID_BOT_ID = /^[A-Za-z0-9_-]+$/;

export const ACCESS_DENIED_MESSAGE = "Bot not found or access denied";

interface BotSubscription {
  unsubscribe: () => void;
  clients: Set<Response>;
}

interface SubscriptionResult {
  subscription: BotSubscription | null;
  warningsSent: boolean;
}

@Controller("logs")
@UseGuards(AuthCombinedGuard)
export class LogsController {
  // Static maps are intentional: NestJS controllers are singletons and a
  // single NATS subscription per bot must fan out to all SSE clients in the process.
  private static activeSubscriptions = new Map<string, BotSubscription>();
  private static pendingSubscriptions = new Map<string, Promise<SubscriptionResult>>();

  /** @internal Reset shared state for test isolation. Only works in test env. */
  static _resetForTest() {
    if (process.env.NODE_ENV !== "test") {
      return;
    }
    for (const [, sub] of LogsController.activeSubscriptions) {
      sub.unsubscribe();
    }
    LogsController.activeSubscriptions.clear();
    LogsController.pendingSubscriptions.clear();
  }

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
      res.status(400);
      res.setHeader("Content-Type", "text/event-stream");
      res.setHeader("Cache-Control", "no-cache");
      res.flushHeaders();
      res.write(`event: error\ndata: ${JSON.stringify({ message: "Invalid bot ID" })}\n\n`);
      res.end();
      return;
    }

    this.sendSSEHeaders(res);

    // Register cleanup early — before any awaits — so client disconnects
    // during getLogs are caught. Track disconnected flag to avoid adding
    // a stale client to the fan-out set.
    let disconnected = false;
    let keepalive: ReturnType<typeof setInterval> | null = null;
    const cleanup = () => {
      disconnected = true;
      if (keepalive) clearInterval(keepalive);

      const sub = LogsController.activeSubscriptions.get(botId);
      if (sub) {
        sub.clients.delete(res);
        if (sub.clients.size === 0) {
          sub.unsubscribe();
          LogsController.activeSubscriptions.delete(botId);
        }
      }
    };
    req.on("close", cleanup);

    // Send historical logs
    const shouldContinue = await this.sendHistoricalLogs(res, botId);
    if (!shouldContinue) return;

    // If client disconnected during history fetch, clean up immediately
    if (disconnected) {
      cleanup();
      return;
    }

    // Subscribe to NATS or join existing fan-out
    const { subscription } = await this.getOrCreateSubscription(botId, res);

    // If client disconnected during subscription, clean up immediately
    if (disconnected) {
      cleanup();
      return;
    }

    this.setupFanOut(res, botId, subscription, (interval) => {
      keepalive = interval;
    });
  }

  private sendSSEHeaders(res: Response): void {
    res.setHeader("Content-Type", "text/event-stream");
    res.setHeader("Cache-Control", "no-cache");
    res.setHeader("Connection", "keep-alive");
    res.setHeader("X-Accel-Buffering", "no");
    res.flushHeaders();
  }

  private async sendHistoricalLogs(
    res: Response,
    botId: string,
  ): Promise<boolean> {
    // JS getFullYear()/getMonth()/getDate() return local time, matching
    // the runtime's minio_logger which uses time.Now().Format("20060102").
    // This assumes API and runtime run in the same timezone (true in Docker Compose).
    const today = new Date();
    const todayStr =
      today.getFullYear().toString() +
      (today.getMonth() + 1).toString().padStart(2, "0") +
      today.getDate().toString().padStart(2, "0");

    const historyResult = await this.logsService.getLogs(botId, {
      date: todayStr,
      limit: 1000,
      offset: 0,
    });

    if (
      !historyResult.success &&
      (historyResult.error?.includes("not found") ||
        historyResult.error?.includes("access denied"))
    ) {
      res.write(`event: error\ndata: ${JSON.stringify({ message: ACCESS_DENIED_MESSAGE })}\n\n`);
      res.end();
      return false;
    }

    if (historyResult.success && historyResult.data) {
      res.write(
        `event: history\ndata: ${JSON.stringify(historyResult.data)}\n\n`,
      );
    } else if (!historyResult.success) {
      res.write(
        `event: warning\ndata: ${JSON.stringify({ message: "History unavailable" })}\n\n`,
      );
    }

    return true;
  }

  private async getOrCreateSubscription(
    botId: string,
    res: Response,
  ): Promise<SubscriptionResult> {
    // No backoff needed: each new SSE connection is a natural retry boundary,
    // and failed subscriptions are not stored (see createSubscription).
    let subscription = LogsController.activeSubscriptions.get(botId);

    if (subscription) {
      return { subscription, warningsSent: false };
    }

    // Prevent duplicate subscriptions under concurrency: if another request
    // is already creating a subscription for this botId, wait for it
    const pending = LogsController.pendingSubscriptions.get(botId);
    if (pending) {
      const result = await pending;
      if (!result.subscription && !result.warningsSent) {
        res.write(
          `event: warning\ndata: ${JSON.stringify({ message: "Live streaming unavailable" })}\n\n`,
        );
        return { subscription: null, warningsSent: true };
      }
      // Re-check active map — the pending promise may have populated it
      subscription = LogsController.activeSubscriptions.get(botId);
      return { subscription: subscription ?? null, warningsSent: result.warningsSent };
    }

    const createPromise = this.createSubscription(botId, res);
    LogsController.pendingSubscriptions.set(botId, createPromise);
    try {
      return await createPromise;
    } finally {
      LogsController.pendingSubscriptions.delete(botId);
    }
  }

  private setupFanOut(
    res: Response,
    botId: string,
    subscription: BotSubscription | null,
    onKeepalive: (interval: ReturnType<typeof setInterval>) => void,
  ): void {
    if (subscription) {
      subscription.clients.add(res);
    }

    // Keepalive every 15 seconds
    const keepalive = setInterval(() => {
      try {
        if (res.writableEnded) {
          clearInterval(keepalive);
          this.removeStaleClient(res, botId);
          return;
        }
        res.write(":\n\n");
      } catch {
        clearInterval(keepalive);
        this.removeStaleClient(res, botId);
      }
    }, 15000);

    onKeepalive(keepalive);
  }

  private removeStaleClient(client: Response, botId: string): void {
    const sub = LogsController.activeSubscriptions.get(botId);
    if (sub) {
      sub.clients.delete(client);
      if (sub.clients.size === 0) {
        sub.unsubscribe();
        LogsController.activeSubscriptions.delete(botId);
      }
    }
  }

  private async createSubscription(
    botId: string,
    res: Response,
  ): Promise<SubscriptionResult> {
    const clients = new Set<Response>();
    // Add the initiating client before subscribing so the first NATS
    // message isn't dropped (the callback fans out to `clients`)
    clients.add(res);
    const natsSubject = BOT_LOG_TOPICS.forBot(botId);

    const subscribeResult = this.natsService.subscribe(
      natsSubject,
      (msg: string) => {
        const data = JSON.stringify({
          content: msg,
          timestamp: new Date().toISOString(),
        });
        const event = `event: update\ndata: ${data}\n\n`;
        for (const client of clients) {
          try {
            if (client.writableEnded) {
              clients.delete(client);
            } else {
              client.write(event);
            }
          } catch {
            clients.delete(client);
          }
        }
        // If all clients disconnected, clean up the NATS subscription
        if (clients.size === 0) {
          const sub = LogsController.activeSubscriptions.get(botId);
          if (sub) {
            sub.unsubscribe();
            LogsController.activeSubscriptions.delete(botId);
          }
        }
      },
    );

    if (!subscribeResult.success) {
      this.logger.error(
        { botId, error: subscribeResult.error },
        "Failed to subscribe to NATS for bot logs",
      );
      res.write(
        `event: warning\ndata: ${JSON.stringify({ message: "Live streaming unavailable" })}\n\n`,
      );
      return { subscription: null, warningsSent: true };
    }

    const unsubscribe = subscribeResult.data ?? (() => {});
    const subscription: BotSubscription = { unsubscribe, clients };
    LogsController.activeSubscriptions.set(botId, subscription);
    return { subscription, warningsSent: false };
  }
}
