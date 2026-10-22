import { Injectable } from "@nestjs/common";
import { Request, Response } from "express";
import { LogsService } from "./logs.service";
import { LogFanOut } from "./log-fan-out";
import { isValidBotId } from "./bot-id";
import { sendSSEHeaders, sseEvent } from "./sse";
import { utcDay } from "./log-entry";

export const ACCESS_DENIED_MESSAGE = "Bot not found or access denied";

const KEEPALIVE_MS = 15000;

export interface StreamOptions {
  history?: boolean;
}

@Injectable()
export class LogStreamService {
  constructor(
    private readonly logsService: LogsService,
    private readonly fanOut: LogFanOut,
  ) {}

  /**
   * `history: false` skips the read of today's file. The ownership check
   * runs either way: the stream must never outlive a refused user.
   */
  async stream(
    botId: string,
    userId: string,
    req: Request,
    res: Response,
    options: StreamOptions = {},
  ): Promise<void> {
    // Validate botId before using as NATS subject
    if (!isValidBotId(botId)) {
      res.status(400);
      res.setHeader("Content-Type", "text/event-stream");
      res.setHeader("Cache-Control", "no-cache");
      res.flushHeaders();
      res.write(sseEvent("error", { message: "Invalid bot ID" }));
      res.end();
      return;
    }

    sendSSEHeaders(res);

    // Register cleanup early - before any awaits - so client disconnects
    // during getLogs are caught. Track disconnected flag to avoid adding
    // a stale client to the fan-out set.
    let disconnected = false;
    let keepalive: ReturnType<typeof setInterval> | null = null;
    const cleanup = () => {
      disconnected = true;
      if (keepalive) clearInterval(keepalive);
      this.fanOut.leave(botId, res);
    };
    req.on("close", cleanup);

    const shouldContinue =
      options.history === false
        ? await this.checkAccess(res, botId, userId)
        : await this.sendHistoricalLogs(res, botId, userId);
    if (!shouldContinue) return;

    // If client disconnected during history fetch, clean up immediately
    if (disconnected) {
      cleanup();
      return;
    }

    await this.fanOut.join(botId, res);

    // If client disconnected during subscription, clean up immediately
    if (disconnected) {
      cleanup();
      return;
    }

    keepalive = this.startKeepalive(res, botId);
  }

  private async checkAccess(
    res: Response,
    botId: string,
    userId: string,
  ): Promise<boolean> {
    const access = await this.logsService.checkAccess(botId, userId);
    return access.success || !this.endOnAccessError(res, access.error);
  }

  private async sendHistoricalLogs(
    res: Response,
    botId: string,
    userId: string,
  ): Promise<boolean> {
    const historyResult = await this.logsService.getLogs(
      botId,
      {
        date: utcDay(new Date()),
        limit: 1000,
        offset: 0,
      },
      userId,
    );

    if (historyResult.success && historyResult.data) {
      res.write(sseEvent("history", historyResult.data.entries));
      return true;
    }
    if (this.endOnAccessError(res, historyResult.error)) return false;
    res.write(sseEvent("warning", { message: "History unavailable" }));
    return true;
  }

  /** @returns true when the error refused access and the stream was ended. */
  private endOnAccessError(res: Response, error: string | null): boolean {
    if (error?.includes("Authentication required")) {
      res.write(sseEvent("error", { message: "Authentication required" }));
      res.end();
      return true;
    }
    if (error?.includes("not found") || error?.includes("access denied")) {
      res.write(sseEvent("error", { message: ACCESS_DENIED_MESSAGE }));
      res.end();
      return true;
    }
    return false;
  }

  private startKeepalive(
    res: Response,
    botId: string,
  ): ReturnType<typeof setInterval> {
    const keepalive = setInterval(() => {
      try {
        if (res.writableEnded) {
          clearInterval(keepalive);
          this.fanOut.leave(botId, res);
          return;
        }
        res.write(":\n\n");
      } catch {
        clearInterval(keepalive);
        this.fanOut.leave(botId, res);
      }
    }, KEEPALIVE_MS);
    return keepalive;
  }
}
