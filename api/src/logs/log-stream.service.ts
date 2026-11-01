import { Injectable } from "@nestjs/common";
import { Request, Response } from "express";
import { LogsService } from "./logs.service";
import { LogFanOut } from "./log-fan-out";
import { isValidBotId } from "./bot-id";
import { sendSSEHeaders, sseEvent } from "./sse";
import { utcDay } from "./log-entry";
import { AppError } from "@/common/app-error";

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

    // Registered before any await so a disconnect during the access check or
    // history read is seen; `disconnected` keeps a gone client out of the
    // fan-out set.
    let disconnected = false;
    let keepalive: ReturnType<typeof setInterval> | null = null;
    const cleanup = () => {
      disconnected = true;
      if (keepalive) clearInterval(keepalive);
      this.fanOut.leave(botId, res);
    };
    req.on("close", cleanup);

    const access = await this.logsService.checkAccess(botId, userId);
    if (!access.success) {
      this.endWithError(res, access.error);
      return;
    }
    if (
      options.history !== false &&
      !(await this.sendHistoricalLogs(res, botId, userId))
    ) {
      return;
    }
    // Some proxies send the response head only with the first body chunk,
    // which a stream without history might not write until the keepalive.
    res.write(": connected\n\n");

    if (disconnected) {
      cleanup();
      return;
    }

    await this.fanOut.join(botId, res);

    if (disconnected) {
      cleanup();
      return;
    }

    keepalive = this.startKeepalive(res, botId);
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
        sort: "asc",
      },
      userId,
    );

    if (historyResult.success && historyResult.data) {
      res.write(sseEvent("history", historyResult.data.entries));
      return true;
    }
    // Access was checked first, so only a bot deleted since then lands here.
    if (historyResult.error.code === "not_found") {
      this.endWithError(res, historyResult.error);
      return false;
    }
    res.write(sseEvent("warning", { message: "History unavailable" }));
    return true;
  }

  private endWithError(res: Response, error: AppError): void {
    const message =
      error.code === "not_found" ? ACCESS_DENIED_MESSAGE : error.message;
    res.write(sseEvent("error", { message }));
    res.end();
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
