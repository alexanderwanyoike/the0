import { Injectable } from "@nestjs/common";
import { Request, Response } from "express";
import { LogsService } from "./logs.service";
import { LogFanOut } from "./log-fan-out";
import { isValidBotId } from "./bot-id";
import { sendSSEHeaders, sseEvent } from "./sse";

export const ACCESS_DENIED_MESSAGE = "Bot not found or access denied";

const KEEPALIVE_MS = 15000;

@Injectable()
export class LogStreamService {
  constructor(
    private readonly logsService: LogsService,
    private readonly fanOut: LogFanOut,
  ) {}

  async stream(
    botId: string,
    userId: string,
    req: Request,
    res: Response,
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

    const shouldContinue = await this.sendHistoricalLogs(res, botId, userId);
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

  private async sendHistoricalLogs(
    res: Response,
    botId: string,
    userId: string,
  ): Promise<boolean> {
    // JS getFullYear()/getMonth()/getDate() return local time, matching
    // the runtime's minio_logger which uses time.Now().Format("20060102").
    // This assumes API and runtime run in the same timezone (true in Docker Compose).
    const today = new Date();
    const todayStr =
      today.getFullYear().toString() +
      (today.getMonth() + 1).toString().padStart(2, "0") +
      today.getDate().toString().padStart(2, "0");

    const historyResult = await this.logsService.getLogs(
      botId,
      {
        date: todayStr,
        limit: 1000,
        offset: 0,
      },
      userId,
    );

    if (
      !historyResult.success &&
      historyResult.error?.includes("Authentication required")
    ) {
      res.write(sseEvent("error", { message: "Authentication required" }));
      res.end();
      return false;
    }

    if (
      !historyResult.success &&
      (historyResult.error?.includes("not found") ||
        historyResult.error?.includes("access denied"))
    ) {
      res.write(sseEvent("error", { message: ACCESS_DENIED_MESSAGE }));
      res.end();
      return false;
    }

    if (historyResult.success && historyResult.data) {
      res.write(sseEvent("history", historyResult.data.entries));
    } else if (!historyResult.success) {
      res.write(sseEvent("warning", { message: "History unavailable" }));
    }

    return true;
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
