import { Injectable } from "@nestjs/common";
import { Response } from "express";
import { PinoLogger } from "nestjs-pino";
import { NatsService } from "@/nats/nats.service";
import { BOT_LOG_TOPICS } from "@/bot/bot.constants";
import { sseEvent } from "./sse";

interface BotSubscription {
  unsubscribe: () => void;
  clients: Set<Response>;
}

interface SubscriptionResult {
  subscription: BotSubscription | null;
  warningsSent: boolean;
}

/**
 * One NATS subscription per bot, fanned out to every SSE client of that bot
 * in this process. Must stay a singleton provider: it holds the process-wide
 * subscription maps.
 */
@Injectable()
export class LogFanOut {
  private readonly activeSubscriptions = new Map<string, BotSubscription>();
  private readonly pendingSubscriptions = new Map<
    string,
    Promise<SubscriptionResult>
  >();

  constructor(
    private readonly natsService: NatsService,
    private readonly logger: PinoLogger,
  ) {}

  /** Adds `res` to the bot's fan-out, subscribing to NATS if needed. */
  async join(botId: string, res: Response): Promise<void> {
    const { subscription } = await this.getOrCreateSubscription(botId, res);
    if (subscription) {
      subscription.clients.add(res);
    }
  }

  leave(botId: string, client: Response): void {
    const sub = this.activeSubscriptions.get(botId);
    if (sub) {
      sub.clients.delete(client);
      if (sub.clients.size === 0) {
        sub.unsubscribe();
        this.activeSubscriptions.delete(botId);
      }
    }
  }

  private async getOrCreateSubscription(
    botId: string,
    res: Response,
  ): Promise<SubscriptionResult> {
    // No backoff needed: each new SSE connection is a natural retry boundary,
    // and failed subscriptions are not stored (see createSubscription).
    let subscription = this.activeSubscriptions.get(botId);

    if (subscription) {
      return { subscription, warningsSent: false };
    }

    // Prevent duplicate subscriptions under concurrency: if another request
    // is already creating a subscription for this botId, wait for it
    const pending = this.pendingSubscriptions.get(botId);
    if (pending) {
      const result = await pending;
      if (!result.subscription && !result.warningsSent) {
        res.write(
          sseEvent("warning", { message: "Live streaming unavailable" }),
        );
        return { subscription: null, warningsSent: true };
      }
      // Re-check active map - the pending promise may have populated it
      subscription = this.activeSubscriptions.get(botId);
      return {
        subscription: subscription ?? null,
        warningsSent: result.warningsSent,
      };
    }

    const createPromise = this.createSubscription(botId, res);
    this.pendingSubscriptions.set(botId, createPromise);
    try {
      return await createPromise;
    } finally {
      this.pendingSubscriptions.delete(botId);
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
        const event = sseEvent("update", {
          content: msg,
          timestamp: new Date().toISOString(),
        });
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
          const sub = this.activeSubscriptions.get(botId);
          if (sub) {
            sub.unsubscribe();
            this.activeSubscriptions.delete(botId);
          }
        }
      },
    );

    if (!subscribeResult.success) {
      this.logger.error(
        { botId, error: subscribeResult.error },
        "Failed to subscribe to NATS for bot logs",
      );
      res.write(sseEvent("warning", { message: "Live streaming unavailable" }));
      return { subscription: null, warningsSent: true };
    }

    const unsubscribe = subscribeResult.data ?? (() => {});
    const subscription: BotSubscription = { unsubscribe, clients };
    this.activeSubscriptions.set(botId, subscription);
    return { subscription, warningsSent: false };
  }
}
