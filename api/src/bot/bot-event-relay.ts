import {
  Injectable,
  OnApplicationBootstrap,
  OnModuleDestroy,
} from "@nestjs/common";
import { PinoLogger } from "nestjs-pino";
import { NatsService } from "@/nats/nats.service";
import {
  BotEventOutbox,
  PendingBotEvent,
  PublishOutcome,
} from "./bot-event-outbox";

const BATCH_SIZE = 100;
// Kept short because the outbox transaction, and the relay lock, stay open
// while a publish waits for its ack.
const PUBLISH_TIMEOUT_MS = 2000;
const RETRY_BASE_MS = 1000;
const RETRY_CAP_MS = 30_000;
// Matches the THE0_EVENTS stream's max_age: past it, JetStream has dropped
// the message too.
const RETENTION_MS = 7 * 24 * 60 * 60 * 1000;
const PRUNE_EVERY_MS = 60 * 60 * 1000;

/**
 * Publishes the bot event outbox to JetStream in commit order. It drains
 * on startup and whenever asked after a commit, and polls only while
 * events are left unpublished: an idle api issues no queries, so a
 * serverless Postgres can suspend. Whoever commits events must therefore
 * call publishPending, without waiting for it.
 */
@Injectable()
export class BotEventRelay implements OnApplicationBootstrap, OnModuleDestroy {
  private running: Promise<void> = Promise.resolve();
  private queued: Promise<void> | null = null;
  private retryTimer: NodeJS.Timeout | null = null;
  private failedRetries = 0;
  private lastPrunedAt = 0;
  private stopped = false;

  constructor(
    private readonly outbox: BotEventOutbox,
    private readonly nats: NatsService,
    private readonly logger: PinoLogger,
  ) {}

  onApplicationBootstrap(): void {
    void this.publishPending();
  }

  async onModuleDestroy(): Promise<void> {
    this.stopped = true;
    this.cancelRetry();
    await this.settled();
  }

  /**
   * Resolves once a drain that starts after this call has finished. Calls
   * made before that drain starts share it. Never rejects.
   */
  publishPending(): Promise<void> {
    if (this.stopped) return Promise.resolve();
    if (!this.queued) {
      this.queued = this.running.then(() => {
        this.queued = null;
        return this.drain();
      });
      this.running = this.queued;
    }
    return this.queued;
  }

  /** Resolves when no drain is running or queued. */
  settled(): Promise<void> {
    return this.running;
  }

  private async drain(): Promise<void> {
    if (this.stopped) return;

    let outcome: PublishOutcome;
    try {
      do {
        outcome = await this.outbox.publishPending(
          (event) => this.publish(event),
          BATCH_SIZE,
        );
      } while (outcome === "more");
    } catch (err) {
      this.logger.error({ err }, "Could not read the bot event outbox");
      outcome = "failed";
    }

    if (outcome === "drained") {
      this.failedRetries = 0;
      this.cancelRetry();
      await this.pruneIfDue();
    } else {
      this.scheduleRetry();
    }
  }

  private async publish(event: PendingBotEvent): Promise<boolean> {
    const result = await this.nats.publish(
      event.subject,
      JSON.parse(event.payload),
      { msgId: String(event.id), timeoutMs: PUBLISH_TIMEOUT_MS },
    );
    if (!result.success) {
      this.logger.warn(
        {
          error: result.error,
          outboxId: event.id,
          botId: event.botId,
          subject: event.subject,
        },
        "Bot event not published yet, will retry",
      );
    }
    return result.success;
  }

  /**
   * A retry already scheduled is left alone, so requests made during an
   * outage neither add timers nor stretch the backoff.
   */
  private scheduleRetry(): void {
    if (this.retryTimer || this.stopped) return;
    const delay = Math.min(
      RETRY_CAP_MS,
      RETRY_BASE_MS * 2 ** this.failedRetries,
    );
    this.failedRetries++;
    this.retryTimer = setTimeout(() => {
      this.retryTimer = null;
      void this.publishPending();
    }, delay);
    this.retryTimer.unref();
  }

  private cancelRetry(): void {
    if (this.retryTimer) clearTimeout(this.retryTimer);
    this.retryTimer = null;
  }

  private async pruneIfDue(): Promise<void> {
    const now = Date.now();
    if (now - this.lastPrunedAt < PRUNE_EVERY_MS) return;
    this.lastPrunedAt = now;
    try {
      await this.outbox.prunePublishedBefore(new Date(now - RETENTION_MS));
    } catch (err) {
      this.logger.warn({ err }, "Could not prune published bot events");
    }
  }
}
