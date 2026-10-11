import { PinoLogger } from "nestjs-pino";
import { BotEventRelay } from "../bot-event-relay";
import {
  BotEventOutbox,
  PendingBotEvent,
  PublishOutcome,
} from "../bot-event-outbox";
import { NatsService } from "@/nats/nats.service";
import { Failure, Ok } from "@/common/result";
import { createMockLogger } from "@/test/mock-logger";

const SECOND = 1000;
const HOUR = 60 * 60 * SECOND;
const DAY = 24 * HOUR;

class FakeOutbox {
  events: PendingBotEvent[] = [];
  published = new Set<number>();
  calledAt: number[] = [];
  prunedBefore: Date[] = [];
  private forced: (PublishOutcome | Error | "hang")[] = [];

  add(...subjects: string[]) {
    for (const subject of subjects) {
      const id = this.events.length + 1;
      this.events.push({
        id,
        botId: `bot-${id}`,
        subject,
        payload: JSON.stringify({ id: `bot-${id}` }),
      });
    }
  }

  force(...outcomes: (PublishOutcome | Error | "hang")[]) {
    this.forced.push(...outcomes);
  }

  async publishNext(
    publish: (event: PendingBotEvent) => Promise<boolean>,
  ): Promise<PublishOutcome> {
    this.calledAt.push(Date.now());
    const forced = this.forced.shift();
    if (forced === "hang") return new Promise<never>(() => undefined);
    if (forced instanceof Error) throw forced;
    if (forced) return forced;

    const next = this.events.find((event) => !this.published.has(event.id));
    if (!next) return "drained";
    if (!(await publish(next))) return "failed";
    this.published.add(next.id);
    return "published";
  }

  async prunePublishedBefore(cutoff: Date): Promise<number> {
    this.prunedBefore.push(cutoff);
    return 0;
  }
}

class FlakyNats {
  failures = 0;
  publish = jest.fn(async () => {
    if (this.failures > 0) {
      this.failures--;
      return Failure<void, string>("TIMEOUT");
    }
    return Ok<void, string>(undefined);
  });

  subjects(): string[] {
    return this.publish.mock.calls.map((call: unknown[]) => call[0] as string);
  }
}

describe("BotEventRelay", () => {
  let outbox: FakeOutbox;
  let nats: FlakyNats;
  let relay: BotEventRelay;
  let startedAt: number;

  const callTimes = () => outbox.calledAt.map((time) => time - startedAt);

  beforeEach(() => {
    jest.useFakeTimers();
    startedAt = Date.now();
    outbox = new FakeOutbox();
    nats = new FlakyNats();
    relay = new BotEventRelay(
      outbox as unknown as BotEventOutbox,
      nats as unknown as NatsService,
      createMockLogger() as unknown as PinoLogger,
    );
  });

  afterEach(async () => {
    await relay.onModuleDestroy();
    jest.useRealTimers();
  });

  describe("publish on startup and on request", () => {
    it("publishes the events left in the outbox when the api starts", async () => {
      outbox.add("the0.bot.created", "the0.bot.deleted");

      relay.onApplicationBootstrap();
      await relay.settled();

      expect(nats.subjects()).toEqual(["the0.bot.created", "the0.bot.deleted"]);
    });

    it("publishes each event with its outbox id as the message id and a short ack timeout", async () => {
      outbox.add("the0.bot.created");

      await relay.publishPending();

      expect(nats.publish).toHaveBeenCalledWith(
        "the0.bot.created",
        { id: "bot-1" },
        { msgId: "1", timeoutMs: 2000 },
      );
    });

    it("publishes one event per outbox call until the outbox is empty", async () => {
      outbox.add(...Array.from({ length: 250 }, () => "the0.bot.updated"));

      await relay.publishPending();

      expect(nats.publish).toHaveBeenCalledTimes(250);
      expect(outbox.calledAt).toHaveLength(251);
    });

    it("folds requests made before a drain starts into that drain", async () => {
      outbox.add("the0.bot.created");

      await Promise.all([
        relay.publishPending(),
        relay.publishPending(),
        relay.publishPending(),
      ]);

      expect(nats.publish).toHaveBeenCalledTimes(1);
      expect(outbox.calledAt).toHaveLength(2);
    });
  });

  describe("stay quiet while there is nothing to publish", () => {
    it("issues no queries once the outbox is empty", async () => {
      relay.onApplicationBootstrap();
      await relay.settled();

      await jest.advanceTimersByTimeAsync(DAY);

      expect(outbox.calledAt).toEqual([startedAt]);
    });
  });

  describe("retry with backoff until NATS accepts", () => {
    it("retries a failed publish after 1s, 2s and 4s, then stops polling once it succeeds", async () => {
      outbox.add("the0.bot.deleted");
      nats.failures = 3;

      await relay.publishPending();
      await jest.advanceTimersByTimeAsync(DAY);

      expect(callTimes()).toEqual([
        0,
        SECOND,
        3 * SECOND,
        7 * SECOND,
        7 * SECOND,
      ]);
      expect(outbox.published).toEqual(new Set([1]));
    });

    it("waits at most 30s between retries", async () => {
      outbox.add("the0.bot.deleted");
      nats.failures = 7;

      await relay.publishPending();
      await jest.advanceTimersByTimeAsync(DAY);

      const attempts = [...new Set(callTimes())];
      const gaps = attempts.slice(1).map((time, i) => time - attempts[i]);
      expect(gaps).toEqual([1, 2, 4, 8, 16, 30, 30].map((s) => s * SECOND));
    });

    it("retries when the outbox cannot be read", async () => {
      outbox.add("the0.bot.created");
      outbox.force(new Error("connection terminated"));

      await relay.publishPending();
      await jest.advanceTimersByTimeAsync(DAY);

      expect(callTimes()).toEqual([0, SECOND, SECOND]);
      expect(nats.subjects()).toEqual(["the0.bot.created"]);
    });

    it("gives up on an outbox call that hangs after 10s, then retries and publishes", async () => {
      outbox.add("the0.bot.created");
      outbox.force("hang");

      const firstDrain = relay.publishPending();
      await jest.advanceTimersByTimeAsync(10 * SECOND);
      await firstDrain;
      await jest.advanceTimersByTimeAsync(DAY);

      expect(callTimes()).toEqual([0, 11 * SECOND, 11 * SECOND]);
      expect(nats.subjects()).toEqual(["the0.bot.created"]);
    });

    it("publishes straight away when asked during a backoff, and cancels the pending retry once drained", async () => {
      outbox.add("the0.bot.created");
      nats.failures = 1;
      await relay.publishPending();

      await jest.advanceTimersByTimeAsync(SECOND / 2);
      outbox.add("the0.bot.updated");
      await relay.publishPending();
      await jest.advanceTimersByTimeAsync(DAY);

      expect(callTimes()).toEqual([0, SECOND / 2, SECOND / 2, SECOND / 2]);
      expect(nats.subjects()).toEqual([
        "the0.bot.created",
        "the0.bot.created",
        "the0.bot.updated",
      ]);
    });
  });

  describe("forget published events after a week", () => {
    it("prunes events published more than 7 days ago when a drain empties the outbox, at most hourly", async () => {
      await relay.publishPending();
      await jest.advanceTimersByTimeAsync(HOUR / 2);
      await relay.publishPending();
      await jest.advanceTimersByTimeAsync(HOUR);
      await relay.publishPending();

      expect(outbox.prunedBefore.map((cutoff) => cutoff.getTime())).toEqual([
        startedAt - 7 * DAY,
        startedAt + HOUR + HOUR / 2 - 7 * DAY,
      ]);
    });
  });

  describe("stop with the api", () => {
    it("drops a pending retry on shutdown", async () => {
      outbox.add("the0.bot.created");
      nats.failures = 1;
      await relay.publishPending();

      await relay.onModuleDestroy();
      await jest.advanceTimersByTimeAsync(DAY);

      expect(outbox.calledAt).toHaveLength(1);
    });
  });
});
