import { PgliteDatabase } from "drizzle-orm/pglite";
import {
  appendBotEvents,
  BotEventOutbox,
  PendingBotEvent,
} from "../bot-event-outbox";
import { botEventOutboxTable } from "@/database/schema/bot-event-outbox";
import {
  resetTestDatabase,
  startTestDatabase,
  stopTestDatabase,
} from "@/test/test-database";

jest.mock(
  "@/database/connection",
  () => jest.requireActual("@/test/test-database").connection,
);

const DAY_MS = 24 * 60 * 60 * 1000;

describe("BotEventOutbox", () => {
  let db: PgliteDatabase;
  let outbox: BotEventOutbox;

  const appendEvents = (...subjects: string[]) =>
    appendBotEvents(
      db,
      subjects.map((subject) => ({
        botId: "bot-1",
        subject,
        payload: { id: "bot-1", subject },
      })),
    );

  const unpublishedIds = async () =>
    (await db.select().from(botEventOutboxTable))
      .filter((row) => row.publishedAt === null)
      .map((row) => row.id)
      .sort();

  function publisher(rejectIds: number[] = []) {
    const seen: PendingBotEvent[] = [];
    const publish = async (event: PendingBotEvent) => {
      seen.push(event);
      return !rejectIds.includes(event.id);
    };
    return { seen, publish };
  }

  beforeAll(async () => {
    db = await startTestDatabase();
    outbox = new BotEventOutbox();
  });

  afterAll(async () => {
    await stopTestDatabase();
  });

  beforeEach(async () => {
    await resetTestDatabase();
  });

  describe("hand unpublished events to the publisher in commit order", () => {
    it("publishes each event once, with its outbox id, and marks it published", async () => {
      await appendEvents("the0.bot.created", "the0.bot.updated");
      const { seen, publish } = publisher();

      const outcome = await outbox.publishPending(publish, 100);
      const again = await outbox.publishPending(publish, 100);

      expect(outcome).toBe("drained");
      expect(again).toBe("drained");
      expect(seen).toEqual([
        {
          id: 1,
          botId: "bot-1",
          subject: "the0.bot.created",
          payload: JSON.stringify({ id: "bot-1", subject: "the0.bot.created" }),
        },
        {
          id: 2,
          botId: "bot-1",
          subject: "the0.bot.updated",
          payload: JSON.stringify({ id: "bot-1", subject: "the0.bot.updated" }),
        },
      ]);
      expect(await unpublishedIds()).toEqual([]);
    });

    it("stops at the first rejected event, keeping it and every later one for the next attempt", async () => {
      await appendEvents(
        "the0.bot.created",
        "the0.bot.updated",
        "the0.bot.deleted",
      );
      const first = publisher([2]);

      const outcome = await outbox.publishPending(first.publish, 100);

      expect(outcome).toBe("failed");
      expect(first.seen.map((event) => event.id)).toEqual([1, 2]);
      expect(await unpublishedIds()).toEqual([2, 3]);

      const retry = publisher();
      expect(await outbox.publishPending(retry.publish, 100)).toBe("drained");
      expect(retry.seen.map((event) => event.id)).toEqual([2, 3]);
    });

    it("reports more when a batch fills up, and carries on from there", async () => {
      await appendEvents(
        "the0.bot.created",
        "the0.bot.updated",
        "the0.bot.deleted",
      );
      const { seen, publish } = publisher();

      expect(await outbox.publishPending(publish, 2)).toBe("more");
      expect(await outbox.publishPending(publish, 2)).toBe("drained");
      expect(seen.map((event) => event.id)).toEqual([1, 2, 3]);
    });

    it("reports drained when there is nothing to publish", async () => {
      const { seen, publish } = publisher();

      expect(await outbox.publishPending(publish, 100)).toBe("drained");
      expect(seen).toEqual([]);
    });
  });

  describe("forget published events after the retention", () => {
    it("deletes events published before the cutoff and keeps the rest", async () => {
      const now = Date.now();
      await db.insert(botEventOutboxTable).values([
        {
          botId: "old",
          subject: "the0.bot.created",
          payload: "{}",
          publishedAt: new Date(now - 8 * DAY_MS),
        },
        {
          botId: "recent",
          subject: "the0.bot.created",
          payload: "{}",
          publishedAt: new Date(now - DAY_MS),
        },
        {
          botId: "stuck",
          subject: "the0.bot.created",
          payload: "{}",
          createdAt: new Date(now - 30 * DAY_MS),
        },
      ]);

      const removed = await outbox.prunePublishedBefore(
        new Date(now - 7 * DAY_MS),
      );

      expect(removed).toBe(1);
      const left = await db.select().from(botEventOutboxTable);
      expect(left.map((row) => row.botId).sort()).toEqual(["recent", "stuck"]);
    });
  });
});
