import { sql } from "drizzle-orm";
import {
  appendBotEvents,
  BotEventOutbox,
  PendingBotEvent,
} from "../bot-event-outbox";
import { botEventOutboxTable } from "@/database/schema/bot-event-outbox";
import { useTestDatabase } from "@/test/test-database";

jest.mock(
  "@/database/connection",
  () => jest.requireActual("@/test/test-database").connection,
);

const DAY_MS = 24 * 60 * 60 * 1000;

describe("BotEventOutbox", () => {
  const db = useTestDatabase();
  const outbox = new BotEventOutbox();

  const appendEvents = (...subjects: string[]) =>
    appendBotEvents(
      db(),
      subjects.map((subject) => ({
        botId: "bot-1",
        subject,
        payload: { id: "bot-1", subject },
      })),
    );

  const unpublishedIds = async () =>
    (await db().select().from(botEventOutboxTable))
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

  describe("hand unpublished events to the publisher in commit order, one per transaction", () => {
    it("publishes the oldest unpublished event, with its outbox id, and marks it published", async () => {
      await appendEvents("the0.bot.created", "the0.bot.updated");
      const { seen, publish } = publisher();

      const first = await outbox.publishNext(publish);
      const unpublishedAfterFirst = await unpublishedIds();
      const second = await outbox.publishNext(publish);
      const third = await outbox.publishNext(publish);

      expect([first, second, third]).toEqual([
        "published",
        "published",
        "drained",
      ]);
      expect(unpublishedAfterFirst).toEqual([2]);
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

      expect(await outbox.publishNext(first.publish)).toBe("published");
      expect(await outbox.publishNext(first.publish)).toBe("failed");
      expect(first.seen.map((event) => event.id)).toEqual([1, 2]);
      expect(await unpublishedIds()).toEqual([2, 3]);

      const retry = publisher();
      expect(await outbox.publishNext(retry.publish)).toBe("published");
      expect(await outbox.publishNext(retry.publish)).toBe("published");
      expect(await outbox.publishNext(retry.publish)).toBe("drained");
      expect(retry.seen.map((event) => event.id)).toEqual([2, 3]);
    });

    it("reports drained when there is nothing to publish", async () => {
      const { seen, publish } = publisher();

      expect(await outbox.publishNext(publish)).toBe("drained");
      expect(seen).toEqual([]);
    });
  });

  describe("hold no database transaction across the publish", () => {
    it("lets other queries run while the publish waits for its ack", async () => {
      await appendEvents("the0.bot.created");
      let probe: string | undefined;

      await outbox.publishNext(async () => {
        // PGlite has one connection and queues every query behind an open
        // transaction, so the probe answers in time only if none is open.
        probe = await Promise.race([
          db()
            .execute(sql`SELECT 1`)
            .then(() => "answered"),
          new Promise<string>((resolve) =>
            setTimeout(() => resolve("blocked"), 500),
          ),
        ]);
        return true;
      });

      expect(probe).toBe("answered");
    });
  });

  describe("forget published events after the retention", () => {
    it("deletes events published before the cutoff and keeps the rest", async () => {
      const now = Date.now();
      await db()
        .insert(botEventOutboxTable)
        .values([
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
      const left = await db().select().from(botEventOutboxTable);
      expect(left.map((row) => row.botId).sort()).toEqual(["recent", "stuck"]);
    });
  });
});
