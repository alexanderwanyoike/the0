import { PgliteDatabase } from "drizzle-orm/pglite";
import { BotRepository } from "../bot.repository";
import { BotEvent } from "../bot-event-outbox";
import { Failure } from "@/common/result";
import { usersTable } from "@/database/schema/users";
import { customBotsTable } from "@/database/schema/custom-bots";
import { botsTable } from "@/database/schema/bots";
import { botEventOutboxTable } from "@/database/schema/bot-event-outbox";
import { mockBotConfig } from "@/test/mock-bot";
import {
  resetTestDatabase,
  startTestDatabase,
  stopTestDatabase,
} from "@/test/test-database";

jest.mock(
  "@/database/connection",
  () => jest.requireActual("@/test/test-database").connection,
);
// The failure cases are expected, so their error logs are noise here.
jest.mock("pino", () => ({
  __esModule: true,
  default: () => ({ error: jest.fn(), warn: jest.fn(), info: jest.fn() }),
}));

const OWNER = "user-1";
const CUSTOM_BOT = "sma-bot-1.0.0";
// Longer than the subject column, so storing the event fails.
const UNSTORABLE_SUBJECT = "x".repeat(300);

function event(botId: string, subject: string): BotEvent {
  return { botId, subject, payload: { id: botId, config: { symbol: "AAPL" } } };
}

describe("BotRepository writes with events", () => {
  let db: PgliteDatabase;
  let repository: BotRepository;

  const outboxRows = () =>
    db.select().from(botEventOutboxTable).orderBy(botEventOutboxTable.id);
  const botRows = () => db.select().from(botsTable);

  const newBot = (id: string, userId = OWNER) => ({
    id,
    userId,
    name: "SMA",
    config: mockBotConfig({ type: "scheduled/sma-bot", version: "1.0.0" }),
    topic: "the0-scheduled-custom-bot",
    customBotId: CUSTOM_BOT,
  });

  beforeAll(async () => {
    db = await startTestDatabase();
    repository = new BotRepository();
  });

  afterAll(async () => {
    await stopTestDatabase();
  });

  beforeEach(async () => {
    await resetTestDatabase();
    await db.insert(usersTable).values({
      id: OWNER,
      username: "owner",
      email: "owner@example.com",
      passwordHash: "hash",
    });
    await db.insert(customBotsTable).values({
      id: CUSTOM_BOT,
      userId: OWNER,
      name: "sma-bot",
      version: "1.0.0",
      config: {} as never,
    });
  });

  describe("commit a bot change and its events together", () => {
    it("stores a deployed bot with its event, unpublished, payload as JSON text", async () => {
      const created = event("bot-1", "the0.bot-schedule.created");

      const result = await repository.createWithEvents(newBot("bot-1"), [
        created,
      ]);

      expect(result.success).toBe(true);
      expect(result.data).toEqual(expect.objectContaining({ id: "bot-1" }));
      expect(await botRows()).toHaveLength(1);
      expect(await outboxRows()).toEqual([
        expect.objectContaining({
          id: 1,
          botId: "bot-1",
          subject: "the0.bot-schedule.created",
          payload: JSON.stringify(created.payload),
          publishedAt: null,
        }),
      ]);
    });

    it("stores an update with its event and returns the updated bot", async () => {
      await repository.createWithEvents(newBot("bot-1"), []);

      const result = await repository.updateWithEvents(
        OWNER,
        "bot-1",
        { name: "SMA v2" },
        [event("bot-1", "the0.bot-schedule.updated")],
      );

      expect(result.success).toBe(true);
      expect(result.data).toEqual(
        expect.objectContaining({ id: "bot-1", name: "SMA v2" }),
      );
      expect((await outboxRows()).map((row) => row.subject)).toEqual([
        "the0.bot-schedule.updated",
      ]);
    });

    it("deletes a bot with its event", async () => {
      await repository.createWithEvents(newBot("bot-1"), []);

      const result = await repository.removeWithEvents(OWNER, "bot-1", [
        event("bot-1", "the0.bot-schedule.deleted"),
      ]);

      expect(result.success).toBe(true);
      expect(await botRows()).toEqual([]);
      expect((await outboxRows()).map((row) => row.subject)).toEqual([
        "the0.bot-schedule.deleted",
      ]);
    });

    it("numbers events in the order their changes commit", async () => {
      await repository.createWithEvents(newBot("bot-1"), [
        event("bot-1", "the0.bot-schedule.created"),
      ]);
      await repository.updateWithEvents(OWNER, "bot-1", { name: "v2" }, [
        event("bot-1", "the0.bot-schedule.updated"),
      ]);
      await repository.removeWithEvents(OWNER, "bot-1", [
        event("bot-1", "the0.bot-schedule.deleted"),
      ]);

      expect(
        (await outboxRows()).map(({ id, subject }) => [id, subject]),
      ).toEqual([
        [1, "the0.bot-schedule.created"],
        [2, "the0.bot-schedule.updated"],
        [3, "the0.bot-schedule.deleted"],
      ]);
    });
  });

  describe("roll back the bot change when its events cannot be stored", () => {
    it("does not deploy the bot", async () => {
      const result = await repository.createWithEvents(newBot("bot-1"), [
        event("bot-1", UNSTORABLE_SUBJECT),
      ]);

      expect(result.success).toBe(false);
      expect(await botRows()).toEqual([]);
      expect(await outboxRows()).toEqual([]);
    });

    it("keeps the bot as it was", async () => {
      await repository.createWithEvents(newBot("bot-1"), []);

      const result = await repository.updateWithEvents(
        OWNER,
        "bot-1",
        { name: "SMA v2" },
        [event("bot-1", UNSTORABLE_SUBJECT)],
      );

      expect(result.success).toBe(false);
      expect((await botRows()).map((bot) => bot.name)).toEqual(["SMA"]);
      expect(await outboxRows()).toEqual([]);
    });

    it("does not delete the bot", async () => {
      await repository.createWithEvents(newBot("bot-1"), []);

      const result = await repository.removeWithEvents(OWNER, "bot-1", [
        event("bot-1", UNSTORABLE_SUBJECT),
      ]);

      expect(result.success).toBe(false);
      expect(await botRows()).toHaveLength(1);
      expect(await outboxRows()).toEqual([]);
    });
  });

  describe("store no event when the bot change does not happen", () => {
    it("stores no event when the bot cannot be stored", async () => {
      const result = await repository.createWithEvents(
        newBot("bot-1", "no-such-user"),
        [event("bot-1", "the0.bot-schedule.created")],
      );

      expect(result.success).toBe(false);
      expect(await outboxRows()).toEqual([]);
    });

    it("stores no event for another user's bot and reports it not found", async () => {
      await repository.createWithEvents(newBot("bot-1"), []);

      const updated = await repository.updateWithEvents(
        "user-2",
        "bot-1",
        { name: "stolen" },
        [event("bot-1", "the0.bot-schedule.updated")],
      );
      const removed = await repository.removeWithEvents("user-2", "bot-1", [
        event("bot-1", "the0.bot-schedule.deleted"),
      ]);

      expect(updated).toEqual(Failure("Not found"));
      expect(removed.success).toBe(true);
      expect(await botRows()).toHaveLength(1);
      expect(await outboxRows()).toEqual([]);
    });
  });
});
