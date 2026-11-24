import { BotRepository } from "../bot.repository";
import { BotEvent } from "../bot-event-outbox";
import { appError, WRITE_FAILED } from "@/common/app-error";
import { Failure } from "@/common/result";
import { botsTable } from "@/database/schema/bots";
import { botEventOutboxTable } from "@/database/schema/bot-event-outbox";
import { customBotsTable } from "@/database/schema/custom-bots";
import { usersTable } from "@/database/schema/users";
import { mockBotConfig } from "@/test/mock-bot";
import { TEST_OWNER_ID, useTestDatabase } from "@/test/test-database";

jest.mock(
  "@/database/connection",
  () => jest.requireActual("@/test/test-database").connection,
);
jest.mock("pino", () => ({
  __esModule: true,
  default: () => ({ error: jest.fn(), warn: jest.fn(), info: jest.fn() }),
}));

const CUSTOM_BOT = "sma-1.0.0";
const NAME = "mm-btc-paper";
const conflict = Failure(
  appError(
    "conflict",
    `Bot instance named "${NAME}" already exists. Use "bot update" to modify it.`,
  ),
);

function newBot(id: string, name = NAME, userId = TEST_OWNER_ID) {
  return {
    id,
    userId,
    name,
    customBotId: CUSTOM_BOT,
    config: mockBotConfig({ name }),
  };
}

function event(botId: string): BotEvent {
  return {
    botId,
    subject: "the0.bot-schedule.created",
    payload: { id: botId },
  };
}

describe("bot names within an owner's account", () => {
  const db = useTestDatabase();
  let repository: BotRepository;

  beforeAll(() => {
    repository = new BotRepository();
  });

  beforeEach(async () => {
    await db()
      .insert(customBotsTable)
      .values({
        id: CUSTOM_BOT,
        userId: TEST_OWNER_ID,
        name: "sma",
        version: "1.0.0",
        config: {} as never,
      });
  });

  it("commits only one competing deploy and no event for the refused deploy", async () => {
    expect(await repository.hasBotNamed(TEST_OWNER_ID, NAME)).toEqual({
      success: true,
      data: false,
      error: null,
    });
    const results = await Promise.all([
      repository.createWithEvents(newBot("bot-1"), [event("bot-1")]),
      repository.createWithEvents(newBot("bot-2"), [event("bot-2")]),
    ]);

    expect(results.filter((result) => result.success)).toHaveLength(1);
    expect(results.find((result) => !result.success)).toEqual(conflict);
    const bots = await db().select().from(botsTable);
    expect(bots).toHaveLength(1);
    expect(await db().select().from(botEventOutboxTable)).toEqual([
      expect.objectContaining({ botId: bots[0].id }),
    ]);
  });

  it("refuses duplicate names even when a writer bypasses the repository", async () => {
    await db().insert(botsTable).values(newBot("bot-1"));
    await expect(
      db().insert(botsTable).values(newBot("bot-2")),
    ).rejects.toMatchObject({ cause: { code: "23505" } });
  });

  it("leaves the bot and outbox unchanged when a rename collides", async () => {
    await db()
      .insert(botsTable)
      .values([newBot("bot-1"), newBot("bot-2", "other-name")]);
    const before = await db().select().from(botsTable).orderBy(botsTable.id);

    expect(
      await repository.updateWithEvents(
        TEST_OWNER_ID,
        "bot-2",
        { name: NAME, config: mockBotConfig({ name: NAME }) },
        [event("bot-2")],
      ),
    ).toEqual(conflict);
    expect(await db().select().from(botsTable).orderBy(botsTable.id)).toEqual(
      before,
    );
    expect(await db().select().from(botEventOutboxTable)).toEqual([]);
  });

  it("allows an update that keeps the bot's own name", async () => {
    await db().insert(botsTable).values(newBot("bot-1"));
    const result = await repository.updateWithEvents(
      TEST_OWNER_ID,
      "bot-1",
      { name: NAME },
      [event("bot-1")],
    );
    expect(result.success).toBe(true);
    expect(await db().select().from(botEventOutboxTable)).toHaveLength(1);
  });

  it("allows the same name for another owner and keeps matching case-sensitive", async () => {
    await db().insert(usersTable).values({
      id: "user-2",
      username: "other",
      email: "other@example.com",
      passwordHash: "hash",
    });
    for (const bot of [
      newBot("bot-1"),
      newBot("bot-2", NAME, "user-2"),
      newBot("bot-3", NAME.toUpperCase()),
    ]) {
      expect((await repository.createWithEvents(bot, [])).success).toBe(true);
    }
    expect(await db().select().from(botsTable)).toHaveLength(3);
  });

  it("allows a name to be reused after its bot is deleted", async () => {
    await repository.createWithEvents(newBot("bot-1"), []);
    await repository.removeWithEvents(TEST_OWNER_ID, "bot-1", []);
    expect(
      (await repository.createWithEvents(newBot("bot-2"), [])).success,
    ).toBe(true);
  });

  it("keeps unrelated unique failures as write failures", async () => {
    await repository.createWithEvents(newBot("bot-1"), []);
    expect(
      await repository.createWithEvents(newBot("bot-1", "other-name"), [
        event("bot-1"),
      ]),
    ).toEqual(Failure(WRITE_FAILED));
    expect(await db().select().from(botEventOutboxTable)).toEqual([]);
  });
});
