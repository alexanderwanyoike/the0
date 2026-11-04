import { Test } from "@nestjs/testing";
import { PinoLogger } from "nestjs-pino";
import { BotRepository } from "../bot.repository";
import { BotValidator } from "../bot.validator";
import { BotEventRelay } from "../bot-event-relay";
import { SCHEDULED_BOT_TOPICS } from "../bot.constants";
import { DeleteBot, DeployBot, UpdateBot } from "../commands/bot-commands";
import { CommandBus } from "@/command-bus/command-bus";
import { CustomBotRepository } from "@/custom-bot/custom-bot.repository";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { StorageService } from "@/custom-bot/storage.service";
import { botEventOutboxTable } from "@/database/schema/bot-event-outbox";
import { customBotsTable } from "@/database/schema/custom-bots";
import { usersTable } from "@/database/schema/users";
import { BOT_COMMAND_PROVIDERS } from "@/test/bot-command-providers";
import { mockActor } from "@/test/mock-actor";
import { createMockLogger } from "@/test/mock-logger";
import { TEST_OWNER_ID, useTestDatabase } from "@/test/test-database";

jest.mock(
  "@/database/connection",
  () => jest.requireActual("@/test/test-database").connection,
);

const UPLOADER = "uploader-1";
const DEPLOYER = mockActor({ userId: TEST_OWNER_ID });

const customBotVersion = (id: string, version: string) => ({
  id,
  userId: UPLOADER,
  name: "sma-bot",
  version,
  config: {
    name: "sma-bot",
    version,
    type: "scheduled",
    runtime: "python3.11",
    entrypoints: { bot: "main.py" },
    schema: { bot: { type: "object" } },
  } as never,
  filePath: `${UPLOADER}/sma-bot/${version}`,
});

const botConfig = (version: string) => ({
  name: "SMA",
  type: "scheduled/sma-bot",
  version,
  schedule: "0 * * * *",
});

describe("Bot commands on another Uploader's Custom bot", () => {
  const db = useTestDatabase();
  let commands: CommandBus;

  beforeAll(async () => {
    const moduleRef = await Test.createTestingModule({
      providers: [
        ...BOT_COMMAND_PROVIDERS,
        BotRepository,
        BotValidator,
        CustomBotService,
        CustomBotRepository,
        { provide: StorageService, useValue: {} },
        { provide: BotEventRelay, useValue: { publishCommitted: jest.fn() } },
        { provide: PinoLogger, useValue: createMockLogger() },
      ],
    }).compile();
    await moduleRef.init();
    commands = moduleRef.get(CommandBus);
  });

  beforeEach(async () => {
    await db().insert(usersTable).values({
      id: UPLOADER,
      username: "uploader",
      email: "uploader@example.com",
      passwordHash: "hash",
    });
    await db()
      .insert(customBotsTable)
      .values([
        customBotVersion("cb-1", "1.0.0"),
        customBotVersion("cb-2", "1.1.0"),
      ]);
  });

  const outbox = async () =>
    (await db().select().from(botEventOutboxTable)).map((row) => ({
      botId: row.botId,
      subject: row.subject,
      payload: JSON.parse(row.payload),
    }));

  const deploy = async () => {
    const result = await commands.execute(
      new DeployBot(DEPLOYER, { name: "SMA", config: botConfig("1.0.0") }),
    );
    expect(result.success).toBe(true);
    return result.data!.id;
  };

  it("stores the created event for the runtime", async () => {
    const id = await deploy();

    expect(await outbox()).toEqual([
      {
        botId: id,
        subject: SCHEDULED_BOT_TOPICS.CREATED,
        payload: expect.objectContaining({
          id,
          config: expect.objectContaining({ customBotId: "cb-1" }),
          custom: expect.objectContaining({
            version: "1.0.0",
            filePath: `${UPLOADER}/sma-bot/1.0.0`,
          }),
        }),
      },
    ]);
  });

  it("stores the updated event for the runtime", async () => {
    const id = await deploy();

    const result = await commands.execute(
      new UpdateBot(DEPLOYER, id, { config: botConfig("1.1.0") }),
    );

    expect(result.success).toBe(true);
    expect((await outbox()).map(({ subject }) => subject)).toEqual([
      SCHEDULED_BOT_TOPICS.CREATED,
      SCHEDULED_BOT_TOPICS.UPDATED,
    ]);
    expect((await outbox())[1].payload.custom.version).toBe("1.1.0");
  });

  it("stores the deleted event on the Custom bot's subject only", async () => {
    const id = await deploy();

    const result = await commands.execute(new DeleteBot(DEPLOYER, id));

    expect(result.success).toBe(true);
    expect((await outbox()).map(({ subject }) => subject)).toEqual([
      SCHEDULED_BOT_TOPICS.CREATED,
      SCHEDULED_BOT_TOPICS.DELETED,
    ]);
  });
});
