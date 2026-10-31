import { INestApplication, ValidationPipe } from "@nestjs/common";
import { Test } from "@nestjs/testing";
import { PinoLogger } from "nestjs-pino";
import request from "supertest";
import { BotController } from "../bot.controller";
import { BotRepository } from "../bot.repository";
import { BotService } from "../bot.service";
import { BotValidator } from "../bot.validator";
import { BotEventRelay } from "../bot-event-relay";
import { ApiKeyService } from "@/api-key/api-key.service";
import { AuthService } from "@/auth/auth.service";
import { Ok } from "@/common/result";
import { CustomBotRepository } from "@/custom-bot/custom-bot.repository";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { StorageService } from "@/custom-bot/storage.service";
import { botsTable } from "@/database/schema/bots";
import { customBotsTable } from "@/database/schema/custom-bots";
import { BOT_COMMAND_PROVIDERS } from "@/test/bot-command-providers";
import { createMockLogger } from "@/test/mock-logger";
import { TEST_OWNER_ID, useTestDatabase } from "@/test/test-database";

jest.mock(
  "@/database/connection",
  () => jest.requireActual("@/test/test-database").connection,
);

const CUSTOM_BOT_CONFIG = {
  name: "sma-bot",
  description: "SMA crossover",
  version: "1.0.0",
  type: "scheduled",
  runtime: "python3.11",
  author: "owner",
  entrypoints: { bot: "main.py" },
  schema: {
    bot: {
      type: "object",
      properties: { symbol: { type: "string" } },
      required: ["symbol"],
    },
  },
  readme: "",
};

const botConfig = (overrides: Record<string, unknown> = {}) => ({
  name: "SMA",
  type: "scheduled/sma-bot",
  version: "1.0.0",
  schedule: "0 * * * *",
  symbol: "AAPL",
  ...overrides,
});

describe("/bot responses", () => {
  const db = useTestDatabase();
  let app: INestApplication;
  const apiKeyService = { validateApiKey: jest.fn() };

  beforeAll(async () => {
    const moduleRef = await Test.createTestingModule({
      controllers: [BotController],
      providers: [
        ...BOT_COMMAND_PROVIDERS,
        BotService,
        BotRepository,
        BotValidator,
        CustomBotService,
        CustomBotRepository,
        { provide: StorageService, useValue: {} },
        { provide: BotEventRelay, useValue: { publishCommitted: jest.fn() } },
        { provide: ApiKeyService, useValue: apiKeyService },
        { provide: AuthService, useValue: { validateToken: jest.fn() } },
        { provide: PinoLogger, useValue: createMockLogger() },
      ],
    }).compile();

    app = moduleRef.createNestApplication();
    // As main.ts configures it.
    app.useGlobalPipes(
      new ValidationPipe({
        whitelist: true,
        forbidNonWhitelisted: true,
        transform: true,
      }),
    );
    await app.init();
  });

  afterAll(async () => {
    await app.close();
  });

  beforeEach(async () => {
    jest.restoreAllMocks();
    apiKeyService.validateApiKey.mockResolvedValue(
      Ok({ id: "key-1", userId: TEST_OWNER_ID, name: "cli" }),
    );
    await db()
      .insert(customBotsTable)
      .values({
        id: "cb-1",
        userId: TEST_OWNER_ID,
        name: "sma-bot",
        version: "1.0.0",
        config: CUSTOM_BOT_CONFIG as never,
        filePath: `${TEST_OWNER_ID}/sma-bot/1.0.0`,
      });
  });

  const api = () => request(app.getHttpServer());
  const asOwner = { "x-api-key": "the0_valid" };

  const deployedBot = async () => {
    const res = await api()
      .post("/bot")
      .set(asOwner)
      .send({ name: "SMA", config: botConfig() });
    expect(res.status).toBe(201);
    return res.body.id as string;
  };

  describe("refuse invalid deploys with 400 and the reason", () => {
    it.each([
      {
        problem: "a type without a kind",
        config: botConfig({ type: "sma-bot" }),
        message: "Invalid bot type format. Expected format: type/name",
      },
      {
        problem: "a version that is not semver",
        config: botConfig({ version: "one" }),
        message: "Invalid version format. Expected format: x.y.z",
      },
      {
        problem: "a Custom bot missing from the Catalogue",
        config: botConfig({ type: "scheduled/nope" }),
        message: "Bot type 'scheduled/nope' not found",
      },
      {
        problem: "a config the Custom bot's schema rejects",
        config: botConfig({ symbol: undefined }),
        message: "must have required property 'symbol'",
      },
    ])("refuses $problem", async ({ config, message }) => {
      const res = await api()
        .post("/bot")
        .set(asOwner)
        .send({ name: "SMA", config });

      expect(res.status).toBe(400);
      expect(res.body).toEqual({
        statusCode: 400,
        message,
        error: "Bad Request",
      });
    });

    it("refuses a body without a name through the DTO", async () => {
      const res = await api()
        .post("/bot")
        .set(asOwner)
        .send({ config: botConfig() });

      expect(res.status).toBe(400);
      expect(res.body.message).toEqual(
        expect.arrayContaining(["name must be a string"]),
      );
    });
  });

  describe("answer a bot the user cannot see", () => {
    it("answers a read with 404 Not found", async () => {
      const res = await api().get("/bot/missing").set(asOwner);

      expect(res.status).toBe(404);
      expect(res.body).toEqual({
        statusCode: 404,
        message: "Not found",
        error: "Not Found",
      });
    });

    it("answers an update with 400 Bot not found", async () => {
      const res = await api()
        .put("/bot/missing")
        .set(asOwner)
        .send({ config: botConfig() });

      expect(res.status).toBe(400);
      expect(res.body).toEqual({
        statusCode: 400,
        message: "Bot not found",
        error: "Bad Request",
      });
    });

    it("answers a delete with 400 Bot not found", async () => {
      const res = await api().delete("/bot/missing").set(asOwner);

      expect(res.status).toBe(400);
      expect(res.body).toEqual({
        statusCode: 400,
        message: "Bot not found",
        error: "Bad Request",
      });
    });
  });

  describe("refuse an update that needs a redeploy", () => {
    it("refuses a major version change with 400", async () => {
      const id = await deployedBot();
      await db()
        .insert(customBotsTable)
        .values({
          id: "cb-2",
          userId: TEST_OWNER_ID,
          name: "sma-bot",
          version: "2.0.0",
          config: { ...CUSTOM_BOT_CONFIG, version: "2.0.0" } as never,
          filePath: `${TEST_OWNER_ID}/sma-bot/2.0.0`,
        });

      const res = await api()
        .put(`/bot/${id}`)
        .set(asOwner)
        .send({ config: botConfig({ version: "2.0.0" }) });

      expect(res.status).toBe(400);
      expect(res.body.message).toBe(
        "Major version upgrade (1.0.0 -> 2.0.0) requires delete and redeploy. " +
          "In-place updates are only supported for minor/patch bumps to preserve state compatibility.",
      );
    });
  });

  describe("succeed", () => {
    it("deploys, reads, updates and deletes a bot", async () => {
      const id = await deployedBot();

      const read = await api().get(`/bot/${id}`).set(asOwner);
      const updated = await api()
        .put(`/bot/${id}`)
        .set(asOwner)
        .send({ config: botConfig({ symbol: "MSFT" }) });
      const deleted = await api().delete(`/bot/${id}`).set(asOwner);

      expect(read.status).toBe(200);
      expect(updated.status).toBe(200);
      expect(updated.body.config.symbol).toBe("MSFT");
      expect(deleted.status).toBe(200);
      expect(deleted.body).toEqual({
        orphanedVersion: {
          name: "sma-bot",
          version: "1.0.0",
          customBotId: "cb-1",
        },
      });
      expect(await db().select().from(botsTable)).toEqual([]);
    });
  });
});
