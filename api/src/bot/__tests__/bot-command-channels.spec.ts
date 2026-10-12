import { INestApplication } from "@nestjs/common";
import { Test } from "@nestjs/testing";
import { PinoLogger } from "nestjs-pino";
import request from "supertest";
import { BotController } from "../bot.controller";
import { BotService } from "../bot.service";
import { DeleteBot, DeployBot, UpdateBot } from "../commands/bot-commands";
import { McpController } from "@/mcp/mcp.controller";
import { McpService } from "@/mcp/mcp.service";
import { ApiKeyService } from "@/api-key/api-key.service";
import { AuthService } from "@/auth/auth.service";
import { Actor } from "@/auth/actor";
import { BotQueryService } from "@/bot-query/bot-query.service";
import { BotStateService } from "@/bot-state/bot-state.service";
import { CommandBus } from "@/command-bus/command-bus";
import { Command } from "@/command-bus/command";
import { Ok } from "@/common/result";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { LogsService } from "@/logs/logs.service";
import { mockActor } from "@/test/mock-actor";
import { mockBot, mockBotConfig } from "@/test/mock-bot";
import { createMockLogger } from "@/test/mock-logger";

const API_KEY = { "x-api-key": "the0_valid" };
const restActor = mockActor({
  userId: "user-1",
  via: "api-key",
  apiKeyId: "key-1",
});
const mcpActor = mockActor({ userId: "user-1", via: "mcp", apiKeyId: "key-1" });

const config = mockBotConfig({
  name: "Momentum",
  type: "scheduled/test-bot",
  version: "1.0.0",
  schedule: "0 9 * * *",
});

describe("Bot commands from REST and MCP", () => {
  let app: INestApplication;
  const bus = {
    execute: jest.fn(async (command: Command<unknown>) =>
      command instanceof DeleteBot
        ? Ok({})
        : Ok(mockBot({ id: "bot-1", name: "Momentum", config })),
    ),
  };

  beforeAll(async () => {
    const moduleRef = await Test.createTestingModule({
      controllers: [BotController, McpController],
      providers: [
        McpService,
        { provide: CommandBus, useValue: bus },
        { provide: BotService, useValue: {} },
        {
          provide: ApiKeyService,
          useValue: {
            validateApiKey: jest
              .fn()
              .mockResolvedValue(
                Ok({ id: "key-1", userId: "user-1", name: "agent" }),
              ),
          },
        },
        { provide: AuthService, useValue: {} },
        { provide: CustomBotService, useValue: {} },
        { provide: LogsService, useValue: {} },
        { provide: BotStateService, useValue: {} },
        { provide: BotQueryService, useValue: {} },
        { provide: PinoLogger, useValue: createMockLogger() },
      ],
    }).compile();

    app = moduleRef.createNestApplication();
    await app.init();
  });

  afterAll(async () => {
    await app.close();
  });

  beforeEach(() => {
    bus.execute.mockClear();
  });

  function callTool(name: string, args: Record<string, unknown>) {
    return request(app.getHttpServer())
      .post("/mcp")
      .set(API_KEY)
      .send({
        jsonrpc: "2.0",
        id: 1,
        method: "tools/call",
        params: { name, arguments: args },
      });
  }

  function dispatched(): Command<unknown>[] {
    return bus.execute.mock.calls.map(([command]) => command);
  }

  describe("dispatch the same Bot command for the same input, differing only in the Actor's channel", () => {
    it.each([
      {
        change: "a deploy",
        rest: () =>
          request(app.getHttpServer())
            .post("/bot")
            .set(API_KEY)
            .send({ name: "Momentum", config }),
        mcp: () => callTool("bot_deploy", { config }),
        command: (actor: Actor) =>
          new DeployBot(actor, { name: "Momentum", config }),
      },
      {
        change: "an update with a new name",
        rest: () =>
          request(app.getHttpServer())
            .put("/bot/bot-1")
            .set(API_KEY)
            .send({ name: "Renamed", config }),
        mcp: () =>
          callTool("bot_update", { bot_id: "bot-1", name: "Renamed", config }),
        command: (actor: Actor) =>
          new UpdateBot(actor, "bot-1", { name: "Renamed", config }),
      },
      {
        change: "an update that keeps the name",
        rest: () =>
          request(app.getHttpServer())
            .put("/bot/bot-1")
            .set(API_KEY)
            .send({ config }),
        mcp: () => callTool("bot_update", { bot_id: "bot-1", config }),
        command: (actor: Actor) => new UpdateBot(actor, "bot-1", { config }),
      },
      {
        change: "a delete",
        rest: () =>
          request(app.getHttpServer()).delete("/bot/bot-1").set(API_KEY),
        mcp: () => callTool("bot_delete", { bot_id: "bot-1" }),
        command: (actor: Actor) => new DeleteBot(actor, "bot-1"),
      },
    ])("$change", async ({ rest, mcp, command }) => {
      const restResponse = await rest();
      const mcpResponse = await mcp();

      expect(restResponse.status).toBeLessThan(300);
      expect(mcpResponse.body.result.isError).toBeUndefined();
      expect(dispatched()).toStrictEqual([
        command(restActor),
        command(mcpActor),
      ]);
    });
  });
});
