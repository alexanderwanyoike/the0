import { Test, TestingModule } from "@nestjs/testing";
import { ModuleRef } from "@nestjs/core";
import { PinoLogger } from "nestjs-pino";
import { McpService } from "../mcp.service";
import { Actor } from "@/auth/actor";
import { BotService } from "@/bot/bot.service";
import { BotRepository } from "@/bot/bot.repository";
import { BotValidator } from "@/bot/bot.validator";
import { SCHEDULED_BOT_TOPICS } from "@/bot/bot.constants";
import { Bot } from "@/bot/entities/bot.entity";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { CustomBot } from "@/custom-bot/custom-bot.types";
import { LogsService } from "@/logs/logs.service";
import { ApiKeyService } from "@/api-key/api-key.service";
import { BotStateService } from "@/bot-state/bot-state.service";
import { BotQueryService } from "@/bot-query/bot-query.service";
import { NatsService } from "@/nats/nats.service";
import { Ok, Failure } from "@/common/result";
import { createMockLogger } from "@/test/mock-logger";
import { mockBot, mockBotConfig } from "@/test/mock-bot";

const CATALOGUE_VERSIONS = ["1.0.0", "1.1.0", "2.0.0"];

const catalogueVersion = (version: string): CustomBot => ({
  id: `custom-bot-${version}`,
  name: "test-bot",
  version,
  userId: "author-123",
  config: {
    name: "test-bot",
    version,
    description: "A test bot",
    type: "scheduled",
    runtime: "python3.11",
    author: "Test Author",
    entrypoints: { bot: "main.py" },
    schema: { bot: { type: "object", properties: {} } },
    readme: "# Test Bot",
  },
  filePath: "/path/to/bot.zip",
  status: "active",
  createdAt: new Date("2026-01-01"),
  updatedAt: new Date("2026-01-01"),
});

const findInCatalogue = async (name: string, version: string) =>
  name === "test-bot" && CATALOGUE_VERSIONS.includes(version)
    ? Ok<CustomBot, string>(catalogueVersion(version))
    : Failure<CustomBot, string>("Not found");

/** Scopes every query by user, as RoleRepository does in SQL. */
function createBotStore(seed: Bot[]) {
  const bots = new Map(seed.map((bot) => [bot.id, bot]));
  const owned = (userId: string, id: string) => {
    const bot = bots.get(id);
    return bot?.userId === userId ? bot : undefined;
  };
  let created = 0;

  return {
    bots,
    async findAll(userId: string) {
      return Ok([...bots.values()].filter((bot) => bot.userId === userId));
    },
    async findOne(userId: string, id: string) {
      const bot = owned(userId, id);
      return bot ? Ok(bot) : Failure("Not found");
    },
    async create(data: Partial<Bot>) {
      created += 1;
      const bot = {
        ...data,
        id: `bot-new-${created}`,
        createdAt: new Date(),
        updatedAt: new Date(),
      } as Bot;
      bots.set(bot.id, bot);
      return Ok(bot);
    },
    async update(userId: string, id: string, data: Partial<Bot>) {
      const bot = owned(userId, id);
      if (!bot) return Failure("Not found");
      const updated = { ...bot, ...data, updatedAt: new Date() };
      bots.set(id, updated);
      return Ok(updated);
    },
    async remove(userId: string, id: string) {
      if (owned(userId, id)) bots.delete(id);
      return Ok(null);
    },
  };
}

describe("McpService", () => {
  let service: McpService;
  let store: ReturnType<typeof createBotStore>;
  let natsService: { publish: jest.Mock };
  let customBotService: jest.Mocked<CustomBotService>;
  let logsService: jest.Mocked<LogsService>;
  let botStateService: { listKeys: jest.Mock; getKey: jest.Mock };
  let botQueryService: { executeQuery: jest.Mock };
  let moduleRef: ModuleRef;

  const userId = "user-123";
  const otherUserId = "user-456";
  const actor: Actor = { userId, via: "mcp", apiKeyId: "key-123" };

  const callerBot = mockBot({
    id: "bot-123",
    name: "Test Bot",
    userId,
    customBotId: "custom-bot-1.0.0",
    config: mockBotConfig({
      name: "Test Bot",
      type: "scheduled/test-bot",
      version: "1.0.0",
      schedule: "0 9 * * *",
    }),
  });

  const otherUsersBot = mockBot({
    id: "bot-456",
    name: "Someone Else's Bot",
    userId: otherUserId,
    customBotId: "custom-bot-1.0.0",
    config: mockBotConfig({
      name: "Someone Else's Bot",
      type: "scheduled/test-bot",
      version: "1.0.0",
      schedule: "0 9 * * *",
    }),
  });

  const mockCustomBot = {
    id: "custom-bot-123",
    name: "test-custom-bot",
    version: "1.0.0",
    userId: "author-123",
    config: {
      name: "test-custom-bot",
      version: "1.0.0",
      description: "A test bot",
      type: "scheduled" as const,
      runtime: "python3.11" as const,
      author: "Test Author",
      entrypoints: { bot: "main.py" },
      schema: { bot: { type: "object", properties: {} } },
      readme: "# Test Bot\n\nA test bot for testing.",
    },
    filePath: "/path/to/bot.zip",
    status: "active" as const,
    createdAt: new Date(),
    updatedAt: new Date(),
  };

  const mockLogEntry = {
    date: "20251202",
    content: "Bot executed successfully",
    timestamp: "2025-12-02T10:00:00Z",
  };

  const parse = (result: { content: Array<{ text: string }> }) =>
    JSON.parse(result.content[0].text);

  beforeEach(async () => {
    store = createBotStore([callerBot, otherUsersBot]);

    natsService = { publish: jest.fn().mockResolvedValue(Ok(null)) };

    const mockCustomBotService = {
      getAllGlobalCustomBots: jest.fn(),
      getAllGlobalVersions: jest.fn(),
      getGlobalLatestVersion: jest.fn(),
      getGlobalSpecificVersion: jest.fn(findInCatalogue),
      // BotService builds update and delete events from the caller's own
      // copy of the custom bot; answering from the shared catalogue keeps
      // these tests about MCP rather than about that lookup.
      getUserSpecificVersion: jest.fn(
        (_uid: string, name: string, version: string) =>
          findInCatalogue(name, version),
      ),
      checkOrphaned: jest
        .fn()
        .mockResolvedValue(
          Ok({ orphaned: false, name: "test-bot", version: "1.0.0" }),
        ),
    };

    const mockLogsService = {
      getLogs: jest.fn(),
    };

    const mockBotStateService = {
      listKeys: jest.fn(),
      getKey: jest.fn(),
    };

    const mockBotQueryService = {
      executeQuery: jest.fn(),
    };

    const module: TestingModule = await Test.createTestingModule({
      providers: [
        McpService,
        BotService,
        BotValidator,
        { provide: BotRepository, useValue: store },
        { provide: NatsService, useValue: natsService },
        { provide: CustomBotService, useValue: mockCustomBotService },
        { provide: LogsService, useValue: mockLogsService },
        { provide: ApiKeyService, useValue: { validateApiKey: jest.fn() } },
        { provide: BotStateService, useValue: mockBotStateService },
        { provide: BotQueryService, useValue: mockBotQueryService },
        { provide: PinoLogger, useValue: createMockLogger() },
      ],
    }).compile();

    service = module.get<McpService>(McpService);
    customBotService = module.get(CustomBotService);
    logsService = module.get(LogsService);
    botStateService = mockBotStateService;
    botQueryService = mockBotQueryService;
    moduleRef = module.get(ModuleRef);
  });

  describe("handleToolCall", () => {
    describe("auth_status", () => {
      it("should return valid status", async () => {
        const result = await service.handleToolCall("auth_status", {}, actor);

        expect(result.isError).toBeUndefined();
        expect(result.content[0].type).toBe("text");
        const data = JSON.parse(result.content[0].text);
        expect(data.valid).toBe(true);
      });
    });

    describe("reading bots shows only the caller's own", () => {
      it("lists summaries of the caller's bots and none of another user's", async () => {
        const result = await service.handleToolCall("bot_list", {}, actor);

        expect(result.isError).toBeUndefined();
        expect(parse(result)).toEqual([
          {
            id: "bot-123",
            name: "Test Bot",
            type: "scheduled/test-bot",
            version: "1.0.0",
            createdAt: callerBot.createdAt.toISOString(),
            updatedAt: callerBot.updatedAt.toISOString(),
          },
        ]);
      });

      it("returns one of the caller's bots in full", async () => {
        const result = await service.handleToolCall(
          "bot_get",
          { bot_id: "bot-123" },
          actor,
        );

        expect(result.isError).toBeUndefined();
        expect(parse(result)).toEqual(JSON.parse(JSON.stringify(callerBot)));
      });

      it("treats another user's bot as not found", async () => {
        const result = await service.handleToolCall(
          "bot_get",
          { bot_id: "bot-456" },
          actor,
        );

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("Not found");
        expect(result.content[0].text).not.toContain("Someone Else's Bot");
      });
    });

    describe("deploying a bot stores it and tells the runtime", () => {
      const deployConfig = mockBotConfig({
        name: "Momentum",
        type: "scheduled/test-bot",
        version: "1.1.0",
        schedule: "0 9 * * *",
      });

      it("stores the bot for the caller and publishes the created event", async () => {
        const result = await service.handleToolCall(
          "bot_deploy",
          { config: deployConfig },
          actor,
        );

        expect(result.isError).toBeUndefined();
        expect(parse(result)).toEqual({
          message: "Bot deployed successfully",
          bot: {
            id: "bot-new-1",
            name: "Momentum",
            type: "scheduled/test-bot",
            version: "1.1.0",
          },
        });
        expect(store.bots.get("bot-new-1")).toMatchObject({
          name: "Momentum",
          userId,
          customBotId: "custom-bot-1.1.0",
        });
        expect(natsService.publish).toHaveBeenCalledWith(
          SCHEDULED_BOT_TOPICS.CREATED,
          expect.objectContaining({
            id: "bot-new-1",
            config: expect.objectContaining({
              ...deployConfig,
              customBotId: "custom-bot-1.1.0",
            }),
          }),
        );
      });

      it("requires a name in the config", async () => {
        const unnamed: Record<string, unknown> = { ...deployConfig };
        delete unnamed.name;

        const result = await service.handleToolCall(
          "bot_deploy",
          { config: unnamed },
          actor,
        );

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("name");
        expect(store.bots.size).toBe(2);
        expect(natsService.publish).not.toHaveBeenCalled();
      });
    });

    describe("updating a bot stores it and tells the runtime", () => {
      it("stores the new name and config and publishes the updated event", async () => {
        const newConfig = {
          ...callerBot.config,
          version: "1.1.0",
          schedule: "0 10 * * *",
        };

        const result = await service.handleToolCall(
          "bot_update",
          { bot_id: "bot-123", name: "Renamed", config: newConfig },
          actor,
        );

        expect(result.isError).toBeUndefined();
        expect(parse(result)).toEqual({
          message: "Bot updated successfully",
          bot: { id: "bot-123", name: "Renamed" },
        });
        expect(store.bots.get("bot-123")).toMatchObject({
          name: "Renamed",
          config: expect.objectContaining(newConfig),
          customBotId: "custom-bot-1.1.0",
        });
        expect(natsService.publish).toHaveBeenCalledWith(
          SCHEDULED_BOT_TOPICS.UPDATED,
          expect.objectContaining({
            id: "bot-123",
            config: expect.objectContaining({
              ...newConfig,
              customBotId: "custom-bot-1.1.0",
            }),
          }),
        );
      });

      it("keeps the name when none is given", async () => {
        await service.handleToolCall(
          "bot_update",
          { bot_id: "bot-123", config: callerBot.config },
          actor,
        );

        expect(store.bots.get("bot-123")?.name).toBe("Test Bot");
      });

      it("cannot update another user's bot", async () => {
        const result = await service.handleToolCall(
          "bot_update",
          { bot_id: "bot-456", config: otherUsersBot.config },
          actor,
        );

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toMatch(/not found/i);
        expect(store.bots.get("bot-456")).toEqual(otherUsersBot);
        expect(natsService.publish).not.toHaveBeenCalled();
      });
    });

    describe("deleting a bot removes it and tells the runtime", () => {
      it("removes the bot and publishes the deleted event", async () => {
        const result = await service.handleToolCall(
          "bot_delete",
          { bot_id: "bot-123" },
          actor,
        );

        expect(result.isError).toBeUndefined();
        expect(parse(result)).toEqual({
          message: "Bot deleted successfully",
          bot_id: "bot-123",
        });
        expect(store.bots.has("bot-123")).toBe(false);
        expect(natsService.publish).toHaveBeenCalledWith(
          SCHEDULED_BOT_TOPICS.DELETED,
          { id: "bot-123", config: callerBot.config },
        );
      });

      it("cannot delete another user's bot", async () => {
        const result = await service.handleToolCall(
          "bot_delete",
          { bot_id: "bot-456" },
          actor,
        );

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toMatch(/not found/i);
        expect(store.bots.get("bot-456")).toEqual(otherUsersBot);
        expect(natsService.publish).not.toHaveBeenCalled();
      });
    });

    describe("bot validation failures reach the caller", () => {
      const validConfig = mockBotConfig({
        name: "Momentum",
        type: "scheduled/test-bot",
        version: "1.0.0",
        schedule: "0 9 * * *",
      });

      it.each([
        {
          change: "a deploy of an unknown custom bot",
          tool: "bot_deploy",
          args: { config: { ...validConfig, type: "scheduled/unknown-bot" } },
          error: "Bot type 'scheduled/unknown-bot' not found",
        },
        {
          change: "a deploy with a version that is not semver",
          tool: "bot_deploy",
          args: { config: { ...validConfig, version: "1.0" } },
          error: "Invalid version format",
        },
        {
          change: "a deploy with an invalid schedule",
          tool: "bot_deploy",
          args: { config: { ...validConfig, schedule: "every morning" } },
          error: "every morning is not a valid cron expression",
        },
        {
          change: "an update across a major version",
          tool: "bot_update",
          args: {
            bot_id: "bot-123",
            config: { ...callerBot.config, version: "2.0.0" },
          },
          error: "Major version upgrade (1.0.0 -> 2.0.0)",
        },
        {
          change: "an update without the bot type and version",
          tool: "bot_update",
          args: { bot_id: "bot-123", config: { schedule: "0 10 * * *" } },
          error: "Bot type is required",
        },
      ])(
        "rejects $change without storing or publishing anything",
        async ({ tool, args, error }) => {
          const result = await service.handleToolCall(tool, args, actor);

          expect(result.isError).toBe(true);
          expect(result.content[0].text).toContain(error);
          expect([...store.bots.values()]).toEqual([callerBot, otherUsersBot]);
          expect(natsService.publish).not.toHaveBeenCalled();
        },
      );
    });

    describe("logs_get", () => {
      it("should return bot logs", async () => {
        logsService.getLogs.mockResolvedValue(
          Ok({ entries: [mockLogEntry], hasMore: false }),
        );

        const result = await service.handleToolCall(
          "logs_get",
          { bot_id: "bot-123", date: "20251202" },
          actor,
        );

        expect(result.isError).toBeUndefined();
        const data = JSON.parse(result.content[0].text);
        expect(data.bot_id).toBe("bot-123");
        expect(data.count).toBe(1);
        expect(data.logs[0].date).toBe("20251202");
      });

      it("should cap limit at 500", async () => {
        logsService.getLogs.mockResolvedValue(
          Ok({ entries: [], hasMore: false }),
        );

        await service.handleToolCall(
          "logs_get",
          { bot_id: "bot-123", limit: 1000 },
          actor,
        );

        expect(logsService.getLogs).toHaveBeenCalledWith(
          "bot-123",
          {
            date: undefined,
            dateRange: undefined,
            limit: 500,
            offset: 0,
          },
          "user-123",
        );
      });
    });

    describe("logs_summary", () => {
      it("should return log summary", async () => {
        logsService.getLogs.mockResolvedValue(
          Ok({ entries: [mockLogEntry], hasMore: false }),
        );

        const result = await service.handleToolCall(
          "logs_summary",
          { bot_id: "bot-123" },
          actor,
        );

        expect(result.isError).toBeUndefined();
        const data = JSON.parse(result.content[0].text);
        expect(data.bot_id).toBe("bot-123");
        expect(data.total_entries).toBe(1);
        expect(data.error_count).toBe(0);
      });

      it("should count errors in logs", async () => {
        const logsWithErrors = [
          {
            date: "20251202",
            content: "something went wrong with error",
            timestamp: "2025-12-02T10:00:00Z",
          },
          {
            date: "20251202",
            content: "success message",
            timestamp: "2025-12-02T10:01:00Z",
          },
          {
            date: "20251202",
            content: "task failed to complete",
            timestamp: "2025-12-02T10:02:00Z",
          },
        ];
        logsService.getLogs.mockResolvedValue(
          Ok({ entries: logsWithErrors, hasMore: false }),
        );

        const result = await service.handleToolCall(
          "logs_summary",
          { bot_id: "bot-123" },
          actor,
        );

        const data = JSON.parse(result.content[0].text);
        // 2 logs contain error keywords: "error" and "failed"
        expect(data.error_count).toBe(2);
      });
    });

    describe("bot_state_list", () => {
      it("should require a bot_id", async () => {
        const result = await service.handleToolCall(
          "bot_state_list",
          {},
          actor,
        );

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("bot_id");
        expect(botStateService.listKeys).not.toHaveBeenCalled();
      });

      it("should list state keys for a bot", async () => {
        botStateService.listKeys.mockResolvedValue(
          Ok([{ key: "positions", size: 128 }]),
        );

        const result = await service.handleToolCall(
          "bot_state_list",
          { bot_id: "bot-123" },
          actor,
        );

        expect(result.isError).toBeUndefined();
        expect(botStateService.listKeys).toHaveBeenCalledWith("bot-123");
        const data = JSON.parse(result.content[0].text);
        expect(data.bot_id).toBe("bot-123");
        expect(data.keys).toEqual([{ key: "positions", size: 128 }]);
      });

      it("should resolve the state service in a request context carrying the calling user", async () => {
        botStateService.listKeys.mockResolvedValue(Ok([]));
        const registerRequest = jest.spyOn(
          moduleRef,
          "registerRequestByContextId",
        );

        await service.handleToolCall(
          "bot_state_list",
          { bot_id: "bot-123" },
          actor,
        );

        expect(registerRequest).toHaveBeenCalledWith(
          { user: { uid: userId } },
          expect.anything(),
        );
      });

      it("should surface access errors", async () => {
        botStateService.listKeys.mockResolvedValue(
          Failure({
            code: "BOT_NOT_FOUND",
            message: "Bot not found or access denied",
          }),
        );

        const result = await service.handleToolCall(
          "bot_state_list",
          { bot_id: "bot-999" },
          actor,
        );

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("access denied");
      });
    });

    describe("bot_state_get", () => {
      it("should require a bot_id and key", async () => {
        const missingKey = await service.handleToolCall(
          "bot_state_get",
          { bot_id: "bot-123" },
          actor,
        );
        const missingBotId = await service.handleToolCall(
          "bot_state_get",
          { key: "positions" },
          actor,
        );

        expect(missingKey.isError).toBe(true);
        expect(missingKey.content[0].text).toContain("key");
        expect(missingBotId.isError).toBe(true);
        expect(missingBotId.content[0].text).toContain("bot_id");
        expect(botStateService.getKey).not.toHaveBeenCalled();
      });

      it("should return the state value for a key", async () => {
        botStateService.getKey.mockResolvedValue(Ok({ qty: 10 }));

        const result = await service.handleToolCall(
          "bot_state_get",
          { bot_id: "bot-123", key: "positions" },
          actor,
        );

        expect(result.isError).toBeUndefined();
        expect(botStateService.getKey).toHaveBeenCalledWith(
          "bot-123",
          "positions",
        );
        const data = JSON.parse(result.content[0].text);
        expect(data.bot_id).toBe("bot-123");
        expect(data.key).toBe("positions");
        expect(data.value).toEqual({ qty: 10 });
      });

      it("should surface invalid key errors", async () => {
        botStateService.getKey.mockResolvedValue(
          Failure({ code: "INVALID_KEY", message: "Invalid state key" }),
        );

        const result = await service.handleToolCall(
          "bot_state_get",
          { bot_id: "bot-123", key: "../etc/passwd" },
          actor,
        );

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("Invalid state key");
      });
    });

    describe("bot_query", () => {
      it("should execute a query against a running bot", async () => {
        botQueryService.executeQuery.mockResolvedValue(
          Ok({
            status: "success",
            data: { price: 42 },
            duration: 5,
            timestamp: "2026-07-18T10:00:00Z",
          }),
        );

        const result = await service.handleToolCall(
          "bot_query",
          {
            bot_id: "bot-123",
            query_path: "/positions",
            params: { symbol: "AAPL" },
            timeout_sec: 10,
          },
          actor,
        );

        expect(result.isError).toBeUndefined();
        expect(botQueryService.executeQuery).toHaveBeenCalledWith("bot-123", {
          queryPath: "/positions",
          params: { symbol: "AAPL" },
          timeoutSec: 10,
        });
        const data = JSON.parse(result.content[0].text);
        expect(data.status).toBe("success");
        expect(data.data).toEqual({ price: 42 });
      });

      it("should require a query path", async () => {
        const result = await service.handleToolCall(
          "bot_query",
          { bot_id: "bot-123" },
          actor,
        );

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("query_path");
      });

      it("should require a bot_id", async () => {
        const result = await service.handleToolCall(
          "bot_query",
          { query_path: "/positions" },
          actor,
        );

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("bot_id");
        expect(botQueryService.executeQuery).not.toHaveBeenCalled();
      });

      it("should surface query failures", async () => {
        botQueryService.executeQuery.mockResolvedValue(
          Failure({
            code: "QUERY_FAILED",
            message: "Query failed: bot not running",
          }),
        );

        const result = await service.handleToolCall(
          "bot_query",
          { bot_id: "bot-123", query_path: "/positions" },
          actor,
        );

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("bot not running");
      });
    });

    describe("custom_bot_list", () => {
      it("should return list of custom bots", async () => {
        const mockBotsWithVersions = [
          {
            id: "cb-1",
            name: "test-bot",
            userId: "author-1",
            latestVersion: "1.0.0",
            versions: [
              {
                id: mockCustomBot.id,
                version: mockCustomBot.version,
                config: mockCustomBot.config,
                userId: mockCustomBot.userId,
                filePath: mockCustomBot.filePath,
                status: mockCustomBot.status,
                createdAt: mockCustomBot.createdAt,
                updatedAt: mockCustomBot.updatedAt,
              },
            ],
            createdAt: new Date(),
            updatedAt: new Date(),
          },
        ];
        customBotService.getAllGlobalCustomBots.mockResolvedValue(
          Ok(mockBotsWithVersions),
        );

        const result = await service.handleToolCall(
          "custom_bot_list",
          {},
          actor,
        );

        expect(result.isError).toBeUndefined();
        const data = JSON.parse(result.content[0].text);
        expect(Array.isArray(data)).toBe(true);
        expect(data[0].name).toBe("test-bot");
      });
    });

    describe("custom_bot_get", () => {
      it("should return custom bot details", async () => {
        customBotService.getGlobalLatestVersion.mockResolvedValue(
          Ok(mockCustomBot),
        );

        const result = await service.handleToolCall(
          "custom_bot_get",
          {
            name: "test-custom-bot",
          },
          actor,
        );

        expect(result.isError).toBeUndefined();
        const data = JSON.parse(result.content[0].text);
        expect(data.name).toBe("test-custom-bot");
        expect(data.version).toBe("1.0.0");
      });

      it("should get specific version when provided", async () => {
        customBotService.getGlobalSpecificVersion.mockResolvedValue(
          Ok(mockCustomBot),
        );

        await service.handleToolCall(
          "custom_bot_get",
          {
            name: "test-custom-bot",
            version: "1.0.0",
          },
          actor,
        );

        expect(customBotService.getGlobalSpecificVersion).toHaveBeenCalledWith(
          "test-custom-bot",
          "1.0.0",
        );
      });
    });

    describe("custom_bot_schema", () => {
      it("should return bot configuration schema", async () => {
        customBotService.getGlobalLatestVersion.mockResolvedValue(
          Ok(mockCustomBot),
        );

        const result = await service.handleToolCall(
          "custom_bot_schema",
          {
            name: "test-custom-bot",
          },
          actor,
        );

        expect(result.isError).toBeUndefined();
        const data = JSON.parse(result.content[0].text);
        expect(data.name).toBe("test-custom-bot");
        expect(data.schema).toEqual({ type: "object", properties: {} });
      });
    });

    describe("unknown tool", () => {
      it("should return error for unknown tool", async () => {
        const result = await service.handleToolCall("unknown_tool", {}, actor);

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("Unknown tool: unknown_tool");
      });
    });
  });
});
