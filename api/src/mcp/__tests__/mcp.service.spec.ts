import { Test, TestingModule } from "@nestjs/testing";
import { ModuleRef } from "@nestjs/core";
import { McpService } from "../mcp.service";
import { BotRepository } from "@/bot/bot.repository";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { LogsService } from "@/logs/logs.service";
import { ApiKeyService } from "@/api-key/api-key.service";
import { BotStateService } from "@/bot-state/bot-state.service";
import { BotQueryService } from "@/bot-query/bot-query.service";
import { Ok, Failure } from "@/common/result";
import { createMockLogger } from "@/test/mock-logger";
import { PinoLogger } from "nestjs-pino";
import { mockBot as createMockBot, mockBotConfig } from "@/test/mock-bot";

describe("McpService", () => {
  let service: McpService;
  let botRepository: jest.Mocked<BotRepository>;
  let customBotService: jest.Mocked<CustomBotService>;
  let logsService: jest.Mocked<LogsService>;
  let apiKeyService: jest.Mocked<ApiKeyService>;
  let botStateService: { listKeys: jest.Mock; getKey: jest.Mock };
  let botQueryService: { executeQuery: jest.Mock };
  let moduleRef: { registerRequestByContextId: jest.Mock; resolve: jest.Mock };

  const userId = "user-123";

  const testBot = createMockBot({
    id: "bot-123",
    name: "Test Bot",
    userId,
    topic: "the0-scheduled-custom-bot",
    customBotId: "custom-bot-123",
    config: mockBotConfig({
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

  beforeEach(async () => {
    const mockBotRepository = {
      findAll: jest.fn(),
      findOne: jest.fn(),
      create: jest.fn(),
      update: jest.fn(),
      remove: jest.fn(),
    };

    const mockCustomBotService = {
      getAllGlobalCustomBots: jest.fn(),
      getAllGlobalVersions: jest.fn(),
      getGlobalLatestVersion: jest.fn(),
      getGlobalSpecificVersion: jest.fn(),
    };

    const mockLogsService = {
      getLogs: jest.fn(),
    };

    const mockApiKeyService = {
      validateApiKey: jest.fn(),
    };

    const mockBotStateService = {
      listKeys: jest.fn(),
      getKey: jest.fn(),
    };

    const mockBotQueryService = {
      executeQuery: jest.fn(),
    };

    const mockModuleRef = {
      registerRequestByContextId: jest.fn(),
      resolve: jest
        .fn()
        .mockImplementation(async (provider: unknown) =>
          provider === BotStateService
            ? mockBotStateService
            : mockBotQueryService,
        ),
    };

    const module: TestingModule = await Test.createTestingModule({
      providers: [
        McpService,
        {
          provide: BotRepository,
          useValue: mockBotRepository,
        },
        {
          provide: CustomBotService,
          useValue: mockCustomBotService,
        },
        {
          provide: LogsService,
          useValue: mockLogsService,
        },
        {
          provide: ApiKeyService,
          useValue: mockApiKeyService,
        },
        {
          provide: PinoLogger,
          useValue: createMockLogger(),
        },
        {
          provide: ModuleRef,
          useValue: mockModuleRef,
        },
      ],
    }).compile();

    service = module.get<McpService>(McpService);
    botRepository = module.get(BotRepository);
    customBotService = module.get(CustomBotService);
    logsService = module.get(LogsService);
    apiKeyService = module.get(ApiKeyService);
    botStateService = mockBotStateService;
    botQueryService = mockBotQueryService;
    moduleRef = mockModuleRef;
  });

  describe("handleToolCall", () => {
    describe("auth_status", () => {
      it("should return valid status", async () => {
        const result = await service.handleToolCall("auth_status", {}, userId);

        expect(result.isError).toBeUndefined();
        expect(result.content[0].type).toBe("text");
        const data = JSON.parse(result.content[0].text);
        expect(data.valid).toBe(true);
      });
    });

    describe("bot_list", () => {
      it("should require authentication", async () => {
        const result = await service.handleToolCall("bot_list", {});

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("Authentication required");
      });

      it("should return list of bots", async () => {
        botRepository.findAll.mockResolvedValue(Ok([testBot]));

        const result = await service.handleToolCall("bot_list", {}, userId);

        expect(result.isError).toBeUndefined();
        const data = JSON.parse(result.content[0].text);
        expect(Array.isArray(data)).toBe(true);
        expect(data[0].id).toBe("bot-123");
        expect(data[0].name).toBe("Test Bot");
      });

      it("should handle repository errors", async () => {
        botRepository.findAll.mockResolvedValue(Failure("Database error"));

        const result = await service.handleToolCall("bot_list", {}, userId);

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("Database error");
      });
    });

    describe("bot_get", () => {
      it("should return bot details", async () => {
        botRepository.findOne.mockResolvedValue(Ok(testBot));

        const result = await service.handleToolCall(
          "bot_get",
          { bot_id: "bot-123" },
          userId,
        );

        expect(result.isError).toBeUndefined();
        const data = JSON.parse(result.content[0].text);
        expect(data.id).toBe("bot-123");
        expect(botRepository.findOne).toHaveBeenCalledWith(userId, "bot-123");
      });

      it("should handle not found error", async () => {
        botRepository.findOne.mockResolvedValue(Failure("Not found"));

        const result = await service.handleToolCall(
          "bot_get",
          { bot_id: "nonexistent" },
          userId,
        );

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("Not found");
      });
    });

    describe("bot_deploy", () => {
      it("should deploy a new bot", async () => {
        customBotService.getGlobalSpecificVersion.mockResolvedValue(
          Ok(mockCustomBot),
        );
        botRepository.create.mockResolvedValue(Ok(testBot));

        const result = await service.handleToolCall(
          "bot_deploy",
          {
            config: {
              name: "Test Bot",
              type: "scheduled/test-bot",
              version: "1.0.0",
            },
          },
          userId,
        );

        expect(result.isError).toBeUndefined();
        const data = JSON.parse(result.content[0].text);
        expect(data.message).toBe("Bot deployed successfully");
        expect(data.bot.id).toBe("bot-123");
      });

      it("should fail if custom bot not found", async () => {
        customBotService.getGlobalSpecificVersion.mockResolvedValue(
          Failure("Custom bot not found"),
        );

        const result = await service.handleToolCall(
          "bot_deploy",
          {
            config: {
              name: "Test Bot",
              type: "scheduled/nonexistent-bot",
              version: "1.0.0",
            },
          },
          userId,
        );

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("not found");
      });
    });

    describe("bot_update", () => {
      it("should update bot configuration", async () => {
        botRepository.update.mockResolvedValue(Ok(testBot));

        const result = await service.handleToolCall(
          "bot_update",
          {
            bot_id: "bot-123",
            config: { schedule: "0 10 * * *" },
          },
          userId,
        );

        expect(result.isError).toBeUndefined();
        const data = JSON.parse(result.content[0].text);
        expect(data.message).toBe("Bot updated successfully");
      });
    });

    describe("bot_delete", () => {
      it("should delete a bot", async () => {
        botRepository.remove.mockResolvedValue(Ok(null));

        const result = await service.handleToolCall(
          "bot_delete",
          { bot_id: "bot-123" },
          userId,
        );

        expect(result.isError).toBeUndefined();
        const data = JSON.parse(result.content[0].text);
        expect(data.message).toBe("Bot deleted successfully");
        expect(data.bot_id).toBe("bot-123");
      });
    });

    describe("logs_get", () => {
      it("should return bot logs", async () => {
        logsService.getLogs.mockResolvedValue(
          Ok({ entries: [mockLogEntry], hasMore: false }),
        );

        const result = await service.handleToolCall(
          "logs_get",
          { bot_id: "bot-123", date: "20251202" },
          userId,
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
          userId,
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
          userId,
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
          userId,
        );

        const data = JSON.parse(result.content[0].text);
        // 2 logs contain error keywords: "error" and "failed"
        expect(data.error_count).toBe(2);
      });
    });

    describe("bot_state_list", () => {
      it("should require authentication", async () => {
        const result = await service.handleToolCall("bot_state_list", {
          bot_id: "bot-123",
        });

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("Authentication required");
      });

      it("should require a bot_id", async () => {
        const result = await service.handleToolCall("bot_state_list", {}, userId);

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
          userId,
        );

        expect(result.isError).toBeUndefined();
        expect(botStateService.listKeys).toHaveBeenCalledWith("bot-123");
        const data = JSON.parse(result.content[0].text);
        expect(data.bot_id).toBe("bot-123");
        expect(data.keys).toEqual([{ key: "positions", size: 128 }]);
      });

      it("should resolve the state service in a request context carrying the calling user", async () => {
        botStateService.listKeys.mockResolvedValue(Ok([]));

        await service.handleToolCall(
          "bot_state_list",
          { bot_id: "bot-123" },
          userId,
        );

        expect(moduleRef.registerRequestByContextId).toHaveBeenCalledWith(
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
          userId,
        );

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("access denied");
      });
    });

    describe("bot_state_get", () => {
      it("should require authentication", async () => {
        const result = await service.handleToolCall("bot_state_get", {
          bot_id: "bot-123",
          key: "positions",
        });

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("Authentication required");
      });

      it("should require a bot_id and key", async () => {
        const missingKey = await service.handleToolCall(
          "bot_state_get",
          { bot_id: "bot-123" },
          userId,
        );
        const missingBotId = await service.handleToolCall(
          "bot_state_get",
          { key: "positions" },
          userId,
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
          userId,
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
          userId,
        );

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("Invalid state key");
      });
    });

    describe("bot_query", () => {
      it("should require authentication", async () => {
        const result = await service.handleToolCall("bot_query", {
          bot_id: "bot-123",
          query_path: "/positions",
        });

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("Authentication required");
      });

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
          userId,
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
          userId,
        );

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("query_path");
      });

      it("should require a bot_id", async () => {
        const result = await service.handleToolCall(
          "bot_query",
          { query_path: "/positions" },
          userId,
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
          userId,
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

        const result = await service.handleToolCall("custom_bot_list", {});

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

        const result = await service.handleToolCall("custom_bot_get", {
          name: "test-custom-bot",
        });

        expect(result.isError).toBeUndefined();
        const data = JSON.parse(result.content[0].text);
        expect(data.name).toBe("test-custom-bot");
        expect(data.version).toBe("1.0.0");
      });

      it("should get specific version when provided", async () => {
        customBotService.getGlobalSpecificVersion.mockResolvedValue(
          Ok(mockCustomBot),
        );

        await service.handleToolCall("custom_bot_get", {
          name: "test-custom-bot",
          version: "1.0.0",
        });

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

        const result = await service.handleToolCall("custom_bot_schema", {
          name: "test-custom-bot",
        });

        expect(result.isError).toBeUndefined();
        const data = JSON.parse(result.content[0].text);
        expect(data.name).toBe("test-custom-bot");
        expect(data.schema).toEqual({ type: "object", properties: {} });
      });
    });

    describe("unknown tool", () => {
      it("should return error for unknown tool", async () => {
        const result = await service.handleToolCall("unknown_tool", {}, userId);

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("Unknown tool: unknown_tool");
      });
    });
  });
});
