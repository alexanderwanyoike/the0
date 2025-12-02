import { Test, TestingModule } from "@nestjs/testing";
import { McpService } from "../mcp.service";
import { BotRepository } from "@/bot/bot.repository";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { LogsService } from "@/logs/logs.service";
import { ApiKeyService } from "@/api-key/api-key.service";
import { Ok, Failure } from "@/common/result";
import { createMockLogger } from "@/test/mock-logger";
import { getLoggerToken } from "nestjs-pino";

describe("McpService", () => {
  let service: McpService;
  let botRepository: jest.Mocked<BotRepository>;
  let customBotService: jest.Mocked<CustomBotService>;
  let logsService: jest.Mocked<LogsService>;
  let apiKeyService: jest.Mocked<ApiKeyService>;

  const userId = "user-123";

  const mockBot = {
    id: "bot-123",
    name: "Test Bot",
    userId,
    topic: "the0-scheduled-custom-bot",
    customBotId: "custom-bot-123",
    config: {
      type: "scheduled/test-bot",
      version: "1.0.0",
      schedule: "0 9 * * *",
    },
    createdAt: new Date(),
    updatedAt: new Date(),
  };

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
          provide: getLoggerToken(McpService.name),
          useValue: createMockLogger(),
        },
      ],
    }).compile();

    service = module.get<McpService>(McpService);
    botRepository = module.get(BotRepository);
    customBotService = module.get(CustomBotService);
    logsService = module.get(LogsService);
    apiKeyService = module.get(ApiKeyService);
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
        botRepository.findAll.mockResolvedValue(Ok([mockBot]));

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
        botRepository.findOne.mockResolvedValue(Ok(mockBot));

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
        botRepository.create.mockResolvedValue(Ok(mockBot));

        const result = await service.handleToolCall(
          "bot_deploy",
          {
            name: "Test Bot",
            config: { type: "scheduled/test-bot", version: "1.0.0" },
          },
          userId,
        );

        expect(result.isError).toBeUndefined();
        const data = JSON.parse(result.content[0].text);
        expect(data.message).toBe("Bot deployed successfully");
        expect(data.bot.id).toBe("bot-123");
      });
    });

    describe("bot_update", () => {
      it("should update bot configuration", async () => {
        botRepository.update.mockResolvedValue(Ok(mockBot));

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
        logsService.getLogs.mockResolvedValue(Ok([mockLogEntry]));

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
        logsService.getLogs.mockResolvedValue(Ok([]));

        await service.handleToolCall(
          "logs_get",
          { bot_id: "bot-123", limit: 1000 },
          userId,
        );

        expect(logsService.getLogs).toHaveBeenCalledWith("bot-123", {
          date: undefined,
          dateRange: undefined,
          limit: 500,
          offset: 0,
        });
      });
    });

    describe("logs_summary", () => {
      it("should return log summary", async () => {
        logsService.getLogs.mockResolvedValue(Ok([mockLogEntry]));

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
          { date: "20251202", content: "something went wrong with error" },
          { date: "20251202", content: "success message" },
          { date: "20251202", content: "task failed to complete" },
        ];
        logsService.getLogs.mockResolvedValue(Ok(logsWithErrors));

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
        const result = await service.handleToolCall(
          "unknown_tool",
          {},
          userId,
        );

        expect(result.isError).toBe(true);
        expect(result.content[0].text).toContain("Unknown tool: unknown_tool");
      });
    });
  });
});
