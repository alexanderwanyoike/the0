import { Test, TestingModule } from "@nestjs/testing";
import { ConfigService } from "@nestjs/config";
import { PinoLogger } from "nestjs-pino";
import { BotQueryService, BotQueryErrorCode } from "../bot-query.service";
import { BotService } from "@/bot/bot.service";
import { createMockLogger } from "@/test/mock-logger";

describe("BotQueryService", () => {
  let service: BotQueryService;
  let mockBotService: jest.Mocked<BotService>;
  let mockConfigService: jest.Mocked<ConfigService>;
  let mockFetch: jest.SpyInstance;

  const mockBot = {
    id: "test-bot-id",
    name: "Test Bot",
    config: { type: "scheduled/test-bot", version: "1.0.0" },
    userId: "test-user-id",
    topic: "the0-scheduled-custom-bot",
    createdAt: new Date(),
    updatedAt: new Date(),
    customBotId: "test-custom-bot",
  };

  beforeEach(async () => {
    mockBotService = {
      findOne: jest.fn(),
    } as unknown as jest.Mocked<BotService>;

    mockConfigService = {
      get: jest.fn().mockReturnValue("http://localhost:9477"),
    } as unknown as jest.Mocked<ConfigService>;

    const module: TestingModule = await Test.createTestingModule({
      providers: [
        BotQueryService,
        {
          provide: BotService,
          useValue: mockBotService,
        },
        {
          provide: ConfigService,
          useValue: mockConfigService,
        },
        {
          provide: PinoLogger,
          useValue: createMockLogger(),
        },
      ],
    }).compile();

    service = await module.resolve<BotQueryService>(BotQueryService);

    // Mock global fetch
    mockFetch = jest.spyOn(global, "fetch");
  });

  afterEach(() => {
    mockFetch.mockRestore();
  });

  describe("executeQuery", () => {
    it("should execute query successfully", async () => {
      mockBotService.findOne.mockResolvedValue({
        success: true,
        data: mockBot,
        error: null,
      });

      mockFetch.mockResolvedValue({
        ok: true,
        json: () =>
          Promise.resolve({
            status: "ok",
            data: { positions: [{ symbol: "BTC", amount: 1.5 }] },
            duration: 150,
            timestamp: "2026-01-04T12:00:00Z",
          }),
      } as Response);

      const result = await service.executeQuery("test-bot-id", {
        queryPath: "/portfolio",
        params: { symbol: "BTC" },
      });

      expect(result.success).toBe(true);
      expect(result.data.status).toBe("ok");
      expect(result.data.data).toEqual({ positions: [{ symbol: "BTC", amount: 1.5 }] });
      expect(mockFetch).toHaveBeenCalledWith(
        "http://localhost:9477/query",
        expect.objectContaining({
          method: "POST",
          headers: { "Content-Type": "application/json" },
          body: JSON.stringify({
            bot_id: "test-bot-id",
            query_path: "/portfolio",
            params: { symbol: "BTC" },
            timeout_sec: 30,
          }),
        }),
      );
    });

    it("should return BOT_NOT_FOUND when bot does not exist", async () => {
      mockBotService.findOne.mockResolvedValue({
        success: false,
        data: null,
        error: "Bot not found",
      });

      const result = await service.executeQuery("nonexistent-bot", {
        queryPath: "/portfolio",
      });

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotQueryErrorCode.BOT_NOT_FOUND);
      expect(result.error?.message).toBe("Bot not found or access denied");
      expect(mockFetch).not.toHaveBeenCalled();
    });

    it("should return BOT_NOT_FOUND when runtime returns 404", async () => {
      mockBotService.findOne.mockResolvedValue({
        success: true,
        data: mockBot,
        error: null,
      });

      mockFetch.mockResolvedValue({
        ok: false,
        status: 404,
        text: () => Promise.resolve("bot not found in runtime"),
      } as Response);

      const result = await service.executeQuery("test-bot-id", {
        queryPath: "/portfolio",
      });

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotQueryErrorCode.BOT_NOT_FOUND);
      expect(result.error?.message).toBe("Bot not found in runtime");
    });

    it("should return QUERY_FAILED when runtime returns error", async () => {
      mockBotService.findOne.mockResolvedValue({
        success: true,
        data: mockBot,
        error: null,
      });

      mockFetch.mockResolvedValue({
        ok: false,
        status: 500,
        text: () => Promise.resolve("Internal server error"),
      } as Response);

      const result = await service.executeQuery("test-bot-id", {
        queryPath: "/portfolio",
      });

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotQueryErrorCode.QUERY_FAILED);
      expect(result.error?.message).toContain("Internal server error");
    });

    it("should return TIMEOUT when request times out", async () => {
      mockBotService.findOne.mockResolvedValue({
        success: true,
        data: mockBot,
        error: null,
      });

      const abortError = new Error("Aborted");
      abortError.name = "AbortError";
      mockFetch.mockRejectedValue(abortError);

      const result = await service.executeQuery("test-bot-id", {
        queryPath: "/portfolio",
        timeoutSec: 5,
      });

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotQueryErrorCode.TIMEOUT);
      expect(result.error?.message).toContain("timed out after 5 seconds");
    });

    it("should return RUNTIME_UNAVAILABLE when connection refused", async () => {
      mockBotService.findOne.mockResolvedValue({
        success: true,
        data: mockBot,
        error: null,
      });

      const connError = new Error("Connection refused") as any;
      connError.code = "ECONNREFUSED";
      mockFetch.mockRejectedValue(connError);

      const result = await service.executeQuery("test-bot-id", {
        queryPath: "/portfolio",
      });

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotQueryErrorCode.RUNTIME_UNAVAILABLE);
      expect(result.error?.message).toContain("runtime is not available");
    });

    it("should use custom timeout when provided", async () => {
      mockBotService.findOne.mockResolvedValue({
        success: true,
        data: mockBot,
        error: null,
      });

      mockFetch.mockResolvedValue({
        ok: true,
        json: () =>
          Promise.resolve({
            status: "ok",
            data: {},
          }),
      } as Response);

      await service.executeQuery("test-bot-id", {
        queryPath: "/test",
        timeoutSec: 60,
      });

      expect(mockFetch).toHaveBeenCalledWith(
        "http://localhost:9477/query",
        expect.objectContaining({
          body: expect.stringContaining('"timeout_sec":60'),
        }),
      );
    });

    it("should use default timeout when not provided", async () => {
      mockBotService.findOne.mockResolvedValue({
        success: true,
        data: mockBot,
        error: null,
      });

      mockFetch.mockResolvedValue({
        ok: true,
        json: () =>
          Promise.resolve({
            status: "ok",
            data: {},
          }),
      } as Response);

      await service.executeQuery("test-bot-id", {
        queryPath: "/test",
      });

      expect(mockFetch).toHaveBeenCalledWith(
        "http://localhost:9477/query",
        expect.objectContaining({
          body: expect.stringContaining('"timeout_sec":30'),
        }),
      );
    });

    it("should handle empty params", async () => {
      mockBotService.findOne.mockResolvedValue({
        success: true,
        data: mockBot,
        error: null,
      });

      mockFetch.mockResolvedValue({
        ok: true,
        json: () =>
          Promise.resolve({
            status: "ok",
            data: {},
          }),
      } as Response);

      await service.executeQuery("test-bot-id", {
        queryPath: "/test",
      });

      expect(mockFetch).toHaveBeenCalledWith(
        "http://localhost:9477/query",
        expect.objectContaining({
          body: expect.stringContaining('"params":{}'),
        }),
      );
    });
  });
});
