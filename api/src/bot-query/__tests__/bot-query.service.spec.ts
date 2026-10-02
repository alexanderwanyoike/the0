import { Test, TestingModule } from "@nestjs/testing";
import { ConfigService } from "@nestjs/config";
import { PinoLogger } from "nestjs-pino";
import { BotQueryService, BotQueryErrorCode } from "../bot-query.service";
import { BotService } from "@/bot/bot.service";
import { createMockLogger } from "@/test/mock-logger";
import { mockBot as createMockBot, mockBotConfig } from "@/test/mock-bot";

describe("BotQueryService", () => {
  let service: BotQueryService;
  let mockBotService: jest.Mocked<BotService>;
  let mockConfigService: jest.Mocked<ConfigService>;
  let mockFetch: jest.SpyInstance;
  let mockLogger: ReturnType<typeof createMockLogger>;

  const testBot = createMockBot({
    id: "test-bot-id",
    name: "Test Bot",
    config: mockBotConfig({ type: "scheduled/test-bot", version: "1.0.0" }),
    userId: "test-user-id",
    topic: "the0-scheduled-custom-bot",
    customBotId: "test-custom-bot",
  });

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
          useValue: (mockLogger = createMockLogger()),
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
        data: testBot,
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
      expect(result.data.data).toEqual({
        positions: [{ symbol: "BTC", amount: 1.5 }],
      });
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
        data: testBot,
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

    it("should return NO_QUERY_ENTRYPOINT when the bot defines no queries", async () => {
      mockBotService.findOne.mockResolvedValue({
        success: true,
        data: testBot,
        error: null,
      });

      mockFetch.mockResolvedValue({
        ok: false,
        status: 422,
        text: () =>
          Promise.resolve(
            '{"status":"error","error":"bot has no query entrypoint: test-bot-id"}',
          ),
      } as Response);

      const result = await service.executeQuery("test-bot-id", {
        queryPath: "/status",
      });

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotQueryErrorCode.NO_QUERY_ENTRYPOINT);
      expect(result.error?.message).toBe(
        "This bot defines no queries. Add a query entrypoint (entrypoints.query in bot-config.yaml) to answer them.",
      );
    });

    it("should return QUERY_FAILED when runtime returns error", async () => {
      mockBotService.findOne.mockResolvedValue({
        success: true,
        data: testBot,
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
        data: testBot,
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
        data: testBot,
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
        data: testBot,
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
        data: testBot,
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
        data: testBot,
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

  describe("executeQuery response mapping", () => {
    beforeEach(() => {
      mockBotService.findOne.mockResolvedValue({
        success: true,
        data: testBot,
        error: null,
      });
    });

    it("should fill defaults when the runtime omits optional fields", async () => {
      mockFetch.mockResolvedValue({
        ok: true,
        json: () => Promise.resolve({ data: { a: 1 } }),
      } as Response);

      const result = await service.executeQuery("test-bot-id", {
        queryPath: "/portfolio",
      });

      expect(result.success).toBe(true);
      expect(result.data).toEqual({
        status: "ok",
        data: { a: 1 },
        error: undefined,
        duration: 0,
        timestamp: expect.stringMatching(/^\d{4}-\d{2}-\d{2}T/),
      });
    });

    it("should pass through the runtime's query error", async () => {
      mockFetch.mockResolvedValue({
        ok: true,
        json: () =>
          Promise.resolve({
            status: "error",
            error: "handler threw",
            duration: 12,
            timestamp: "2026-01-04T12:00:00Z",
          }),
      } as Response);

      const result = await service.executeQuery("test-bot-id", {
        queryPath: "/portfolio",
      });

      expect(result.data).toEqual({
        status: "error",
        data: undefined,
        error: "handler threw",
        duration: 12,
        timestamp: "2026-01-04T12:00:00Z",
      });
    });

    it("should log and report the body of a failed runtime response", async () => {
      mockFetch.mockResolvedValue({
        ok: false,
        status: 500,
        text: () => Promise.resolve("Internal server error"),
      } as Response);

      const result = await service.executeQuery("test-bot-id", {
        queryPath: "/portfolio",
      });

      expect(result.error).toEqual({
        code: BotQueryErrorCode.QUERY_FAILED,
        message: "Query failed: Internal server error",
      });
      expect(mockLogger.error).toHaveBeenCalledWith(
        {
          botId: "test-bot-id",
          queryPath: "/portfolio",
          status: 500,
          error: "Internal server error",
        },
        "Query request failed",
      );
    });
  });

  describe("executeQuery transport failures", () => {
    beforeEach(() => {
      mockBotService.findOne.mockResolvedValue({
        success: true,
        data: testBot,
        error: null,
      });
    });

    afterEach(() => {
      jest.useRealTimers();
    });

    it("should abort the request once the timeout elapses", async () => {
      jest.useFakeTimers();
      mockFetch.mockImplementation(
        (_url: string, init: RequestInit) =>
          new Promise((_resolve, reject) => {
            init.signal!.addEventListener("abort", () => {
              const abortError = new Error("Aborted");
              abortError.name = "AbortError";
              reject(abortError);
            });
          }),
      );

      const pending = service.executeQuery("test-bot-id", {
        queryPath: "/portfolio",
        timeoutSec: 2,
      });
      await jest.advanceTimersByTimeAsync(2000);
      const result = await pending;

      expect(result.error).toEqual({
        code: BotQueryErrorCode.TIMEOUT,
        message: "Query timed out after 2 seconds",
      });
    });

    it("should report the default timeout in the timeout message", async () => {
      const abortError = new Error("Aborted");
      abortError.name = "AbortError";
      mockFetch.mockRejectedValue(abortError);

      const result = await service.executeQuery("test-bot-id", {
        queryPath: "/portfolio",
      });

      expect(result.error?.message).toBe("Query timed out after 30 seconds");
    });

    it("should detect a refused connection reported through the error cause", async () => {
      const fetchError = Object.assign(new TypeError("fetch failed"), {
        cause: { code: "ECONNREFUSED" },
      });
      mockFetch.mockRejectedValue(fetchError);

      const result = await service.executeQuery("test-bot-id", {
        queryPath: "/portfolio",
      });

      expect(result.error).toEqual({
        code: BotQueryErrorCode.RUNTIME_UNAVAILABLE,
        message:
          "Bot runtime is not available. Ensure the bot-runner service is running.",
      });
      expect(mockLogger.error).toHaveBeenCalledWith(
        {
          botId: "test-bot-id",
          queryPath: "/portfolio",
          error: "fetch failed",
        },
        "Runtime unavailable",
      );
    });

    it("should report other transport errors as query failures", async () => {
      mockFetch.mockRejectedValue(new Error("socket hang up"));

      const result = await service.executeQuery("test-bot-id", {
        queryPath: "/portfolio",
      });

      expect(result.error).toEqual({
        code: BotQueryErrorCode.QUERY_FAILED,
        message: "Query failed: socket hang up",
      });
      expect(mockLogger.error).toHaveBeenCalledWith(
        {
          botId: "test-bot-id",
          queryPath: "/portfolio",
          error: "socket hang up",
        },
        "Query execution error",
      );
    });

    it("should report an unreadable response body as a query failure", async () => {
      mockFetch.mockResolvedValue({
        ok: true,
        json: () => Promise.reject(new Error("Unexpected token")),
      } as unknown as Response);

      const result = await service.executeQuery("test-bot-id", {
        queryPath: "/portfolio",
      });

      expect(result.error).toEqual({
        code: BotQueryErrorCode.QUERY_FAILED,
        message: "Query failed: Unexpected token",
      });
    });
  });
});
