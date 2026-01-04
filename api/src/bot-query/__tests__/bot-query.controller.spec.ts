import { Test, TestingModule } from "@nestjs/testing";
import { BadRequestException, NotFoundException, ServiceUnavailableException, GatewayTimeoutException, CanActivate } from "@nestjs/common";
import { BotQueryController } from "../bot-query.controller";
import { BotQueryService, BotQueryErrorCode } from "../bot-query.service";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";

// Mock guard that always allows access
const mockAuthGuard: CanActivate = {
  canActivate: jest.fn().mockReturnValue(true),
};

describe("BotQueryController", () => {
  let controller: BotQueryController;
  let mockBotQueryService: jest.Mocked<BotQueryService>;

  beforeEach(async () => {
    mockBotQueryService = {
      executeQuery: jest.fn(),
    } as unknown as jest.Mocked<BotQueryService>;

    const module: TestingModule = await Test.createTestingModule({
      controllers: [BotQueryController],
      providers: [
        {
          provide: BotQueryService,
          useValue: mockBotQueryService,
        },
      ],
    })
      .overrideGuard(AuthCombinedGuard)
      .useValue(mockAuthGuard)
      .compile();

    controller = module.get<BotQueryController>(BotQueryController);
  });

  describe("executeQuery", () => {
    it("should execute query successfully", async () => {
      const queryResult = {
        status: "ok",
        data: { positions: [{ symbol: "BTC", amount: 1.5 }] },
        duration: 150,
        timestamp: "2026-01-04T12:00:00Z",
      };

      mockBotQueryService.executeQuery.mockResolvedValue({
        success: true,
        data: queryResult,
        error: null,
      });

      const result = await controller.executeQuery("test-bot-id", {
        query_path: "/portfolio",
        params: { symbol: "BTC" },
      });

      expect(result.success).toBe(true);
      expect(result.data).toEqual(queryResult);
      expect(result.message).toBe("Query executed successfully");
      expect(mockBotQueryService.executeQuery).toHaveBeenCalledWith("test-bot-id", {
        queryPath: "/portfolio",
        params: { symbol: "BTC" },
        timeoutSec: undefined,
      });
    });

    it("should prepend / to query_path if missing", async () => {
      mockBotQueryService.executeQuery.mockResolvedValue({
        success: true,
        data: { status: "ok", data: {}, duration: 0, timestamp: new Date().toISOString() },
        error: null,
      });

      await controller.executeQuery("test-bot-id", {
        query_path: "portfolio",
      });

      expect(mockBotQueryService.executeQuery).toHaveBeenCalledWith("test-bot-id", {
        queryPath: "/portfolio",
        params: undefined,
        timeoutSec: undefined,
      });
    });

    it("should throw BadRequestException when query_path is missing", async () => {
      await expect(
        controller.executeQuery("test-bot-id", {
          query_path: "",
        }),
      ).rejects.toThrow(BadRequestException);
    });

    it("should throw NotFoundException when bot not found", async () => {
      mockBotQueryService.executeQuery.mockResolvedValue({
        success: false,
        data: null,
        error: {
          code: BotQueryErrorCode.BOT_NOT_FOUND,
          message: "Bot not found",
        },
      });

      await expect(
        controller.executeQuery("nonexistent-bot", {
          query_path: "/portfolio",
        }),
      ).rejects.toThrow(NotFoundException);
    });

    it("should throw ServiceUnavailableException when runtime unavailable", async () => {
      mockBotQueryService.executeQuery.mockResolvedValue({
        success: false,
        data: null,
        error: {
          code: BotQueryErrorCode.RUNTIME_UNAVAILABLE,
          message: "Runtime unavailable",
        },
      });

      await expect(
        controller.executeQuery("test-bot-id", {
          query_path: "/portfolio",
        }),
      ).rejects.toThrow(ServiceUnavailableException);
    });

    it("should throw GatewayTimeoutException when query times out", async () => {
      mockBotQueryService.executeQuery.mockResolvedValue({
        success: false,
        data: null,
        error: {
          code: BotQueryErrorCode.TIMEOUT,
          message: "Query timed out",
        },
      });

      await expect(
        controller.executeQuery("test-bot-id", {
          query_path: "/portfolio",
        }),
      ).rejects.toThrow(GatewayTimeoutException);
    });

    it("should throw BadRequestException for other errors", async () => {
      mockBotQueryService.executeQuery.mockResolvedValue({
        success: false,
        data: null,
        error: {
          code: BotQueryErrorCode.QUERY_FAILED,
          message: "Query execution failed",
        },
      });

      await expect(
        controller.executeQuery("test-bot-id", {
          query_path: "/portfolio",
        }),
      ).rejects.toThrow(BadRequestException);
    });

    it("should pass timeout_sec to service", async () => {
      mockBotQueryService.executeQuery.mockResolvedValue({
        success: true,
        data: { status: "ok", data: {}, duration: 0, timestamp: new Date().toISOString() },
        error: null,
      });

      await controller.executeQuery("test-bot-id", {
        query_path: "/portfolio",
        timeout_sec: 60,
      });

      expect(mockBotQueryService.executeQuery).toHaveBeenCalledWith("test-bot-id", {
        queryPath: "/portfolio",
        params: undefined,
        timeoutSec: 60,
      });
    });
  });
});
