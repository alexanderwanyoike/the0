import { Test, TestingModule } from "@nestjs/testing";
import {
  BadRequestException,
  NotFoundException,
  ServiceUnavailableException,
  GatewayTimeoutException,
  UnprocessableEntityException,
  CanActivate,
} from "@nestjs/common";
import { BotQueryController } from "../bot-query.controller";
import { BotQueryService } from "../bot-query.service";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";
import { mockActor } from "@/test/mock-actor";

// Mock guard that always allows access
const mockAuthGuard: CanActivate = {
  canActivate: jest.fn().mockReturnValue(true),
};

describe("BotQueryController", () => {
  let controller: BotQueryController;
  let mockBotQueryService: jest.Mocked<BotQueryService>;
  const actor = mockActor();

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

      const result = await controller.executeQuery(actor, "test-bot-id", {
        query_path: "/portfolio",
        params: { symbol: "BTC" },
      });

      expect(result.success).toBe(true);
      expect(result.data).toEqual(queryResult);
      expect(result.message).toBe("Query executed successfully");
      expect(mockBotQueryService.executeQuery).toHaveBeenCalledWith(
        actor,
        "test-bot-id",
        {
          queryPath: "/portfolio",
          params: { symbol: "BTC" },
          timeoutSec: undefined,
        },
      );
    });

    it("should prepend / to query_path if missing", async () => {
      mockBotQueryService.executeQuery.mockResolvedValue({
        success: true,
        data: {
          status: "ok",
          data: {},
          duration: 0,
          timestamp: new Date().toISOString(),
        },
        error: null,
      });

      await controller.executeQuery(actor, "test-bot-id", {
        query_path: "portfolio",
      });

      expect(mockBotQueryService.executeQuery).toHaveBeenCalledWith(
        actor,
        "test-bot-id",
        {
          queryPath: "/portfolio",
          params: undefined,
          timeoutSec: undefined,
        },
      );
    });

    it("should throw NotFoundException when bot not found", async () => {
      mockBotQueryService.executeQuery.mockResolvedValue({
        success: false,
        data: null,
        error: {
          code: "not_found",
          message: "Bot not found",
        },
      });

      await expect(
        controller.executeQuery(actor, "nonexistent-bot", {
          query_path: "/portfolio",
        }),
      ).rejects.toThrow(NotFoundException);
    });

    it("should throw UnprocessableEntityException when the bot defines no queries", async () => {
      const message = "This bot defines no queries.";
      mockBotQueryService.executeQuery.mockResolvedValue({
        success: false,
        data: null,
        error: {
          code: "unprocessable",
          message,
        },
      });

      await expect(
        controller.executeQuery(actor, "no-query-bot", {
          query_path: "/status",
        }),
      ).rejects.toThrow(new UnprocessableEntityException(message));
    });

    it("should throw ServiceUnavailableException when runtime unavailable", async () => {
      mockBotQueryService.executeQuery.mockResolvedValue({
        success: false,
        data: null,
        error: {
          code: "unavailable",
          message: "Runtime unavailable",
        },
      });

      await expect(
        controller.executeQuery(actor, "test-bot-id", {
          query_path: "/portfolio",
        }),
      ).rejects.toThrow(ServiceUnavailableException);
    });

    it("should throw GatewayTimeoutException when query times out", async () => {
      mockBotQueryService.executeQuery.mockResolvedValue({
        success: false,
        data: null,
        error: {
          code: "timeout",
          message: "Query timed out",
        },
      });

      await expect(
        controller.executeQuery(actor, "test-bot-id", {
          query_path: "/portfolio",
        }),
      ).rejects.toThrow(GatewayTimeoutException);
    });

    it("should throw BadRequestException for other errors", async () => {
      mockBotQueryService.executeQuery.mockResolvedValue({
        success: false,
        data: null,
        error: {
          code: "validation",
          message: "Query execution failed",
        },
      });

      await expect(
        controller.executeQuery(actor, "test-bot-id", {
          query_path: "/portfolio",
        }),
      ).rejects.toThrow(BadRequestException);
    });

    it("should pass timeout_sec to service", async () => {
      mockBotQueryService.executeQuery.mockResolvedValue({
        success: true,
        data: {
          status: "ok",
          data: {},
          duration: 0,
          timestamp: new Date().toISOString(),
        },
        error: null,
      });

      await controller.executeQuery(actor, "test-bot-id", {
        query_path: "/portfolio",
        timeout_sec: 60,
      });

      expect(mockBotQueryService.executeQuery).toHaveBeenCalledWith(
        actor,
        "test-bot-id",
        {
          queryPath: "/portfolio",
          params: undefined,
          timeoutSec: 60,
        },
      );
    });
  });
});
