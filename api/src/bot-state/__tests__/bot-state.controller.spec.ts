import { Test, TestingModule } from "@nestjs/testing";
import { BotStateController } from "../bot-state.controller";
import { BotStateService } from "../bot-state.service";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";
import { NotFoundException, BadRequestException } from "@nestjs/common";
import { Ok, Failure } from "@/common/result";

describe("BotStateController", () => {
  let controller: BotStateController;
  let mockBotStateService: jest.Mocked<Partial<BotStateService>>;

  const testBotId = "test-bot-id";

  beforeEach(async () => {
    mockBotStateService = {
      listKeys: jest.fn(),
      getKey: jest.fn(),
      deleteKey: jest.fn(),
      clearState: jest.fn(),
    };

    const module: TestingModule = await Test.createTestingModule({
      controllers: [BotStateController],
      providers: [
        {
          provide: BotStateService,
          useValue: mockBotStateService,
        },
      ],
    })
      .overrideGuard(AuthCombinedGuard)
      .useValue({ canActivate: () => true })
      .compile();

    controller = module.get<BotStateController>(BotStateController);
  });

  describe("listKeys", () => {
    it("should return state keys on success", async () => {
      const keys = [
        { key: "portfolio", size: 100 },
        { key: "trade_count", size: 10 },
      ];
      mockBotStateService.listKeys!.mockResolvedValue(Ok(keys));

      const result = await controller.listKeys(testBotId);

      expect(result.success).toBe(true);
      expect(result.data).toEqual(keys);
      expect(mockBotStateService.listKeys).toHaveBeenCalledWith(testBotId);
    });

    it("should throw NotFoundException when bot not found", async () => {
      mockBotStateService.listKeys!.mockResolvedValue(
        Failure("Bot not found or access denied"),
      );

      await expect(controller.listKeys(testBotId)).rejects.toThrow(
        NotFoundException,
      );
    });

    it("should throw BadRequestException on other errors", async () => {
      mockBotStateService.listKeys!.mockResolvedValue(
        Failure("Some other error"),
      );

      await expect(controller.listKeys(testBotId)).rejects.toThrow(
        BadRequestException,
      );
    });
  });

  describe("getKey", () => {
    it("should return state value on success", async () => {
      const stateValue = { AAPL: 100, GOOGL: 50 };
      mockBotStateService.getKey!.mockResolvedValue(Ok(stateValue));

      const result = await controller.getKey(testBotId, "portfolio");

      expect(result.success).toBe(true);
      expect(result.data).toEqual(stateValue);
      expect(mockBotStateService.getKey).toHaveBeenCalledWith(
        testBotId,
        "portfolio",
      );
    });

    it("should throw NotFoundException when key not found", async () => {
      mockBotStateService.getKey!.mockResolvedValue(
        Failure("State key not found"),
      );

      await expect(controller.getKey(testBotId, "nonexistent")).rejects.toThrow(
        NotFoundException,
      );
    });

    it("should throw NotFoundException when bot not found", async () => {
      mockBotStateService.getKey!.mockResolvedValue(
        Failure("Bot not found or access denied"),
      );

      await expect(controller.getKey(testBotId, "portfolio")).rejects.toThrow(
        NotFoundException,
      );
    });

    it("should throw BadRequestException for invalid key", async () => {
      mockBotStateService.getKey!.mockResolvedValue(Failure("Invalid state key"));

      await expect(controller.getKey(testBotId, "../escape")).rejects.toThrow(
        BadRequestException,
      );
    });
  });

  describe("deleteKey", () => {
    it("should return deleted true when key existed", async () => {
      mockBotStateService.deleteKey!.mockResolvedValue(Ok(true));

      const result = await controller.deleteKey(testBotId, "portfolio");

      expect(result.success).toBe(true);
      expect(result.data.deleted).toBe(true);
      expect(result.message).toContain("deleted successfully");
    });

    it("should return deleted false when key did not exist", async () => {
      mockBotStateService.deleteKey!.mockResolvedValue(Ok(false));

      const result = await controller.deleteKey(testBotId, "nonexistent");

      expect(result.success).toBe(true);
      expect(result.data.deleted).toBe(false);
      expect(result.message).toContain("not found");
    });

    it("should throw NotFoundException when bot not found", async () => {
      mockBotStateService.deleteKey!.mockResolvedValue(
        Failure("Bot not found or access denied"),
      );

      await expect(
        controller.deleteKey(testBotId, "portfolio"),
      ).rejects.toThrow(NotFoundException);
    });

    it("should throw BadRequestException for invalid key", async () => {
      mockBotStateService.deleteKey!.mockResolvedValue(
        Failure("Invalid state key"),
      );

      await expect(
        controller.deleteKey(testBotId, "../escape"),
      ).rejects.toThrow(BadRequestException);
    });
  });

  describe("clearState", () => {
    it("should return cleared true on success", async () => {
      mockBotStateService.clearState!.mockResolvedValue(Ok(true));

      const result = await controller.clearState(testBotId);

      expect(result.success).toBe(true);
      expect(result.data.cleared).toBe(true);
      expect(result.message).toContain("cleared successfully");
    });

    it("should throw NotFoundException when bot not found", async () => {
      mockBotStateService.clearState!.mockResolvedValue(
        Failure("Bot not found or access denied"),
      );

      await expect(controller.clearState(testBotId)).rejects.toThrow(
        NotFoundException,
      );
    });

    it("should throw BadRequestException on other errors", async () => {
      mockBotStateService.clearState!.mockResolvedValue(
        Failure("Failed to clear state"),
      );

      await expect(controller.clearState(testBotId)).rejects.toThrow(
        BadRequestException,
      );
    });
  });
});
