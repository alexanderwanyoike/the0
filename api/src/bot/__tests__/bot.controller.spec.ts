import { Test, TestingModule } from "@nestjs/testing";
import { appError } from "@/common/app-error";
import { BotController } from "../bot.controller";
import { BotService } from "../bot.service";
import { BotRepository } from "../bot.repository";
import { Bot } from "../entities/bot.entity";
import {
  BadRequestException,
  InternalServerErrorException,
  CanActivate,
  NotFoundException,
} from "@nestjs/common";
import { BotValidator } from "../bot.validator";
import { ConfigModule } from "@nestjs/config";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";
import { ApiKeyService } from "@/api-key/api-key.service";
import { BotEventRelay } from "../bot-event-relay";
// FeatureGateService removed for OSS version
import { Ok } from "@/common/result";
import { PinoLogger } from "nestjs-pino";
import { createMockLogger } from "@/test/mock-logger";
import { mockBot, mockBotConfig } from "@/test/mock-bot";
import { mockActor } from "@/test/mock-actor";
import { BOT_COMMAND_PROVIDERS } from "@/test/bot-command-providers";

describe("BotController - Enhanced Tests", () => {
  let controller: BotController;
  let service: BotService;
  let repository: BotRepository;
  let validator: BotValidator;
  let customBotService: CustomBotService;
  const uid = "test-user-id";
  const actor = mockActor({ userId: uid });

  const mockCustomBot = {
    id: "test-custom-bot",
    name: "test-custom-bot",
    version: "1.0.0",
    filePath: "gs://test-bucket/test-custom-bot",
    userId: "another-user-id",
    status: "active",
    createdAt: new Date(),
    updatedAt: new Date(),
    config: {
      name: "test-custom-bot",
      type: "scheduled",
      runtime: "python3.11",
      description: "A test custom bot",
      entrypoints: {
        bot: "bot.py",
        backtest: "backtest.py",
      },
      schema: {
        bot: {
          type: "object",
          properties: {
            foo: { type: "string" },
            bar: { type: "number" },
          },
          required: ["foo", "bar"],
        },
      },
      readme: "This is a test custom bot",
      metadata: {
        categories: ["test"],
        instruments: ["BTC"],
        exchanges: ["Binance"],
        tags: ["test", "bot"],
      },
    },
  };

  const mockCustomBotService = {
    getGlobalSpecificVersion: jest.fn().mockResolvedValue({
      success: true,
      error: null,
      data: mockCustomBot,
    }),
    createCustomBot: jest.fn(),
    updateCustomBot: jest.fn(),
    getAllGlobalVersions: jest.fn(),
    checkOrphaned: jest
      .fn()
      .mockResolvedValue(
        Ok({ orphaned: false, name: "test", version: "1.0.0" }),
      ),
  };

  beforeEach(async () => {
    const mockForceGuard: CanActivate = { canActivate: jest.fn(() => true) };

    const module: TestingModule = await Test.createTestingModule({
      imports: [ConfigModule],
      controllers: [BotController],
      providers: [
        ...BOT_COMMAND_PROVIDERS,
        BotService,
        BotRepository,
        BotValidator,
        {
          provide: CustomBotService,
          useValue: mockCustomBotService,
        },
        {
          provide: ApiKeyService,
          useValue: {},
        },
        {
          provide: BotEventRelay,
          useValue: { publishCommitted: jest.fn() },
        },
        {
          provide: PinoLogger,
          useValue: createMockLogger(),
        },
        // FeatureGateService removed for OSS version
      ],
    })
      .overrideGuard(AuthCombinedGuard)
      .useValue(mockForceGuard)
      .compile();
    await module.init();

    repository = module.get<BotRepository>(BotRepository);
    controller = module.get<BotController>(BotController);
    service = module.get<BotService>(BotService);
    validator = module.get<BotValidator>(BotValidator);
    customBotService = module.get<CustomBotService>(CustomBotService);

    // Reset all mocks
    jest.clearAllMocks();
  });

  describe("create", () => {
    const validBot = {
      name: "Test Bot",
      config: mockBotConfig({
        type: "scheduled/test-bot",
        version: "1.0.0",
        foo: "test",
        bar: 123,
      }),
    };

    beforeEach(() => {
      validator.validate = jest.fn().mockReturnValue({
        success: true,
        error: null,
        data: null,
      });

      jest.spyOn(repository, "findAll").mockResolvedValue({
        success: true,
        error: null,
        data: [],
      });

      jest.spyOn(repository, "createWithEvents").mockResolvedValue({
        success: true,
        error: null,
        data: mockBot({
          id: "test-id",
          ...validBot,
          userId: uid,
          topic: "the0-scheduled-custom-bot",
        }),
      });
    });

    it("should create a bot successfully", async () => {
      jest.spyOn(repository, "findOne").mockResolvedValue({
        success: true,
        error: null,
        data: mockBot({
          id: "test-id",
          ...validBot,
          userId: uid,
          topic: "the0-scheduled-custom-bot",
        }),
      });
      const result = await controller.create(actor, validBot);

      expect(result).toMatchObject({
        id: "test-id",
        ...validBot,
        userId: uid,
        topic: "the0-scheduled-custom-bot",
      });
      expect(repository.createWithEvents).toHaveBeenCalledWith(
        {
          ...validBot,
          id: expect.any(String),
          config: {
            ...validBot.config,
            hasFrontend: false, // Added by bot.service from custom bot config
          },
          userId: uid,
          topic: "the0-scheduled-custom-bot",
          customBotId: mockCustomBot.id,
        },
        expect.any(Array),
      );
    });

    it("should throw InternalServerErrorException when storing fails", async () => {
      jest.spyOn(repository, "createWithEvents").mockResolvedValue({
        success: false,
        error: appError("internal", "Creation failed"),
        data: null,
      });

      await expect(controller.create(actor, validBot)).rejects.toThrow(
        InternalServerErrorException,
      );
    });

    it("should throw BadRequestException for invalid bot type format", async () => {
      const invalidBot = {
        ...validBot,
        config: mockBotConfig({ ...validBot.config, type: "invalid-format" }),
      };

      await expect(controller.create(actor, invalidBot)).rejects.toThrow(
        BadRequestException,
      );
    });

    it("should throw BadRequestException for invalid version format", async () => {
      const invalidBot = {
        ...validBot,
        config: mockBotConfig({
          ...validBot.config,
          version: "invalid-version",
        }),
      };

      await expect(controller.create(actor, invalidBot)).rejects.toThrow(
        BadRequestException,
      );
    });

    it("should throw BadRequestException when custom bot not found", async () => {
      mockCustomBotService.getGlobalSpecificVersion = jest
        .fn()
        .mockResolvedValue({
          success: false,
          error: appError("not_found", "Bot not found"),
          data: null,
        });

      await expect(controller.create(actor, validBot)).rejects.toThrow(
        BadRequestException,
      );
    });

    it("should throw BadRequestException when validation fails", async () => {
      validator.validate = jest.fn().mockReturnValue({
        success: false,
        error: ["Invalid configuration"],
        data: null,
      });

      await expect(controller.create(actor, validBot)).rejects.toThrow(
        BadRequestException,
      );
    });

    // Test removed - UserBotsService dependency not needed in OSS version
  });

  describe("findAll", () => {
    it("should return all bots successfully", async () => {
      const bots = [
        mockBot({ id: "bot1", name: "Bot 1" }),
        mockBot({ id: "bot2", name: "Bot 2" }),
      ];

      jest.spyOn(repository, "findAll").mockResolvedValue({
        success: true,
        error: null,
        data: bots,
      });

      const result = await controller.findAll(actor);

      expect(result).toEqual(bots);
      expect(repository.findAll).toHaveBeenCalledWith(uid);
    });

    it("should throw InternalServerErrorException when reading fails", async () => {
      jest.spyOn(repository, "findAll").mockResolvedValue({
        success: false,
        error: appError("internal", "Database error"),
        data: null,
      });

      await expect(controller.findAll(actor)).rejects.toThrow(
        InternalServerErrorException,
      );
    });

    it("should return empty array when no bots exist", async () => {
      jest.spyOn(repository, "findAll").mockResolvedValue({
        success: true,
        error: null,
        data: [],
      });

      const result = await controller.findAll(actor);

      expect(result).toEqual([]);
    });
  });

  describe("findOne", () => {
    const testBot = mockBot({
      id: "test-id",
      name: "Test Bot",
      config: mockBotConfig({ type: "scheduled/test-bot", version: "1.0.0" }),
    });

    it("should return a bot successfully", async () => {
      jest.spyOn(repository, "findOne").mockResolvedValue({
        success: true,
        error: null,
        data: testBot,
      });

      const result = await controller.findOne(actor, "test-id");

      expect(result).toEqual(testBot);
      expect(repository.findOne).toHaveBeenCalledWith(uid, "test-id");
    });

    it("should throw NotFoundException when bot not found", async () => {
      jest.spyOn(repository, "findOne").mockResolvedValue({
        success: false,
        error: appError("not_found", "Bot not found"),
        data: null,
      });

      await expect(controller.findOne(actor, "nonexistent-id")).rejects.toThrow(
        NotFoundException,
      );
    });

    it("should handle invalid bot ID format", async () => {
      jest.spyOn(repository, "findOne").mockResolvedValue({
        success: false,
        error: appError("validation", "Invalid ID format"),
        data: null,
      });

      await expect(controller.findOne(actor, "invalid-id")).rejects.toThrow(
        BadRequestException,
      );
    });
  });

  describe("update", () => {
    const updateData = {
      name: "Updated Bot",
      config: mockBotConfig({
        type: "scheduled/test-bot",
        version: "1.0.0",
        foo: "updated",
        bar: 456,
      }),
    };

    const existingBot = mockBot({
      id: "test-id",
      name: "Test Bot",
      config: mockBotConfig({
        type: "scheduled/test-bot",
        version: "1.0.0",
        foo: "original",
        bar: 123,
      }),
      userId: uid,
      customBotId: "test-custom-bot",
      topic: "the0-scheduled-custom-bot",
    });

    beforeEach(() => {
      validator.validate = jest.fn().mockReturnValue({
        success: true,
        error: null,
        data: null,
      });

      jest.spyOn(repository, "findOne").mockResolvedValue({
        success: true,
        error: null,
        data: existingBot,
      });
    });

    it("should update a bot successfully", async () => {
      const updatedBot = mockBot({ id: "test-id", ...updateData });

      // Reset the mock to return success for this test
      mockCustomBotService.getGlobalSpecificVersion = jest
        .fn()
        .mockResolvedValue({
          success: true,
          error: null,
          data: mockCustomBot,
        });

      jest.spyOn(repository, "updateWithEvents").mockResolvedValue({
        success: true,
        error: null,
        data: updatedBot,
      });

      const result = await controller.update(actor, "test-id", updateData);

      expect(result).toEqual(updatedBot);
      expect(repository.updateWithEvents).toHaveBeenCalledWith(
        uid,
        "test-id",
        {
          ...updateData,
          config: { ...updateData.config, hasFrontend: false },
          customBotId: existingBot.customBotId,
        },
        expect.any(Array),
      );
    });

    it("should throw InternalServerErrorException when storing fails", async () => {
      jest.spyOn(repository, "updateWithEvents").mockResolvedValue({
        success: false,
        error: appError("internal", "Update failed"),
        data: null,
      });

      await expect(
        controller.update(actor, "test-id", updateData),
      ).rejects.toThrow(InternalServerErrorException);
    });

    it("should throw BadRequestException when validation fails", async () => {
      validator.validate = jest.fn().mockReturnValue({
        success: false,
        error: ["Invalid configuration"],
        data: null,
      });

      await expect(
        controller.update(actor, "test-id", updateData),
      ).rejects.toThrow(BadRequestException);
    });

    it("should throw BadRequestException for invalid bot type on update", async () => {
      mockCustomBotService.getGlobalSpecificVersion = jest
        .fn()
        .mockResolvedValue({
          success: false,
          error: appError("not_found", "Bot type not found"),
          data: null,
        });

      await expect(
        controller.update(actor, "test-id", updateData),
      ).rejects.toThrow(BadRequestException);
    });
  });

  describe("remove", () => {
    it("should remove a bot successfully", async () => {
      const botToRemove = mockBot({
        id: "test-id",
        name: "Test Bot",
        config: mockBotConfig({ type: "scheduled/test-bot", version: "1.0.0" }),
        topic: "the0-scheduled-custom-bot",
        userId: uid,
        customBotId: "custom-bot-123",
      });

      jest.spyOn(repository, "findOne").mockResolvedValue({
        success: true,
        error: null,
        data: botToRemove,
      });

      jest.spyOn(repository, "removeWithEvents").mockResolvedValue({
        success: true,
        error: null,
        data: null,
      });

      const result = await controller.remove(actor, "test-id");
      expect(result).toBeDefined();
      expect(repository.removeWithEvents).toHaveBeenCalledWith(
        uid,
        "test-id",
        expect.any(Array),
      );
    });

    it("should throw NotFoundException when the bot is not the user's", async () => {
      jest.spyOn(repository, "findOne").mockResolvedValue({
        success: false,
        error: appError("not_found", "Not found"),
        data: null,
      });
      jest.spyOn(repository, "removeWithEvents");

      await expect(controller.remove(actor, "test-id")).rejects.toThrow(
        NotFoundException,
      );
      expect(repository.removeWithEvents).not.toHaveBeenCalled();
    });
  });
});
