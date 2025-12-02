import { Test, TestingModule } from "@nestjs/testing";
import { BotService } from "../bot.service";
import { BotRepository } from "../bot.repository";
import { Bot } from "../entities/bot.entity";
import { REQUEST } from "@nestjs/core";
import { BotValidator } from "../bot.validator";
import { HttpModule } from "@nestjs/axios";
import { ConfigModule } from "@nestjs/config";
import { CustomBotModule } from "@/custom-bot/custom-bot.module";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { NatsService } from "@/nats/nats.service";
// FeatureGateService removed for OSS version
import { Ok, Failure } from "@/common/result";
import { getLoggerToken } from "nestjs-pino";
import { createMockLogger } from "@/test/mock-logger";

describe("BotService - Enhanced Tests", () => {
  let service: BotService;
  let repository: BotRepository;
  let validator: BotValidator;
  let mockCustomBotService: CustomBotService;
  // FeatureGateService removed for OSS version
  const uid = "test-user-id";

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

  beforeEach(async () => {
    mockCustomBotService = {
      getGlobalSpecificVersion: jest.fn().mockResolvedValue({
        success: true,
        error: null,
        data: mockCustomBot,
      }),
      getUserSpecificVersion: jest.fn().mockResolvedValue({
        success: true,
        error: null,
        data: mockCustomBot,
      }),
      createCustomBot: jest.fn(),
      updateCustomBot: jest.fn(),
      getAllUserVersions: jest.fn(),
      getAllGlobalVersions: jest.fn(),
      getAllUserSpecificVersions: jest.fn(),
    } as unknown as CustomBotService;

    // FeatureGateService removed for OSS version

    const module: TestingModule = await Test.createTestingModule({
      providers: [
        BotService,
        BotRepository,
        BotValidator,
        {
          provide: CustomBotService,
          useValue: mockCustomBotService,
        },
        {
          provide: NatsService,
          useValue: {
            publish: jest.fn().mockResolvedValue(Ok(null)),
          },
        },
        {
          provide: REQUEST,
          useValue: { user: { uid } },
        },
        {
          provide: getLoggerToken(BotService.name),
          useValue: createMockLogger(),
        },
        // FeatureGateService removed for OSS version
      ],
    }).compile();

    service = await module.resolve<BotService>(BotService);
    repository = module.get<BotRepository>(BotRepository);
    validator = module.get<BotValidator>(BotValidator);

    // Reset all mocks
    jest.clearAllMocks();
  });

  describe("create", () => {
    const validBotData = {
      name: "Test Bot",
      config: {
        type: "scheduled/test-custom-bot",
        version: "1.0.0",
        foo: "test",
        bar: 123,
      },
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

      jest.spyOn(repository, "create").mockResolvedValue({
        success: true,
        error: null,
        data: {
          id: "test-id",
          ...validBotData,
          userId: uid,
          topic: "the0-scheduled-custom-bot",
        } as Bot,
      });
    });

    it("should create a bot successfully", async () => {
      jest.spyOn(repository, "findOne").mockResolvedValue({
        success: true,
        error: null,
        data: {
          id: "test-id",
          ...validBotData,
          userId: uid,
          topic: "the0-scheduled-custom-bot",
        } as Bot,
      });
      const result = await service.create(validBotData);

      expect(result.success).toBe(true);
      expect(result.data.id).toBe("test-id");
      // FeatureGateService removed for OSS version
      expect(repository.create).toHaveBeenCalledWith({
        ...validBotData,
        topic: "the0-scheduled-custom-bot",
        userId: uid,
        customBotId: mockCustomBot.id,
      });
    });

    it("should validate bot type and version format", async () => {
      const invalidTypeBot = {
        ...validBotData,
        config: { ...validBotData.config, type: "invalid-format" },
      };

      const result = await service.create(invalidTypeBot);

      expect(result.success).toBe(false);
      expect(result.error).toContain("Invalid bot type format");
    });

    it("should validate semantic version format", async () => {
      const invalidVersionBot = {
        ...validBotData,
        config: { ...validBotData.config, version: "invalid-version" },
      };

      const result = await service.create(invalidVersionBot);

      expect(result.success).toBe(false);
      expect(result.error).toContain("Invalid version format");
    });

    // Feature gate tests removed for OSS version

    it("should fail when custom bot type not found", async () => {
      mockCustomBotService.getGlobalSpecificVersion = jest
        .fn()
        .mockResolvedValue({
          success: false,
          error: "Not found",
          data: null,
        });

      const result = await service.create(validBotData);

      expect(result.success).toBe(false);
      expect(result.error).toContain("not found");
    });

    it("should fail when bot config validation fails", async () => {
      validator.validate = jest.fn().mockReturnValue({
        success: false,
        error: ["Missing required field"],
        data: null,
      });

      const result = await service.create(validBotData);

      expect(result.success).toBe(false);
      expect(result.error).toContain("Missing required field");
    });

    describe("deployment authorization", () => {
      it("should allow deployment for bot owner", async () => {
        jest.spyOn(repository, "findOne").mockResolvedValue({
          success: true,
          error: null,
          data: {
            id: "test-id",
            ...validBotData,
            userId: uid,
            topic: "the0-scheduled-custom-bot",
          } as Bot,
        });
        mockCustomBotService.getGlobalSpecificVersion = jest
          .fn()
          .mockResolvedValue({
            success: true,
            data: { ...mockCustomBot, userId: uid, status: "active" },
          });

        const result = await service.create(validBotData);

        expect(result.success).toBe(true);
      });

      it("should allow deployment for non-owners in OSS version", async () => {
        jest.spyOn(repository, "findOne").mockResolvedValue({
          success: true,
          error: null,
          data: {
            id: "test-id",
            ...validBotData,
            userId: uid,
            topic: "the0-scheduled-custom-bot",
          } as Bot,
        });
        const result = await service.create(validBotData);

        expect(result.success).toBe(true);
      });

      it("should allow deployment of all bots in OSS version", async () => {
        mockCustomBotService.getGlobalSpecificVersion = jest
          .fn()
          .mockResolvedValue({
            success: true,
            data: { ...mockCustomBot },
          });

        jest.spyOn(repository, "create").mockResolvedValue({
          success: true,
          data: { ...validBotData, id: "test-id" },
        } as any);

        jest.spyOn(repository, "findOne").mockResolvedValue({
          success: true,
          error: null,
          data: {
            id: "test-id",
            ...validBotData,
            userId: uid,
            topic: "the0-scheduled-custom-bot",
          } as Bot,
        });

        const result = await service.create(validBotData);

        expect(result.success).toBe(true);
        // OSS version allows all bots - no approval gate
      });
    });
  });

  describe("findAll", () => {
    it("should return all user bots", async () => {
      const bots = [
        { id: "bot1", name: "Bot 1" },
        { id: "bot2", name: "Bot 2" },
      ] as Bot[];

      jest.spyOn(repository, "findAll").mockResolvedValue({
        success: true,
        error: null,
        data: bots,
      });

      const result = await service.findAll();

      expect(result.success).toBe(true);
      expect(result.data).toEqual(bots);
      expect(repository.findAll).toHaveBeenCalledWith(uid);
    });

    it("should handle repository errors", async () => {
      jest.spyOn(repository, "findAll").mockResolvedValue({
        success: false,
        error: "Database error",
        data: null,
      });

      const result = await service.findAll();

      expect(result.success).toBe(false);
      expect(result.error).toBe("Database error");
    });
  });

  describe("findOne", () => {
    it("should return a specific bot", async () => {
      const bot = { id: "test-id", name: "Test Bot" } as Bot;

      jest.spyOn(repository, "findOne").mockResolvedValue({
        success: true,
        error: null,
        data: bot,
      });

      const result = await service.findOne("test-id");

      expect(result.success).toBe(true);
      expect(result.data).toEqual(bot);
      expect(repository.findOne).toHaveBeenCalledWith(uid, "test-id");
    });

    it("should handle bot not found", async () => {
      jest.spyOn(repository, "findOne").mockResolvedValue({
        success: false,
        error: "Bot not found",
        data: null,
      });

      const result = await service.findOne("nonexistent-id");

      expect(result.success).toBe(false);
      expect(result.error).toBe("Bot not found");
    });
  });

  describe("update", () => {
    const updateData = {
      name: "Updated Bot",
      config: {
        type: "scheduled/test-custom-bot",
        version: "1.0.0",
        foo: "updated",
        bar: 456,
      },
    };

    beforeEach(() => {
      validator.validate = jest.fn().mockReturnValue({
        success: true,
        error: null,
        data: null,
      });
    });

    it("should update a bot successfully", async () => {
      const updatedBot = { id: "test-id", ...updateData } as Bot;

      jest.spyOn(repository, "update").mockResolvedValue({
        success: true,
        error: null,
        data: updatedBot,
      });

      const result = await service.update("test-id", updateData);

      expect(result.success).toBe(true);
      expect(result.data).toEqual(updatedBot);
      expect(repository.update).toHaveBeenCalledWith(
        uid,
        "test-id",
        updateData,
      );
    });

    it("should validate update data", async () => {
      validator.validate = jest.fn().mockReturnValue({
        success: false,
        error: ["Invalid configuration"],
        data: null,
      });

      const result = await service.update("test-id", updateData);

      expect(result.success).toBe(false);
      expect(result.error).toContain("Invalid configuration");
    });

    it("should handle repository update failures", async () => {
      jest.spyOn(repository, "update").mockResolvedValue({
        success: false,
        error: "Update failed",
        data: null,
      });

      const result = await service.update("test-id", updateData);

      expect(result.success).toBe(false);
      expect(result.error).toBe("Update failed");
    });
  });

  describe("remove", () => {
    it("should remove a bot successfully", async () => {
      const mockBot = {
        id: "test-id",
        name: "Test Bot",
        config: { type: "scheduled/test-bot", version: "1.0.0" },
        topic: "the0-scheduled-custom-bot",
        userId: uid,
      } as Bot;

      jest.spyOn(repository, "findOne").mockResolvedValue({
        success: true,
        error: null,
        data: mockBot,
      });

      jest.spyOn(repository, "remove").mockResolvedValue({
        success: true,
        error: null,
        data: undefined,
      });

      const result = await service.remove("test-id");

      expect(result.success).toBe(true);
      expect(repository.findOne).toHaveBeenCalledWith(uid, "test-id");
      expect(repository.remove).toHaveBeenCalledWith(uid, "test-id");
    });

    it("should handle removal failures", async () => {
      jest.spyOn(repository, "remove").mockResolvedValue({
        success: false,
        error: "Bot not found",
        data: null,
      });

      const result = await service.remove("test-id");

      expect(result.success).toBe(false);
      expect(result.error).toBe("Bot not found");
    });
  });
});
