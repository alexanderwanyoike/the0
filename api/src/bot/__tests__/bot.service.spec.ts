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
import { StorageService } from "@/custom-bot/storage.service";
import { NatsService } from "@/nats/nats.service";
// FeatureGateService removed for OSS version
import { Ok, Failure } from "@/common/result";
import { PinoLogger } from "nestjs-pino";
import { createMockLogger } from "@/test/mock-logger";
import { mockBot, mockBotConfig } from "@/test/mock-bot";

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
          provide: StorageService,
          useValue: {
            deletePrefixFromBucket: jest.fn().mockResolvedValue(Ok(0)),
          },
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
          provide: PinoLogger,
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
      config: mockBotConfig({
        type: "scheduled/test-custom-bot",
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

      jest.spyOn(repository, "create").mockResolvedValue({
        success: true,
        error: null,
        data: mockBot({
          id: "test-id",
          ...validBotData,
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
          ...validBotData,
          userId: uid,
          topic: "the0-scheduled-custom-bot",
        }),
      });
      const result = await service.create(validBotData);

      expect(result.success).toBe(true);
      expect(result.data.id).toBe("test-id");
      // FeatureGateService removed for OSS version
      expect(repository.create).toHaveBeenCalledWith({
        ...validBotData,
        config: {
          ...validBotData.config,
          hasFrontend: false, // Added by bot.service from custom bot config
        },
        topic: "the0-scheduled-custom-bot",
        userId: uid,
        customBotId: mockCustomBot.id,
      });
    });

    it("should validate bot type and version format", async () => {
      const invalidTypeBot = {
        ...validBotData,
        config: mockBotConfig({
          ...validBotData.config,
          type: "invalid-format",
        }),
      };

      const result = await service.create(invalidTypeBot);

      expect(result.success).toBe(false);
      expect(result.error).toContain("Invalid bot type format");
    });

    it("should validate semantic version format", async () => {
      const invalidVersionBot = {
        ...validBotData,
        config: mockBotConfig({
          ...validBotData.config,
          version: "invalid-version",
        }),
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
          data: mockBot({
            id: "test-id",
            ...validBotData,
            userId: uid,
            topic: "the0-scheduled-custom-bot",
          }),
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
          data: mockBot({
            id: "test-id",
            ...validBotData,
            userId: uid,
            topic: "the0-scheduled-custom-bot",
          }),
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
          error: null,
          data: mockBot({ ...validBotData, id: "test-id" }),
        });

        jest.spyOn(repository, "findOne").mockResolvedValue({
          success: true,
          error: null,
          data: mockBot({
            id: "test-id",
            ...validBotData,
            userId: uid,
            topic: "the0-scheduled-custom-bot",
          }),
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
        mockBot({ id: "bot1", name: "Bot 1" }),
        mockBot({ id: "bot2", name: "Bot 2" }),
      ];

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
      const bot = mockBot({ id: "test-id", name: "Test Bot" });

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
      config: mockBotConfig({
        type: "scheduled/test-custom-bot",
        version: "1.0.0",
        foo: "updated",
        bar: 456,
      }),
    };

    const existingBot = mockBot({
      id: "test-id",
      name: "Test Bot",
      config: mockBotConfig({
        type: "scheduled/test-custom-bot",
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

      jest.spyOn(repository, "update").mockResolvedValue({
        success: true,
        error: null,
        data: updatedBot,
      });

      const result = await service.update("test-id", updateData);

      expect(result.success).toBe(true);
      expect(result.data).toEqual(updatedBot);
    });

    it("should update customBotId when config version changes (non-breaking)", async () => {
      const newCustomBot = {
        ...mockCustomBot,
        id: "new-custom-bot-v1.1.0",
        version: "1.1.0",
      };

      mockCustomBotService.getGlobalSpecificVersion = jest
        .fn()
        .mockResolvedValue({
          success: true,
          error: null,
          data: newCustomBot,
        });

      const patchUpdateData = {
        name: "Updated Bot",
        config: mockBotConfig({
          type: "scheduled/test-custom-bot",
          version: "1.1.0",
          foo: "updated",
          bar: 456,
        }),
      };

      jest.spyOn(repository, "update").mockResolvedValue({
        success: true,
        error: null,
        data: mockBot({ id: "test-id", ...patchUpdateData }),
      });

      const result = await service.update("test-id", patchUpdateData);

      expect(result.success).toBe(true);
      expect(repository.update).toHaveBeenCalledWith(uid, "test-id", {
        ...patchUpdateData,
        config: { ...patchUpdateData.config, hasFrontend: false },
        customBotId: "new-custom-bot-v1.1.0",
      });
    });

    it("should update hasFrontend flag from new custom bot version", async () => {
      const newCustomBotWithFrontend = {
        ...mockCustomBot,
        id: "new-custom-bot-v1.0.1",
        version: "1.0.1",
        config: { ...mockCustomBot.config, hasFrontend: true },
      };

      mockCustomBotService.getGlobalSpecificVersion = jest
        .fn()
        .mockResolvedValue({
          success: true,
          error: null,
          data: newCustomBotWithFrontend,
        });

      const patchUpdateData = {
        name: "Updated Bot",
        config: mockBotConfig({
          type: "scheduled/test-custom-bot",
          version: "1.0.1",
          foo: "updated",
          bar: 456,
        }),
      };

      jest.spyOn(repository, "update").mockResolvedValue({
        success: true,
        error: null,
        data: mockBot({ id: "test-id", ...patchUpdateData }),
      });

      const result = await service.update("test-id", patchUpdateData);

      expect(result.success).toBe(true);
      expect(repository.update).toHaveBeenCalledWith(uid, "test-id", {
        ...patchUpdateData,
        config: { ...patchUpdateData.config, hasFrontend: true },
        customBotId: "new-custom-bot-v1.0.1",
      });
    });

    it("should keep customBotId unchanged when version hasn't changed", async () => {
      jest.spyOn(repository, "update").mockResolvedValue({
        success: true,
        error: null,
        data: mockBot({ id: "test-id", ...updateData }),
      });

      const result = await service.update("test-id", updateData);

      expect(result.success).toBe(true);
      // customBotId should be preserved from the existing bot record, not rotated
      expect(repository.update).toHaveBeenCalledWith(uid, "test-id", {
        ...updateData,
        config: { ...updateData.config, hasFrontend: false },
        customBotId: existingBot.customBotId,
      });
    });

    it("should reject update when major version changes", async () => {
      const newCustomBotV2 = {
        ...mockCustomBot,
        id: "new-custom-bot-v2.0.0",
        version: "2.0.0",
      };

      mockCustomBotService.getGlobalSpecificVersion = jest
        .fn()
        .mockResolvedValue({
          success: true,
          error: null,
          data: newCustomBotV2,
        });

      const updateSpy = jest.spyOn(repository, "update");

      const majorUpdateData = {
        name: "Updated Bot",
        config: mockBotConfig({
          type: "scheduled/test-custom-bot",
          version: "2.0.0",
          foo: "updated",
          bar: 456,
        }),
      };

      const result = await service.update("test-id", majorUpdateData);

      expect(result.success).toBe(false);
      expect(result.error).toContain("Major version upgrade");
      expect(result.error).toContain("requires delete and redeploy");
      expect(updateSpy).not.toHaveBeenCalled();
    });

    it("should allow update when minor version changes", async () => {
      const newCustomBot = {
        ...mockCustomBot,
        id: "new-custom-bot-v1.3.0",
        version: "1.3.0",
      };

      mockCustomBotService.getGlobalSpecificVersion = jest
        .fn()
        .mockResolvedValue({
          success: true,
          error: null,
          data: newCustomBot,
        });

      const minorUpdateData = {
        name: "Updated Bot",
        config: mockBotConfig({
          type: "scheduled/test-custom-bot",
          version: "1.3.0",
          foo: "updated",
          bar: 456,
        }),
      };

      jest.spyOn(repository, "update").mockResolvedValue({
        success: true,
        error: null,
        data: mockBot({ id: "test-id", ...minorUpdateData }),
      });

      const result = await service.update("test-id", minorUpdateData);

      expect(result.success).toBe(true);
    });

    it("should allow update when patch version changes", async () => {
      const newCustomBot = {
        ...mockCustomBot,
        id: "new-custom-bot-v1.0.1",
        version: "1.0.1",
      };

      mockCustomBotService.getGlobalSpecificVersion = jest
        .fn()
        .mockResolvedValue({
          success: true,
          error: null,
          data: newCustomBot,
        });

      const patchUpdateData = {
        name: "Updated Bot",
        config: mockBotConfig({
          type: "scheduled/test-custom-bot",
          version: "1.0.1",
          foo: "updated",
          bar: 456,
        }),
      };

      jest.spyOn(repository, "update").mockResolvedValue({
        success: true,
        error: null,
        data: mockBot({ id: "test-id", ...patchUpdateData }),
      });

      const result = await service.update("test-id", patchUpdateData);

      expect(result.success).toBe(true);
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
    it("should remove a bot and clean up logs and state", async () => {
      const botToRemove = mockBot({
        id: "test-id",
        name: "Test Bot",
        config: mockBotConfig({ type: "scheduled/test-bot", version: "1.0.0" }),
        topic: "the0-scheduled-custom-bot",
        userId: uid,
      });

      jest.spyOn(repository, "findOne").mockResolvedValue({
        success: true,
        error: null,
        data: botToRemove,
      });

      jest.spyOn(repository, "remove").mockResolvedValue({
        success: true,
        error: null,
        data: undefined,
      });

      (mockCustomBotService.checkOrphaned as jest.Mock) = jest
        .fn()
        .mockResolvedValue(Ok({ orphaned: false, name: "test-bot", version: "1.0.0" }));

      const result = await service.remove("test-id");

      expect(result.success).toBe(true);
      expect(repository.findOne).toHaveBeenCalledWith(uid, "test-id");
      expect(repository.remove).toHaveBeenCalledWith(uid, "test-id");
    });

    it("should return orphaned version info when version becomes orphaned", async () => {
      const botToRemove = mockBot({
        id: "test-id",
        name: "Test Bot",
        config: mockBotConfig({ type: "scheduled/test-bot", version: "1.0.0" }),
        topic: "the0-scheduled-custom-bot",
        userId: uid,
        customBotId: "cb-123",
      });

      jest.spyOn(repository, "findOne").mockResolvedValue({
        success: true,
        error: null,
        data: botToRemove,
      });

      jest.spyOn(repository, "remove").mockResolvedValue({
        success: true,
        error: null,
        data: undefined,
      });

      (mockCustomBotService.checkOrphaned as jest.Mock) = jest
        .fn()
        .mockResolvedValue(Ok({ orphaned: true, name: "test-bot", version: "1.0.0" }));

      const result = await service.remove("test-id");

      expect(result.success).toBe(true);
      expect(result.data).toEqual({
        orphanedVersion: {
          name: "test-bot",
          version: "1.0.0",
          customBotId: "cb-123",
        },
      });
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
