import { Test, TestingModule } from "@nestjs/testing";
import { BotRepository } from "../bot.repository";
import { BotValidator } from "../bot.validator";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { BotEventRelay } from "../bot-event-relay";
import { Ok, Failure } from "@/common/result";
import { PinoLogger } from "nestjs-pino";
import { createMockLogger } from "@/test/mock-logger";
import { mockBot, mockBotConfig } from "@/test/mock-bot";
import { mockActor } from "@/test/mock-actor";
import { BOT_COMMAND_PROVIDERS } from "@/test/bot-command-providers";
import { CommandBus } from "@/command-bus/command-bus";
import { Actor } from "@/auth/actor";
import { DeleteBot, DeployBot, UpdateBot } from "../commands/bot-commands";
import { CreateBotDto } from "../dto/create-bot.dto";
import { UpdateBotDto } from "../dto/update-bot.dto";

describe("Bot commands", () => {
  let commands: CommandBus;
  let repository: BotRepository;
  let validator: BotValidator;
  let mockCustomBotService: CustomBotService;
  let relay: { publishCommitted: jest.Mock };
  let logger: ReturnType<typeof createMockLogger>;
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

    const module: TestingModule = await Test.createTestingModule({
      providers: [
        ...BOT_COMMAND_PROVIDERS,
        BotRepository,
        BotValidator,
        {
          provide: CustomBotService,
          useValue: mockCustomBotService,
        },
        {
          provide: BotEventRelay,
          useValue: { publishCommitted: jest.fn() },
        },
        {
          provide: PinoLogger,
          useValue: createMockLogger(),
        },
      ],
    }).compile();
    await module.init();

    commands = module.get(CommandBus);
    repository = module.get<BotRepository>(BotRepository);
    validator = module.get<BotValidator>(BotValidator);
    relay = module.get(BotEventRelay);
    logger = module.get(PinoLogger);

    // Reset all mocks
    jest.clearAllMocks();
  });

  const deploy = (by: Actor, bot: CreateBotDto) =>
    commands.execute(new DeployBot(by, { ...bot }));
  const update = (by: Actor, id: string, changes: UpdateBotDto) =>
    commands.execute(new UpdateBot(by, id, { ...changes }));
  const remove = (by: Actor, id: string) =>
    commands.execute(new DeleteBot(by, id));

  describe("DeployBot", () => {
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

      jest.spyOn(repository, "createWithEvents").mockResolvedValue({
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
      const result = await deploy(actor, validBotData);

      expect(result.success).toBe(true);
      expect(result.data.id).toBe("test-id");
      expect(repository.createWithEvents).toHaveBeenCalledWith(
        {
          ...validBotData,
          id: expect.any(String),
          config: {
            ...validBotData.config,
            hasFrontend: false, // Added by bot.service from custom bot config
          },
          topic: "the0-scheduled-custom-bot",
          userId: uid,
          customBotId: mockCustomBot.id,
        },
        expect.any(Array),
      );
    });

    it("should validate bot type and version format", async () => {
      const invalidTypeBot = {
        ...validBotData,
        config: mockBotConfig({
          ...validBotData.config,
          type: "invalid-format",
        }),
      };

      const result = await deploy(actor, invalidTypeBot);

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

      const result = await deploy(actor, invalidVersionBot);

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

      const result = await deploy(actor, validBotData);

      expect(result.success).toBe(false);
      expect(result.error).toContain("not found");
    });

    it("should fail when bot config validation fails", async () => {
      validator.validate = jest.fn().mockReturnValue({
        success: false,
        error: ["Missing required field"],
        data: null,
      });

      const result = await deploy(actor, validBotData);

      expect(result.success).toBe(false);
      expect(result.error).toContain("Missing required field");
    });

    it("should fail when the repository cannot store the bot", async () => {
      jest
        .spyOn(repository, "createWithEvents")
        .mockResolvedValue(Failure("db down"));
      jest.spyOn(repository, "findOne");

      const result = await deploy(actor, validBotData);

      expect(result).toEqual(Failure("db down"));
      expect(relay.publishCommitted).not.toHaveBeenCalled();
      expect(repository.findOne).not.toHaveBeenCalled();
    });

    describe("creation event", () => {
      const createdBot = mockBot({ id: "test-id", userId: uid });

      beforeEach(() => {
        jest.spyOn(repository, "findOne").mockResolvedValue(Ok(createdBot));
      });

      it("should store the creation event, built from the validated custom bot, with the bot", async () => {
        const result = await deploy(actor, validBotData);

        expect(result).toEqual(Ok(createdBot));
        const [row, events] = (repository.createWithEvents as jest.Mock).mock
          .calls[0];
        expect(events).toEqual([
          {
            botId: row.id,
            subject: "the0.bot-schedule.created",
            payload: {
              id: row.id,
              config: { ...validBotData.config, customBotId: mockCustomBot.id },
              custom: {
                config: mockCustomBot.config,
                createdAt: mockCustomBot.createdAt,
                updatedAt: mockCustomBot.updatedAt,
                filePath: mockCustomBot.filePath,
                version: "1.0.0",
              },
            },
          },
        ]);
        expect(repository.findOne).toHaveBeenCalledWith(uid, row.id);
      });

      it("should ask the relay to publish once the bot is stored", async () => {
        await deploy(actor, validBotData);

        const store = repository.createWithEvents as jest.Mock;
        const [, events] = store.mock.calls[0];
        expect(relay.publishCommitted).toHaveBeenCalledWith(events);
        expect(store.mock.invocationCallOrder[0]).toBeLessThan(
          relay.publishCommitted.mock.invocationCallOrder[0],
        );
      });

      it("should skip the event for an unknown bot type", async () => {
        mockCustomBotService.getGlobalSpecificVersion = jest
          .fn()
          .mockResolvedValue(
            Ok({
              ...mockCustomBot,
              config: { ...mockCustomBot.config, type: "other" },
            }),
          );

        const result = await deploy(actor, validBotData);

        expect(result).toEqual(Ok(createdBot));
        expect(repository.createWithEvents).toHaveBeenCalledWith(
          expect.anything(),
          [],
        );
        expect(relay.publishCommitted).toHaveBeenCalledWith([]);
      });
    });
  });

  describe("ownership follows the Actor", () => {
    const mcpActor = mockActor({
      userId: "mcp-user-id",
      via: "mcp",
      apiKeyId: "key-1",
    });

    it("deploys the bot for the Actor's user", async () => {
      validator.validate = jest.fn().mockReturnValue(Ok(null));
      const created = mockBot({ id: "test-id", userId: "mcp-user-id" });
      jest.spyOn(repository, "createWithEvents").mockResolvedValue(Ok(created));
      jest.spyOn(repository, "findOne").mockResolvedValue(Ok(created));

      await deploy(mcpActor, {
        name: "Test Bot",
        config: mockBotConfig({
          type: "scheduled/test-custom-bot",
          version: "1.0.0",
        }),
      });

      expect(repository.createWithEvents).toHaveBeenCalledWith(
        expect.objectContaining({ userId: "mcp-user-id" }),
        expect.any(Array),
      );
      const [row] = (repository.createWithEvents as jest.Mock).mock.calls[0];
      expect(repository.findOne).toHaveBeenCalledWith("mcp-user-id", row.id);
    });

    it("cannot update or delete a bot the Actor's user does not own", async () => {
      validator.validate = jest.fn().mockReturnValue(Ok(null));
      jest.spyOn(repository, "findOne").mockResolvedValue(Failure("missing"));
      jest
        .spyOn(repository, "updateWithEvents")
        .mockResolvedValue(Ok(mockBot()));
      jest
        .spyOn(repository, "removeWithEvents")
        .mockResolvedValue(Ok(undefined));
      const otherUser = mockActor({
        userId: "other-user-id",
        via: "api-key",
        apiKeyId: "key-2",
      });

      const updated = await update(otherUser, "test-id", {
        config: mockBotConfig({
          type: "scheduled/test-custom-bot",
          version: "1.0.0",
        }),
      });
      const removed = await remove(otherUser, "test-id");

      expect(updated).toEqual(Failure("Bot not found"));
      expect(removed).toEqual(Failure("Bot not found"));
      expect(repository.findOne).toHaveBeenCalledWith(
        "other-user-id",
        "test-id",
      );
      expect(repository.updateWithEvents).not.toHaveBeenCalled();
      expect(repository.removeWithEvents).not.toHaveBeenCalled();
      expect(relay.publishCommitted).not.toHaveBeenCalled();
    });
  });

  describe("UpdateBot", () => {
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

      jest.spyOn(repository, "updateWithEvents").mockResolvedValue({
        success: true,
        error: null,
        data: updatedBot,
      });

      const result = await update(actor, "test-id", updateData);

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

      jest.spyOn(repository, "updateWithEvents").mockResolvedValue({
        success: true,
        error: null,
        data: mockBot({ id: "test-id", ...patchUpdateData }),
      });

      const result = await update(actor, "test-id", patchUpdateData);

      expect(result.success).toBe(true);
      expect(repository.updateWithEvents).toHaveBeenCalledWith(
        uid,
        "test-id",
        {
          ...patchUpdateData,
          config: { ...patchUpdateData.config, hasFrontend: false },
          customBotId: "new-custom-bot-v1.1.0",
        },
        expect.any(Array),
      );
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

      jest.spyOn(repository, "updateWithEvents").mockResolvedValue({
        success: true,
        error: null,
        data: mockBot({ id: "test-id", ...patchUpdateData }),
      });

      const result = await update(actor, "test-id", patchUpdateData);

      expect(result.success).toBe(true);
      expect(repository.updateWithEvents).toHaveBeenCalledWith(
        uid,
        "test-id",
        {
          ...patchUpdateData,
          config: { ...patchUpdateData.config, hasFrontend: true },
          customBotId: "new-custom-bot-v1.0.1",
        },
        expect.any(Array),
      );
    });

    it("should keep customBotId unchanged when version hasn't changed", async () => {
      jest.spyOn(repository, "updateWithEvents").mockResolvedValue({
        success: true,
        error: null,
        data: mockBot({ id: "test-id", ...updateData }),
      });

      const result = await update(actor, "test-id", updateData);

      expect(result.success).toBe(true);
      // customBotId should be preserved from the existing bot record, not rotated
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

      const updateSpy = jest.spyOn(repository, "updateWithEvents");

      const majorUpdateData = {
        name: "Updated Bot",
        config: mockBotConfig({
          type: "scheduled/test-custom-bot",
          version: "2.0.0",
          foo: "updated",
          bar: 456,
        }),
      };

      const result = await update(actor, "test-id", majorUpdateData);

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

      jest.spyOn(repository, "updateWithEvents").mockResolvedValue({
        success: true,
        error: null,
        data: mockBot({ id: "test-id", ...minorUpdateData }),
      });

      const result = await update(actor, "test-id", minorUpdateData);

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

      jest.spyOn(repository, "updateWithEvents").mockResolvedValue({
        success: true,
        error: null,
        data: mockBot({ id: "test-id", ...patchUpdateData }),
      });

      const result = await update(actor, "test-id", patchUpdateData);

      expect(result.success).toBe(true);
    });

    it("should validate update data", async () => {
      validator.validate = jest.fn().mockReturnValue({
        success: false,
        error: ["Invalid configuration"],
        data: null,
      });

      const result = await update(actor, "test-id", updateData);

      expect(result.success).toBe(false);
      expect(result.error).toContain("Invalid configuration");
    });

    it("should handle repository update failures", async () => {
      jest.spyOn(repository, "updateWithEvents").mockResolvedValue({
        success: false,
        error: "Update failed",
        data: null,
      });

      const result = await update(actor, "test-id", updateData);

      expect(result.success).toBe(false);
      expect(result.error).toBe("Update failed");
    });

    it("should fail when the bot to update does not exist", async () => {
      jest.spyOn(repository, "findOne").mockResolvedValue(Failure("missing"));
      jest.spyOn(repository, "updateWithEvents");

      const result = await update(actor, "test-id", updateData);

      expect(result).toEqual(Failure("Bot not found"));
      expect(repository.updateWithEvents).not.toHaveBeenCalled();
    });

    it("should rotate customBotId when the current version is unknown", async () => {
      mockCustomBotService.getGlobalSpecificVersion = jest
        .fn()
        .mockResolvedValue(Ok({ ...mockCustomBot, id: "cb-new" }));
      jest.spyOn(repository, "findOne").mockResolvedValue(
        Ok({
          ...existingBot,
          customBotId: "cb-old",
          config: { ...existingBot.config, version: undefined },
        }),
      );
      jest
        .spyOn(repository, "updateWithEvents")
        .mockResolvedValue(Ok(existingBot));

      await update(actor, "test-id", updateData);

      expect(repository.updateWithEvents).toHaveBeenCalledWith(
        uid,
        "test-id",
        expect.objectContaining({ customBotId: "cb-new" }),
        expect.any(Array),
      );
    });

    describe("update event", () => {
      beforeEach(() => {
        jest
          .spyOn(repository, "updateWithEvents")
          .mockResolvedValue(Ok(existingBot));
      });

      it("should store the update event, built from the user's custom bot version, with the change", async () => {
        const result = await update(actor, "test-id", updateData);

        expect(result).toEqual(Ok(existingBot));
        expect(
          mockCustomBotService.getUserSpecificVersion,
        ).toHaveBeenCalledWith(uid, "test-custom-bot", "1.0.0");
        expect(repository.updateWithEvents).toHaveBeenCalledWith(
          uid,
          "test-id",
          expect.anything(),
          [
            {
              botId: "test-id",
              subject: "the0.bot-schedule.updated",
              payload: {
                id: "test-id",
                config: {
                  ...updateData.config,
                  customBotId: "test-custom-bot",
                },
                custom: {
                  config: mockCustomBot.config,
                  createdAt: mockCustomBot.createdAt,
                  updatedAt: mockCustomBot.updatedAt,
                  filePath: mockCustomBot.filePath,
                  version: "1.0.0",
                },
              },
            },
          ],
        );
        expect(relay.publishCommitted).toHaveBeenCalledTimes(1);
      });

      it("should send an empty filePath when the custom bot has none", async () => {
        mockCustomBotService.getUserSpecificVersion = jest
          .fn()
          .mockResolvedValue(Ok({ ...mockCustomBot, filePath: undefined }));

        await update(actor, "test-id", updateData);

        expect(repository.updateWithEvents).toHaveBeenCalledWith(
          uid,
          "test-id",
          expect.anything(),
          [
            expect.objectContaining({
              payload: expect.objectContaining({
                custom: expect.objectContaining({ filePath: "" }),
              }),
            }),
          ],
        );
      });

      it("should skip the event when the user's custom bot version is unavailable", async () => {
        mockCustomBotService.getUserSpecificVersion = jest
          .fn()
          .mockResolvedValue(Failure("not found"));

        const result = await update(actor, "test-id", updateData);

        expect(result.success).toBe(true);
        expect(repository.updateWithEvents).toHaveBeenCalledWith(
          uid,
          "test-id",
          expect.anything(),
          [],
        );
        expect(relay.publishCommitted).toHaveBeenCalledWith([]);
      });

      it("should skip the event for an unknown bot type", async () => {
        mockCustomBotService.getUserSpecificVersion = jest
          .fn()
          .mockResolvedValue(
            Ok({
              ...mockCustomBot,
              config: { ...mockCustomBot.config, type: "other" },
            }),
          );

        const result = await update(actor, "test-id", updateData);

        expect(result.success).toBe(true);
        expect(repository.updateWithEvents).toHaveBeenCalledWith(
          uid,
          "test-id",
          expect.anything(),
          [],
        );
        expect(relay.publishCommitted).toHaveBeenCalledWith([]);
        expect(logger.warn).toHaveBeenCalledWith(
          { botType: "other" },
          "Unknown bot type",
        );
      });

      it("should not ask the relay to publish when the repository update fails", async () => {
        jest
          .spyOn(repository, "updateWithEvents")
          .mockResolvedValue(Failure("nope"));

        await update(actor, "test-id", updateData);

        expect(relay.publishCommitted).not.toHaveBeenCalled();
      });
    });
  });

  describe("DeleteBot", () => {
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

      jest.spyOn(repository, "removeWithEvents").mockResolvedValue({
        success: true,
        error: null,
        data: undefined,
      });

      (mockCustomBotService.checkOrphaned as jest.Mock) = jest
        .fn()
        .mockResolvedValue(
          Ok({ orphaned: false, name: "test-bot", version: "1.0.0" }),
        );

      const result = await remove(actor, "test-id");

      expect(result.success).toBe(true);
      expect(repository.findOne).toHaveBeenCalledWith(uid, "test-id");
      expect(repository.removeWithEvents).toHaveBeenCalledWith(
        uid,
        "test-id",
        expect.any(Array),
      );
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

      jest.spyOn(repository, "removeWithEvents").mockResolvedValue({
        success: true,
        error: null,
        data: undefined,
      });

      (mockCustomBotService.checkOrphaned as jest.Mock) = jest
        .fn()
        .mockResolvedValue(
          Ok({ orphaned: true, name: "test-bot", version: "1.0.0" }),
        );

      const result = await remove(actor, "test-id");

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
      jest.spyOn(repository, "removeWithEvents").mockResolvedValue({
        success: false,
        error: "Bot not found",
        data: null,
      });

      const result = await remove(actor, "test-id");

      expect(result.success).toBe(false);
      expect(result.error).toBe("Bot not found");
    });

    describe("lifecycle", () => {
      const botToRemove = mockBot({
        id: "test-id",
        config: mockBotConfig({ type: "scheduled/test-bot", version: "1.0.0" }),
        userId: uid,
        customBotId: "cb-123",
      });

      beforeEach(() => {
        jest.spyOn(repository, "findOne").mockResolvedValue(Ok(botToRemove));
        jest
          .spyOn(repository, "removeWithEvents")
          .mockResolvedValue(Ok(undefined));
        (mockCustomBotService.checkOrphaned as jest.Mock) = jest
          .fn()
          .mockResolvedValue(
            Ok({ orphaned: false, name: "test-bot", version: "1.0.0" }),
          );
      });

      it("should fail when the bot does not exist", async () => {
        jest.spyOn(repository, "findOne").mockResolvedValue(Failure("missing"));

        const result = await remove(actor, "test-id");

        expect(result).toEqual(Failure("Bot not found"));
        expect(repository.removeWithEvents).not.toHaveBeenCalled();
      });

      it("should reject a bot type without a name before deleting", async () => {
        jest.spyOn(repository, "findOne").mockResolvedValue(
          Ok({
            ...botToRemove,
            config: { ...botToRemove.config, type: "scheduled/ " },
          }),
        );

        const result = await remove(actor, "test-id");

        expect(result).toEqual(
          Failure("Invalid bot type: missing name after '/'"),
        );
        expect(logger.warn).toHaveBeenCalledWith(
          { type: "scheduled/ " },
          "Invalid bot type: missing name after '/'",
        );
        expect(repository.removeWithEvents).not.toHaveBeenCalled();
      });

      it("should store the deletion event, with the stored config, with the delete", async () => {
        const result = await remove(actor, "test-id");

        expect(result).toEqual(Ok({}));
        expect(
          mockCustomBotService.getUserSpecificVersion,
        ).toHaveBeenCalledWith(uid, "test-bot", "1.0.0");
        expect(repository.removeWithEvents).toHaveBeenCalledWith(
          uid,
          "test-id",
          [
            {
              botId: "test-id",
              subject: "the0.bot-schedule.deleted",
              payload: { id: "test-id", config: botToRemove.config },
            },
          ],
        );
        expect(relay.publishCommitted).toHaveBeenCalledTimes(1);
      });

      it("should delete without an event when the custom bot version is unavailable", async () => {
        mockCustomBotService.getUserSpecificVersion = jest
          .fn()
          .mockResolvedValue(Failure("not found"));

        const result = await remove(actor, "test-id");

        expect(result).toEqual(Ok({}));
        expect(repository.removeWithEvents).toHaveBeenCalledWith(
          uid,
          "test-id",
          [],
        );
        expect(relay.publishCommitted).toHaveBeenCalledWith([]);
      });

      it("should skip the custom bot lookup when the version is missing", async () => {
        jest.spyOn(repository, "findOne").mockResolvedValue(
          Ok({
            ...botToRemove,
            config: { ...botToRemove.config, version: undefined },
          }),
        );

        const result = await remove(actor, "test-id");

        expect(result).toEqual(Ok({}));
        expect(
          mockCustomBotService.getUserSpecificVersion,
        ).not.toHaveBeenCalled();
        expect(repository.removeWithEvents).toHaveBeenCalledWith(
          uid,
          "test-id",
          [],
        );
        expect(relay.publishCommitted).toHaveBeenCalledWith([]);
      });

      it("should neither check orphans nor ask the relay to publish when removal fails", async () => {
        jest
          .spyOn(repository, "removeWithEvents")
          .mockResolvedValue(Failure("db down"));

        const result = await remove(actor, "test-id");

        expect(result).toEqual(Failure("db down"));
        expect(mockCustomBotService.checkOrphaned).not.toHaveBeenCalled();
        expect(relay.publishCommitted).not.toHaveBeenCalled();
      });

      it("should warn and continue when the orphan check fails", async () => {
        (mockCustomBotService.checkOrphaned as jest.Mock).mockResolvedValue(
          Failure("lookup failed"),
        );

        const result = await remove(actor, "test-id");

        expect(result).toEqual(Ok({}));
        expect(logger.warn).toHaveBeenCalledWith(
          {
            botId: "test-id",
            customBotId: "cb-123",
            error: "lookup failed",
          },
          "Failed to check if custom bot version is orphaned",
        );
        expect(relay.publishCommitted).toHaveBeenCalled();
      });

      it("should skip the orphan check when the bot has no customBotId", async () => {
        jest
          .spyOn(repository, "findOne")
          .mockResolvedValue(Ok({ ...botToRemove, customBotId: null }));

        const result = await remove(actor, "test-id");

        expect(result).toEqual(Ok({}));
        expect(mockCustomBotService.checkOrphaned).not.toHaveBeenCalled();
      });
    });
  });
});
