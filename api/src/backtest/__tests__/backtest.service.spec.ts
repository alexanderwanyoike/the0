import { BacktestService } from "../backtest.service";
import { BacktestRepository } from "../backtest.repository";
import { Test, TestingModule } from "@nestjs/testing";
import { ConfigService } from "@nestjs/config";
import { Backtest } from "../entities/backtest.entity";
import { REQUEST } from "@nestjs/core";
import { BacktestValidator } from "../backtest.validator";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { NatsService } from "@/nats/nats.service";
import { Ok, Failure } from "@/common";

describe("BacktestService", () => {
  let service: BacktestService;
  let repository: BacktestRepository;
  let backtestValidator: BacktestValidator;
  let customBotService: CustomBotService;
  let natsService: NatsService;
  const uid = "test-user-id";

  const mockCustomBot = {
    id: "custom-bot-id",
    name: "test-bot",
    version: "1.0.0",
    userId: "test-user-id", // Make the test user the owner
    status: "approved",
    marketplace: { isPublished: true },
    config: {
      entrypoints: {
        bot: "main.py",
        backtest: "backtest.py",
      },
      schema: {
        bot: {
          type: "object",
          properties: {},
        },
        backtest: {
          type: "object",
          properties: {
            type: { type: "string" },
            version: { type: "string" },
          },
        },
      },
    },
  };

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [
        BacktestService,
        {
          provide: BacktestRepository,
          useValue: {
            create: jest.fn(),
            findAll: jest.fn(),
            findOne: jest.fn(),
            remove: jest.fn(),
          },
        },
        {
          provide: BacktestValidator,
          useValue: {
            validate: jest.fn(),
          },
        },
        {
          provide: CustomBotService,
          useValue: {
            getGlobalSpecificVersion: jest.fn(),
          },
        },
        {
          provide: ConfigService,
          useValue: {
            get: jest.fn().mockReturnValue("test-bucket"),
          },
        },
        {
          provide: NatsService,
          useValue: {
            publish: jest.fn().mockResolvedValue(Ok(null)),
          },
        },
      ],
    })
      .overrideProvider(REQUEST)
      .useValue({ user: { uid } })
      .compile();

    service = await module.resolve<BacktestService>(BacktestService);
    repository = module.get<BacktestRepository>(BacktestRepository);
    backtestValidator = module.get<BacktestValidator>(BacktestValidator);
    customBotService = module.get<CustomBotService>(CustomBotService);
    natsService = module.get<NatsService>(NatsService);
  });

  it("should be able to create a backtest", async () => {
    const backtest = {
      name: "Test backtest",
      config: {
        type: "scheduled/test-bot",
        version: "1.0.0",
      },
    } as Backtest;

    // Mock custom bot service to return a valid bot
    (customBotService.getGlobalSpecificVersion as jest.Mock).mockResolvedValue(
      Ok(mockCustomBot),
    );

    // Mock validator to return success
    (backtestValidator.validate as jest.Mock).mockResolvedValue(Ok(true));

    // Mock repository to return success
    (repository.create as jest.Mock).mockResolvedValue({
      success: true,
      error: null,
      data: {
        id: "test-id",
        ...backtest,
        customBotId: mockCustomBot.id,
      },
    });

    const result = await service.create(backtest);
    expect(result).toEqual({
      success: true,
      error: null,
      data: {
        id: "test-id",
        ...backtest,
        customBotId: mockCustomBot.id,
      },
    });
    expect(repository.create).toHaveBeenCalledWith({
      ...backtest,
      userId: uid,
      status: "pending",
      customBotId: mockCustomBot.id,
    });
  });

  it("should be able to get all backtests", async () => {
    const backtests = [
      {
        id: "test-id",
        name: "Test backtest",
        config: {
          type: "alpaca/sma",
        },
      },
    ];

    repository.findAll = jest.fn().mockReturnValue({
      success: true,
      error: null,
      data: backtests,
    });

    const result = await service.findAll();
    expect(result).toEqual({
      success: true,
      error: null,
      data: backtests,
    });
    expect(repository.findAll).toHaveBeenCalledWith(uid);
  });

  it("should be able to get a backtest by id", async () => {
    const backtest = {
      id: "test-id",
      name: "Test backtest",
      config: {
        type: "alpaca/sma",
      },
    } as Backtest;

    repository.findOne = jest.fn().mockReturnValue({
      success: true,
      error: null,
      data: backtest,
    });

    const result = await service.findOne("test-id");
    expect(result).toEqual({
      success: true,
      error: null,
      data: backtest,
    });
    expect(repository.findOne).toHaveBeenCalledWith(uid, "test-id");
  });

  it("should be able to remove a backtest", async () => {
    repository.remove = jest.fn().mockReturnValue({
      success: true,
      error: null,
      data: null,
    });

    const result = await service.remove("test-id");
    expect(result).toEqual({
      success: true,
      error: null,
      data: null,
    });
    expect(repository.remove).toHaveBeenCalledWith(uid, "test-id");
  });

  describe("Bot Validation Tests", () => {
    it("should fail when bot has no backtest entrypoint", async () => {
      const createBacktestDto = {
        name: "Test backtest",
        config: {
          type: "custom/test-bot",
          version: "1.0.0",
        },
      };

      const customBotWithoutBacktestEntrypoint = {
        ...mockCustomBot,
        config: {
          ...mockCustomBot.config,
          entrypoints: {
            bot: "main.py",
            // Missing backtest entrypoint
          },
        },
      };

      (
        customBotService.getGlobalSpecificVersion as jest.Mock
      ).mockResolvedValue(Ok(customBotWithoutBacktestEntrypoint));

      const result = await service.create(createBacktestDto);

      expect(result.success).toBe(false);
      expect(result.error).toBe(
        "Bot does not support backtesting: missing backtest entrypoint",
      );
    });

    it("should fail when bot has no backtest schema", async () => {
      const createBacktestDto = {
        name: "Test backtest",
        config: {
          type: "custom/test-bot",
          version: "1.0.0",
        },
      };

      const customBotWithoutBacktestSchema = {
        ...mockCustomBot,
        config: {
          ...mockCustomBot.config,
          entrypoints: {
            bot: "main.py",
            backtest: "backtest.py",
          },
          schema: {
            bot: {},
            // Missing backtest schema
          },
        },
      };

      (
        customBotService.getGlobalSpecificVersion as jest.Mock
      ).mockResolvedValue(Ok(customBotWithoutBacktestSchema));

      const result = await service.create(createBacktestDto);

      expect(result.success).toBe(false);
      expect(result.error).toBe(
        "Bot does not support backtesting: missing backtest schema",
      );
    });

    it("should fail when bot has neither backtest entrypoint nor schema", async () => {
      const createBacktestDto = {
        name: "Test backtest",
        config: {
          type: "custom/test-bot",
          version: "1.0.0",
        },
      };

      const customBotWithoutBacktest = {
        ...mockCustomBot,
        config: {
          ...mockCustomBot.config,
          entrypoints: {
            bot: "main.py",
            // No backtest entrypoint
          },
          schema: {
            bot: {},
            // No backtest schema
          },
        },
      };

      (
        customBotService.getGlobalSpecificVersion as jest.Mock
      ).mockResolvedValue(Ok(customBotWithoutBacktest));

      const result = await service.create(createBacktestDto);

      expect(result.success).toBe(false);
      expect(result.error).toBe(
        "Bot does not support backtesting: missing backtest entrypoint",
      );
    });
  });
});
