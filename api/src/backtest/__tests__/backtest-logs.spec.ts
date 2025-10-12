import { Test, TestingModule } from "@nestjs/testing";
import { ConfigService } from "@nestjs/config";
import { BacktestService } from "../backtest.service";
import { BacktestRepository } from "../backtest.repository";
import { BacktestValidator } from "../backtest.validator";
import { CustomBotService } from "@/custom-bot/custom-bot.service";
import { NatsService } from "@/nats/nats.service";
import { REQUEST } from "@nestjs/core";
import { Ok, Failure } from "@/common";
import { EventEmitter } from "events";

describe("BacktestService Logs Loading", () => {
  let service: BacktestService;
  let configService: jest.Mocked<ConfigService>;
  let mockRepository: jest.Mocked<BacktestRepository>;

  const mockMinioClient = {
    getObject: jest.fn(),
  };

  const uid = "test-user-id";

  const createMockStream = (content: string) => {
    const stream = new EventEmitter();
    setTimeout(() => {
      stream.emit("data", Buffer.from(content));
      stream.emit("end");
    }, 0);
    return stream;
  };

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [
        BacktestService,
        {
          provide: ConfigService,
          useValue: {
            get: jest.fn().mockReturnValue("test-bucket"),
          },
        },
        {
          provide: BacktestRepository,
          useValue: {
            findOne: jest.fn(),
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
    configService = module.get(ConfigService);
    mockRepository = module.get(BacktestRepository);

    // Mock the MinIO client
    (service as any).minioClient = mockMinioClient;
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  describe("getBacktestLogs", () => {
    const mockBacktest = {
      id: "test-id",
      name: "Test Backtest",
      status: "completed",
      userId: uid,
      config: {},
      analysis: null,
      createdAt: new Date(),
      updatedAt: new Date(),
      customBotId: "bot-id",
    };

    it("should return logs for valid backtest", async () => {
      const mockLogs = `STARTUP: Node.js backtest wrapper starting
STARTUP: Node.js version v20.19.3
STARTUP: Current working directory: /tmp
STARTUP: Signal handlers registered for SIGTERM and SIGINT
CHDIR_ATTEMPT: Changing to /backtest directory
CHDIR_SUCCESS: Changed to working directory: /backtest
PACKAGE_CHECK: package.json found
PACKAGE_DEPS: mathjs
CONFIG_PARSE: Parsing backtest configuration
CONFIG_SUCCESS: Backtest ID: test-id
CONFIG_SUCCESS: Script path: backtest
IMPORT_ATTEMPT: Starting main module import
IMPORT_STEP: Requiring /backtest/backtest.js
IMPORT_STEP: Module loaded successfully
IMPORT_STEP: Main function type: function
IMPORT_SUCCESS: Main function found
EXECUTE_ATTEMPT: Starting backtest execution
EXECUTE_SUCCESS: Backtest execution completed on its own.
RESULT_WRITE: Writing result to /backtest/result.json
RESULT_WRITE: Successfully wrote result file`;

      mockRepository.findOne.mockResolvedValue(Ok(mockBacktest));
      mockMinioClient.getObject.mockResolvedValue(createMockStream(mockLogs));

      const result = await service.getBacktestLogs("test-id");

      expect(result.success).toBe(true);
      expect(result.data).toBe(mockLogs);
      expect(mockMinioClient.getObject).toHaveBeenCalledWith(
        "test-bucket",
        "test-id/logs.txt",
      );
    });

    it("should validate user access before retrieving logs", async () => {
      mockRepository.findOne.mockResolvedValue(Failure("Backtest not found"));

      const result = await service.getBacktestLogs("test-id");

      expect(result.success).toBe(false);
      expect(result.error).toBe("Backtest not found or access denied");
      expect(mockMinioClient.getObject).not.toHaveBeenCalled();
    });

    it("should return error for non-existent logs file", async () => {
      mockRepository.findOne.mockResolvedValue(Ok(mockBacktest));
      mockMinioClient.getObject.mockRejectedValue(
        new Error("The specified key does not exist."),
      );

      const result = await service.getBacktestLogs("test-id");

      expect(result.success).toBe(false);
      expect(result.error).toBe(
        "Failed to load logs: The specified key does not exist.",
      );
    });

    it("should handle MinIO download errors", async () => {
      mockRepository.findOne.mockResolvedValue(Ok(mockBacktest));
      mockMinioClient.getObject.mockRejectedValue(new Error("Network error"));

      const result = await service.getBacktestLogs("test-id");

      expect(result.success).toBe(false);
      expect(result.error).toBe("Failed to load logs: Network error");
    });

    it("should work with failed backtests", async () => {
      const failedBacktest = { ...mockBacktest, status: "failed" };
      const mockLogs = "ERROR: Backtest failed with error XYZ";

      mockRepository.findOne.mockResolvedValue(Ok(failedBacktest));
      mockMinioClient.getObject.mockResolvedValue(createMockStream(mockLogs));

      const result = await service.getBacktestLogs("test-id");

      expect(result.success).toBe(true);
      expect(result.data).toBe(mockLogs);
    });

    it("should work with running backtests", async () => {
      const runningBacktest = { ...mockBacktest, status: "running" };
      const mockLogs =
        "STARTUP: Node.js backtest wrapper starting\\nINFO: Processing...";

      mockRepository.findOne.mockResolvedValue(Ok(runningBacktest));
      mockMinioClient.getObject.mockResolvedValue(createMockStream(mockLogs));

      const result = await service.getBacktestLogs("test-id");

      expect(result.success).toBe(true);
      expect(result.data).toBe(mockLogs);
    });

    it("should handle access denied from repository", async () => {
      mockRepository.findOne.mockResolvedValue(Failure("Access denied"));

      const result = await service.getBacktestLogs("test-id");

      expect(result.success).toBe(false);
      expect(result.error).toBe("Backtest not found or access denied");
    });

    it("should handle empty logs file", async () => {
      mockRepository.findOne.mockResolvedValue(Ok(mockBacktest));
      mockMinioClient.getObject.mockResolvedValue(createMockStream(""));

      const result = await service.getBacktestLogs("test-id");

      expect(result.success).toBe(true);
      expect(result.data).toBe("");
    });

    it("should handle logs with special characters and unicode", async () => {
      const mockLogs =
        "INFO: Processing symbol BTC/USD\\nWARNING: Price jumped 10% 📈\\nERROR: Connection failed ❌";

      mockRepository.findOne.mockResolvedValue(Ok(mockBacktest));
      mockMinioClient.getObject.mockResolvedValue(createMockStream(mockLogs));

      const result = await service.getBacktestLogs("test-id");

      expect(result.success).toBe(true);
      expect(result.data).toBe(mockLogs);
    });

    it("should use correct bucket from config", async () => {
      const customBucket = "custom-backtest-bucket";
      configService.get.mockReturnValue(customBucket);

      // Recreate service to pick up new config
      const module = await Test.createTestingModule({
        providers: [
          BacktestService,
          {
            provide: ConfigService,
            useValue: configService,
          },
          {
            provide: BacktestRepository,
            useValue: mockRepository,
          },
          {
            provide: BacktestValidator,
            useValue: { validate: jest.fn() },
          },
          {
            provide: CustomBotService,
            useValue: { getGlobalSpecificVersion: jest.fn() },
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

      const newService = await module.resolve<BacktestService>(BacktestService);
      (newService as any).minioClient = mockMinioClient;

      mockRepository.findOne.mockResolvedValue(Ok(mockBacktest));
      mockMinioClient.getObject.mockResolvedValue(
        createMockStream("test logs"),
      );

      await newService.getBacktestLogs("test-id");

      expect(mockMinioClient.getObject).toHaveBeenCalledWith(
        customBucket,
        "test-id/logs.txt",
      );
    });
  });
});
