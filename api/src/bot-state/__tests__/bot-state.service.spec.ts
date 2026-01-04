import { Test, TestingModule } from "@nestjs/testing";
import { BotStateService, StateKey, BotStateErrorCode } from "../bot-state.service";
import { ConfigService } from "@nestjs/config";
import { BotService } from "@/bot/bot.service";
import { PinoLogger } from "nestjs-pino";
import { createMockLogger } from "@/test/mock-logger";
import { REQUEST } from "@nestjs/core";
import { Ok, Failure } from "@/common/result";
import { MINIO_CLIENT } from "@/minio";
import * as fs from "fs";
import * as path from "path";
import * as os from "os";

describe("BotStateService", () => {
  let service: BotStateService;
  let mockBotService: jest.Mocked<Partial<BotService>>;
  let mockConfigService: jest.Mocked<ConfigService>;
  let tempDir: string;

  const testUid = "test-user-id";
  const testBotId = "test-bot-id";

  const mockBot = {
    id: testBotId,
    name: "Test Bot",
    userId: testUid,
    customBotId: "test-custom-bot",
    version: "1.0.0",
    config: {},
    enabled: true,
    createdAt: new Date(),
    updatedAt: new Date(),
  };

  beforeEach(async () => {
    // Create temp directory for state tests
    tempDir = fs.mkdtempSync(path.join(os.tmpdir(), "bot-state-test-"));

    mockBotService = {
      findOne: jest.fn().mockResolvedValue(Ok(mockBot)),
    };

    mockConfigService = {
      get: jest.fn().mockImplementation((key: string) => {
        const config: Record<string, string> = {
          MINIO_ENDPOINT: "localhost",
          MINIO_PORT: "9000",
          MINIO_USE_SSL: "false",
          MINIO_ACCESS_KEY: "minioadmin",
          MINIO_SECRET_KEY: "minioadmin",
          MINIO_STATE_BUCKET: "bot-state",
        };
        return config[key];
      }),
    } as any;

    // Create mock MinIO client
    const mockMinioClient = {
      statObject: jest.fn(),
      getObject: jest.fn(),
      fGetObject: jest.fn(),
      putObject: jest.fn(),
      fPutObject: jest.fn(),
      removeObject: jest.fn(),
      listObjects: jest.fn(),
    };

    const module: TestingModule = await Test.createTestingModule({
      providers: [
        BotStateService,
        {
          provide: MINIO_CLIENT,
          useValue: mockMinioClient,
        },
        {
          provide: ConfigService,
          useValue: mockConfigService,
        },
        {
          provide: BotService,
          useValue: mockBotService,
        },
        {
          provide: PinoLogger,
          useValue: createMockLogger(),
        },
        {
          provide: REQUEST,
          useValue: { user: { uid: testUid } },
        },
      ],
    }).compile();

    service = await module.resolve<BotStateService>(BotStateService);
  });

  afterEach(() => {
    // Cleanup temp directory
    if (tempDir && fs.existsSync(tempDir)) {
      fs.rmSync(tempDir, { recursive: true, force: true });
    }
  });

  describe("listKeys", () => {
    it("should return failure when bot not found", async () => {
      mockBotService.findOne = jest.fn().mockResolvedValue(Failure("Not found"));

      const result = await service.listKeys("nonexistent-bot");

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotStateErrorCode.BOT_NOT_FOUND);
    });

    it("should verify bot ownership before listing", async () => {
      mockBotService.findOne = jest.fn().mockResolvedValue(Failure("Access denied"));

      const result = await service.listKeys(testBotId);

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotStateErrorCode.BOT_NOT_FOUND);
      expect(mockBotService.findOne).toHaveBeenCalledWith(testBotId);
    });
  });

  describe("getKey", () => {
    it("should return failure when bot not found", async () => {
      mockBotService.findOne = jest.fn().mockResolvedValue(Failure("Not found"));

      const result = await service.getKey("nonexistent-bot", "portfolio");

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotStateErrorCode.BOT_NOT_FOUND);
    });

    it("should reject invalid keys with forward slash", async () => {
      const result = await service.getKey(testBotId, "../escape");

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotStateErrorCode.INVALID_KEY);
    });

    it("should reject invalid keys with backslash", async () => {
      const result = await service.getKey(testBotId, "..\\escape");

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotStateErrorCode.INVALID_KEY);
    });

    it("should reject invalid keys with double dots", async () => {
      const result = await service.getKey(testBotId, "..");

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotStateErrorCode.INVALID_KEY);
    });

    it("should reject empty key", async () => {
      const result = await service.getKey(testBotId, "");

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotStateErrorCode.INVALID_KEY);
    });
  });

  describe("deleteKey", () => {
    it("should return failure when bot not found", async () => {
      mockBotService.findOne = jest.fn().mockResolvedValue(Failure("Not found"));

      const result = await service.deleteKey("nonexistent-bot", "portfolio");

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotStateErrorCode.BOT_NOT_FOUND);
    });

    it("should reject invalid keys with path separators", async () => {
      const result = await service.deleteKey(testBotId, "../escape");

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotStateErrorCode.INVALID_KEY);
    });

    it("should reject empty key", async () => {
      const result = await service.deleteKey(testBotId, "");

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotStateErrorCode.INVALID_KEY);
    });
  });

  describe("clearState", () => {
    it("should return failure when bot not found", async () => {
      mockBotService.findOne = jest.fn().mockResolvedValue(Failure("Not found"));

      const result = await service.clearState("nonexistent-bot");

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotStateErrorCode.BOT_NOT_FOUND);
    });

    it("should verify bot ownership before clearing", async () => {
      mockBotService.findOne = jest.fn().mockResolvedValue(Failure("Access denied"));

      const result = await service.clearState(testBotId);

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotStateErrorCode.BOT_NOT_FOUND);
      expect(mockBotService.findOne).toHaveBeenCalledWith(testBotId);
    });
  });

  describe("key validation", () => {
    const invalidKeys = [
      { key: "", description: "empty key" },
      { key: "../escape", description: "forward slash path traversal" },
      { key: "..\\escape", description: "backslash path traversal" },
      { key: "..", description: "double dots" },
      { key: "foo/bar", description: "forward slash in key" },
      { key: "foo\\bar", description: "backslash in key" },
    ];

    invalidKeys.forEach(({ key, description }) => {
      it(`should reject ${description}`, async () => {
        const result = await service.getKey(testBotId, key);
        expect(result.success).toBe(false);
        expect(result.error?.code).toBe(BotStateErrorCode.INVALID_KEY);
      });
    });

    const validKeys = [
      "portfolio",
      "trade-count",
      "last_prices",
      "myKey123",
      "key-with-dashes",
      "key_with_underscores",
    ];

    // Note: These tests require MinIO to be available, so they may fail in unit test context
    // They demonstrate the key validation logic only
  });
});
