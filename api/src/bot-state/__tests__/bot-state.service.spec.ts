import { Test, TestingModule } from "@nestjs/testing";
import {
  BotStateService,
  StateKey,
  BotStateErrorCode,
} from "../bot-state.service";
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
import * as tar from "tar";

describe("BotStateService", () => {
  let service: BotStateService;
  let mockBotService: jest.Mocked<Partial<BotService>>;
  let mockConfigService: jest.Mocked<ConfigService>;
  let mockMinioClient: Record<string, jest.Mock>;
  let mockLogger: ReturnType<typeof createMockLogger>;
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
    mockMinioClient = {
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
          useValue: (mockLogger = createMockLogger()),
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

  const statePath = `${testBotId}/state.tar.gz`;
  const notFound = () =>
    Object.assign(new Error("Not Found"), { code: "NotFound" });

  const writeStateArchive = async (
    dest: string,
    files: Record<string, string>,
  ) => {
    const srcDir = fs.mkdtempSync(path.join(tempDir, "src-"));
    const stateDir = path.join(srcDir, ".the0-state");
    fs.mkdirSync(stateDir);
    for (const [name, content] of Object.entries(files)) {
      fs.writeFileSync(path.join(stateDir, name), content);
    }
    await tar.c({ gzip: true, file: dest, cwd: srcDir }, [".the0-state"]);
  };

  const downloadedDirs: string[] = [];
  const storeState = (files: Record<string, string>) => {
    mockMinioClient.statObject.mockResolvedValueOnce({
      size: 100,
      etag: "etag-1",
    });
    mockMinioClient.fGetObject.mockImplementation(
      async (_bucket: string, _path: string, dest: string) => {
        downloadedDirs.push(path.dirname(dest));
        await writeStateArchive(dest, files);
      },
    );
  };

  beforeEach(() => {
    downloadedDirs.length = 0;
  });

  describe("listKeys", () => {
    it("should return failure when bot not found", async () => {
      mockBotService.findOne = jest
        .fn()
        .mockResolvedValue(Failure("Not found"));

      const result = await service.listKeys("nonexistent-bot");

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotStateErrorCode.BOT_NOT_FOUND);
    });

    it("should verify bot ownership before listing", async () => {
      mockBotService.findOne = jest
        .fn()
        .mockResolvedValue(Failure("Access denied"));

      const result = await service.listKeys(testBotId);

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotStateErrorCode.BOT_NOT_FOUND);
      expect(mockBotService.findOne).toHaveBeenCalledWith(testBotId);
    });
  });

  describe("getKey", () => {
    it("should return failure when bot not found", async () => {
      mockBotService.findOne = jest
        .fn()
        .mockResolvedValue(Failure("Not found"));

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
      mockBotService.findOne = jest
        .fn()
        .mockResolvedValue(Failure("Not found"));

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

  describe("reading a key from stored state", () => {
    it("rejects a malformed bot ID before checking ownership", async () => {
      const result = await service.getKey("../other-bot", "portfolio");

      expect(result).toEqual(
        Failure({
          code: BotStateErrorCode.STORAGE_ERROR,
          message: "Invalid bot ID format",
        }),
      );
      expect(mockBotService.findOne).not.toHaveBeenCalled();
    });

    it("reports a missing key when no state archive exists", async () => {
      mockMinioClient.statObject.mockRejectedValueOnce(notFound());

      const result = await service.getKey(testBotId, "portfolio");

      expect(result).toEqual(
        Failure({
          code: BotStateErrorCode.KEY_NOT_FOUND,
          message: "State key not found",
        }),
      );
      expect(mockMinioClient.fGetObject).not.toHaveBeenCalled();
    });

    it("reports a missing key that is not in the archive", async () => {
      storeState({ "other.json": "2" });

      const result = await service.getKey(testBotId, "portfolio");

      expect(result.error?.code).toBe(BotStateErrorCode.KEY_NOT_FOUND);
      expect(fs.existsSync(downloadedDirs[0])).toBe(false);
    });

    it("returns the parsed value and cleans up the temp dir", async () => {
      storeState({ "portfolio.json": '{"cash":100,"positions":["BTC"]}' });

      const result = await service.getKey(testBotId, "portfolio");

      expect(result).toEqual(Ok({ cash: 100, positions: ["BTC"] }));
      expect(mockMinioClient.fGetObject).toHaveBeenCalledWith(
        "bot-state",
        statePath,
        expect.stringMatching(/state\.tar\.gz$/),
      );
      expect(downloadedDirs).toHaveLength(1);
      expect(fs.existsSync(downloadedDirs[0])).toBe(false);
    });

    it("refuses to read a value over the configured size limit", async () => {
      const smallLimitConfig = {
        get: jest.fn((key: string) =>
          key === "MAX_STATE_FILE_SIZE_MB" ? "1" : undefined,
        ),
      } as unknown as ConfigService;
      const limitedService = new BotStateService(
        mockMinioClient as any,
        smallLimitConfig,
        mockBotService as any,
        mockLogger as any,
      );
      const oneMbAndAByte = " ".repeat(1024 * 1024) + "1";
      storeState({ "portfolio.json": oneMbAndAByte });

      const result = await limitedService.getKey(testBotId, "portfolio");

      expect(result).toEqual(
        Failure({
          code: BotStateErrorCode.FILE_TOO_LARGE,
          message: "State file exceeds maximum size limit (1MB)",
        }),
      );
      expect(mockLogger.warn).toHaveBeenCalledWith(
        {
          botId: testBotId,
          key: "portfolio",
          size: 1024 * 1024 + 1,
          maxSize: 1024 * 1024,
        },
        "State file exceeds maximum size",
      );
      expect(fs.existsSync(downloadedDirs[0])).toBe(false);
    });

    it("reports invalid JSON in the stored value", async () => {
      storeState({ "portfolio.json": "{not json" });

      const result = await service.getKey(testBotId, "portfolio");

      expect(result).toEqual(
        Failure({
          code: BotStateErrorCode.INVALID_JSON,
          message: "State file contains invalid JSON",
        }),
      );
      expect(mockLogger.error).toHaveBeenCalledWith(
        { err: expect.any(SyntaxError), botId: testBotId, key: "portfolio" },
        "Invalid JSON in state file",
      );
    });

    it("returns a storage error when the archive cannot be read", async () => {
      const failure = new Error("stat failed");
      mockMinioClient.statObject.mockRejectedValueOnce(failure);

      const result = await service.getKey(testBotId, "portfolio");

      expect(result).toEqual(
        Failure({
          code: BotStateErrorCode.STORAGE_ERROR,
          message: "Failed to get state key",
        }),
      );
      expect(mockLogger.error).toHaveBeenCalledWith(
        { err: failure, botId: testBotId, key: "portfolio" },
        "Error getting state key",
      );
    });

    it("returns a storage error when the archive exceeds the download limit", async () => {
      mockMinioClient.statObject.mockResolvedValueOnce({
        size: 100 * 1024 * 1024 + 1,
        etag: "etag-1",
      });

      const result = await service.getKey(testBotId, "portfolio");

      expect(result.error?.code).toBe(BotStateErrorCode.STORAGE_ERROR);
      expect(mockMinioClient.fGetObject).not.toHaveBeenCalled();
    });
  });

  describe("deleting a key from stored state", () => {
    // The service deletes its temp dir after uploading, so the archive must
    // be inspected while fPutObject is still running.
    const captureUploads = () => {
      const uploads: string[][] = [];
      mockMinioClient.fPutObject.mockImplementation(
        async (_bucket: string, _path: string, file: string) => {
          const outDir = fs.mkdtempSync(path.join(tempDir, "upload-"));
          await tar.x({ file, cwd: outDir });
          uploads.push(fs.readdirSync(path.join(outDir, ".the0-state")).sort());
        },
      );
      return uploads;
    };

    it("returns false without downloading when no state archive exists", async () => {
      mockMinioClient.statObject.mockRejectedValueOnce(notFound());

      const result = await service.deleteKey(testBotId, "portfolio");

      expect(result).toEqual(Ok(false));
      expect(mockMinioClient.fGetObject).not.toHaveBeenCalled();
    });

    it("returns false without writing when the key is absent", async () => {
      storeState({ "other.json": "2" });

      const result = await service.deleteKey(testBotId, "portfolio");

      expect(result).toEqual(Ok(false));
      expect(mockMinioClient.statObject).toHaveBeenCalledTimes(1);
      expect(mockMinioClient.fPutObject).not.toHaveBeenCalled();
      expect(mockMinioClient.removeObject).not.toHaveBeenCalled();
    });

    describe("when other keys remain", () => {
      beforeEach(() => {
        storeState({ "portfolio.json": '{"a":1}', "other.json": "2" });
      });

      it("re-uploads the archive without the key when the ETag is unchanged", async () => {
        mockMinioClient.statObject.mockResolvedValueOnce({ etag: "etag-1" });
        const uploads = captureUploads();

        const result = await service.deleteKey(testBotId, "portfolio");

        expect(result).toEqual(Ok(true));
        expect(mockMinioClient.statObject).toHaveBeenNthCalledWith(
          2,
          "bot-state",
          statePath,
        );
        expect(mockMinioClient.fPutObject).toHaveBeenCalledWith(
          "bot-state",
          statePath,
          expect.stringMatching(/state\.tar\.gz$/),
        );
        expect(uploads).toEqual([["other.json"]]);
        expect(mockMinioClient.removeObject).not.toHaveBeenCalled();
        expect(mockLogger.warn).not.toHaveBeenCalled();
      });

      it("cleans up the downloaded temp dir", async () => {
        mockMinioClient.statObject.mockResolvedValueOnce({ etag: "etag-1" });
        captureUploads();

        await service.deleteKey(testBotId, "portfolio");

        expect(downloadedDirs).toHaveLength(1);
        expect(fs.existsSync(downloadedDirs[0])).toBe(false);
      });

      it("reports a concurrent modification without uploading when the ETag changed", async () => {
        mockMinioClient.statObject.mockResolvedValueOnce({ etag: "etag-2" });

        const result = await service.deleteKey(testBotId, "portfolio");

        expect(result).toEqual(
          Failure({
            code: BotStateErrorCode.CONCURRENT_MODIFICATION,
            message:
              "State was modified by another operation. Please retry the request.",
          }),
        );
        expect(mockLogger.warn).toHaveBeenCalledWith(
          { botId: testBotId, expectedEtag: "etag-1", currentEtag: "etag-2" },
          "Concurrent modification detected - aborting upload to prevent data loss",
        );
        expect(mockMinioClient.fPutObject).not.toHaveBeenCalled();
        expect(mockMinioClient.removeObject).not.toHaveBeenCalled();
      });

      it("reports a concurrent modification when the archive was deleted meanwhile", async () => {
        mockMinioClient.statObject.mockRejectedValueOnce(notFound());

        const result = await service.deleteKey(testBotId, "portfolio");

        expect(result.error?.code).toBe(
          BotStateErrorCode.CONCURRENT_MODIFICATION,
        );
        expect(mockLogger.warn).toHaveBeenCalledWith(
          { botId: testBotId, expectedEtag: "etag-1" },
          "State object was deleted during modification",
        );
        expect(mockMinioClient.fPutObject).not.toHaveBeenCalled();
      });

      it("returns a storage error when the ETag check fails", async () => {
        const failure = new Error("stat failed");
        mockMinioClient.statObject.mockRejectedValueOnce(failure);

        const result = await service.deleteKey(testBotId, "portfolio");

        expect(result).toEqual(
          Failure({
            code: BotStateErrorCode.STORAGE_ERROR,
            message: "Failed to delete state key",
          }),
        );
        expect(mockLogger.error).toHaveBeenCalledWith(
          { err: failure, botId: testBotId, key: "portfolio" },
          "Error deleting state key",
        );
        expect(mockMinioClient.fPutObject).not.toHaveBeenCalled();
      });

      it("returns a storage error when the upload fails", async () => {
        mockMinioClient.statObject.mockResolvedValueOnce({ etag: "etag-1" });
        mockMinioClient.fPutObject.mockRejectedValueOnce(
          new Error("put failed"),
        );

        const result = await service.deleteKey(testBotId, "portfolio");

        expect(result.error?.code).toBe(BotStateErrorCode.STORAGE_ERROR);
      });
    });

    describe("when the last key is removed", () => {
      it("removes the archive when the ETag is unchanged", async () => {
        storeState({ "portfolio.json": "1" });
        mockMinioClient.statObject.mockResolvedValueOnce({ etag: "etag-1" });

        const result = await service.deleteKey(testBotId, "portfolio");

        expect(result).toEqual(Ok(true));
        expect(mockMinioClient.statObject).toHaveBeenCalledTimes(2);
        expect(mockMinioClient.removeObject).toHaveBeenCalledWith(
          "bot-state",
          statePath,
        );
        expect(mockMinioClient.fPutObject).not.toHaveBeenCalled();
      });

      it("treats leftover non-JSON files as empty state", async () => {
        storeState({ "portfolio.json": "1", "notes.txt": "x" });
        mockMinioClient.statObject.mockResolvedValueOnce({ etag: "etag-1" });

        const result = await service.deleteKey(testBotId, "portfolio");

        expect(result).toEqual(Ok(true));
        expect(mockMinioClient.removeObject).toHaveBeenCalledTimes(1);
        expect(mockMinioClient.fPutObject).not.toHaveBeenCalled();
      });

      it("reports a concurrent modification without removing when the ETag changed", async () => {
        storeState({ "portfolio.json": "1" });
        mockMinioClient.statObject.mockResolvedValueOnce({ etag: "etag-2" });

        const result = await service.deleteKey(testBotId, "portfolio");

        expect(result.error?.code).toBe(
          BotStateErrorCode.CONCURRENT_MODIFICATION,
        );
        expect(mockLogger.warn).toHaveBeenCalledWith(
          { botId: testBotId, expectedEtag: "etag-1", currentEtag: "etag-2" },
          "Concurrent modification detected during state deletion",
        );
        expect(mockMinioClient.removeObject).not.toHaveBeenCalled();
      });

      it("still removes the archive when it is already gone", async () => {
        storeState({ "portfolio.json": "1" });
        mockMinioClient.statObject.mockRejectedValueOnce(notFound());

        const result = await service.deleteKey(testBotId, "portfolio");

        expect(result).toEqual(Ok(true));
        expect(mockMinioClient.removeObject).toHaveBeenCalledWith(
          "bot-state",
          statePath,
        );
        expect(mockLogger.warn).not.toHaveBeenCalled();
      });

      it("returns a storage error when the ETag check fails", async () => {
        storeState({ "portfolio.json": "1" });
        mockMinioClient.statObject.mockRejectedValueOnce(
          new Error("stat failed"),
        );

        const result = await service.deleteKey(testBotId, "portfolio");

        expect(result.error?.code).toBe(BotStateErrorCode.STORAGE_ERROR);
        expect(mockMinioClient.removeObject).not.toHaveBeenCalled();
      });

      it("ignores failures removing the archive", async () => {
        storeState({ "portfolio.json": "1" });
        mockMinioClient.statObject.mockResolvedValueOnce({ etag: "etag-1" });
        mockMinioClient.removeObject.mockRejectedValueOnce(
          new Error("remove failed"),
        );

        const result = await service.deleteKey(testBotId, "portfolio");

        expect(result).toEqual(Ok(true));
      });
    });
  });

  // No public method reaches these branches today: deleteKey always leaves
  // the state directory in place and always passes an ETag.
  describe("uploading state outside deleteKey", () => {
    const upload = (dir: string, expectedEtag?: string): Promise<boolean> =>
      (service as any).uploadStateWithLocking(testBotId, dir, expectedEtag);

    describe("without a state directory", () => {
      it("removes the archive without an ETag check when none is expected", async () => {
        await expect(upload(tempDir)).resolves.toBe(true);

        expect(mockMinioClient.statObject).not.toHaveBeenCalled();
        expect(mockMinioClient.removeObject).toHaveBeenCalledWith(
          "bot-state",
          statePath,
        );
      });

      it("removes the archive when the ETag is unchanged", async () => {
        mockMinioClient.statObject.mockResolvedValueOnce({ etag: "etag-1" });

        await expect(upload(tempDir, "etag-1")).resolves.toBe(true);

        expect(mockMinioClient.statObject).toHaveBeenCalledWith(
          "bot-state",
          statePath,
        );
        expect(mockMinioClient.removeObject).toHaveBeenCalledTimes(1);
      });

      it("refuses to remove the archive when the ETag changed", async () => {
        mockMinioClient.statObject.mockResolvedValueOnce({ etag: "etag-2" });

        await expect(upload(tempDir, "etag-1")).resolves.toBe(false);

        expect(mockLogger.warn).toHaveBeenCalledWith(
          { botId: testBotId, expectedEtag: "etag-1", currentEtag: "etag-2" },
          "Concurrent modification detected during state deletion",
        );
        expect(mockMinioClient.removeObject).not.toHaveBeenCalled();
      });

      it("still removes the archive when it is already gone", async () => {
        mockMinioClient.statObject.mockRejectedValueOnce(
          Object.assign(new Error("Not Found"), { code: "NotFound" }),
        );

        await expect(upload(tempDir, "etag-1")).resolves.toBe(true);

        expect(mockMinioClient.removeObject).toHaveBeenCalledTimes(1);
      });

      it("propagates other ETag check failures", async () => {
        mockMinioClient.statObject.mockRejectedValueOnce(
          Object.assign(new Error("denied"), { code: "AccessDenied" }),
        );

        await expect(upload(tempDir, "etag-1")).rejects.toThrow("denied");

        expect(mockMinioClient.removeObject).not.toHaveBeenCalled();
      });

      it("ignores failures removing the archive", async () => {
        mockMinioClient.removeObject.mockRejectedValueOnce(
          new Error("remove failed"),
        );

        await expect(upload(tempDir)).resolves.toBe(true);
      });
    });

    describe("without an expected ETag", () => {
      it("uploads remaining keys without an ETag check", async () => {
        fs.mkdirSync(path.join(tempDir, ".the0-state"));
        fs.writeFileSync(path.join(tempDir, ".the0-state", "a.json"), "1");

        await expect(upload(tempDir)).resolves.toBe(true);

        expect(mockMinioClient.statObject).not.toHaveBeenCalled();
        expect(mockMinioClient.fPutObject).toHaveBeenCalledWith(
          "bot-state",
          statePath,
          path.join(tempDir, "state.tar.gz"),
        );
        expect(fs.existsSync(path.join(tempDir, "state.tar.gz"))).toBe(true);
      });

      it("removes the archive without an ETag check when no keys remain", async () => {
        fs.mkdirSync(path.join(tempDir, ".the0-state"));

        await expect(upload(tempDir)).resolves.toBe(true);

        expect(mockMinioClient.statObject).not.toHaveBeenCalled();
        expect(mockMinioClient.removeObject).toHaveBeenCalledTimes(1);
        expect(mockMinioClient.fPutObject).not.toHaveBeenCalled();
      });
    });
  });

  describe("clearState", () => {
    it("should return failure when bot not found", async () => {
      mockBotService.findOne = jest
        .fn()
        .mockResolvedValue(Failure("Not found"));

      const result = await service.clearState("nonexistent-bot");

      expect(result.success).toBe(false);
      expect(result.error?.code).toBe(BotStateErrorCode.BOT_NOT_FOUND);
    });

    it("should verify bot ownership before clearing", async () => {
      mockBotService.findOne = jest
        .fn()
        .mockResolvedValue(Failure("Access denied"));

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
