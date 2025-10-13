import { Test, TestingModule } from "@nestjs/testing";
import { ConfigService } from "@nestjs/config";
import { StorageService } from "../storage.service";
import { Ok, Failure } from "@/common/result";
import * as Minio from "minio";
import { EventEmitter } from "events";
import AdmZip from "adm-zip";

// Mock the minio module
jest.mock("minio");

describe("StorageService", () => {
  let service: StorageService;
  let configService: jest.Mocked<ConfigService>;
  let mockMinioClient: jest.Mocked<Minio.Client>;

  // Helper to create mock stream
  const createMockStream = (content: Buffer) => {
    const stream = new EventEmitter();
    setTimeout(() => {
      if (content) {
        stream.emit("data", content);
      }
      stream.emit("end");
    }, 0);
    return stream;
  };

  // Sample ZIP file buffer for testing
  const createSampleZipBuffer = () => {
    // Create a real ZIP file using AdmZip for testing
    const zip = new AdmZip();
    zip.addFile("main.js", Buffer.from('console.log("hello world");'));
    zip.addFile("config.yaml", Buffer.from("name: test-bot\nversion: 1.0.0"));
    return zip.toBuffer();
  };

  beforeEach(async () => {
    // Create mock MinIO client
    mockMinioClient = {
      putObject: jest.fn(),
      getObject: jest.fn(),
      statObject: jest.fn(),
      removeObject: jest.fn(),
      bucketExists: jest.fn(),
      makeBucket: jest.fn(),
      presignedPutObject: jest.fn(),
    } as any;

    // Mock the Minio.Client constructor
    (Minio.Client as jest.Mock).mockImplementation(() => mockMinioClient);

    const module: TestingModule = await Test.createTestingModule({
      providers: [
        StorageService,
        {
          provide: ConfigService,
          useValue: {
            get: jest.fn((key: string) => {
              const config = {
                MINIO_ENDPOINT: "localhost",
                MINIO_PORT: "9000",
                MINIO_USE_SSL: "false",
                MINIO_ACCESS_KEY: "testkey",
                MINIO_SECRET_KEY: "testsecret",
                CUSTOM_BOTS_BUCKET: "custom-bots",
              };
              return config[key];
            }),
          },
        },
      ],
    }).compile();

    service = module.get<StorageService>(StorageService);
    configService = module.get(ConfigService);
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  describe("constructor", () => {
    it("should initialize MinIO client with correct configuration", () => {
      expect(Minio.Client).toHaveBeenCalledWith({
        endPoint: "localhost",
        port: 9000,
        useSSL: false,
        accessKey: "testkey",
        secretKey: "testsecret",
      });
    });

    it("should set the correct bucket name", () => {
      expect(configService.get).toHaveBeenCalledWith("CUSTOM_BOTS_BUCKET");
    });
  });

  describe("ensureBucket", () => {
    it("should create bucket if it does not exist", async () => {
      mockMinioClient.bucketExists.mockResolvedValue(false);
      mockMinioClient.makeBucket.mockResolvedValue(undefined);

      const result = await service.ensureBucket();

      expect(result.success).toBe(true);
      expect(mockMinioClient.bucketExists).toHaveBeenCalledWith("custom-bots");
      expect(mockMinioClient.makeBucket).toHaveBeenCalledWith("custom-bots");
    });

    it("should not create bucket if it already exists", async () => {
      // Clear any previous calls from constructor
      jest.clearAllMocks();
      mockMinioClient.bucketExists.mockResolvedValue(true);

      const result = await service.ensureBucket();

      expect(result.success).toBe(true);
      expect(mockMinioClient.bucketExists).toHaveBeenCalledWith("custom-bots");
      expect(mockMinioClient.makeBucket).not.toHaveBeenCalled();
    });

    it("should handle bucket creation errors", async () => {
      mockMinioClient.bucketExists.mockResolvedValue(false);
      mockMinioClient.makeBucket.mockRejectedValue(
        new Error("Bucket creation failed"),
      );

      const result = await service.ensureBucket();

      expect(result.success).toBe(false);
      expect(result.error).toContain("Bucket creation failed");
    });
  });

  describe("fileExists", () => {
    it("should return true if file exists", async () => {
      mockMinioClient.statObject.mockResolvedValue({
        size: 1024,
        lastModified: new Date(),
      } as any);

      const result = await service.fileExists("test/file.zip");

      expect(result.success).toBe(true);
      expect(result.data).toBe(true);
      expect(mockMinioClient.statObject).toHaveBeenCalledWith(
        "custom-bots",
        "test/file.zip",
      );
    });

    it("should return false if file does not exist", async () => {
      const error = new Error("The specified key does not exist.") as any;
      error.code = "NoSuchKey";
      mockMinioClient.statObject.mockRejectedValue(error);

      const result = await service.fileExists("test/nonexistent.zip");

      expect(result.success).toBe(true);
      expect(result.data).toBe(false);
    });

    it("should handle other errors", async () => {
      mockMinioClient.statObject.mockRejectedValue(new Error("Network error"));

      const result = await service.fileExists("test/file.zip");

      expect(result.success).toBe(false);
      expect(result.error).toContain("Network error");
    });
  });

  describe("validateZipStructure", () => {
    it("should validate ZIP structure successfully", async () => {
      const zipBuffer = createSampleZipBuffer();
      const mockStream = {
        [Symbol.asyncIterator]: async function* () {
          yield zipBuffer;
        },
      };
      mockMinioClient.getObject.mockResolvedValue(mockStream as any);

      const result = await service.validateZipStructure("test/bot.zip", [
        "main.js",
      ]);

      expect(result.success).toBe(true);
      expect(mockMinioClient.getObject).toHaveBeenCalledWith(
        "custom-bots",
        "test/bot.zip",
      );
    });

    it("should fail if required files are missing", async () => {
      const zipBuffer = createSampleZipBuffer();
      const mockStream = {
        [Symbol.asyncIterator]: async function* () {
          yield zipBuffer;
        },
      };
      mockMinioClient.getObject.mockResolvedValue(mockStream as any);

      const result = await service.validateZipStructure("test/bot.zip", [
        "main.js",
        "missing-file.js",
      ]);

      expect(result.success).toBe(false);
      expect(result.error).toContain("Required file missing");
    });

    it("should handle corrupted ZIP files", async () => {
      const corruptBuffer = Buffer.from("not a zip file");
      mockMinioClient.getObject.mockResolvedValue(
        createMockStream(corruptBuffer) as any,
      );

      const result = await service.validateZipStructure("test/corrupt.zip", [
        "main.js",
      ]);

      expect(result.success).toBe(false);
      expect(result.error).toContain("Error validating ZIP structure");
    });
  });

  describe("uploadBotFile", () => {
    it("should upload bot file successfully", async () => {
      const fileBuffer = createSampleZipBuffer();
      mockMinioClient.putObject.mockResolvedValue({
        etag: "test-etag",
        versionId: "test-version",
      } as any);

      const result = await service.uploadBotFile(
        fileBuffer,
        "1.0.0",
        "user123",
        "test-bot",
      );

      expect(result.success).toBe(true);
      expect(result.data).toBe("user123/test-bot/1.0.0");
      expect(mockMinioClient.putObject).toHaveBeenCalledWith(
        "custom-bots",
        "user123/test-bot/1.0.0",
        fileBuffer,
        fileBuffer.length,
        {
          "Content-Type": "application/zip",
          "X-Upload-Source": "custom-bot-api",
          "X-Upload-Time": expect.any(String),
        },
      );
    });

    it("should handle upload errors", async () => {
      const fileBuffer = createSampleZipBuffer();
      mockMinioClient.putObject.mockRejectedValue(new Error("Upload failed"));

      const result = await service.uploadBotFile(
        fileBuffer,
        "1.0.0",
        "user123",
        "test-bot",
      );

      expect(result.success).toBe(false);
      expect(result.error).toContain("Upload failed");
    });
  });

  describe("downloadFile", () => {
    it("should download file successfully", async () => {
      const fileBuffer = createSampleZipBuffer();
      const mockStream = {
        [Symbol.asyncIterator]: async function* () {
          yield fileBuffer;
        },
      };
      mockMinioClient.getObject.mockResolvedValue(mockStream as any);

      const result = await service.downloadFile("user123/test-bot/1.0.0");

      expect(result.success).toBe(true);
      expect(Buffer.isBuffer(result.data)).toBe(true);
      expect(mockMinioClient.getObject).toHaveBeenCalledWith(
        "custom-bots",
        "user123/test-bot/1.0.0",
      );
    });

    it("should handle download errors", async () => {
      mockMinioClient.getObject.mockRejectedValue(new Error("File not found"));

      const result = await service.downloadFile("user123/nonexistent.zip");

      expect(result.success).toBe(false);
      expect(result.error).toContain("File not found");
    });
  });

  describe("deleteFile", () => {
    it("should delete file successfully", async () => {
      mockMinioClient.removeObject.mockResolvedValue(undefined);

      const result = await service.deleteFile("user123/test-bot/1.0.0");

      expect(result.success).toBe(true);
      expect(mockMinioClient.removeObject).toHaveBeenCalledWith(
        "custom-bots",
        "user123/test-bot/1.0.0",
      );
    });

    it("should handle deletion errors", async () => {
      mockMinioClient.removeObject.mockRejectedValue(
        new Error("Deletion failed"),
      );

      const result = await service.deleteFile("user123/test-bot/1.0.0");

      expect(result.success).toBe(false);
      expect(result.error).toContain("Deletion failed");
    });
  });

  describe("getFileInfo", () => {
    it("should get file info successfully", async () => {
      const mockStat = {
        size: 1024,
        lastModified: new Date("2025-01-01"),
        etag: "test-etag",
      };
      mockMinioClient.statObject.mockResolvedValue(mockStat as any);

      const result = await service.getFileInfo("user123/test-bot/1.0.0");

      expect(result.success).toBe(true);
      expect(result.data).toEqual({
        size: 1024,
        lastModified: new Date("2025-01-01"),
        etag: "test-etag",
      });
    });

    it("should handle file info errors", async () => {
      mockMinioClient.statObject.mockRejectedValue(new Error("File not found"));

      const result = await service.getFileInfo("user123/nonexistent.zip");

      expect(result.success).toBe(false);
      expect(result.error).toContain("File not found");
    });
  });
});
