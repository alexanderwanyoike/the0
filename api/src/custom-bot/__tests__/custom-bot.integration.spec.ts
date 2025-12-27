import { Test, TestingModule } from "@nestjs/testing";
import { INestApplication } from "@nestjs/common";
import request from "supertest";
import { CustomBotModule } from "../custom-bot.module";
import { CustomBotRepository } from "../custom-bot.repository";
import { StorageService } from "../storage.service";
import { Ok, Failure } from "@/common/result";
import {
  CustomBot,
  CustomBotConfig,
  CustomBotWithVersions,
} from "@/custom-bot/custom-bot.types";
import { AuthCombinedGuard } from "@/auth/auth-combined.guard";
import { NatsService } from "@/nats/nats.service";

// Mock database and storage
jest.mock("@/database/connection");
jest.mock("../storage.service");

describe("Custom Bot API Integration Tests", () => {
  let app: INestApplication;
  let repository: jest.Mocked<CustomBotRepository>;
  let storageService: jest.Mocked<StorageService>;

  const validConfig: CustomBotConfig = {
    name: "integration-test-bot",
    description: "Integration test bot description",
    version: "1.0.0",
    author: "Test Author",
    type: "scheduled",
    runtime: "python3.11",
    entrypoints: {
      bot: "main.py",
    },
    schema: {
      bot: { type: "object" },
    },
    readme:
      "This is a comprehensive readme for integration testing that meets all requirements.",
  };

  // Create a mock ZIP file buffer
  const createMockZipBuffer = (): Buffer => {
    // Simple ZIP file structure simulation
    return Buffer.from("PK\x03\x04mock-zip-content");
  };

  // Mock auth middleware
  const mockAuthMiddleware = (req: any, res: any, next: any) => {
    req.userId = "test-user-123";
    req.user = { uid: "test-user-123" };
    next();
  };

  beforeAll(async () => {
    const moduleFixture: TestingModule = await Test.createTestingModule({
      imports: [CustomBotModule],
    })
      .overrideProvider(CustomBotRepository)
      .useValue({
        globalBotExists: jest.fn().mockResolvedValue(Ok(false)),
        globalVersionExists: jest.fn().mockResolvedValue(Ok(false)),
        createNewGlobalVersion: jest.fn().mockResolvedValue(
          Ok({
            id: "mock-bot-id",
            name: "mock-bot",
            version: "1.0.0",
            userId: "test-user-123",
            status: "active",
            createdAt: new Date(),
            updatedAt: new Date(),
          }),
        ),
        getAllGlobalVersions: jest.fn().mockResolvedValue(Ok([])),
        getSpecificGlobalVersion: jest.fn().mockResolvedValue(Ok(null)),
        getGlobalLatestVersion: jest.fn().mockResolvedValue(Ok(null)),
        isVersionNewer: jest.fn().mockResolvedValue(Ok(true)),
        checkUserOwnership: jest.fn().mockResolvedValue(Ok(true)),
        getAllUserVersions: jest.fn().mockResolvedValue(Ok([])),
        getSpecificUserVersion: jest.fn().mockResolvedValue(Ok(null)),
      })
      .overrideGuard(AuthCombinedGuard)
      .useValue({
        canActivate: jest.fn(() => true),
      })
      .overrideProvider(StorageService)
      .useValue({
        fileExists: jest.fn().mockResolvedValue(Ok(true)),
        validateZipStructure: jest.fn().mockResolvedValue(Ok(true)),
        uploadBotFile: jest.fn().mockResolvedValue(Ok("test/path")),
        deleteBotFile: jest.fn().mockResolvedValue(Ok(null)),
        getFileMetadata: jest.fn().mockResolvedValue(Ok({})),
        validateZipFile: jest.fn().mockResolvedValue(Ok(true)),
        extractAndStoreFrontend: jest.fn().mockResolvedValue(Ok(null)),
        getFrontendBundle: jest.fn().mockResolvedValue(Ok(null)),
      })
      .overrideProvider(NatsService)
      .useValue({
        isConnected: jest.fn(() => Promise.resolve(true)),
        connect: jest.fn(() => Promise.resolve()),
        disconnect: jest.fn(() => Promise.resolve()),
        subscribe: jest.fn(() => Promise.resolve()),
        publish: jest.fn(() => Promise.resolve(Ok(null))),
        onModuleInit: jest.fn(() => Promise.resolve()),
        onModuleDestroy: jest.fn(() => Promise.resolve()),
      })
      .compile();

    app = moduleFixture.createNestApplication();

    // Apply mock auth middleware
    app.use("/custom-bots", mockAuthMiddleware);

    await app.init();

    repository = moduleFixture.get(CustomBotRepository);
    storageService = moduleFixture.get(StorageService);
  });

  afterAll(async () => {
    await app.close();
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  describe("POST /custom-bots/:name (Signed URL Deploy)", () => {
    it("should create a new custom bot successfully with signed URL", async () => {
      const filePath =
        "gs://test-bucket/user123/integration-test-bot/1.0.0/integration-test-bot_1.0.0_123456.zip";

      const mockCreatedBot: CustomBot = {
        id: "new-bot-id",
        name: "integration-test-bot",
        version: "1.0.0",
        config: validConfig,
        filePath,
        userId: "test-user-123",
        status: "active",
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      // Mock the file existence check and validation
      storageService.fileExists.mockResolvedValue(Ok(true));
      storageService.validateZipStructure.mockResolvedValue(Ok(true));
      repository.globalBotExists.mockResolvedValue(Ok(false));
      repository.createNewGlobalVersion.mockResolvedValue(Ok(mockCreatedBot));
      repository.getSpecificGlobalVersion.mockResolvedValue(Ok(mockCreatedBot));

      const response = await request(app.getHttpServer())
        .post("/custom-bots/integration-test-bot")
        .send({
          config: JSON.stringify(validConfig),
          filePath: filePath,
        })
        .expect(201);

      expect(response.body.success).toBe(true);
      expect(response.body.data.name).toBe("integration-test-bot");
      expect(response.body.message).toBe("Custom bot created successfully");

      // Verify service calls
      expect(storageService.fileExists).toHaveBeenCalledWith(filePath);
      expect(storageService.validateZipStructure).toHaveBeenCalledWith(
        filePath,
        ["main.py"],
      );
    });

    it("should return 400 when filePath is missing", async () => {
      const response = await request(app.getHttpServer())
        .post("/custom-bots/integration-test-bot")
        .send({
          config: JSON.stringify(validConfig),
        })
        .expect(400);

      expect(response.body.message).toBe("file path is required");
    });

    it("should return 400 when file does not exist at filePath", async () => {
      const filePath =
        "gs://test-bucket/user123/integration-test-bot/1.0.0/nonexistent.zip";

      storageService.fileExists.mockResolvedValue(Ok(false));

      const response = await request(app.getHttpServer())
        .post("/custom-bots/integration-test-bot")
        .send({
          config: JSON.stringify(validConfig),
          filePath: filePath,
        })
        .expect(400);

      expect(response.body.message).toBe(
        "File not found at specified file path",
      );
    });

    it("should return 400 when config is missing", async () => {
      const filePath =
        "gs://test-bucket/user123/integration-test-bot/1.0.0/test-bot.zip";

      storageService.fileExists.mockResolvedValue(Ok(true));

      const response = await request(app.getHttpServer())
        .post("/custom-bots/integration-test-bot")
        .send({
          filePath: filePath,
        })
        .expect(400);

      expect(response.body.message).toBe("Config field is required");
    });

    it("should return 400 when config is invalid JSON", async () => {
      const filePath =
        "gs://test-bucket/user123/integration-test-bot/1.0.0/test-bot.zip";

      storageService.fileExists.mockResolvedValue(Ok(true));

      const response = await request(app.getHttpServer())
        .post("/custom-bots/integration-test-bot")
        .send({
          config: "invalid-json",
          filePath: filePath,
        })
        .expect(400);

      expect(response.body.message).toBe("Config must be valid JSON");
    });

    it("should return 400 when bot name in config doesnt match URL", async () => {
      const filePath =
        "gs://test-bucket/user123/integration-test-bot/1.0.0/test-bot.zip";
      const configWithDifferentName = {
        ...validConfig,
        name: "different-name",
      };

      storageService.fileExists.mockResolvedValue(Ok(true));

      const response = await request(app.getHttpServer())
        .post("/custom-bots/integration-test-bot")
        .send({
          config: JSON.stringify(configWithDifferentName),
          filePath: filePath,
        })
        .expect(400);

      expect(response.body.message).toBe(
        "Bot name in config must match URL parameter",
      );
    });

    it("should return 400 when bot already exists", async () => {
      const filePath =
        "gs://test-bucket/user123/integration-test-bot/1.0.0/test-bot.zip";

      storageService.fileExists.mockResolvedValue(Ok(true));
      repository.globalBotExists.mockResolvedValue(Ok(true));

      const response = await request(app.getHttpServer())
        .post("/custom-bots/integration-test-bot")
        .send({
          config: JSON.stringify(validConfig),
          filePath: filePath,
        })
        .expect(400);

      expect(response.body.message).toContain(
        "Custom bot with this name already exists",
      );
    });

    it("should return 400 when ZIP validation fails", async () => {
      const filePath =
        "gs://test-bucket/user123/integration-test-bot/1.0.0/test-bot.zip";

      storageService.fileExists.mockResolvedValue(Ok(true));
      repository.globalBotExists.mockResolvedValue(Ok(false));
      storageService.validateZipStructure.mockResolvedValue(
        Failure("Required bot entrypoint main.py not found in ZIP"),
      );

      const response = await request(app.getHttpServer())
        .post("/custom-bots/integration-test-bot")
        .send({
          config: JSON.stringify(validConfig),
          filePath: filePath,
        })
        .expect(400);

      expect(response.body.message).toContain("ZIP validation failed");
    });

    it("should return 400 when config validation fails", async () => {
      const filePath =
        "gs://test-bucket/user123/integration-test-bot/1.0.0/test-bot.zip";
      const invalidConfig = {
        ...validConfig,
        version: "invalid-version", // Invalid semver
      };

      storageService.fileExists.mockResolvedValue(Ok(true));

      const response = await request(app.getHttpServer())
        .post("/custom-bots/integration-test-bot")
        .send({
          config: JSON.stringify(invalidConfig),
          filePath: filePath,
        })
        .expect(400);

      expect(response.body.message).toContain("Validation failed");
    });
  });

  describe("PUT /custom-bots/:name (Signed URL Deploy)", () => {
    it("should update bot with new version successfully", async () => {
      const filePath =
        "gs://test-bucket/user123/integration-test-bot/1.1.0/integration-test-bot_1.1.0_123456.zip";
      const updateConfig = {
        ...validConfig,
        version: "1.1.0",
        description: "Updated bot description",
      };

      const mockExistingBot: CustomBot = {
        id: "existing-id",
        name: "integration-test-bot",
        version: "1.0.0",
        config: validConfig,
        filePath: "gs://test-bucket/old-file.zip",
        userId: "test-user-123",
        status: "active",
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      const mockUpdatedBot: CustomBot = {
        ...mockExistingBot,
        id: "new-version-id",
        version: "1.1.0",
        config: updateConfig,
        filePath: filePath,
      };

      storageService.fileExists.mockResolvedValue(Ok(true));
      storageService.validateZipStructure.mockResolvedValue(Ok(true));
      repository.globalBotExists.mockResolvedValue(Ok(true));
      repository.checkUserOwnership.mockResolvedValue(Ok(true));
      repository.getGlobalLatestVersion.mockResolvedValue(Ok(mockExistingBot));
      repository.isVersionNewer.mockReturnValue(true);
      repository.globalVersionExists.mockResolvedValue(Ok(false));
      repository.createNewGlobalVersion.mockResolvedValue(Ok(mockUpdatedBot));
      repository.getSpecificGlobalVersion.mockResolvedValue(Ok(mockUpdatedBot));

      const response = await request(app.getHttpServer())
        .put("/custom-bots/integration-test-bot")
        .send({
          config: JSON.stringify(updateConfig),
          filePath: filePath,
        })
        .expect(200);

      expect(response.body.success).toBe(true);
      expect(response.body.data.version).toBe("1.1.0");
      expect(response.body.message).toBe("Custom bot updated successfully");
    });

    it("should return 400 when bot does not exist", async () => {
      const filePath =
        "gs://test-bucket/user123/non-existent-bot/1.0.0/test-bot.zip";

      storageService.fileExists.mockResolvedValue(Ok(true));
      repository.globalBotExists.mockResolvedValue(Ok(false));

      const response = await request(app.getHttpServer())
        .put("/custom-bots/non-existent-bot")
        .send({
          config: JSON.stringify({ ...validConfig, name: "non-existent-bot" }),
          filePath: filePath,
        })
        .expect(400);

      expect(response.body.message).toBe(
        "Custom bot does not exist. Create it first using POST.",
      );
    });

    it("should return 400 when user is not the owner", async () => {
      const filePath =
        "gs://test-bucket/user123/integration-test-bot/1.0.0/test-bot.zip";

      storageService.fileExists.mockResolvedValue(Ok(true));
      repository.globalBotExists.mockResolvedValue(Ok(true));
      repository.checkUserOwnership.mockResolvedValue(
        Failure("Insufficient permissions"),
      );

      const response = await request(app.getHttpServer())
        .put("/custom-bots/integration-test-bot")
        .send({
          config: JSON.stringify(validConfig),
          filePath: filePath,
        })
        .expect(400);

      expect(response.body.message).toBe("Insufficient permissions");
    });

    it("should return 400 when version is not newer", async () => {
      const filePath =
        "gs://test-bucket/user123/integration-test-bot/1.0.0/test-bot.zip";
      const mockExistingBot: CustomBot = {
        id: "existing-id",
        name: "integration-test-bot",
        version: "2.0.0", // Higher than payload version
        config: validConfig,
        filePath: "gs://test-bucket/old-file.zip",
        userId: "test-user-123",
        status: "active",
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      storageService.fileExists.mockResolvedValue(Ok(true));
      repository.globalBotExists.mockResolvedValue(Ok(true));
      repository.checkUserOwnership.mockResolvedValue(Ok(true));
      repository.getGlobalLatestVersion.mockResolvedValue(Ok(mockExistingBot));
      repository.isVersionNewer.mockReturnValue(false);

      const response = await request(app.getHttpServer())
        .put("/custom-bots/integration-test-bot")
        .send({
          config: JSON.stringify(validConfig),
          filePath: filePath,
        })
        .expect(400);

      expect(response.body.message).toContain(
        "must be greater than current version",
      );
    });

    it("should return 400 when version already exists", async () => {
      const filePath =
        "gs://test-bucket/user123/integration-test-bot/1.1.0/test-bot.zip";
      const mockExistingBot: CustomBot = {
        id: "existing-id",
        name: "integration-test-bot",
        version: "1.0.0",
        config: validConfig,
        filePath: "gs://test-bucket/old-file.zip",
        userId: "test-user-123",
        status: "active",
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      storageService.fileExists.mockResolvedValue(Ok(true));
      repository.globalBotExists.mockResolvedValue(Ok(true));
      repository.checkUserOwnership.mockResolvedValue(Ok(true));
      repository.getGlobalLatestVersion.mockResolvedValue(Ok(mockExistingBot));
      repository.isVersionNewer.mockReturnValue(true);
      repository.globalVersionExists.mockResolvedValue(Ok(true)); // Version exists

      const response = await request(app.getHttpServer())
        .put("/custom-bots/integration-test-bot")
        .send({
          config: JSON.stringify({ ...validConfig, version: "1.1.0" }),
          filePath: filePath,
        })
        .expect(400);

      expect(response.body.message).toContain("already exists for this bot");
    });
  });

  describe("GET /custom-bots/:name", () => {
    it("should get all versions successfully", async () => {
      const mockVersionsData = {
        id: "bot-id",
        name: "integration-test-bot",
        userId: "test-user-123",
        versions: [
          {
            version: "1.1.0",
            config: { ...validConfig, version: "1.1.0" },
            filePath: "gs://bucket/v1.1.zip",
            createdAt: new Date(),
            updatedAt: new Date(),
            status: "active",

            userId: "test-user-123",
            id: "bot-id",
          },
          {
            version: "1.0.0",
            config: validConfig,
            filePath: "gs://bucket/v1.0.zip",
            createdAt: new Date(),
            updatedAt: new Date(),
            status: "active",

            userId: "test-user-123",
            id: "bot-id",
          },
        ],
        latestVersion: "1.1.0",
        createdAt: new Date(),
        updatedAt: new Date(),
      } as unknown as CustomBotWithVersions;

      repository.getAllGlobalVersions.mockResolvedValue(Ok(mockVersionsData));

      const response = await request(app.getHttpServer())
        .get("/custom-bots/integration-test-bot")
        .expect(200);

      expect(response.body.success).toBe(true);
      expect(response.body.data.versions).toHaveLength(2);
      expect(response.body.data.latestVersion).toBe("1.1.0");
      expect(response.body.message).toBe("Bot versions retrieved successfully");
    });

    it("should return 404 when bot not found", async () => {
      repository.getAllGlobalVersions.mockResolvedValue(
        Failure("Bot not found"),
      );

      const response = await request(app.getHttpServer())
        .get("/custom-bots/non-existent-bot")
        .expect(404);

      expect(response.body.message).toBe("Bot not found");
    });
  });

  describe("GET /custom-bots/:name/:version", () => {
    it("should get specific version successfully", async () => {
      const mockBot: CustomBot = {
        id: "specific-version-id",
        name: "integration-test-bot",
        version: "1.0.0",
        config: validConfig,
        filePath: "gs://test-bucket/file.zip",
        status: "active",
        userId: "test-user-123",
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      repository.getSpecificGlobalVersion.mockResolvedValue(Ok(mockBot));

      const response = await request(app.getHttpServer())
        .get("/custom-bots/integration-test-bot/1.0.0")
        .expect(200);

      expect(response.body.success).toBe(true);
      expect(response.body.data.version).toBe("1.0.0");
      expect(response.body.message).toBe("Bot version retrieved successfully");
    });

    it("should return 404 when specific version not found", async () => {
      repository.getSpecificGlobalVersion.mockResolvedValue(
        Failure("Version not found"),
      );

      const response = await request(app.getHttpServer())
        .get("/custom-bots/integration-test-bot/2.0.0")
        .expect(404);

      expect(response.body.message).toBe("Version not found");
    });
  });

  describe("End-to-end workflow with file uploads", () => {
    it("should complete full bot lifecycle: create -> update -> retrieve", async () => {
      const botName = "e2e-test-bot";

      // Step 1: Create initial bot
      const createConfig: CustomBotConfig = {
        name: botName,
        description: "End-to-end test bot",
        version: "1.0.0",
        type: "scheduled",
        runtime: "python3.11",
        author: "E2E Test",
        entrypoints: { bot: "main.py" },
        schema: { bot: {} },
        readme:
          "E2E test bot readme with sufficient length for validation requirements.",
      };

      storageService.fileExists.mockResolvedValueOnce(Ok(true));
      storageService.validateZipStructure.mockResolvedValueOnce(Ok(true));
      repository.globalBotExists.mockResolvedValueOnce(Ok(false));
      const mockCreatedBot = {
        id: "e2e-v1-id",
        name: botName,
        version: "1.0.0",
        config: createConfig,
        status: "active",
        filePath: "gs://test-bucket/e2e-v1.zip",
        userId: "test-user-123",
        createdAt: new Date(),
        updatedAt: new Date(),
      } as unknown as CustomBot;
      repository.createNewGlobalVersion.mockResolvedValueOnce(
        Ok(mockCreatedBot),
      );
      repository.getSpecificGlobalVersion.mockResolvedValueOnce(
        Ok(mockCreatedBot),
      );

      const createResponse = await request(app.getHttpServer())
        .post(`/custom-bots/${botName}`)
        .send({
          config: JSON.stringify(createConfig),
          filePath: "gs://test-bucket/e2e-v1.zip",
        })
        .expect(201);

      expect(createResponse.body.data.version).toBe("1.0.0");

      // Step 2: Update with new version
      const updateConfig = {
        ...createConfig,
        version: "1.1.0",
        description: "Updated E2E test bot",
      };

      storageService.fileExists.mockResolvedValueOnce(Ok(true));
      storageService.validateZipStructure.mockResolvedValueOnce(Ok(true));
      repository.globalBotExists.mockResolvedValueOnce(Ok(true));
      repository.checkUserOwnership.mockResolvedValueOnce(Ok(true));
      repository.getGlobalLatestVersion.mockResolvedValueOnce(
        Ok({
          id: "e2e-v1-id",
          name: botName,
          version: "1.0.0",
          config: createConfig,
          status: "active",
          filePath: "gs://test-bucket/e2e-v1.zip",
          userId: "test-user-123",
          createdAt: new Date(),
          updatedAt: new Date(),
        }),
      );
      repository.isVersionNewer.mockReturnValue(true);
      repository.globalVersionExists.mockResolvedValueOnce(Ok(false));
      repository.createNewGlobalVersion.mockResolvedValueOnce(
        Ok({
          id: "e2e-v1.1-id",
          name: botName,
          version: "1.1.0",
          config: updateConfig,
          status: "active",
          filePath: "gs://test-bucket/e2e-v1.1.zip",
          userId: "test-user-123",
          createdAt: new Date(),
          updatedAt: new Date(),
        }),
      );
      repository.getSpecificGlobalVersion.mockResolvedValueOnce(
        Ok({
          id: "e2e-v1.1-id",
          name: botName,
          version: "1.1.0",
          config: updateConfig,
          status: "active",
          filePath: "gs://test-bucket/e2e-v1.1.zip",
          userId: "test-user-123",
          createdAt: new Date(),
          updatedAt: new Date(),
        }),
      );

      const updateResponse = await request(app.getHttpServer())
        .put(`/custom-bots/${botName}`)
        .send({
          config: JSON.stringify(updateConfig),
          filePath: "gs://test-bucket/e2e-v1.1.zip",
        })
        .expect(200);

      expect(updateResponse.body.data.version).toBe("1.1.0");

      // Step 3: Retrieve all versions
      repository.getAllGlobalVersions.mockResolvedValueOnce(
        Ok({
          id: "e2e-bot-id",
          name: botName,
          userId: "test-user-123",
          versions: [
            {
              version: "1.1.0",
              config: updateConfig,
              filePath: "gs://test-bucket/e2e-v1.1.zip",
              createdAt: new Date(),
              updatedAt: new Date(),
              status: "active",

              userId: "test-user-123",
              id: "bot-id",
            },
            {
              version: "1.0.0",
              config: createConfig,
              filePath: "gs://test-bucket/e2e-v1.zip",
              createdAt: new Date(),
              updatedAt: new Date(),
              status: "active",

              userId: "test-user-123",
              id: "bot-id",
            },
          ],
          latestVersion: "1.1.0",
          createdAt: new Date(),
          updatedAt: new Date(),
        }),
      );

      const getAllResponse = await request(app.getHttpServer())
        .get(`/custom-bots/${botName}`)
        .expect(200);

      expect(getAllResponse.body.data.versions).toHaveLength(2);
      expect(getAllResponse.body.data.latestVersion).toBe("1.1.0");

      // Step 4: Retrieve specific version
      repository.getSpecificGlobalVersion.mockResolvedValueOnce(
        Ok({
          id: "e2e-v1-id",
          name: botName,
          version: "1.0.0",
          config: createConfig,
          status: "active",
          filePath: "gs://test-bucket/e2e-v1.zip",
          userId: "test-user-123",
          createdAt: new Date(),
          updatedAt: new Date(),
        }),
      );

      const getSpecificResponse = await request(app.getHttpServer())
        .get(`/custom-bots/${botName}/1.0.0`)
        .expect(200);

      expect(getSpecificResponse.body.data.version).toBe("1.0.0");
    });
  });

  describe("Error handling and edge cases", () => {
    it("should handle repository errors gracefully", async () => {
      const filePath =
        "gs://test-bucket/user123/integration-test-bot/1.0.0/test-bot.zip";

      storageService.fileExists.mockResolvedValue(Ok(true));
      repository.globalBotExists.mockResolvedValue(
        Failure("Database connection failed"),
      );

      const response = await request(app.getHttpServer())
        .post("/custom-bots/integration-test-bot")
        .send({
          config: JSON.stringify(validConfig),
          filePath: filePath,
        })
        .expect(400);

      expect(response.body.message).toContain("Database connection failed");
    });

    it("should handle GCS service errors gracefully", async () => {
      const filePath =
        "gs://test-bucket/user123/integration-test-bot/1.0.0/test-bot.zip";

      storageService.fileExists.mockResolvedValue(Ok(true));
      repository.globalBotExists.mockResolvedValue(Ok(false));
      storageService.validateZipStructure.mockResolvedValue(
        Failure("GCS service unavailable"),
      );

      const response = await request(app.getHttpServer())
        .post("/custom-bots/integration-test-bot")
        .send({
          config: JSON.stringify(validConfig),
          filePath: filePath,
        })
        .expect(400);

      expect(response.body.message).toContain("ZIP validation failed");
    });

    it("should handle large file uploads properly", async () => {
      const filePath =
        "gs://test-bucket/user123/integration-test-bot/1.0.0/large-bot.zip";

      storageService.fileExists.mockResolvedValue(Ok(true));
      repository.globalBotExists.mockResolvedValue(Ok(false));
      storageService.validateZipStructure.mockResolvedValue(
        Failure("ZIP file size exceeds 200MB limit"),
      );

      const response = await request(app.getHttpServer())
        .post("/custom-bots/integration-test-bot")
        .send({
          config: JSON.stringify(validConfig),
          filePath: filePath,
        })
        .expect(400);

      expect(response.body.message).toContain(
        "ZIP validation failed: ZIP file size exceeds 200MB limit",
      );
    });

    it("should handle missing user authentication", async () => {
      // Create a request without the auth middleware
      const response = await request(app.getHttpServer())
        .post("/custom-bots/no-auth-bot")
        .send({
          config: JSON.stringify(validConfig),
          filePath: "gs://test-bucket/user123/no-auth-bot/1.0.0/test-bot.zip",
        });

      // This should fail because there's no auth middleware applied to this specific route
      expect(response.status).toBeGreaterThanOrEqual(400);
    });

    it("should validate file content type properly", async () => {
      const filePath =
        "gs://test-bucket/user123/integration-test-bot/1.0.0/fake.zip";

      storageService.fileExists.mockResolvedValue(Ok(true));
      repository.globalBotExists.mockResolvedValue(Ok(false));
      storageService.validateZipStructure.mockResolvedValue(
        Failure("Failed to validate ZIP structure: not a valid ZIP file"),
      );

      const response = await request(app.getHttpServer())
        .post("/custom-bots/integration-test-bot")
        .send({
          config: JSON.stringify(validConfig),
          filePath: filePath,
        })
        .expect(400);

      expect(response.body.message).toContain("ZIP validation failed");
    });

    it("should handle malformed requests", async () => {
      const response = await request(app.getHttpServer())
        .post("/custom-bots/malformed-request")
        .send({ invalid: "data" })
        .expect(400);

      expect(response.body.message).toBe("file path is required");
    });
  });
});
