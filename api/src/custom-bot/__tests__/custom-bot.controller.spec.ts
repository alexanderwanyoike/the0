import { BadRequestException, NotFoundException } from "@nestjs/common";
import { CustomBotController } from "../custom-bot.controller";
import {
  CustomBot,
  CustomBotConfig,
  CustomBotWithVersions,
} from "../custom-bot.types";
import { Ok, Failure } from "@/common/result";
import { StorageService } from "../storage.service";
import { AuthenticatedUser } from "@/auth/auth.types";

describe("CustomBotController", () => {
  let controller: CustomBotController;
  let mockService: any;
  let mockStorageService: jest.Mocked<StorageService>;
  // Removed duplicate declaration

  const mockUser = { uid: "user123" } as AuthenticatedUser;

  const validConfig: CustomBotConfig = {
    name: "test-bot",
    description: "Test bot description",
    version: "1.0.0",
    type: "scheduled",
    runtime: "python3.11",
    author: "Test Author",
    entrypoints: {
      bot: "main.py",
    },
    schema: {
      bot: { type: "object" },
    },
    readme:
      "This is a test bot with enough content to pass validation requirements.",
  };

  beforeEach(() => {
    // Create a completely mocked service without NestJS testing module
    mockService = {
      createCustomBot: jest.fn(),
      updateCustomBot: jest.fn(),
      getUserCustomBots: jest.fn(),
      getAllGlobalVersions: jest.fn(),
      getGlobalSpecificVersion: jest.fn(),
      getPublishedBots: jest.fn(),
      getPublishedBotsByCategory: jest.fn(),
      getUserPublishedBots: jest.fn(),
      getVersionsWithInstanceCounts: jest.fn(),
      deleteVersion: jest.fn(),
      deleteAllVersions: jest.fn(),
    };

    mockStorageService = {
      fileExists: jest.fn(),
      validateZipFile: jest.fn(),
      validateZipStructure: jest.fn(),
    } as any;

    // Removed GcsService - using StorageService

    // Directly instantiate the controller with the mock services
    controller = new CustomBotController(mockService, mockStorageService);
  });

  afterEach(() => {
    jest.clearAllMocks();
  });

  describe("createCustomBot", () => {
    it("should create custom bot successfully", async () => {
      const mockBot = {
        id: "new-bot-id",
        name: "test-bot",
        version: "1.0.0",
        config: validConfig,
        filePath:
          "user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip",
        userId: "user123",
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      const filePath =
        "user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip";
      const body = { config: JSON.stringify(validConfig), filePath };

      // Mock GCS file validation
      mockStorageService.fileExists.mockResolvedValue({
        success: true,
        data: true,
        error: null,
      });
      mockService.createCustomBot.mockResolvedValue(Ok(mockBot));

      const result = await controller.createCustomBot(
        "test-bot",
        body,
        mockUser,
      );

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockBot);
      expect(result.message).toBe("Custom bot created successfully");
      expect(mockService.createCustomBot).toHaveBeenCalledWith(
        "user123",
        validConfig,
        filePath,
      );
    });

    it("should throw BadRequestException when filePath is missing", async () => {
      const body = { config: JSON.stringify(validConfig) } as any;

      await expect(
        controller.createCustomBot("test-bot", body, mockUser),
      ).rejects.toThrow(BadRequestException);
    });

    it("should throw BadRequestException when file does not exist at filePath", async () => {
      const filePath =
        "user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip";
      const body = { config: JSON.stringify(validConfig), filePath };

      // Mock GCS file validation to return file not found
      mockStorageService.fileExists.mockResolvedValue({
        success: true,
        data: false,
        error: null,
      });

      await expect(
        controller.createCustomBot("test-bot", body, mockUser),
      ).rejects.toThrow(BadRequestException);
    });

    it("should throw BadRequestException when GCS validation fails", async () => {
      const filePath =
        "user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip";
      const body = { config: JSON.stringify(validConfig), filePath };

      // Mock GCS file validation to return error
      mockStorageService.fileExists.mockResolvedValue({
        success: false,
        data: null,
        error: "GCS error",
      });

      await expect(
        controller.createCustomBot("test-bot", body, mockUser),
      ).rejects.toThrow(BadRequestException);
    });

    it("should throw BadRequestException when config is missing", async () => {
      const filePath =
        "user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip";
      const body = { config: "", filePath };

      mockStorageService.fileExists.mockResolvedValue({
        success: true,
        data: true,
        error: null,
      });

      await expect(
        controller.createCustomBot("test-bot", body, mockUser),
      ).rejects.toThrow(BadRequestException);
    });

    it("should throw BadRequestException when config is invalid JSON", async () => {
      const filePath =
        "user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip";
      const body = { config: "invalid json", filePath };

      mockStorageService.fileExists.mockResolvedValue({
        success: true,
        data: true,
        error: null,
      });

      await expect(
        controller.createCustomBot("test-bot", body, mockUser),
      ).rejects.toThrow(BadRequestException);
    });

    it("should throw BadRequestException when bot name mismatch", async () => {
      const configWithDifferentName = {
        ...validConfig,
        name: "different-bot",
      };
      const filePath =
        "user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip";
      const body = {
        config: JSON.stringify(configWithDifferentName),
        filePath,
      };

      mockStorageService.fileExists.mockResolvedValue({
        success: true,
        data: true,
        error: null,
      });

      await expect(
        controller.createCustomBot("test-bot", body, mockUser),
      ).rejects.toThrow(BadRequestException);
    });

    it("should throw BadRequestException when service returns error", async () => {
      const filePath =
        "user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip";
      const body = { config: JSON.stringify(validConfig), filePath };

      mockStorageService.fileExists.mockResolvedValue({
        success: true,
        data: true,
        error: null,
      });
      mockService.createCustomBot.mockResolvedValue(
        Failure("Bot already exists"),
      );

      await expect(
        controller.createCustomBot("test-bot", body, mockUser),
      ).rejects.toThrow(BadRequestException);
    });

    it("should reject filePath not scoped to the requesting user", async () => {
      const filePath = "other-user/test-bot/1.0.0/test-bot.zip";
      const body = { config: JSON.stringify(validConfig), filePath };

      await expect(
        controller.createCustomBot("test-bot", body, mockUser),
      ).rejects.toThrow(BadRequestException);
    });

    it("should reject filePath with path traversal", async () => {
      const filePath = "user123/../other-user/test-bot/1.0.0/test-bot.zip";
      const body = { config: JSON.stringify(validConfig), filePath };

      await expect(
        controller.createCustomBot("test-bot", body, mockUser),
      ).rejects.toThrow(BadRequestException);
    });
  });

  describe("updateCustomBot", () => {
    it("should update custom bot successfully", async () => {
      const updateConfig = {
        ...validConfig,
        version: "1.1.0",
      };

      const mockUpdatedBot = {
        id: "bot-id",
        name: "test-bot",
        version: "1.1.0",
        config: updateConfig,
        filePath:
          "user123/test-bot/1.1.0/test-bot_1.1.0_123456.zip",
        userId: "user123",
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      const filePath =
        "user123/test-bot/1.1.0/test-bot_1.1.0_123456.zip";
      const body = { config: JSON.stringify(updateConfig), filePath };

      mockStorageService.fileExists.mockResolvedValue({
        success: true,
        data: true,
        error: null,
      });
      mockService.updateCustomBot.mockResolvedValue(Ok(mockUpdatedBot));

      const result = await controller.updateCustomBot(
        "test-bot",
        body,
        mockUser,
      );

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockUpdatedBot);
      expect(result.message).toBe("Custom bot updated successfully");
      expect(mockService.updateCustomBot).toHaveBeenCalledWith(
        "user123",
        "test-bot",
        updateConfig,
        filePath,
      );
    });

    it("should throw BadRequestException when service returns error", async () => {
      const filePath =
        "user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip";
      const body = { config: JSON.stringify(validConfig), filePath };

      mockStorageService.fileExists.mockResolvedValue({
        success: true,
        data: true,
        error: null,
      });
      mockService.updateCustomBot.mockResolvedValue(
        Failure("Version must be newer"),
      );

      await expect(
        controller.updateCustomBot("test-bot", body, mockUser),
      ).rejects.toThrow(BadRequestException);
    });

    it("should throw BadRequestException when filePath is missing", async () => {
      const body = { config: JSON.stringify(validConfig) } as any;

      await expect(
        controller.updateCustomBot("test-bot", body, mockUser),
      ).rejects.toThrow(BadRequestException);
    });

    it("should throw BadRequestException when config name mismatch", async () => {
      const configWithDifferentName = {
        ...validConfig,
        name: "different-bot",
      };
      const filePath =
        "user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip";
      const body = {
        config: JSON.stringify(configWithDifferentName),
        filePath,
      };

      mockStorageService.fileExists.mockResolvedValue({
        success: true,
        data: true,
        error: null,
      });

      await expect(
        controller.updateCustomBot("test-bot", body, mockUser),
      ).rejects.toThrow(BadRequestException);
    });

    it("should reject filePath not scoped to the requesting user", async () => {
      const filePath = "other-user/test-bot/1.0.0/test-bot.zip";
      const body = { config: JSON.stringify(validConfig), filePath };

      await expect(
        controller.updateCustomBot("test-bot", body, mockUser),
      ).rejects.toThrow(BadRequestException);
    });
  });

  describe("getUserCustomBots", () => {
    it("should get user custom bots successfully", async () => {
      const mockUserBots: CustomBotWithVersions[] = [
        {
          id: "bot-id-1",
          name: "my-arbitrage-bot",
          userId: "user123",
          latestVersion: "1.2.0",
          versions: [
            {
              version: "1.2.0",
              config: {
                ...validConfig,
                name: "my-arbitrage-bot",
                version: "1.2.0",
                description: "Advanced arbitrage trading bot",
              },
              status: "active",

              userId: "test-user-123",
              id: "bot-id",
              filePath: "user123/my-arbitrage-bot/1.2.0/file.zip",
              createdAt: new Date("2025-01-15T10:30:00.000Z"),
              updatedAt: new Date("2025-01-15T10:30:00.000Z"),
            },
            {
              version: "1.1.0",
              config: {
                ...validConfig,
                name: "my-arbitrage-bot",
                version: "1.1.0",
                description: "Basic arbitrage trading bot",
              },
              filePath: "user123/my-arbitrage-bot/1.1.0/file.zip",
              createdAt: new Date("2025-01-10T09:15:00.000Z"),
              updatedAt: new Date("2025-01-10T09:15:00.000Z"),
              status: "active",

              userId: "test-user-123",
              id: "bot-id",
            },
          ],
          createdAt: new Date("2025-01-10T09:15:00.000Z"),
          updatedAt: new Date("2025-01-15T10:30:00.000Z"),
        },
        {
          id: "bot-id-2",
          name: "trend-follower",
          userId: "user123",
          latestVersion: "2.0.0",
          versions: [
            {
              version: "2.0.0",
              config: {
                ...validConfig,
                name: "trend-follower",
                version: "2.0.0",
                description: "Trend following strategy",
              },
              filePath: "user123/trend-follower/2.0.0/file.zip",
              createdAt: new Date("2025-01-12T14:20:00.000Z"),
              updatedAt: new Date("2025-01-12T14:20:00.000Z"),
              status: "active",

              userId: "test-user-123",
              id: "bot-id",
            },
          ],
          createdAt: new Date("2025-01-12T14:20:00.000Z"),
          updatedAt: new Date("2025-01-12T14:20:00.000Z"),
        },
      ];

      mockService.getUserCustomBots.mockResolvedValue(Ok(mockUserBots));

      const result = await controller.getUserCustomBots(mockUser);

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockUserBots);
      expect(result.message).toBe("User custom bots retrieved successfully");
      expect(mockService.getUserCustomBots).toHaveBeenCalledWith("user123");
    });

    it("should return empty array when user has no custom bots", async () => {
      mockService.getUserCustomBots.mockResolvedValue(Ok([]));

      const result = await controller.getUserCustomBots(mockUser);

      expect(result.success).toBe(true);
      expect(result.data).toEqual([]);
      expect(result.message).toBe("User custom bots retrieved successfully");
      expect(mockService.getUserCustomBots).toHaveBeenCalledWith("user123");
    });

    it("should throw BadRequestException when service returns error", async () => {
      mockService.getUserCustomBots.mockResolvedValue(
        Failure("Database connection failed"),
      );

      await expect(controller.getUserCustomBots(mockUser)).rejects.toThrow(
        BadRequestException,
      );

      expect(mockService.getUserCustomBots).toHaveBeenCalledWith("user123");
    });

  });

  describe("getAllVersions", () => {
    it("should get all versions successfully", async () => {
      const mockVersions = {
        id: "bot-id",
        name: "test-bot",
        userId: "user123",
        versions: [
          {
            version: "1.1.0",
            config: validConfig,
            filePath: "user123/v1.1.zip",
            createdAt: new Date(),
            updatedAt: new Date(),
          },
          {
            version: "1.0.0",
            config: validConfig,
            filePath: "user123/v1.0.zip",
            createdAt: new Date(),
            updatedAt: new Date(),
          },
        ],
        latestVersion: "1.1.0",
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      mockService.getAllGlobalVersions.mockResolvedValue(Ok(mockVersions));

      const result = await controller.getAllVersions("test-bot");

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockVersions);
      expect(result.message).toBe("Bot versions retrieved successfully");
      expect(mockService.getAllGlobalVersions).toHaveBeenCalledWith("test-bot");
    });

    it("should throw NotFoundException when bot not found", async () => {
      mockService.getAllGlobalVersions.mockResolvedValue(
        Failure("Bot not found"),
      );

      await expect(
        controller.getAllVersions("non-existent-bot"),
      ).rejects.toThrow(NotFoundException);
    });
  });

  describe("getSpecificVersion", () => {
    it("should get specific version successfully", async () => {
      const mockBot = {
        id: "bot-version-id",
        name: "test-bot",
        version: "1.0.0",
        config: validConfig,
        filePath:
          "user123/test-bot/1.0.0/test-bot_1.0.0_123456.zip",
        userId: "user123",
        createdAt: new Date(),
        updatedAt: new Date(),
      };

      mockService.getGlobalSpecificVersion.mockResolvedValue(Ok(mockBot));

      const result = await controller.getSpecificVersion(
        "test-bot",
        "1.0.0",
      );

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockBot);
      expect(result.message).toBe("Bot version retrieved successfully");
      expect(mockService.getGlobalSpecificVersion).toHaveBeenCalledWith(
        "test-bot",
        "1.0.0",
      );
    });

    it("should throw NotFoundException when version not found", async () => {
      mockService.getGlobalSpecificVersion.mockResolvedValue(
        Failure("Version not found"),
      );

      await expect(
        controller.getSpecificVersion("test-bot", "2.0.0"),
      ).rejects.toThrow(NotFoundException);
    });
  });

  describe("getVersionsWithInstances", () => {
    it("should return versions with instance counts", async () => {
      const mockVersions = [
        { version: "2.0.0", instanceCount: 3, id: "cb-1" },
        { version: "1.0.0", instanceCount: 0, id: "cb-2" },
      ];

      mockService.getVersionsWithInstanceCounts.mockResolvedValue(
        Ok(mockVersions),
      );

      const result = await controller.getVersionsWithInstances(
        "test-bot",
        mockUser,
      );

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockVersions);
      expect(mockService.getVersionsWithInstanceCounts).toHaveBeenCalledWith(
        "user123",
        "test-bot",
      );
    });

    it("should throw NotFoundException when bot not found", async () => {
      mockService.getVersionsWithInstanceCounts.mockResolvedValue(
        Failure("Bot not found"),
      );

      await expect(
        controller.getVersionsWithInstances("nonexistent", mockUser),
      ).rejects.toThrow(NotFoundException);
    });
  });

  describe("deleteVersion", () => {
    it("should delete a specific version successfully", async () => {
      mockService.deleteVersion.mockResolvedValue(Ok(null));

      const result = await controller.deleteVersion(
        "test-bot",
        "1.0.0",
        mockUser,
      );

      expect(result.success).toBe(true);
      expect(mockService.deleteVersion).toHaveBeenCalledWith(
        "user123",
        "test-bot",
        "1.0.0",
      );
    });

    it("should throw BadRequestException when version has instances", async () => {
      mockService.deleteVersion.mockResolvedValue(
        Failure("Cannot delete version 1.0.0: 3 active instance(s) reference it"),
      );

      await expect(
        controller.deleteVersion("test-bot", "1.0.0", mockUser),
      ).rejects.toThrow(BadRequestException);
    });
  });

  describe("deleteAllVersions", () => {
    it("should delete all versions successfully", async () => {
      mockService.deleteAllVersions.mockResolvedValue(Ok(null));

      const result = await controller.deleteAllVersions("test-bot", mockUser);

      expect(result.success).toBe(true);
      expect(mockService.deleteAllVersions).toHaveBeenCalledWith(
        "user123",
        "test-bot",
      );
    });

    it("should throw BadRequestException when versions have instances", async () => {
      mockService.deleteAllVersions.mockResolvedValue(
        Failure("Cannot delete: versions with active instance(s)"),
      );

      await expect(
        controller.deleteAllVersions("test-bot", mockUser),
      ).rejects.toThrow(BadRequestException);
    });
  });
});
