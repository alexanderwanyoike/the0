import { CustomBotService } from "../custom-bots.service";
import { ApiClient } from "@/lib/api-client";

// Mock ApiClient
jest.mock("@/lib/api-client", () => ({
  ApiClient: {
    get: jest.fn(),
    post: jest.fn(),
    put: jest.fn(),
    delete: jest.fn(),
  },
}));

const mockApiClient = ApiClient as jest.Mocked<typeof ApiClient>;

describe("CustomBotService", () => {
  const mockBotWithVersions = {
    id: "bot-1",
    name: "test-bot",
    userId: "user-1",
    latestVersion: "1.0.0",
    createdAt: "2024-01-01T00:00:00.000Z",
    updatedAt: "2024-01-02T00:00:00.000Z",
    versions: [
      {
        id: "version-1",
        version: "1.0.0",
        userId: "user-1",
        createdAt: "2024-01-01T00:00:00.000Z",
        status: "active",
        config: { name: "test-bot", description: "A test bot" },
        filePath: "/bots/test-bot/1.0.0",
      },
    ],
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe("getCustomBots", () => {
    it("should fetch custom bots successfully", async () => {
      mockApiClient.get.mockResolvedValue({
        data: {
          success: true,
          data: [mockBotWithVersions],
          message: "Success",
        },
      });

      const result = await CustomBotService.getCustomBots();

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data).toHaveLength(1);
        expect(result.data[0].name).toBe("test-bot");
        expect(result.data[0].versions).toHaveLength(1);
      }
      expect(mockApiClient.get).toHaveBeenCalledWith("/api/custom-bots");
    });

    it("should handle API error response", async () => {
      mockApiClient.get.mockResolvedValue({
        data: {
          success: false,
        },
        error: {
          message: "Failed to fetch",
          statusCode: 500,
        },
      });

      const result = await CustomBotService.getCustomBots();

      expect(result.success).toBe(false);
      if (!result.success) {
        expect(result.error.message).toBe("Failed to fetch");
        expect(result.error.statusCode).toBe(500);
      }
    });

    it("should handle 401 unauthorized error", async () => {
      mockApiClient.get.mockResolvedValue({
        error: {
          message: "Unauthorized",
          statusCode: 401,
        },
      });

      const result = await CustomBotService.getCustomBots();

      expect(result.success).toBe(false);
      if (!result.success) {
        expect(result.error.statusCode).toBe(401);
      }
    });

    it("should handle network errors", async () => {
      mockApiClient.get.mockRejectedValue(new Error("Network error"));

      const result = await CustomBotService.getCustomBots();

      expect(result.success).toBe(false);
      if (!result.success) {
        expect(result.error.message).toBe("Network error");
        expect(result.error.statusCode).toBe(500);
      }
    });

    it("should transform raw bot data to versioned format", async () => {
      // Raw format without versions array
      const rawBot = {
        id: "bot-1",
        name: "test-bot",
        userId: "user-1",
        version: "1.0.0",
        createdAt: "2024-01-01T00:00:00.000Z",
        updatedAt: "2024-01-02T00:00:00.000Z",
        status: "active",
        config: { name: "test-bot" },
        filePath: "/bots/test-bot/1.0.0",
      };

      mockApiClient.get.mockResolvedValue({
        data: {
          success: true,
          data: [rawBot],
          message: "Success",
        },
      });

      const result = await CustomBotService.getCustomBots();

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data[0].versions).toBeDefined();
        expect(result.data[0].latestVersion).toBe("1.0.0");
      }
    });
  });

  describe("getCustomBot", () => {
    it("should fetch a specific bot by name", async () => {
      mockApiClient.get.mockResolvedValue({
        data: {
          success: true,
          data: [mockBotWithVersions],
          message: "Success",
        },
      });

      const result = await CustomBotService.getCustomBot("test-bot");

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data.name).toBe("test-bot");
      }
    });

    it("should return 404 error for non-existent bot", async () => {
      mockApiClient.get.mockResolvedValue({
        data: {
          success: true,
          data: [mockBotWithVersions],
          message: "Success",
        },
      });

      const result = await CustomBotService.getCustomBot("non-existent-bot");

      expect(result.success).toBe(false);
      if (!result.success) {
        expect(result.error.statusCode).toBe(404);
        expect(result.error.message).toBe("Bot not found");
      }
    });

    it("should propagate API errors", async () => {
      mockApiClient.get.mockResolvedValue({
        error: {
          message: "Server error",
          statusCode: 500,
        },
      });

      const result = await CustomBotService.getCustomBot("test-bot");

      expect(result.success).toBe(false);
      if (!result.success) {
        expect(result.error.statusCode).toBe(500);
      }
    });
  });

  describe("getCustomBotVersion", () => {
    it("should fetch specific version of a bot", async () => {
      const versionData = {
        id: "version-1",
        name: "test-bot",
        version: "1.0.0",
        createdAt: "2024-01-01T00:00:00.000Z",
        updatedAt: "2024-01-02T00:00:00.000Z",
        status: "active",
      };

      mockApiClient.get.mockResolvedValue({
        data: versionData,
      });

      const result = await CustomBotService.getCustomBotVersion(
        "test-bot",
        "1.0.0",
      );

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data.version).toBe("1.0.0");
      }
      expect(mockApiClient.get).toHaveBeenCalledWith(
        "/api/custom-bots/test-bot/1.0.0",
      );
    });

    it("should handle version not found", async () => {
      mockApiClient.get.mockResolvedValue({
        error: {
          message: "Version not found",
          statusCode: 404,
        },
      });

      const result = await CustomBotService.getCustomBotVersion(
        "test-bot",
        "9.9.9",
      );

      expect(result.success).toBe(false);
      if (!result.success) {
        expect(result.error.statusCode).toBe(404);
      }
    });

    it("should URL encode bot name and version", async () => {
      mockApiClient.get.mockResolvedValue({
        data: { version: "1.0.0" },
      });

      await CustomBotService.getCustomBotVersion("bot with spaces", "1.0.0");

      expect(mockApiClient.get).toHaveBeenCalledWith(
        "/api/custom-bots/bot%20with%20spaces/1.0.0",
      );
    });
  });

  describe("getCustomBotVersions", () => {
    it("should fetch all versions for a bot", async () => {
      mockApiClient.get.mockResolvedValue({
        data: ["1.0.0", "0.9.0", "0.8.0"],
      });

      const result = await CustomBotService.getCustomBotVersions("test-bot");

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data).toHaveLength(3);
        expect(result.data).toContain("1.0.0");
      }
      expect(mockApiClient.get).toHaveBeenCalledWith(
        "/api/custom-bots/test-bot/versions",
      );
    });

    it("should handle error fetching versions", async () => {
      mockApiClient.get.mockResolvedValue({
        error: {
          message: "Failed to fetch versions",
          statusCode: 500,
        },
      });

      const result = await CustomBotService.getCustomBotVersions("test-bot");

      expect(result.success).toBe(false);
      if (!result.success) {
        expect(result.error.message).toBe("Failed to fetch versions");
      }
    });
  });
});
