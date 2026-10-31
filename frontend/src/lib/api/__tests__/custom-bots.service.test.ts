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
});
