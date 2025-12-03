import { ApiKeyService, ApiKey, ApiKeyStats } from "../api-key-service";
import axios from "@/lib/axios-interceptor";

// Mock axios-interceptor
jest.mock("@/lib/axios-interceptor", () => ({
  get: jest.fn(),
  post: jest.fn(),
  delete: jest.fn(),
}));

const mockAxios = axios as jest.Mocked<typeof axios>;

describe("ApiKeyService", () => {
  const mockApiKey: ApiKey = {
    id: "key-1",
    userId: "user-1",
    name: "Test API Key",
    key: "the0_test_key_123",
    isActive: true,
    createdAt: "2024-01-01T00:00:00.000Z",
    updatedAt: "2024-01-02T00:00:00.000Z",
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe("getApiKeys", () => {
    it("should fetch all API keys successfully", async () => {
      mockAxios.get.mockResolvedValue({
        data: [mockApiKey],
      });

      const result = await ApiKeyService.getApiKeys();

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data).toHaveLength(1);
        expect(result.data[0].name).toBe("Test API Key");
      }
      expect(mockAxios.get).toHaveBeenCalledWith("/api/api-keys", {
        timeout: 10000,
      });
    });

    it("should return empty array when no keys exist", async () => {
      mockAxios.get.mockResolvedValue({
        data: [],
      });

      const result = await ApiKeyService.getApiKeys();

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data).toHaveLength(0);
      }
    });

    it("should handle API error", async () => {
      mockAxios.get.mockRejectedValue({
        response: {
          status: 500,
          data: { message: "Server error" },
        },
      });

      const result = await ApiKeyService.getApiKeys();

      expect(result.success).toBe(false);
    });

    it("should handle network error", async () => {
      mockAxios.get.mockRejectedValue(new Error("Network error"));

      const result = await ApiKeyService.getApiKeys();

      expect(result.success).toBe(false);
    });
  });

  describe("createApiKey", () => {
    it("should create a new API key successfully", async () => {
      const newKey = { ...mockApiKey, id: "new-key-id", name: "New Key" };
      mockAxios.post.mockResolvedValue({
        data: newKey,
      });

      const result = await ApiKeyService.createApiKey({ name: "New Key" });

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data.name).toBe("New Key");
        expect(result.data.key).toBeDefined();
      }
      expect(mockAxios.post).toHaveBeenCalledWith(
        "/api/api-keys",
        { name: "New Key" },
        { timeout: 10000 },
      );
    });

    it("should handle creation error", async () => {
      mockAxios.post.mockRejectedValue({
        response: {
          status: 400,
          data: { message: "Invalid name" },
        },
      });

      const result = await ApiKeyService.createApiKey({ name: "" });

      expect(result.success).toBe(false);
    });
  });

  describe("getApiKey", () => {
    it("should fetch a specific API key by ID", async () => {
      mockAxios.get.mockResolvedValue({
        data: mockApiKey,
      });

      const result = await ApiKeyService.getApiKey("key-1");

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data.id).toBe("key-1");
      }
      expect(mockAxios.get).toHaveBeenCalledWith("/api/api-keys/key-1", {
        timeout: 10000,
      });
    });

    it("should handle key not found", async () => {
      mockAxios.get.mockRejectedValue({
        response: {
          status: 404,
          data: { message: "Key not found" },
        },
      });

      const result = await ApiKeyService.getApiKey("non-existent");

      expect(result.success).toBe(false);
    });
  });

  describe("deleteApiKey", () => {
    it("should delete an API key successfully", async () => {
      mockAxios.delete.mockResolvedValue({
        data: { message: "API key deleted successfully" },
      });

      const result = await ApiKeyService.deleteApiKey("key-1");

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data.message).toBe("API key deleted successfully");
      }
      expect(mockAxios.delete).toHaveBeenCalledWith("/api/api-keys/key-1", {
        timeout: 10000,
      });
    });

    it("should handle delete error", async () => {
      mockAxios.delete.mockRejectedValue({
        response: {
          status: 404,
          data: { message: "Key not found" },
        },
      });

      const result = await ApiKeyService.deleteApiKey("non-existent");

      expect(result.success).toBe(false);
    });
  });

  describe("getApiKeyStats", () => {
    it("should fetch API key stats successfully", async () => {
      const stats: ApiKeyStats = { total: 5, active: 3 };
      mockAxios.get.mockResolvedValue({
        data: stats,
      });

      const result = await ApiKeyService.getApiKeyStats();

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data.total).toBe(5);
        expect(result.data.active).toBe(3);
      }
      expect(mockAxios.get).toHaveBeenCalledWith(
        "/api/api-keys/stats/summary",
        {
          timeout: 10000,
        },
      );
    });

    it("should handle stats error", async () => {
      mockAxios.get.mockRejectedValue({
        response: {
          status: 500,
          data: { message: "Server error" },
        },
      });

      const result = await ApiKeyService.getApiKeyStats();

      expect(result.success).toBe(false);
    });
  });
});
