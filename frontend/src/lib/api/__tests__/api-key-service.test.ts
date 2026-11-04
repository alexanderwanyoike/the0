import { ApiKeyService, ApiKey } from "../api-key-service";
import { setAuthFetchService } from "@/lib/auth-fetch";

const mockApiKey: ApiKey = {
  id: "key-1",
  userId: "user-1",
  name: "Test API Key",
  key: "the0_test_key_123",
  isActive: true,
  createdAt: "2024-01-01T00:00:00.000Z",
  updatedAt: "2024-01-02T00:00:00.000Z",
};

let fetchMock: jest.SpyInstance;

beforeEach(() => {
  setAuthFetchService({ getToken: () => "jwt", logout: jest.fn() }, jest.fn());
  fetchMock = jest.spyOn(globalThis, "fetch");
});

afterEach(() => jest.restoreAllMocks());

function respond(data: unknown, status = 200) {
  fetchMock.mockResolvedValueOnce(
    new Response(JSON.stringify(data), {
      status,
      headers: { "content-type": "application/json" },
    }),
  );
}

function sent(): { url: string; method?: string; body?: unknown } {
  const [url, init] = fetchMock.mock.calls[0];
  return { url, method: init.method, body: init.body };
}

describe("ApiKeyService", () => {
  describe("getApiKeys", () => {
    it("returns the user's keys", async () => {
      respond([mockApiKey]);

      const result = await ApiKeyService.getApiKeys();

      expect(result).toEqual({ success: true, data: [mockApiKey] });
      expect(sent()).toMatchObject({ url: "/api/api-keys", method: "GET" });
    });

    it("returns the api's message and status on error", async () => {
      respond({ message: "Server error", statusCode: 500 }, 500);

      const result = await ApiKeyService.getApiKeys();

      expect(result).toEqual({
        success: false,
        error: { message: "Server error", statusCode: 500 },
      });
    });

    it("fails when no response arrives", async () => {
      fetchMock.mockRejectedValueOnce(new TypeError("Failed to fetch"));

      const result = await ApiKeyService.getApiKeys();

      expect(result.success).toBe(false);
    });
  });

  describe("createApiKey", () => {
    it("posts the name and returns the new key", async () => {
      respond({ ...mockApiKey, name: "New Key" }, 201);

      const result = await ApiKeyService.createApiKey({ name: "New Key" });

      expect(result).toEqual({
        success: true,
        data: { ...mockApiKey, name: "New Key" },
      });
      expect(sent()).toEqual({
        url: "/api/api-keys",
        method: "POST",
        body: JSON.stringify({ name: "New Key" }),
      });
    });

    it("returns the api's validation message", async () => {
      respond({ message: ["name should not be empty"], statusCode: 400 }, 400);

      const result = await ApiKeyService.createApiKey({ name: "" });

      expect(result).toEqual({
        success: false,
        error: { message: "name should not be empty", statusCode: 400 },
      });
    });
  });

  describe("getApiKey", () => {
    it("returns one key", async () => {
      respond(mockApiKey);

      const result = await ApiKeyService.getApiKey("key-1");

      expect(result).toEqual({ success: true, data: mockApiKey });
      expect(sent()).toMatchObject({ url: "/api/api-keys/key-1" });
    });

    it("reports a missing key with 404", async () => {
      respond({ message: "API key not found", statusCode: 404 }, 404);

      const result = await ApiKeyService.getApiKey("non-existent");

      expect(result).toEqual({
        success: false,
        error: { message: "API key not found", statusCode: 404 },
      });
    });
  });

  describe("deleteApiKey", () => {
    it("deletes the key", async () => {
      respond({ message: "API key deleted successfully" });

      const result = await ApiKeyService.deleteApiKey("key-1");

      expect(result).toEqual({
        success: true,
        data: { message: "API key deleted successfully" },
      });
      expect(sent()).toMatchObject({
        url: "/api/api-keys/key-1",
        method: "DELETE",
      });
    });

    it("reports a missing key with 404", async () => {
      respond({ message: "API key not found", statusCode: 404 }, 404);

      const result = await ApiKeyService.deleteApiKey("non-existent");

      expect(result.success).toBe(false);
    });
  });

  describe("getApiKeyStats", () => {
    it("returns the counts", async () => {
      respond({ total: 5, active: 3 });

      const result = await ApiKeyService.getApiKeyStats();

      expect(result).toEqual({ success: true, data: { total: 5, active: 3 } });
      expect(sent()).toMatchObject({ url: "/api/api-keys/stats/summary" });
    });

    it("returns the api's message on error", async () => {
      respond({ message: "Server error", statusCode: 500 }, 500);

      const result = await ApiKeyService.getApiKeyStats();

      expect(result.success).toBe(false);
    });
  });
});
