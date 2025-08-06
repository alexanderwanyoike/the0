import { ApiClient, setApiClientService } from "../api-client";

describe("ApiClient", () => {
  const mockAuthService = {
    getToken: jest.fn(),
    logout: jest.fn(),
  };
  const mockLogout = jest.fn();
  const originalFetch = global.fetch;

  beforeEach(() => {
    jest.clearAllMocks();
    setApiClientService(mockAuthService, mockLogout);
  });

  afterEach(() => {
    global.fetch = originalFetch;
  });

  describe("GET requests", () => {
    it("should make authenticated GET request", async () => {
      mockAuthService.getToken.mockReturnValue("valid-token");
      global.fetch = jest.fn().mockResolvedValue({
        ok: true,
        json: async () => ({ success: true, data: { id: 1, name: "test" } }),
        clone: () => ({ ok: true }),
      });

      const response = await ApiClient.get("/api/test");

      expect(response.data).toEqual({
        success: true,
        data: { id: 1, name: "test" },
      });
      expect(response.error).toBeUndefined();
    });

    it("should return auth error when no token available", async () => {
      mockAuthService.getToken.mockReturnValue(null);

      const response = await ApiClient.get("/api/test");

      expect(response.error).toEqual({
        message: "Not authenticated",
        code: "NO_AUTH",
        statusCode: 401,
      });
      expect(response.data).toBeUndefined();
    });

    it("should return auth error when auth service not set", async () => {
      setApiClientService(null as any, null as any);

      const response = await ApiClient.get("/api/test");

      expect(response.error).toEqual({
        message: "Not authenticated",
        code: "NO_AUTH",
        statusCode: 401,
      });
    });
  });

  describe("POST requests", () => {
    it("should make authenticated POST request with data", async () => {
      mockAuthService.getToken.mockReturnValue("valid-token");
      global.fetch = jest.fn().mockResolvedValue({
        ok: true,
        json: async () => ({ success: true, data: { id: 1, created: true } }),
        clone: () => ({ ok: true }),
      });

      const requestData = { name: "test", value: 123 };
      const response = await ApiClient.post("/api/test", requestData);

      expect(response.data).toEqual({
        success: true,
        data: { id: 1, created: true },
      });
    });

    it("should make authenticated POST request without data", async () => {
      mockAuthService.getToken.mockReturnValue("valid-token");
      global.fetch = jest.fn().mockResolvedValue({
        ok: true,
        json: async () => ({ success: true }),
        clone: () => ({ ok: true }),
      });

      const response = await ApiClient.post("/api/test");

      expect(response.data).toBeDefined();
    });
  });

  describe("PUT requests", () => {
    it("should make authenticated PUT request", async () => {
      mockAuthService.getToken.mockReturnValue("valid-token");
      global.fetch = jest.fn().mockResolvedValue({
        ok: true,
        json: async () => ({ success: true, data: { id: 1, updated: true } }),
        clone: () => ({ ok: true }),
      });

      const updateData = { name: "updated", value: 456 };
      const response = await ApiClient.put("/api/test/1", updateData);

      expect(response.data).toEqual({
        success: true,
        data: { id: 1, updated: true },
      });
    });
  });

  describe("DELETE requests", () => {
    it("should make authenticated DELETE request", async () => {
      mockAuthService.getToken.mockReturnValue("valid-token");
      global.fetch = jest.fn().mockResolvedValue({
        ok: true,
        json: async () => ({ success: true }),
        clone: () => ({ ok: true }),
      });

      const response = await ApiClient.delete("/api/test/1");

      expect(response.data).toBeDefined();
    });
  });

  describe("error handling", () => {
    it("should handle 401 responses with centralized logout", async () => {
      mockAuthService.getToken.mockReturnValue("valid-token");
      global.fetch = jest.fn().mockResolvedValue({
        ok: false,
        status: 401,
        json: async () => ({ message: "Unauthorized", code: "AUTH_FAILED" }),
        clone: () => ({ ok: false, status: 401 }),
      });

      const response = await ApiClient.get("/api/test");

      expect(mockLogout).toHaveBeenCalled();
      expect(response.error).toEqual({
        message: "Unauthorized",
        code: "AUTH_FAILED",
        statusCode: 401,
      });
    });

    it("should fallback to auth service logout when centralized logout not available", async () => {
      setApiClientService(mockAuthService, null as any);
      mockAuthService.getToken.mockReturnValue("valid-token");
      global.fetch = jest.fn().mockResolvedValue({
        ok: false,
        status: 401,
        json: async () => ({ message: "Unauthorized" }),
        clone: () => ({ ok: false, status: 401 }),
      });

      const response = await ApiClient.get("/api/test");

      expect(mockAuthService.logout).toHaveBeenCalled();
      expect(response.error?.statusCode).toBe(401);
    });

    it("should handle non-401 HTTP errors", async () => {
      mockAuthService.getToken.mockReturnValue("valid-token");
      global.fetch = jest.fn().mockResolvedValue({
        ok: false,
        status: 500,
        json: async () => ({ message: "Server Error", code: "SERVER_ERROR" }),
        clone: () => ({ ok: false, status: 500 }),
      });

      const response = await ApiClient.get("/api/test");

      expect(mockLogout).not.toHaveBeenCalled();
      expect(response.error).toEqual({
        message: "Server Error",
        code: "SERVER_ERROR",
        statusCode: 500,
      });
    });

    it("should handle network errors", async () => {
      mockAuthService.getToken.mockReturnValue("valid-token");
      global.fetch = jest.fn().mockRejectedValue(new Error("Network error"));

      const response = await ApiClient.get("/api/test");

      expect(response.error).toEqual({
        message: "Network error",
        code: "NETWORK_ERROR",
        statusCode: 500,
      });
    });

    it("should handle JSON parsing errors", async () => {
      mockAuthService.getToken.mockReturnValue("valid-token");
      global.fetch = jest.fn().mockResolvedValue({
        ok: true,
        json: async () => {
          throw new Error("Invalid JSON");
        },
        clone: () => ({ ok: true }),
      });

      const response = await ApiClient.get("/api/test");

      expect(response.error).toEqual({
        message: "Invalid JSON",
        code: "NETWORK_ERROR",
        statusCode: 500,
      });
    });

    it("should handle responses with error field but no message", async () => {
      mockAuthService.getToken.mockReturnValue("valid-token");
      global.fetch = jest.fn().mockResolvedValue({
        ok: false,
        status: 400,
        json: async () => ({ error: "Bad request" }),
        clone: () => ({ ok: false, status: 400 }),
      });

      const response = await ApiClient.get("/api/test");

      expect(response.error).toEqual({
        message: "Bad request",
        code: undefined,
        statusCode: 400,
      });
    });

    it("should use default error message when no message provided", async () => {
      mockAuthService.getToken.mockReturnValue("valid-token");
      global.fetch = jest.fn().mockResolvedValue({
        ok: false,
        status: 400,
        json: async () => ({}),
        clone: () => ({ ok: false, status: 400 }),
      });

      const response = await ApiClient.get("/api/test");

      expect(response.error).toEqual({
        message: "Request failed",
        code: undefined,
        statusCode: 400,
      });
    });
  });

  describe("setApiClientService", () => {
    it("should register new auth service and logout function", async () => {
      const newAuthService = {
        getToken: jest.fn().mockReturnValue("new-token"),
        logout: jest.fn(),
      };
      const newLogout = jest.fn();

      setApiClientService(newAuthService, newLogout);
      global.fetch = jest.fn().mockResolvedValue({
        ok: true,
        json: async () => ({ data: "success" }),
        clone: () => ({ ok: true }),
      });

      await ApiClient.get("/api/test");

      expect(newAuthService.getToken).toHaveBeenCalled();
    });
  });
});
