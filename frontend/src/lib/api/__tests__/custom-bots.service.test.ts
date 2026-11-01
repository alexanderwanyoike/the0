import { CustomBotService } from "../custom-bots.service";
import { setAuthFetchService } from "@/lib/auth-fetch";

let fetchMock: jest.SpyInstance;

function respond(data: unknown, status = 200) {
  fetchMock.mockResolvedValue(
    new Response(JSON.stringify(data), {
      status,
      headers: { "content-type": "application/json" },
    }),
  );
}

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
    setAuthFetchService(
      { getToken: () => "jwt", logout: jest.fn() },
      jest.fn(),
    );
    fetchMock = jest.spyOn(globalThis, "fetch");
  });

  afterEach(() => jest.restoreAllMocks());

  describe("getCustomBots", () => {
    it("should fetch custom bots successfully", async () => {
      respond({
        success: true,
        data: [mockBotWithVersions],
        message: "Success",
      });

      const result = await CustomBotService.getCustomBots();

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data).toHaveLength(1);
        expect(result.data[0].name).toBe("test-bot");
        expect(result.data[0].versions).toHaveLength(1);
      }
      expect(fetchMock.mock.calls[0][0]).toBe("/api/custom-bots");
    });

    it("should handle API error response", async () => {
      respond({ success: false, message: "Failed to fetch" }, 500);

      const result = await CustomBotService.getCustomBots();

      expect(result.success).toBe(false);
      if (!result.success) {
        expect(result.error.message).toBe("Failed to fetch");
        expect(result.error.statusCode).toBe(500);
      }
    });

    it("should handle 401 unauthorized error", async () => {
      respond({ message: "Unauthorized", statusCode: 401 }, 401);

      const result = await CustomBotService.getCustomBots();

      expect(result.success).toBe(false);
      if (!result.success) {
        expect(result.error.statusCode).toBe(401);
      }
    });

    it("should handle network errors", async () => {
      fetchMock.mockRejectedValue(new TypeError("Failed to fetch"));

      const result = await CustomBotService.getCustomBots();

      expect(result.success).toBe(false);
      if (!result.success) {
        expect(result.error.message).toBe("No response received from server");
        expect(result.error.statusCode).toBe(500);
      }
    });

    it.each([
      ["no data", null],
      ["data that is not a list", {}],
    ])("should fail rather than throw on a body with %s", async (_, data) => {
      respond({ success: true, data, message: "Success" });

      const result = await CustomBotService.getCustomBots();

      expect(result.success).toBe(false);
      if (!result.success) {
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

      respond({ success: true, data: [rawBot], message: "Success" });

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
      respond({
        success: true,
        data: [mockBotWithVersions],
        message: "Success",
      });

      const result = await CustomBotService.getCustomBot("test-bot");

      expect(result.success).toBe(true);
      if (result.success) {
        expect(result.data.name).toBe("test-bot");
      }
    });

    it("should return 404 error for non-existent bot", async () => {
      respond({
        success: true,
        data: [mockBotWithVersions],
        message: "Success",
      });

      const result = await CustomBotService.getCustomBot("non-existent-bot");

      expect(result.success).toBe(false);
      if (!result.success) {
        expect(result.error.statusCode).toBe(404);
        expect(result.error.message).toBe("Bot not found");
      }
    });

    it("should propagate API errors", async () => {
      respond({ message: "Server error", statusCode: 500 }, 500);

      const result = await CustomBotService.getCustomBot("test-bot");

      expect(result.success).toBe(false);
      if (!result.success) {
        expect(result.error.statusCode).toBe(500);
      }
    });
  });
});
