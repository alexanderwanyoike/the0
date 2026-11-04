import { Bot, BotService } from "../api-client";
import { setAuthFetchService } from "@/lib/auth-fetch";

const bot: Bot = {
  id: "bot-1",
  config: { name: "dca", enabled: true },
  createdAt: "2026-10-01T00:00:00.000Z",
  updatedAt: "2026-10-02T00:00:00.000Z",
};

let fetchMock: jest.SpyInstance;

beforeEach(() => {
  setAuthFetchService({ getToken: () => "jwt", logout: jest.fn() }, jest.fn());
  fetchMock = jest.spyOn(globalThis, "fetch");
});

afterEach(() => jest.restoreAllMocks());

function respond(data: unknown, status = 200) {
  fetchMock.mockResolvedValueOnce(
    new Response(data === undefined ? null : JSON.stringify(data), {
      status,
      headers: { "content-type": "application/json" },
    }),
  );
}

function sent(): { url: string; method?: string; body?: unknown } {
  const [url, init] = fetchMock.mock.calls[0];
  return { url, method: init.method, body: init.body };
}

describe("BotService", () => {
  describe("reading bots", () => {
    it("lists the user's bots", async () => {
      respond([bot]);

      const result = await BotService.getBots();

      expect(result).toEqual({ success: true, data: [bot] });
      expect(sent()).toMatchObject({ url: "/api/bot", method: "GET" });
    });

    it("gets one bot", async () => {
      respond(bot);

      const result = await BotService.getBot("bot-1");

      expect(result).toEqual({ success: true, data: bot });
      expect(sent()).toMatchObject({ url: "/api/bot/bot-1", method: "GET" });
    });

    it("returns the api's message and status when a bot is missing", async () => {
      respond({ message: "Bot not found", statusCode: 404 }, 404);

      const result = await BotService.getBot("missing");

      expect(result).toEqual({
        success: false,
        error: { message: "Bot not found", statusCode: 404 },
      });
    });
  });

  describe("changing bots", () => {
    it("puts the config with its name", async () => {
      respond(bot);

      await BotService.updateBot("bot-1", { name: "dca", enabled: false });

      expect(sent()).toEqual({
        url: "/api/bot/bot-1",
        method: "PUT",
        body: JSON.stringify({
          name: "dca",
          config: { name: "dca", enabled: false },
        }),
      });
    });

    it("returns the api's validation message when an update is rejected", async () => {
      respond({ message: ["config.symbol is required"], statusCode: 400 }, 400);

      const result = await BotService.updateBot("bot-1", { name: "dca" });

      expect(result).toEqual({
        success: false,
        error: { message: "config.symbol is required", statusCode: 400 },
      });
    });

    it("deletes a bot", async () => {
      respond({ success: true });

      const result = await BotService.deleteBot("bot-1");

      expect(result).toEqual({ success: true, data: null });
      expect(sent()).toMatchObject({
        url: "/api/bot/bot-1",
        method: "DELETE",
      });
    });
  });
});
