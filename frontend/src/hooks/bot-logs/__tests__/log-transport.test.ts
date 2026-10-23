import { fetchLogsPage } from "../log-transport";
import { authFetch } from "@/lib/auth-fetch";

jest.mock("@/lib/auth-fetch", () => ({
  authFetch: jest.fn(),
}));

const mockAuthFetch = authFetch as jest.MockedFunction<typeof authFetch>;

const jsonResponse = (body: unknown) =>
  ({ ok: true, json: async () => body }) as Response;

describe("fetchLogsPage", () => {
  describe("reading a page", () => {
    it("returns the api's entries, hasMore and nextCursor as sent", async () => {
      const entries = [
        {
          key: "1792713599999000000-0",
          date: "20261022",
          timestamp: "2026-10-22T23:59:59.999Z",
          content:
            '{"_key":"1792713599999000000-0","message":"Bought 1 BTC","timestamp":"2026-10-22T23:59:59.999Z"}',
        },
        {
          key: "20261022:0",
          date: "20261022",
          timestamp: null,
          content: "plain line from before keyed lines",
        },
      ];
      mockAuthFetch.mockResolvedValue(
        jsonResponse({
          success: true,
          data: entries,
          hasMore: true,
          nextCursor: "20261022:0",
          message: "Logs retrieved successfully",
        }),
      );

      const page = await fetchLogsPage(
        "bot-1",
        { date: "20261022" },
        new AbortController().signal,
      );

      expect(page).toEqual({
        entries,
        hasMore: true,
        nextCursor: "20261022:0",
      });
    });
  });
});
