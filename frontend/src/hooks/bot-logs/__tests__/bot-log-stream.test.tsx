import React from "react";
import { renderHook, waitFor, act } from "@testing-library/react";
import { BotLogStreamProvider } from "../bot-log-stream";
import { useBotLogs } from "@/hooks/use-bot-logs";
import { useAuth } from "@/contexts/auth-context";
import { useToast } from "@/hooks/use-toast";
import { authFetch } from "@/lib/auth-fetch";
import { validateSSEAuth } from "@/lib/sse/sse-auth";
import type { LogEntry, LogsQuery } from "@/types/logs";

jest.mock("@/contexts/auth-context", () => ({
  useAuth: jest.fn(),
}));

jest.mock("@/hooks/use-toast", () => ({
  useToast: jest.fn(),
}));

jest.mock("@/lib/auth-fetch", () => ({
  authFetch: jest.fn(),
}));

jest.mock("@/lib/sse/sse-auth", () => ({
  validateSSEAuth: jest.fn(),
}));

const mockUseAuth = useAuth as jest.MockedFunction<typeof useAuth>;
const mockUseToast = useToast as jest.MockedFunction<typeof useToast>;
const mockAuthFetch = authFetch as jest.MockedFunction<typeof authFetch>;
const mockValidateSSEAuth = validateSSEAuth as jest.MockedFunction<
  typeof validateSSEAuth
>;

const encoder = new TextEncoder();

interface MockStream {
  url: string;
  signal: AbortSignal;
  push: (entries: LogEntry[]) => void;
}

const entry = (content: string): LogEntry => ({
  key: content,
  date: "20261022",
  timestamp: "2026-10-22T10:00:00.000Z",
  content,
});

const METRIC = entry('{"_metric":"price","value":1}');
const PLAIN = entry('{"message":"bought"}');

let streams: MockStream[];
let streamOk: boolean;

function mockApi() {
  mockAuthFetch.mockImplementation(async (input, opts) => {
    const url = String(input);
    if (url.includes("/stream")) {
      if (!streamOk) return { ok: false, status: 503 } as Response;
      let controller!: ReadableStreamDefaultController<Uint8Array>;
      const body = new ReadableStream<Uint8Array>({
        start(c) {
          controller = c;
          opts?.signal?.addEventListener("abort", () => {
            try {
              c.error(new TypeError("Failed to fetch"));
            } catch {
              // already closed
            }
          });
        },
      });
      streams.push({
        url,
        signal: opts?.signal,
        push: (entries) =>
          controller.enqueue(
            encoder.encode(
              `event: update\ndata: ${JSON.stringify(entries)}\n\n`,
            ),
          ),
      });
      return { ok: true, body } as Response;
    }
    return {
      ok: true,
      json: async () => ({
        success: true,
        data: [],
        hasMore: false,
        nextCursor: null,
      }),
    } as Response;
  });
}

const restUrls = () =>
  mockAuthFetch.mock.calls
    .map((call) => call[0] as string)
    .filter((url) => !url.includes("/stream"));

interface PageViews {
  consoleQuery?: LogsQuery;
  dashboardQuery?: LogsQuery;
}

/** The console and the dashboard of one bot page, each with its own query. */
function renderBotPage({
  consoleQuery = { limit: 100, sort: "asc" },
  dashboardQuery = { limit: 100, sort: "asc", type: "metrics" },
}: PageViews = {}) {
  return renderHook(
    () => ({
      console: useBotLogs({
        botId: "bot-1",
        streaming: true,
        refreshInterval: 50,
        initialQuery: consoleQuery,
      }),
      dashboard: useBotLogs({
        botId: "bot-1",
        streaming: true,
        refreshInterval: 50,
        initialQuery: dashboardQuery,
      }),
    }),
    {
      wrapper: ({ children }: { children: React.ReactNode }) => (
        <BotLogStreamProvider botId="bot-1">{children}</BotLogStreamProvider>
      ),
    },
  );
}

describe("BotLogStreamProvider", () => {
  beforeEach(() => {
    jest.clearAllMocks();
    streams = [];
    streamOk = true;
    mockUseAuth.mockReturnValue({ user: { id: "user-1" } } as never);
    mockUseToast.mockReturnValue({
      toast: jest.fn(),
      toasts: [],
      dismiss: jest.fn(),
    });
    mockValidateSSEAuth.mockReturnValue({
      success: true,
      token: "test-token",
    } as never);
    mockApi();
  });

  describe("one stream per bot page", () => {
    it("opens one stream, without history, for the console and the dashboard", async () => {
      const { result, unmount } = renderBotPage();

      await waitFor(() => {
        expect(result.current.console.connected).toBe(true);
        expect(result.current.dashboard.connected).toBe(true);
      });

      expect(streams).toHaveLength(1);
      expect(streams[0].url).toBe("/api/logs/bot-1/stream?history=false");
      unmount();
    });

    it("hands each view the live lines it shows", async () => {
      const { result, unmount } = renderBotPage();
      await waitFor(() => {
        expect(result.current.console.connected).toBe(true);
        expect(result.current.console.loading).toBe(false);
        expect(result.current.dashboard.loading).toBe(false);
      });

      act(() => {
        streams[0].push([PLAIN, METRIC]);
      });

      await waitFor(() => {
        expect(result.current.console.logs).toEqual([PLAIN, METRIC]);
        expect(result.current.dashboard.logs).toEqual([METRIC]);
      });
      unmount();
    });

    it("closes the stream when the bot page goes away", async () => {
      const { result, unmount } = renderBotPage();
      await waitFor(() => expect(result.current.console.connected).toBe(true));

      unmount();

      expect(streams[0].signal.aborted).toBe(true);
    });
  });

  describe("only while a view is live", () => {
    it("opens no stream while every view is pinned to a date", async () => {
      const { result, unmount } = renderBotPage({
        consoleQuery: { limit: 100, date: "20261021" },
        dashboardQuery: { limit: 100, date: "20261021", type: "metrics" },
      });
      await waitFor(() => {
        expect(result.current.console.loading).toBe(false);
        expect(result.current.dashboard.loading).toBe(false);
      });

      expect(streams).toHaveLength(0);

      act(() => {
        result.current.console.setDateFilter(null);
      });

      await waitFor(() => expect(result.current.console.connected).toBe(true));
      expect(streams).toHaveLength(1);
      expect(result.current.dashboard.connected).toBe(false);
      unmount();
    });

    it("keeps the stream open while another view is still live", async () => {
      const { result, unmount } = renderBotPage();
      await waitFor(() => expect(result.current.console.connected).toBe(true));

      act(() => {
        result.current.console.setDateFilter("20261021");
      });
      await waitFor(() => expect(result.current.console.connected).toBe(false));

      expect(streams).toHaveLength(1);
      expect(streams[0].signal.aborted).toBe(false);
      expect(result.current.dashboard.connected).toBe(true);
      unmount();
    });
  });

  describe("refresh and failures", () => {
    it("keeps a healthy stream when a view refreshes", async () => {
      const { result, unmount } = renderBotPage();
      await waitFor(() => expect(result.current.console.connected).toBe(true));

      act(() => {
        result.current.console.refresh();
      });
      await waitFor(() =>
        expect(result.current.console.isFetching).toBe(false),
      );

      expect(streams).toHaveLength(1);
      expect(streams[0].signal.aborted).toBe(false);
      unmount();
    });

    it("polls every view while the stream is down, and reconnects on refresh", async () => {
      streamOk = false;
      const { result, unmount } = renderBotPage();

      await waitFor(() => {
        const urls = restUrls();
        const metrics = urls.filter((u) => u.includes("type=metrics"));
        expect(metrics.length).toBeGreaterThanOrEqual(3);
        expect(urls.length - metrics.length).toBeGreaterThanOrEqual(3);
      });
      expect(result.current.console.connected).toBe(false);
      expect(result.current.dashboard.connected).toBe(false);

      streamOk = true;
      act(() => {
        result.current.console.refresh();
      });

      await waitFor(() => {
        expect(result.current.console.connected).toBe(true);
        expect(result.current.dashboard.connected).toBe(true);
      });
      expect(streams).toHaveLength(1);
      unmount();
    });
  });
});
