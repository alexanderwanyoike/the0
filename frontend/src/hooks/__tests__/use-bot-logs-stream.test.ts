import { renderHook, waitFor, act } from "@testing-library/react";
import { useBotLogsStream } from "../use-bot-logs-stream";
import { useAuth } from "@/contexts/auth-context";
import { useToast } from "@/hooks/use-toast";
import { authFetch } from "@/lib/auth-fetch";
import { validateSSEAuth } from "@/lib/sse/sse-auth";

// Mock dependencies
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

// ---- ReadableStream-based SSE mock ----

interface MockSSEStreamController {
  push: (eventType: string, data: any) => void;
  close: () => void;
}

function createMockSSEStream(): {
  stream: ReadableStream<Uint8Array>;
  controller: MockSSEStreamController;
} {
  const encoder = new TextEncoder();
  let streamController: ReadableStreamDefaultController<Uint8Array>;

  const stream = new ReadableStream<Uint8Array>({
    start(controller) {
      streamController = controller;
    },
  });

  return {
    stream,
    controller: {
      push(eventType: string, data: any) {
        const payload = `event: ${eventType}\ndata: ${JSON.stringify(data)}\n\n`;
        streamController.enqueue(encoder.encode(payload));
      },
      close() {
        try {
          streamController.close();
        } catch {
          // Already closed
        }
      },
    },
  };
}

describe("useBotLogsStream", () => {
  const mockToast = jest.fn();
  let currentMockStream: {
    stream: ReadableStream<Uint8Array>;
    controller: MockSSEStreamController;
  } | null = null;

  function setupDefaultMocks() {
    mockUseAuth.mockReturnValue({
      user: { id: "user-1" },
    } as any);

    mockUseToast.mockReturnValue({
      toast: mockToast,
      toasts: [],
      dismiss: jest.fn(),
    });

    mockValidateSSEAuth.mockReturnValue({
      success: true,
      token: "test-token",
    } as any);

    // Default: SSE stream mock
    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        currentMockStream = createMockSSEStream();
        return {
          ok: true,
          body: currentMockStream.stream,
        } as any;
      }
      // REST fallback
      return {
        ok: true,
        json: async () => ({ data: [], total: 0, hasMore: false }),
      } as any;
    });
  }

  beforeEach(() => {
    jest.clearAllMocks();
    jest.useRealTimers();
    currentMockStream = null;
    setupDefaultMocks();
  });

  afterEach(() => {
    currentMockStream?.controller.close();
  });

  // ---- Interface ----

  it("should return the full interface", () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "" }),
    );

    expect(result.current).toMatchObject({
      logs: [],
      loading: false,
      error: null,
      hasMore: false,
      total: 0,
      connected: false,
      lastUpdate: null,
      hasEarlierLogs: false,
      loadingEarlier: false,
    });
    expect(typeof result.current.refresh).toBe("function");
    expect(typeof result.current.exportLogs).toBe("function");
    expect(typeof result.current.setDateFilter).toBe("function");
    expect(typeof result.current.setDateRangeFilter).toBe("function");
    expect(typeof result.current.loadEarlierLogs).toBe("function");
  });

  // ---- SSE connection ----

  it("should connect via SSE and handle history event", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    act(() => {
      currentMockStream!.controller.push("history", [
        { date: "2024-01-01T10:00:00Z", content: "Log line 1" },
        { date: "2024-01-01T10:01:00Z", content: "Log line 2\nLog line 3" },
      ]);
    });

    await waitFor(() => {
      // Multi-line content should be expanded
      expect(result.current.logs).toHaveLength(3);
      expect(result.current.logs[0].content).toBe("Log line 1");
      expect(result.current.logs[1].content).toBe("Log line 2");
      expect(result.current.logs[2].content).toBe("Log line 3");
      expect(result.current.loading).toBe(false);
    });
  });

  it("should handle update events and append to logs", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    act(() => {
      currentMockStream!.controller.push("history", [
        { date: "2024-01-01T10:00:00Z", content: "Initial" },
      ]);
    });

    await waitFor(() => expect(result.current.logs).toHaveLength(1));

    act(() => {
      currentMockStream!.controller.push("update", {
        content: "New log entry",
        timestamp: "2024-01-01T10:02:00Z",
      });
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(2);
      expect(result.current.logs[1].content).toBe("New log entry");
      expect(result.current.lastUpdate).not.toBeNull();
    });
  });

  // ---- Fallback to REST ----

  it("should fall back to REST when SSE errors before initial load", async () => {
    // SSE immediately fails, should fall back to REST
    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        throw new Error("Connection refused");
      }
      return {
        ok: true,
        json: async () => ({
          data: [{ date: "2024-01-01", content: "REST fallback log" }],
          total: 1,
          hasMore: false,
        }),
      } as any;
    });

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.logs.length).toBeGreaterThan(0);
      expect(result.current.logs[0].content).toBe("REST fallback log");
      expect(result.current.connected).toBe(false);
    });
  });

  it("should fall back to REST when SSE errors", async () => {
    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        throw new Error("SSE connection failed");
      }
      return {
        ok: true,
        json: async () => ({
          data: [{ date: "2024-01-01", content: "Fallback log" }],
          total: 1,
          hasMore: false,
        }),
      } as any;
    });

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.logs.length).toBeGreaterThan(0);
      expect(result.current.connected).toBe(false);
    });
  });

  // ---- Cleanup ----

  it("should clean up on unmount", async () => {
    const { result, unmount } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    unmount();
    // No assertion needed - just verify no errors thrown
  });

  // ---- Bot navigation ----

  it("should reset state when botId changes", async () => {
    const { result, rerender } = renderHook(
      ({ id }) => useBotLogsStream({ botId: id }),
      { initialProps: { id: "bot-a" } },
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    act(() => {
      currentMockStream!.controller.push("history", [
        { date: "2024-01-01T10:00:00Z", content: "Bot A log" },
      ]);
    });

    await waitFor(() => expect(result.current.logs).toHaveLength(1));

    // Switch to bot-b
    rerender({ id: "bot-b" });

    await waitFor(() => {
      expect(result.current.logs).toEqual([]);
      expect(result.current.total).toBe(0);
      expect(result.current.hasEarlierLogs).toBe(false);
    });
  });

  it("should handle rapid botId changes without errors", async () => {
    const { result, rerender } = renderHook(
      ({ id }) => useBotLogsStream({ botId: id }),
      { initialProps: { id: "bot-a" } },
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    // Rapidly switch 5 times
    for (let i = 0; i < 5; i++) {
      rerender({ id: i % 2 === 0 ? "bot-b" : "bot-a" });
    }

    await act(async () => {
      await new Promise((r) => setTimeout(r, 50));
    });

    // Should not throw, error should be null
    expect(result.current.error).toBeNull();
  });

  // ---- No connection without botId/user ----

  it("should not connect when botId is empty", () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "" }),
    );
    expect(result.current.connected).toBe(false);
    expect(result.current.loading).toBe(false);
    expect(mockAuthFetch).not.toHaveBeenCalled();
  });

  it("should not connect when user is null", () => {
    mockUseAuth.mockReturnValue({ user: null } as any);

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );
    expect(result.current.connected).toBe(false);
    expect(mockAuthFetch).not.toHaveBeenCalled();
  });

  // ---- MAX_LOG_ENTRIES cap ----

  it("should cap logs at MAX_LOG_ENTRIES on history", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    // Send 2500 entries (over 2000 cap)
    const entries = Array.from({ length: 2500 }, (_, i) => ({
      date: "2024-01-01T10:00:00Z",
      content: `Entry ${i}`,
    }));

    act(() => {
      currentMockStream!.controller.push("history", entries);
    });

    await waitFor(() => {
      expect(result.current.logs.length).toBeLessThanOrEqual(2000);
      expect(result.current.hasEarlierLogs).toBe(true);
    });
  });

  it("should trim oldest entries when updates exceed cap", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    // Fill to cap with history
    const entries = Array.from({ length: 2000 }, (_, i) => ({
      date: "2024-01-01T10:00:00Z",
      content: `Entry ${i}`,
    }));

    act(() => {
      currentMockStream!.controller.push("history", entries);
    });

    await waitFor(() => expect(result.current.logs).toHaveLength(2000));

    // Push one more update
    act(() => {
      currentMockStream!.controller.push("update", {
        content: "New entry",
        timestamp: "2024-01-01T11:00:00Z",
      });
    });

    await waitFor(() => {
      expect(result.current.logs.length).toBeLessThanOrEqual(2000);
      expect(result.current.logs[result.current.logs.length - 1].content).toBe(
        "New entry",
      );
    });
  });

  // ---- Date filter ----

  it("should switch to REST when date filter is set", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    // Set date filter - should disconnect SSE and fetch via REST
    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        currentMockStream = createMockSSEStream();
        return { ok: true, body: currentMockStream.stream } as any;
      }
      return {
        ok: true,
        json: async () => ({
          data: [{ date: "20240101", content: "Historical log" }],
          total: 1,
          hasMore: false,
        }),
      } as any;
    });

    act(() => {
      result.current.setDateFilter("20240101");
    });

    await waitFor(() => {
      expect(result.current.connected).toBe(false);
      expect(result.current.logs.length).toBeGreaterThan(0);
    });
  });

  it("should reconnect SSE when date filter is cleared", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    // Set then clear date filter
    act(() => {
      result.current.setDateFilter("20240101");
    });

    await waitFor(() => expect(result.current.connected).toBe(false));

    act(() => {
      result.current.setDateFilter(null);
    });

    await waitFor(() => expect(result.current.connected).toBe(true));
  });

  // ---- Error handling ----

  it("should handle REST fetch errors with toast", async () => {
    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        throw new Error("SSE failed");
      }
      throw new Error("REST also failed");
    });

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.error).not.toBeNull();
      expect(mockToast).toHaveBeenCalledWith(
        expect.objectContaining({ variant: "destructive" }),
      );
    });
  });

  it("should handle malformed SSE history data gracefully", async () => {
    const consoleSpy = jest.spyOn(console, "error").mockImplementation();

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    act(() => {
      currentMockStream!.controller.push("history", "not-valid-json{{{");
    });

    // Should not crash, logs should remain empty
    await act(async () => {
      await new Promise((r) => setTimeout(r, 50));
    });

    expect(result.current.logs).toEqual([]);
    consoleSpy.mockRestore();
  });

  // ---- Export ----

  it("should export logs as a downloadable file", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    act(() => {
      currentMockStream!.controller.push("history", [
        { date: "2024-01-01T10:00:00Z", content: "Log to export" },
      ]);
    });

    await waitFor(() => expect(result.current.logs).toHaveLength(1));

    const createElementSpy = jest.spyOn(document, "createElement");
    const createObjectURLSpy = jest
      .spyOn(URL, "createObjectURL")
      .mockReturnValue("blob:test");
    const revokeObjectURLSpy = jest
      .spyOn(URL, "revokeObjectURL")
      .mockImplementation();

    act(() => {
      result.current.exportLogs();
    });

    expect(createElementSpy).toHaveBeenCalledWith("a");
    expect(createObjectURLSpy).toHaveBeenCalled();
    expect(revokeObjectURLSpy).toHaveBeenCalled();

    createElementSpy.mockRestore();
    createObjectURLSpy.mockRestore();
    revokeObjectURLSpy.mockRestore();
  });

  it("should show toast when exporting with no logs", () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "" }),
    );

    act(() => {
      result.current.exportLogs();
    });

    expect(mockToast).toHaveBeenCalledWith(
      expect.objectContaining({ variant: "destructive" }),
    );
  });

  // ---- URL encoding ----

  it("should encode botId in URL", async () => {
    renderHook(() =>
      useBotLogsStream({ botId: "bot/with special&chars" }),
    );

    await waitFor(() => {
      expect(mockAuthFetch).toHaveBeenCalledWith(
        expect.stringContaining("bot%2Fwith%20special%26chars"),
        expect.any(Object),
      );
    });
  });

  it("should preserve timestamp from SSE history events", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    act(() => {
      currentMockStream!.controller.push("history", [
        {
          date: "2026-04-03T10:00:00Z",
          content: "Log line 1",
          timestamp: "2026-04-03T10:00:00Z",
        },
      ]);
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(1);
      expect(result.current.logs[0].timestamp).toBe("2026-04-03T10:00:00Z");
    });
  });

  it("should preserve timestamp from SSE update events", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    act(() => {
      currentMockStream!.controller.push("history", [
        { date: "2026-04-03T10:00:00Z", content: "Initial", timestamp: "2026-04-03T10:00:00Z" },
      ]);
    });

    await waitFor(() => expect(result.current.logs).toHaveLength(1));

    act(() => {
      currentMockStream!.controller.push("update", {
        content: "New entry",
        timestamp: "2026-04-03T10:05:00Z",
      });
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(2);
      expect(result.current.logs[1].timestamp).toBe("2026-04-03T10:05:00Z");
    });
  });

  it("should not start polling when refreshInterval is 0 and SSE fails", async () => {
    jest.useFakeTimers();
    try {
      mockAuthFetch.mockImplementation(async (url: string) => {
        if (typeof url === "string" && url.includes("/stream")) {
          throw new Error("SSE failed");
        }
        return {
          ok: true,
          json: async () => ({
            data: [{ date: "20260406", content: "log" }],
            total: 1,
            hasMore: false,
          }),
        } as any;
      });

      const { result } = renderHook(() =>
        useBotLogsStream({ botId: "bot-1", refreshInterval: 0 }),
      );

      await act(async () => {
        await jest.advanceTimersByTimeAsync(5000);
      });

      const callsAfterFallback = mockAuthFetch.mock.calls.filter(
        (call) => typeof call[0] === "string" && !call[0].includes("/stream"),
      ).length;

      await act(async () => {
        await jest.advanceTimersByTimeAsync(60000);
      });

      const callsAfterWait = mockAuthFetch.mock.calls.filter(
        (call) => typeof call[0] === "string" && !call[0].includes("/stream"),
      ).length;

      expect(callsAfterWait).toBe(callsAfterFallback);
    } finally {
      jest.useRealTimers();
    }
  });

  it("should reconfigure polling interval when refreshInterval prop changes", async () => {
    jest.useFakeTimers();
    try {
      mockAuthFetch.mockImplementation(async (url: string) => {
        if (typeof url === "string" && url.includes("/stream")) {
          throw new Error("SSE failed");
        }
        return {
          ok: true,
          json: async () => ({
            data: [{ date: "20260406", content: "log" }],
            total: 1,
            hasMore: false,
          }),
        } as any;
      });

      const { rerender } = renderHook(
        ({ interval }) =>
          useBotLogsStream({ botId: "bot-1", refreshInterval: interval }),
        { initialProps: { interval: 60000 } },
      );

      await act(async () => {
        await jest.advanceTimersByTimeAsync(5000);
      });

      mockAuthFetch.mockClear();

      rerender({ interval: 5000 });

      await act(async () => {
        await jest.advanceTimersByTimeAsync(5000);
      });

      const restCalls = mockAuthFetch.mock.calls.filter(
        (call) => typeof call[0] === "string" && !call[0].includes("/stream"),
      );
      expect(restCalls.length).toBeGreaterThan(0);
    } finally {
      jest.useRealTimers();
    }
  });

  // ---- Pagination on date range (REST mode) ----

  it("should expose hasMore and loadMore from REST when date range is set", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        currentMockStream = createMockSSEStream();
        return { ok: true, body: currentMockStream.stream } as any;
      }
      const parsedUrl = new URL(url, "http://localhost");
      const offset = parseInt(parsedUrl.searchParams.get("offset") || "0");
      if (offset === 0) {
        return {
          ok: true,
          json: async () => ({
            data: [{ date: "20240101", content: "Page 1 log" }],
            total: 2,
            hasMore: true,
          }),
        } as any;
      }
      return {
        ok: true,
        json: async () => ({
          data: [{ date: "20240101", content: "Page 2 log" }],
          total: 2,
          hasMore: false,
        }),
      } as any;
    });

    act(() => {
      result.current.setDateRangeFilter("20240101", "20240102");
    });

    await waitFor(() => {
      expect(result.current.connected).toBe(false);
      expect(result.current.hasMore).toBe(true);
    });

    expect(typeof result.current.loadMore).toBe("function");

    await act(async () => {
      await result.current.loadMore!();
    });

    await waitFor(() => {
      expect(result.current.hasMore).toBe(false);
      expect(result.current.logs.length).toBe(2);
    });
  });

  it("should not reset hasMore when loading earlier logs via prepend", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    // Switch to REST with hasMore=true
    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        currentMockStream = createMockSSEStream();
        return { ok: true, body: currentMockStream.stream } as any;
      }
      return {
        ok: true,
        json: async () => ({
          data: [{ date: "20240101", content: "Log" }],
          total: 5000,
          hasMore: true,
        }),
      } as any;
    });

    act(() => {
      result.current.setDateRangeFilter("20240101", "20240102");
    });

    await waitFor(() => {
      expect(result.current.hasMore).toBe(true);
    });

    // Now loadEarlierLogs returns hasMore=false
    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        currentMockStream = createMockSSEStream();
        return { ok: true, body: currentMockStream.stream } as any;
      }
      return {
        ok: true,
        json: async () => ({
          data: [{ date: "20240101", content: "Earlier log" }],
          total: 1,
          hasMore: false,
        }),
      } as any;
    });

    await act(async () => {
      result.current.loadEarlierLogs();
    });

    await waitFor(() => {
      expect(result.current.logs.length).toBeGreaterThan(1);
    });

    // hasMore should still be true - the prepend fetch should not overwrite it
    expect(result.current.hasMore).toBe(true);
  });

  it("should not allow duplicate loadMore calls while fetch is in-flight", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    let fetchCount = 0;
    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        currentMockStream = createMockSSEStream();
        return { ok: true, body: currentMockStream.stream } as any;
      }
      fetchCount++;
      // Slow response to test duplicate guard
      await new Promise((r) => setTimeout(r, 50));
      return {
        ok: true,
        json: async () => ({
          data: [{ date: "20240101", content: `Page ${fetchCount}` }],
          total: 100,
          hasMore: true,
        }),
      } as any;
    });

    act(() => {
      result.current.setDateRangeFilter("20240101", "20240102");
    });

    await waitFor(() => {
      expect(result.current.hasMore).toBe(true);
    });

    fetchCount = 0;

    // Call loadMore twice rapidly
    act(() => {
      result.current.loadMore!();
      result.current.loadMore!();
    });

    await act(async () => {
      await new Promise((r) => setTimeout(r, 100));
    });

    // Should only have fetched once, not twice
    expect(fetchCount).toBe(1);
  });

  it("should not advance offset when loadMore fetch fails", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        currentMockStream = createMockSSEStream();
        return { ok: true, body: currentMockStream.stream } as any;
      }
      return {
        ok: true,
        json: async () => ({
          data: [{ date: "20240101", content: "Page 1" }],
          total: 100,
          hasMore: true,
        }),
      } as any;
    });

    act(() => {
      result.current.setDateRangeFilter("20240101", "20240102");
    });

    await waitFor(() => {
      expect(result.current.hasMore).toBe(true);
    });

    // Make next fetch fail
    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        currentMockStream = createMockSSEStream();
        return { ok: true, body: currentMockStream.stream } as any;
      }
      throw new Error("Network error");
    });

    await act(async () => {
      await result.current.loadMore!();
    });

    // hasMore should still be true - offset not advanced
    expect(result.current.hasMore).toBe(true);
  });

  it("should keep prepended logs when buffer is full", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    // Fill buffer to MAX_LOG_ENTRIES with history
    const entries = Array.from({ length: 2000 }, (_, i) => ({
      date: "2024-01-01T10:00:00Z",
      content: `Entry ${i}`,
    }));

    act(() => {
      currentMockStream!.controller.push("history", entries);
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(2000);
    });

    // Now prepend earlier logs via REST
    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        currentMockStream = createMockSSEStream();
        return { ok: true, body: currentMockStream.stream } as any;
      }
      return {
        ok: true,
        json: async () => ({
          data: [{ date: "20240101", content: "Earlier log" }],
          total: 1,
          hasMore: false,
        }),
      } as any;
    });

    await act(async () => {
      result.current.loadEarlierLogs();
    });

    await waitFor(() => {
      // The earlier log should be at the start, not dropped
      expect(result.current.logs[0].content).toBe("Earlier log");
    });
  });

  it("should reset hasMore when reconnecting SSE after clearing date filter", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => expect(result.current.connected).toBe(true));

    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        currentMockStream = createMockSSEStream();
        return { ok: true, body: currentMockStream.stream } as any;
      }
      return {
        ok: true,
        json: async () => ({
          data: [{ date: "20240101", content: "Historical" }],
          total: 100,
          hasMore: true,
        }),
      } as any;
    });

    act(() => {
      result.current.setDateRangeFilter("20240101", "20240102");
    });

    await waitFor(() => {
      expect(result.current.hasMore).toBe(true);
    });

    act(() => {
      result.current.setDateFilter(null);
    });

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
      expect(result.current.hasMore).toBe(false);
    });
  });
});
