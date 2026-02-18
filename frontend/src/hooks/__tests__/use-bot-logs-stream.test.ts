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
  pushRaw: (raw: string) => void;
  close: () => void;
  error: (err?: Error) => void;
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
        const payload =
          `event: ${eventType}\ndata: ${JSON.stringify(data)}\n\n`;
        streamController.enqueue(encoder.encode(payload));
      },
      pushRaw(raw: string) {
        streamController.enqueue(encoder.encode(raw));
      },
      close() {
        try {
          streamController.close();
        } catch {
          // Already closed
        }
      },
      error(err?: Error) {
        try {
          streamController.error(err || new Error("Stream error"));
        } catch {
          // Already errored
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
      data: { isValid: true },
    } as any);

    // Default: mock authFetch to return an SSE stream for stream URLs
    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        currentMockStream = createMockSSEStream();
        return {
          ok: true,
          body: currentMockStream.stream,
        } as any;
      }
      // Default REST response
      return {
        ok: true,
        json: async () => ({ data: [], total: 0, hasMore: false }),
      } as any;
    });
  }

  beforeEach(() => {
    jest.clearAllMocks();
    currentMockStream = null;
    setupDefaultMocks();
  });

  afterEach(() => {
    if (currentMockStream) {
      try {
        currentMockStream.controller.close();
      } catch {
        // ignore
      }
    }
    // Ensure real timers are restored (some tests switch to fake)
    jest.useRealTimers();
  });

  // ---- Basic interface ----

  it("should return the full interface including connected, lastUpdate, and buffer cap fields", () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    expect(result.current).toHaveProperty("logs");
    expect(result.current).toHaveProperty("loading");
    expect(result.current).toHaveProperty("error");
    expect(result.current).toHaveProperty("hasMore");
    expect(result.current).toHaveProperty("total");
    expect(result.current).toHaveProperty("refresh");
    expect(result.current).toHaveProperty("exportLogs");
    expect(result.current).toHaveProperty("setDateFilter");
    expect(result.current).toHaveProperty("setDateRangeFilter");
    expect(result.current).toHaveProperty("connected");
    expect(result.current).toHaveProperty("lastUpdate");
    expect(result.current.connected).toBe(false);
    expect(result.current.lastUpdate).toBeNull();
    expect(result.current).toHaveProperty("hasEarlierLogs");
    expect(result.current).toHaveProperty("loadingEarlier");
    expect(result.current).toHaveProperty("loadEarlierLogs");
    expect(result.current.hasEarlierLogs).toBe(false);
    expect(result.current.loadingEarlier).toBe(false);
    expect(typeof result.current.loadEarlierLogs).toBe("function");
  });

  // ---- SSE streaming ----

  it("should connect via SSE and handle history event with line expansion", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    act(() => {
      currentMockStream!.controller.push("history", [
        { date: "2024-01-01T10:00:00Z", content: "Line 1\nLine 2" },
        { date: "2024-01-01T10:01:00Z", content: "Line 3" },
      ]);
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(3);
    });

    expect(result.current.logs[0].content).toBe("Line 1");
    expect(result.current.logs[1].content).toBe("Line 2");
    expect(result.current.logs[2].content).toBe("Line 3");
    expect(result.current.lastUpdate).not.toBeNull();
    expect(result.current.total).toBe(3);
  });

  it("should handle update events and append to existing logs", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    act(() => {
      currentMockStream!.controller.push("history", [
        { date: "2024-01-01T10:00:00Z", content: "Initial log" },
      ]);
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(1);
    });

    act(() => {
      currentMockStream!.controller.push("update", {
        content: "New line A\nNew line B",
        timestamp: "2024-01-01T10:02:00Z",
      });
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(3);
    });

    expect(result.current.logs[1].content).toBe("New line A");
    expect(result.current.logs[2].content).toBe("New line B");
    expect(result.current.logs[2].date).toBe("2024-01-01T10:02:00Z");
  });

  // ---- Fallback & timeout (need fake timers) ----

  it("should fall back to REST polling after 4 second timeout", async () => {
    jest.useFakeTimers();

    mockValidateSSEAuth.mockReturnValue({
      success: false,
      error: "Auth not ready",
    } as any);

    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [{ date: "2024-01-01", content: "REST log" }],
        total: 1,
        hasMore: false,
      }),
    } as Response);

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    expect(mockAuthFetch).not.toHaveBeenCalled();

    await act(async () => {
      jest.advanceTimersByTime(4000);
    });

    await waitFor(() => {
      expect(mockAuthFetch).toHaveBeenCalled();
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(1);
      expect(result.current.logs[0].content).toBe("REST log");
    });
  });

  it("should fall back to REST when SSE errors before initial load", async () => {
    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        throw new Error("Stream connection failed");
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
      expect(result.current.logs).toHaveLength(1);
      expect(result.current.logs[0].content).toBe("Fallback log");
    });
  });

  // ---- Cleanup ----

  it("should clean up on unmount", async () => {
    const abortSpy = jest.spyOn(AbortController.prototype, "abort");

    const { result, unmount } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    unmount();

    expect(abortSpy).toHaveBeenCalled();
    abortSpy.mockRestore();
  });

  // ---- Export ----

  it("should export current log state", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    act(() => {
      currentMockStream!.controller.push("history", [
        { date: "2024-01-01T10:00:00Z", content: "Export me" },
      ]);
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(1);
    });

    const mockClick = jest.fn();
    const originalCreateObjectURL = global.URL.createObjectURL;
    const originalRevokeObjectURL = global.URL.revokeObjectURL;
    global.URL.createObjectURL = jest.fn(() => "blob:test-url");
    global.URL.revokeObjectURL = jest.fn();

    const createElementSpy = jest
      .spyOn(document, "createElement")
      .mockReturnValue({
        href: "",
        download: "",
        click: mockClick,
      } as any);
    const appendChildSpy = jest
      .spyOn(document.body, "appendChild")
      .mockImplementation(jest.fn() as any);
    const removeChildSpy = jest
      .spyOn(document.body, "removeChild")
      .mockImplementation(jest.fn() as any);

    act(() => {
      result.current.exportLogs();
    });

    expect(global.URL.createObjectURL).toHaveBeenCalled();
    expect(mockClick).toHaveBeenCalled();
    expect(global.URL.revokeObjectURL).toHaveBeenCalledWith("blob:test-url");
    expect(mockToast).toHaveBeenCalledWith(
      expect.objectContaining({
        description: "Logs exported successfully",
      }),
    );

    createElementSpy.mockRestore();
    appendChildSpy.mockRestore();
    removeChildSpy.mockRestore();
    global.URL.createObjectURL = originalCreateObjectURL;
    global.URL.revokeObjectURL = originalRevokeObjectURL;
  });

  it("should show toast when exporting with no logs", () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    act(() => {
      result.current.exportLogs();
    });

    expect(mockToast).toHaveBeenCalledWith(
      expect.objectContaining({
        title: "No logs to export",
        variant: "destructive",
      }),
    );
  });

  // ---- Date filters ----

  it("should switch to REST when date filter is set", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    mockAuthFetch.mockClear();
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [{ date: "2024-01-15", content: "Historical log" }],
        total: 1,
        hasMore: false,
      }),
    } as Response);

    await act(async () => {
      result.current.setDateFilter("20240115");
    });

    expect(result.current.connected).toBe(false);

    await waitFor(() => {
      expect(mockAuthFetch).toHaveBeenCalled();
    });

    const calledUrl = mockAuthFetch.mock.calls[0][0] as string;
    expect(calledUrl).toContain("date=20240115");
  });

  it("should reconnect SSE when clearing date filter", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    mockAuthFetch.mockClear();
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [{ date: "2024-01-15", content: "Historical log" }],
        total: 1,
        hasMore: false,
      }),
    } as Response);

    await act(async () => {
      result.current.setDateFilter("20240115");
    });

    await waitFor(() => {
      expect(result.current.connected).toBe(false);
    });

    // Restore stream mock for reconnection
    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        currentMockStream = createMockSSEStream();
        return {
          ok: true,
          body: currentMockStream.stream,
        } as any;
      }
      return {
        ok: true,
        json: async () => ({ data: [], total: 0, hasMore: false }),
      } as any;
    });

    await act(async () => {
      result.current.setDateFilter(null);
    });

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });
  });

  it("should attempt SSE reconnection with backoff after post-load drop", async () => {
    jest.useFakeTimers();

    // Need to manually trigger the initial connection since useEffect + fake timers
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    // Flush the initial authFetch promise and state updates
    await act(async () => {
      await jest.advanceTimersByTimeAsync(0);
    });

    // Connection should be established
    expect(result.current.connected).toBe(true);

    // Deliver history so initialLoadCompleteRef is true
    act(() => {
      currentMockStream!.controller.push("history", [
        { date: "2024-01-01T10:00:00Z", content: "Log line" },
      ]);
    });

    await act(async () => {
      await jest.advanceTimersByTimeAsync(0);
    });

    expect(result.current.logs).toHaveLength(1);

    // Simulate SSE error (not clean close) to trigger catch handler
    act(() => {
      currentMockStream!.controller.error(new Error("Connection lost"));
    });

    await act(async () => {
      await jest.advanceTimersByTimeAsync(0);
    });

    expect(result.current.connected).toBe(false);

    // Set up fresh mock for reconnection
    mockAuthFetch.mockClear();
    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        currentMockStream = createMockSSEStream();
        return {
          ok: true,
          body: currentMockStream.stream,
        } as any;
      }
      return {
        ok: true,
        json: async () => ({ data: [], total: 0, hasMore: false }),
      } as any;
    });

    // Advance past the 1s backoff (first attempt: 1000 * 2^0 = 1000ms)
    await act(async () => {
      await jest.advanceTimersByTimeAsync(1000);
    });

    // Should have attempted reconnection
    expect(mockAuthFetch).toHaveBeenCalledWith(
      expect.stringContaining("/stream"),
      expect.any(Object),
    );
  });

  it("should switch to REST when date range filter is set", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    mockAuthFetch.mockClear();
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [],
        total: 0,
        hasMore: false,
      }),
    } as Response);

    await act(async () => {
      result.current.setDateRangeFilter("20240101", "20240115");
    });

    expect(result.current.connected).toBe(false);

    await waitFor(() => {
      expect(mockAuthFetch).toHaveBeenCalled();
    });

    const calledUrl = mockAuthFetch.mock.calls[0][0] as string;
    expect(calledUrl).toContain("dateRange=20240101-20240115");
  });

  // ---- Guard clauses ----

  it("should not connect when botId is empty", () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "" }),
    );

    expect(mockAuthFetch).not.toHaveBeenCalled();
    expect(result.current.loading).toBe(false);
    expect(result.current.logs).toEqual([]);
  });

  it("should not connect when user is null", () => {
    mockUseAuth.mockReturnValue({
      user: null,
    } as any);

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    expect(mockAuthFetch).not.toHaveBeenCalled();
    expect(result.current.loading).toBe(false);
  });

  // ---- Error handling ----

  it("should handle REST fetch error with toast notification", async () => {
    jest.useFakeTimers();

    mockValidateSSEAuth.mockReturnValue({
      success: false,
      error: "Auth not ready",
    } as any);

    mockAuthFetch.mockResolvedValue({
      ok: false,
      statusText: "Internal Server Error",
    } as Response);

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await act(async () => {
      jest.advanceTimersByTime(4000);
    });

    await waitFor(() => {
      expect(mockAuthFetch).toHaveBeenCalled();
    });

    await waitFor(() => {
      expect(result.current.error).toContain("Failed to fetch logs");
    });

    expect(mockToast).toHaveBeenCalledWith(
      expect.objectContaining({
        title: "Error",
        variant: "destructive",
      }),
    );
  });

  it("should gracefully handle malformed SSE history event data", async () => {
    const consoleSpy = jest
      .spyOn(console, "error")
      .mockImplementation(() => {});

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    // Push raw malformed SSE data
    act(() => {
      currentMockStream!.controller.pushRaw(
        "event: history\ndata: not valid json{{{\n\n",
      );
    });

    // Give the reader loop time to process the malformed data
    await act(async () => {
      await new Promise((r) => setTimeout(r, 50));
    });

    // Should not crash -- logs remain empty
    expect(result.current.logs).toEqual([]);
    expect(consoleSpy).toHaveBeenCalledWith(
      "Failed to parse history SSE event:",
      expect.any(Error),
    );

    consoleSpy.mockRestore();
  });

  it("should gracefully handle malformed SSE update event data", async () => {
    const consoleSpy = jest
      .spyOn(console, "error")
      .mockImplementation(() => {});

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    // First send valid history
    act(() => {
      currentMockStream!.controller.push("history", [
        { date: "2024-01-01T10:00:00Z", content: "Initial" },
      ]);
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(1);
    });

    // Send malformed update data
    act(() => {
      currentMockStream!.controller.pushRaw(
        "event: update\ndata: {{invalid json\n\n",
      );
    });

    await act(async () => {
      await new Promise((r) => setTimeout(r, 50));
    });

    // Should not crash -- logs remain unchanged
    expect(result.current.logs).toHaveLength(1);
    expect(consoleSpy).toHaveBeenCalledWith(
      "Failed to parse update SSE event:",
      expect.any(Error),
    );

    consoleSpy.mockRestore();
  });

  it("should encode botId in URL to prevent injection", async () => {
    const specialBotId = "bot/with spaces&special=chars";

    renderHook(() =>
      useBotLogsStream({ botId: specialBotId }),
    );

    await waitFor(() => {
      expect(mockAuthFetch).toHaveBeenCalledWith(
        `/api/logs/${encodeURIComponent(specialBotId)}/stream`,
        expect.any(Object),
      );
    });
  });

  // ---- Buffer cap tests ----

  it("should cap logs at MAX_LOG_ENTRIES when history exceeds limit", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    // Generate 3000 entries
    const entries = Array.from({ length: 3000 }, (_, i) => ({
      date: "2024-01-01T10:00:00Z",
      content: `Log line ${i}`,
    }));

    act(() => {
      currentMockStream!.controller.push("history", entries);
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(2000);
    });

    expect(result.current.hasEarlierLogs).toBe(true);
    expect(result.current.logs[0].content).toBe("Log line 1000");
    expect(result.current.logs[result.current.logs.length - 1].content).toBe(
      "Log line 2999",
    );
  });

  it("should trim oldest entries when update pushes past MAX_LOG_ENTRIES", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    // Fill to 2000 via history
    const entries = Array.from({ length: 2000 }, (_, i) => ({
      date: "2024-01-01T10:00:00Z",
      content: `Log line ${i}`,
    }));

    act(() => {
      currentMockStream!.controller.push("history", entries);
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(2000);
    });

    expect(result.current.hasEarlierLogs).toBe(false);

    // Push 100 more updates
    for (let i = 0; i < 100; i++) {
      act(() => {
        currentMockStream!.controller.push("update", {
          content: `New entry ${i}`,
          timestamp: "2024-01-01T11:00:00Z",
        });
      });
    }

    await waitFor(() => {
      expect(
        result.current.logs[result.current.logs.length - 1].content,
      ).toBe("New entry 99");
    });

    expect(result.current.logs).toHaveLength(2000);
    expect(result.current.hasEarlierLogs).toBe(true);
  });

  it("should set hasEarlierLogs to false when history fits within cap", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    const entries = Array.from({ length: 50 }, (_, i) => ({
      date: "2024-01-01T10:00:00Z",
      content: `Log line ${i}`,
    }));

    act(() => {
      currentMockStream!.controller.push("history", entries);
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(50);
    });

    expect(result.current.hasEarlierLogs).toBe(false);
  });

  // ---- loadEarlierLogs tests ----

  it("should fetch earlier logs via REST and prepend to current logs", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    // Simulate history with just entries 50-99 (as if earlier were trimmed)
    const historyEntries = Array.from({ length: 50 }, (_, i) => ({
      date: "2024-01-01T10:00:00Z",
      content: `Log line ${i + 50}`,
    }));

    act(() => {
      currentMockStream!.controller.push("history", historyEntries);
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(50);
    });

    // Mock REST to return the full day's logs (0-99)
    const allEntries = Array.from({ length: 100 }, (_, i) => ({
      date: "2024-01-01T10:00:00Z",
      content: `Log line ${i}`,
    }));

    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        return { ok: true, body: currentMockStream!.stream } as any;
      }
      return {
        ok: true,
        json: async () => ({
          data: allEntries,
          total: 100,
          hasMore: false,
        }),
      } as any;
    });

    await act(async () => {
      result.current.loadEarlierLogs();
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(100);
    });

    expect(result.current.logs[0].content).toBe("Log line 0");
  });

  it("should set loadingEarlier while fetching", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    act(() => {
      currentMockStream!.controller.push("history", [
        { date: "2024-01-01T10:00:00Z", content: "Log line 1" },
      ]);
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(1);
    });

    expect(result.current.loadingEarlier).toBe(false);

    // Mock REST with a delay
    let resolveRest!: (value: any) => void;
    const restPromise = new Promise((resolve) => {
      resolveRest = resolve;
    });

    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        return { ok: true, body: currentMockStream!.stream } as any;
      }
      return restPromise;
    });

    // Start loading (don't await — we want to check intermediate state)
    act(() => {
      result.current.loadEarlierLogs();
    });

    // Should be loading now
    await waitFor(() => {
      expect(result.current.loadingEarlier).toBe(true);
    });

    // Resolve the REST call
    await act(async () => {
      resolveRest({
        ok: true,
        json: async () => ({ data: [], total: 0, hasMore: false }),
      });
    });

    await waitFor(() => {
      expect(result.current.loadingEarlier).toBe(false);
    });
  });

  it("should show toast on loadEarlierLogs error", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    act(() => {
      currentMockStream!.controller.push("history", [
        { date: "2024-01-01T10:00:00Z", content: "Log line 1" },
      ]);
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(1);
    });

    // Mock REST to fail
    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        return { ok: true, body: currentMockStream!.stream } as any;
      }
      return { ok: false, statusText: "Server Error" } as any;
    });

    await act(async () => {
      result.current.loadEarlierLogs();
    });

    await waitFor(() => {
      expect(mockToast).toHaveBeenCalledWith(
        expect.objectContaining({
          title: "Error",
          variant: "destructive",
        }),
      );
    });
  });

  it("should update hasEarlierLogs after loading earlier logs when no more entries exist", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    await waitFor(() => {
      expect(result.current.connected).toBe(true);
    });

    act(() => {
      currentMockStream!.controller.push("history", [
        { date: "2024-01-01T10:00:00Z", content: "Log line 0" },
        { date: "2024-01-01T10:00:00Z", content: "Log line 1" },
      ]);
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(2);
    });

    // Mock REST to return the same entries (no earlier logs exist)
    mockAuthFetch.mockImplementation(async (url: string) => {
      if (typeof url === "string" && url.includes("/stream")) {
        return { ok: true, body: currentMockStream!.stream } as any;
      }
      return {
        ok: true,
        json: async () => ({
          data: [
            { date: "2024-01-01T10:00:00Z", content: "Log line 0" },
            { date: "2024-01-01T10:00:00Z", content: "Log line 1" },
          ],
          total: 2,
          hasMore: false,
        }),
      } as any;
    });

    await act(async () => {
      result.current.loadEarlierLogs();
    });

    await waitFor(() => {
      expect(result.current.hasEarlierLogs).toBe(false);
    });
  });
});
