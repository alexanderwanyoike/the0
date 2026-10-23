import { renderHook, waitFor, act } from "@testing-library/react";
import { useBotLogs } from "../use-bot-logs";
import { useAuth } from "@/contexts/auth-context";
import { useToast } from "@/hooks/use-toast";
import { authFetch } from "@/lib/auth-fetch";
import { validateSSEAuth } from "@/lib/sse/sse-auth";
import type { LogEntry } from "@/types/logs";

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

function createMockSSEStream(signal?: AbortSignal): {
  stream: ReadableStream<Uint8Array>;
  controller: MockSSEStreamController;
} {
  const encoder = new TextEncoder();
  let streamController: ReadableStreamDefaultController<Uint8Array>;

  const stream = new ReadableStream<Uint8Array>({
    start(controller) {
      streamController = controller;
      // Mimic real browser behavior: aborting a fetch whose body is being
      // read rejects the pending read with a TypeError, NOT an AbortError.
      signal?.addEventListener("abort", () => {
        try {
          controller.error(new TypeError("Failed to fetch"));
        } catch {
          // Already closed/errored
        }
      });
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

type TestEntry = Partial<LogEntry> & { content: string };

/** A full api entry; tests that do not care about keys key it by content. */
function logEntry({ content, ...fields }: TestEntry): LogEntry {
  return {
    key: content,
    date: "20240101",
    timestamp: null,
    content,
    ...fields,
  };
}

/** The api pairs hasMore with the cursor of the next older page. */
function restResponse({
  data = [],
  hasMore = false,
  nextCursor = hasMore ? "20240101:0" : null,
}: {
  data?: TestEntry[];
  hasMore?: boolean;
  nextCursor?: string | null;
} = {}) {
  return {
    ok: true,
    json: async () => ({
      success: true,
      data: data.map(logEntry),
      hasMore,
      nextCursor,
      message: "Logs retrieved successfully",
    }),
  } as any;
}

/** Count REST (non-stream) authFetch calls made so far */
function restCallCount() {
  return mockAuthFetch.mock.calls.filter(
    (call) => typeof call[0] === "string" && !call[0].includes("/stream"),
  ).length;
}

/** Count SSE stream connections made so far */
function streamCallCount() {
  return mockAuthFetch.mock.calls.filter(
    (call) => typeof call[0] === "string" && call[0].includes("/stream"),
  ).length;
}

describe("useBotLogs", () => {
  const mockToast = jest.fn();
  const mockLogs = [
    logEntry({ content: "Log line 1", timestamp: "2024-01-01T10:00:00Z" }),
    logEntry({ content: "Log line 2", timestamp: "2024-01-01T10:01:00Z" }),
  ];

  beforeEach(() => {
    jest.clearAllMocks();
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
  });

  it("should return loading state initially", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({ data: mockLogs, hasMore: false }),
    } as Response);

    const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));

    expect(result.current.loading).toBe(true);

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });
  });

  it("holds one log per api entry, as sent", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({ data: mockLogs, hasMore: false, nextCursor: null }),
    } as Response);

    const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.logs).toEqual(mockLogs);
    expect(result.current.error).toBeNull();
  });

  it("should not fetch when botId is missing", async () => {
    const { result } = renderHook(() => useBotLogs({ botId: "" }));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(mockAuthFetch).not.toHaveBeenCalled();
  });

  it("should handle API error", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: false,
      statusText: "Internal Server Error",
    } as Response);

    const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.error).toContain("Failed to fetch logs");
    expect(mockToast).toHaveBeenCalledWith(
      expect.objectContaining({
        title: "Error",
        variant: "destructive",
      }),
    );
  });

  it("should not fetch when user is null", async () => {
    mockUseAuth.mockReturnValue({
      user: null,
    } as any);

    const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(mockAuthFetch).not.toHaveBeenCalled();
    expect(result.current.error).toBeNull();
  });

  it("should support pagination with loadMore", async () => {
    mockAuthFetch
      .mockResolvedValueOnce({
        ok: true,
        json: async () => ({
          data: [logEntry({ content: "Page 1" })],
          hasMore: true,
          nextCursor: "20240101:0",
        }),
      } as Response)
      .mockResolvedValueOnce({
        ok: true,
        json: async () => ({
          data: [logEntry({ content: "Page 2" })],
          hasMore: false,
        }),
      } as Response);

    const { result } = renderHook(() =>
      useBotLogs({ botId: "bot-1", initialQuery: { limit: 1 } }),
    );

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.logs).toHaveLength(1);
    expect(result.current.hasMore).toBe(true);

    await act(async () => {
      await result.current.loadMore();
    });

    expect(result.current.logs).toHaveLength(2);
    expect(result.current.hasMore).toBe(false);
  });

  it("should call API again when refresh is called", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [logEntry({ content: "Log entry" })],
        hasMore: false,
      }),
    } as Response);

    const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    const initialCallCount = mockAuthFetch.mock.calls.length;

    await act(async () => {
      result.current.refresh();
    });

    await waitFor(() => {
      expect(mockAuthFetch.mock.calls.length).toBeGreaterThan(initialCallCount);
    });
  });

  it("should cap logs at MAX_LOG_ENTRIES when loadMore accumulates too many", async () => {
    // Initial fetch returns 8000 entries
    const initialLogs = Array.from({ length: 8000 }, (_, i) =>
      logEntry({ content: `Line ${i}` }),
    );
    mockAuthFetch
      .mockResolvedValueOnce({
        ok: true,
        json: async () => ({
          data: initialLogs,
          hasMore: true,
          nextCursor: "20240101:0",
        }),
      } as Response)
      .mockResolvedValueOnce({
        ok: true,
        json: async () => ({
          data: Array.from({ length: 3000 }, (_, i) =>
            logEntry({ content: `Extra ${i}` }),
          ),
          hasMore: false,
        }),
      } as Response);

    const { result } = renderHook(() =>
      useBotLogs({ botId: "bot-1", initialQuery: { limit: 8000 } }),
    );

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.logs).toHaveLength(8000);

    await act(async () => {
      await result.current.loadMore();
    });

    // Should be capped at 10000, oldest trimmed (8000 + 3000 = 11000 -> 10000)
    expect(result.current.logs).toHaveLength(10000);
    // First entry should be Line 1000 (1000 oldest trimmed from 11000 -> 10000)
    expect(result.current.logs[0].content).toBe("Line 1000");
    expect(result.current.logs[result.current.logs.length - 1].content).toBe(
      "Extra 2999",
    );
  });

  it("should cap logs on initial fetch if response is very large", async () => {
    const hugeLogs = Array.from({ length: 12000 }, (_, i) =>
      logEntry({ content: `Huge ${i}` }),
    );
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: hugeLogs,
        hasMore: false,
      }),
    } as Response);

    const { result } = renderHook(() =>
      useBotLogs({ botId: "bot-1", initialQuery: { limit: 15000 } }),
    );

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    // Capped at 10000 (MAX_LOG_ENTRIES), oldest trimmed
    expect(result.current.logs).toHaveLength(10000);
    expect(result.current.logs[0].content).toBe("Huge 2000");
  });

  it("should reset state when botId changes", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [logEntry({ content: "Bot A log" })],
        hasMore: false,
      }),
    } as Response);

    const { result, rerender } = renderHook(
      ({ id }) => useBotLogs({ botId: id }),
      { initialProps: { id: "bot-a" } },
    );

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(1);
      expect(result.current.loading).toBe(false);
    });

    rerender({ id: "bot-b" });

    await waitFor(() => {
      expect(result.current.logs).toEqual([]);
    });
  });

  it("should not set loading to false after aborted fetch", async () => {
    let callCount = 0;
    mockAuthFetch.mockImplementation(async () => {
      callCount++;
      if (callCount <= 1) {
        return {
          ok: true,
          json: async () => ({
            data: [logEntry({ content: "Bot A" })],
            hasMore: false,
          }),
        } as any;
      }
      // Hang until aborted
      return new Promise(() => {});
    });

    const { result, rerender } = renderHook(
      ({ id }) => useBotLogs({ botId: id }),
      { initialProps: { id: "bot-a" } },
    );

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    // Switch to bot-b (fetch hangs), then bot-c
    rerender({ id: "bot-b" });

    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [logEntry({ content: "Bot C" })],
        hasMore: false,
      }),
    } as any);

    rerender({ id: "bot-c" });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(1);
      expect(result.current.logs[0].content).toBe("Bot C");
      expect(result.current.loading).toBe(false);
    });
  });

  it("should not double-fetch on filter change", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [logEntry({ content: "Log" })],
        hasMore: false,
      }),
    } as Response);

    const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    const callsBefore = mockAuthFetch.mock.calls.length;

    act(() => {
      result.current.setDateFilter("20240101");
    });

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    const callsAfter = mockAuthFetch.mock.calls.length;
    expect(callsAfter - callsBefore).toBe(1);
  });

  it("should encode botId in URL", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({ data: [], hasMore: false }),
    } as Response);

    renderHook(() => useBotLogs({ botId: "bot/special&chars" }));

    await waitFor(() => {
      expect(mockAuthFetch).toHaveBeenCalledWith(
        expect.stringContaining("bot%2Fspecial%26chars"),
        expect.any(Object),
      );
    });
  });

  it("should handle rapid botId changes without errors", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({ data: [], hasMore: false }),
    } as Response);

    const { result, rerender } = renderHook(
      ({ id }) => useBotLogs({ botId: id }),
      { initialProps: { id: "bot-a" } },
    );

    for (let i = 0; i < 5; i++) {
      rerender({ id: i % 2 === 0 ? "bot-b" : "bot-a" });
    }

    await act(async () => {
      await new Promise((r) => setTimeout(r, 50));
    });

    expect(result.current.error).toBeNull();
  });

  it("should not trigger extra fetches when date filter changes during auto-refresh", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [logEntry({ content: "Log" })],
        hasMore: false,
      }),
    } as Response);

    const { result } = renderHook(() =>
      useBotLogs({
        botId: "bot-1",
        autoRefresh: true,
        refreshInterval: 60000,
      }),
    );

    // Wait for initial fetch to complete
    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    const callsAfterInit = mockAuthFetch.mock.calls.length;

    // Change the date range filter
    act(() => {
      result.current.setDateRangeFilter("20260401", "20260403");
    });

    // Wait for the filter-triggered fetch to settle
    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    // Brief pause to catch any spurious extra fetches
    await act(async () => {
      await new Promise((r) => setTimeout(r, 100));
    });

    const callsAfterFilter = mockAuthFetch.mock.calls.length;
    // Expect exactly one additional fetch from the filter change, not multiple
    expect(callsAfterFilter - callsAfterInit).toBe(1);
  });

  it("should use latest query when polling interval fires after filter change", async () => {
    // This tests the stale closure bug: the polling interval captures fetchLogs
    // at setup time. If fetchLogs changes (because query changed), the interval
    // should still call the latest version, not the stale one.
    jest.useFakeTimers();
    try {
      mockAuthFetch.mockResolvedValue({
        ok: true,
        json: async () => ({
          data: [logEntry({ content: "Log" })],
          hasMore: false,
        }),
      } as Response);

      const { result } = renderHook(() =>
        useBotLogs({
          botId: "bot-1",
          autoRefresh: true,
          refreshInterval: 5000,
        }),
      );

      // Flush initial fetch
      await act(async () => {
        await jest.advanceTimersByTimeAsync(0);
      });

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });

      // Change date range filter - this updates the query state and calls fetchLogs
      await act(async () => {
        result.current.setDateRangeFilter("20260401", "20260403");
      });

      await act(async () => {
        await jest.advanceTimersByTimeAsync(0);
      });

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });

      // Clear call history so we can inspect what the next interval tick sends
      mockAuthFetch.mockClear();

      // Advance past the polling interval so it fires
      await act(async () => {
        await jest.advanceTimersByTimeAsync(5000);
      });

      // The polling fetch should have fired
      expect(mockAuthFetch).toHaveBeenCalled();

      // The polling fetch should use the UPDATED date range, not the original query.
      // If the closure is stale, it will use the default query (today's date, no dateRange).
      const pollingUrl = mockAuthFetch.mock.calls[0][0] as string;
      expect(pollingUrl).toContain("dateRange=20260401-20260403");
    } finally {
      jest.useRealTimers();
    }
  });

  // ---- Load more with full buffer ----

  it("should grow buffer past 2000 when loadMore appends", async () => {
    // Initial fetch fills to exactly 2000 entries (old MAX_LOG_ENTRIES)
    const initialLogs = Array.from({ length: 2000 }, (_, i) =>
      logEntry({ content: `Line ${i}` }),
    );
    mockAuthFetch
      .mockResolvedValueOnce({
        ok: true,
        json: async () => ({
          data: initialLogs,
          hasMore: true,
          nextCursor: "20240101:0",
        }),
      } as Response)
      .mockResolvedValueOnce({
        ok: true,
        json: async () => ({
          data: Array.from({ length: 2000 }, (_, i) =>
            logEntry({ content: `Extra ${i}` }),
          ),
          hasMore: false,
        }),
      } as Response);

    const { result } = renderHook(() =>
      useBotLogs({ botId: "bot-1", initialQuery: { limit: 2000 } }),
    );

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.logs).toHaveLength(2000);
    expect(result.current.hasMore).toBe(true);

    await act(async () => {
      await result.current.loadMore();
    });

    // After loadMore, buffer should grow beyond 2000
    expect(result.current.logs.length).toBeGreaterThan(2000);
    expect(result.current.hasMore).toBe(false);
  });

  it("should not overwrite paginated data when auto-refresh fires", async () => {
    jest.useFakeTimers();
    try {
      mockAuthFetch.mockResolvedValue({
        ok: true,
        json: async () => ({
          data: [logEntry({ content: "Page 1" })],
          hasMore: true,
          nextCursor: "20240101:0",
        }),
      } as Response);

      const { result } = renderHook(() =>
        useBotLogs({
          botId: "bot-1",
          autoRefresh: true,
          refreshInterval: 5000,
          initialQuery: { limit: 1 },
        }),
      );

      await act(async () => {
        await jest.advanceTimersByTimeAsync(0);
      });

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });

      expect(result.current.logs).toHaveLength(1);
      expect(result.current.logs[0].content).toBe("Page 1");

      // loadMore appends page 2
      mockAuthFetch.mockResolvedValue({
        ok: true,
        json: async () => ({
          data: [logEntry({ content: "Page 2" })],
          hasMore: false,
        }),
      } as Response);

      await act(async () => {
        await result.current.loadMore();
      });

      expect(result.current.logs).toHaveLength(2);

      // Auto-refresh fires - should NOT overwrite paginated data
      await act(async () => {
        await jest.advanceTimersByTimeAsync(5000);
      });

      // Both pages should still be present
      expect(result.current.logs.length).toBe(2);
    } finally {
      jest.useRealTimers();
    }
  });

  // ---- Existing tests ----

  it("should build correct URL with query parameters", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({ data: [], hasMore: false }),
    } as Response);

    renderHook(() =>
      useBotLogs({
        botId: "bot-1",
        initialQuery: { limit: 50 },
      }),
    );

    await waitFor(() => {
      expect(mockAuthFetch).toHaveBeenCalled();
    });

    const calledUrl = mockAuthFetch.mock.calls[0][0] as string;
    expect(calledUrl).toContain("/api/logs/bot-1");
    expect(calledUrl).toContain("limit=50");
  });

  describe("latest mode (lookbackDays)", () => {
    beforeEach(() => {
      mockAuthFetch.mockResolvedValue({
        ok: true,
        json: async () => ({ data: [], hasMore: false }),
      } as Response);
    });

    it("sends lookbackDays instead of defaulting date to today", async () => {
      renderHook(() =>
        useBotLogs({
          botId: "bot-1",
          initialQuery: {
            limit: 100,
            sort: "desc",
            lookbackDays: 30,
          },
        }),
      );

      await waitFor(() => {
        expect(mockAuthFetch).toHaveBeenCalled();
      });

      const url = mockAuthFetch.mock.calls[0][0] as string;
      expect(url).toContain("lookbackDays=30");
      expect(url).not.toContain("date=");
    });

    it("setLatestFilter clears date filters and queries by lookback", async () => {
      const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });

      act(() => {
        result.current.setDateRangeFilter("20260401", "20260403");
      });
      await waitFor(() => {
        expect(result.current.query.dateRange).toBe("20260401-20260403");
      });

      mockAuthFetch.mockClear();
      act(() => {
        result.current.setLatestFilter();
      });

      await waitFor(() => {
        expect(mockAuthFetch).toHaveBeenCalled();
      });

      const url = mockAuthFetch.mock.calls[0][0] as string;
      expect(url).toContain("lookbackDays=");
      expect(url).not.toContain("dateRange=");
      expect(url).not.toContain("date=");
    });

    it("setDateRangeFilter clears lookbackDays from the query", async () => {
      const { result } = renderHook(() =>
        useBotLogs({
          botId: "bot-1",
          initialQuery: { limit: 100, lookbackDays: 30 },
        }),
      );

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });

      mockAuthFetch.mockClear();
      act(() => {
        result.current.setDateRangeFilter("20260401", "20260403");
      });

      await waitFor(() => {
        expect(mockAuthFetch).toHaveBeenCalled();
      });

      const url = mockAuthFetch.mock.calls[0][0] as string;
      expect(url).toContain("dateRange=20260401-20260403");
      expect(url).not.toContain("lookbackDays=");
      expect(result.current.query.lookbackDays).toBeUndefined();
    });

    it("setDateFilter clears lookbackDays from the query", async () => {
      const { result } = renderHook(() =>
        useBotLogs({
          botId: "bot-1",
          initialQuery: { limit: 100, lookbackDays: 30 },
        }),
      );

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });

      mockAuthFetch.mockClear();
      act(() => {
        result.current.setDateFilter("20260401");
      });

      await waitFor(() => {
        expect(mockAuthFetch).toHaveBeenCalled();
      });

      const url = mockAuthFetch.mock.calls[0][0] as string;
      expect(url).toContain("date=20260401");
      expect(url).not.toContain("lookbackDays=");
    });
  });

  it("asks for chronological order (sort=asc) by default", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({ data: [], hasMore: false }),
    } as Response);

    renderHook(() => useBotLogs({ botId: "bot-1" }));

    await waitFor(() => {
      expect(mockAuthFetch).toHaveBeenCalled();
    });

    const url = mockAuthFetch.mock.calls[0][0] as string;
    expect(url).toContain("sort=asc");
  });

  it("asks for sort=desc when the query sort is changed", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({ data: [], hasMore: false }),
    } as Response);

    const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    mockAuthFetch.mockClear();

    act(() => {
      result.current.updateQuery({ sort: "desc" });
    });

    await waitFor(() => {
      expect(mockAuthFetch).toHaveBeenCalled();
    });

    const url = mockAuthFetch.mock.calls[0][0] as string;
    expect(url).toContain("sort=desc");
  });

  it("should reconfigure polling when refreshInterval changes", async () => {
    jest.useFakeTimers();
    try {
      mockAuthFetch.mockResolvedValue({
        ok: true,
        json: async () => ({
          data: [logEntry({ content: "Log" })],
          hasMore: false,
        }),
      } as Response);

      const { rerender } = renderHook(
        ({ interval }) =>
          useBotLogs({
            botId: "bot-1",
            autoRefresh: true,
            refreshInterval: interval,
          }),
        { initialProps: { interval: 60000 } },
      );

      await act(async () => {
        await jest.advanceTimersByTimeAsync(0);
      });

      mockAuthFetch.mockClear();

      // Change refresh interval to 5000ms
      rerender({ interval: 5000 });

      // Advance 5 seconds - should fire with new interval
      await act(async () => {
        await jest.advanceTimersByTimeAsync(5000);
      });

      // Should have fetched with the new interval
      expect(mockAuthFetch).toHaveBeenCalled();
    } finally {
      jest.useRealTimers();
    }
  });

  it("should not poll when refreshInterval is 0 (Off)", async () => {
    jest.useFakeTimers();
    try {
      mockAuthFetch.mockResolvedValue({
        ok: true,
        json: async () => ({
          data: [logEntry({ content: "Log" })],
          hasMore: false,
        }),
      } as Response);

      renderHook(() =>
        useBotLogs({
          botId: "bot-1",
          autoRefresh: true,
          refreshInterval: 0,
        }),
      );

      // Flush initial fetch
      await act(async () => {
        await jest.advanceTimersByTimeAsync(0);
      });

      mockAuthFetch.mockClear();

      // Advance 60 seconds - should NOT poll
      await act(async () => {
        await jest.advanceTimersByTimeAsync(60000);
      });

      expect(mockAuthFetch).not.toHaveBeenCalled();
    } finally {
      jest.useRealTimers();
    }
  });

  // ---- Pagination fetches in flight ----

  describe("pagination in flight", () => {
    it("exposes a dedicated loadingMore state while loadMore is in flight", async () => {
      let resolveSecond: (value: any) => void;
      let callCount = 0;
      mockAuthFetch.mockImplementation(async () => {
        callCount++;
        if (callCount === 1) {
          return restResponse({
            data: [logEntry({ content: "Page 1" })],
            hasMore: true,
            nextCursor: "20240101:0",
          });
        }
        return new Promise((resolve) => {
          resolveSecond = resolve;
        });
      });

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", initialQuery: { limit: 1 } }),
      );

      await waitFor(() => expect(result.current.hasMore).toBe(true));
      expect(result.current.loadingMore).toBe(false);

      let loadMorePromise: Promise<void>;
      act(() => {
        loadMorePromise = result.current.loadMore();
      });

      await waitFor(() => expect(result.current.loadingMore).toBe(true));
      // The full-replace loading flag must NOT flip for append fetches
      expect(result.current.loading).toBe(false);

      await act(async () => {
        resolveSecond!(
          restResponse({
            data: [logEntry({ content: "Page 2" })],
            hasMore: false,
          }),
        );
        await loadMorePromise!;
      });

      expect(result.current.loadingMore).toBe(false);
      expect(result.current.logs).toHaveLength(2);
    });

    it("does not let a polling tick abort an in-flight loadMore", async () => {
      jest.useFakeTimers();
      try {
        let slowResolve: (value: any) => void;
        let callCount = 0;
        mockAuthFetch.mockImplementation(async () => {
          callCount++;
          if (callCount === 1) {
            return restResponse({
              data: [logEntry({ content: "Page 1" })],
              hasMore: true,
              nextCursor: "20240101:0",
            });
          }
          return new Promise((resolve) => {
            slowResolve = resolve;
          });
        });

        const { result } = renderHook(() =>
          useBotLogs({
            botId: "bot-1",
            autoRefresh: true,
            refreshInterval: 5000,
            initialQuery: { limit: 1 },
          }),
        );

        await act(async () => {
          await jest.advanceTimersByTimeAsync(0);
        });
        await waitFor(() => expect(result.current.hasMore).toBe(true));

        // Start loadMore (its fetch hangs), then let the poll interval fire
        let loadMorePromise: Promise<void>;
        act(() => {
          loadMorePromise = result.current.loadMore();
        });
        const callsBeforeTick = mockAuthFetch.mock.calls.length;

        await act(async () => {
          await jest.advanceTimersByTimeAsync(5000);
        });

        // The tick must be skipped while pagination is in flight - firing
        // would abort the pending loadMore request with no user feedback.
        expect(mockAuthFetch.mock.calls.length).toBe(callsBeforeTick);

        await act(async () => {
          slowResolve!(
            restResponse({
              data: [logEntry({ content: "Page 2" })],
              hasMore: false,
            }),
          );
          await loadMorePromise!;
        });

        expect(result.current.logs).toHaveLength(2);
        expect(result.current.logs[1].content).toBe("Page 2");
      } finally {
        jest.useRealTimers();
      }
    });
  });

  // ---- Abort correctness (REST mode) ----

  describe("abort correctness", () => {
    it("treats an AbortError rejection as silent", async () => {
      mockAuthFetch.mockImplementation(
        (_url: any, opts?: any) =>
          new Promise((_, reject) => {
            opts?.signal?.addEventListener("abort", () =>
              reject(new DOMException("Aborted", "AbortError")),
            );
          }) as any,
      );

      const { result, unmount } = renderHook(() =>
        useBotLogs({ botId: "bot-1" }),
      );

      await waitFor(() => expect(mockAuthFetch).toHaveBeenCalled());
      unmount();

      await act(async () => {
        await new Promise((r) => setTimeout(r, 50));
      });
      expect(result.current.error).toBeNull();
      expect(mockToast).not.toHaveBeenCalled();
    });

    it("treats a TypeError rejection from an aborted request as silent (browser abort quirk)", async () => {
      // Some browsers reject an aborted fetch with TypeError("Failed to fetch")
      // instead of AbortError. The hook must check signal.aborted, not the
      // error name, or intentional teardowns surface as user-facing errors.
      mockAuthFetch.mockImplementation(
        (_url: any, opts?: any) =>
          new Promise((_, reject) => {
            opts?.signal?.addEventListener("abort", () =>
              reject(new TypeError("Failed to fetch")),
            );
          }) as any,
      );

      const { result, unmount } = renderHook(() =>
        useBotLogs({ botId: "bot-1" }),
      );

      await waitFor(() => expect(mockAuthFetch).toHaveBeenCalled());
      unmount();

      await act(async () => {
        await new Promise((r) => setTimeout(r, 50));
      });
      expect(result.current.error).toBeNull();
      expect(mockToast).not.toHaveBeenCalled();
    });

    it("stops polling after unmount (no leaked interval)", async () => {
      jest.useFakeTimers();
      try {
        mockAuthFetch.mockResolvedValue(restResponse());

        const { unmount } = renderHook(() =>
          useBotLogs({
            botId: "bot-1",
            autoRefresh: true,
            refreshInterval: 5000,
          }),
        );

        await act(async () => {
          await jest.advanceTimersByTimeAsync(100);
        });
        unmount();
        const callsAtUnmount = restCallCount();

        await act(async () => {
          await jest.advanceTimersByTimeAsync(60000);
        });
        expect(restCallCount()).toBe(callsAtUnmount);
      } finally {
        jest.useRealTimers();
      }
    });
  });

  // ---- Streaming mode ----

  describe("streaming mode", () => {
    let currentMockStream: {
      stream: ReadableStream<Uint8Array>;
      controller: MockSSEStreamController;
    } | null = null;

    function setupStreamingMocks(
      rest: {
        data?: { date: string; content: string; timestamp?: string }[];
        hasMore?: boolean;
      } = {},
    ) {
      mockAuthFetch.mockImplementation(async (url: any, opts?: any) => {
        if (typeof url === "string" && url.includes("/stream")) {
          currentMockStream = createMockSSEStream(opts?.signal);
          return { ok: true, body: currentMockStream.stream } as any;
        }
        return restResponse(rest);
      });
    }

    beforeEach(() => {
      currentMockStream = null;
      setupStreamingMocks();
    });

    afterEach(() => {
      currentMockStream?.controller.close();
    });

    it("fetches history via REST and connects SSE", async () => {
      setupStreamingMocks({
        data: [
          logEntry({ content: "oldest" }),
          logEntry({ content: "newest" }),
        ],
      });

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", streaming: true }),
      );

      await waitFor(() => {
        expect(result.current.connected).toBe(true);
        expect(result.current.logs).toHaveLength(2);
      });

      expect(result.current.logs.map((l) => l.content)).toEqual([
        "oldest",
        "newest",
      ]);
      expect(streamCallCount()).toBe(1);
      // The history read, then a read once the stream is open
      await waitFor(() => expect(restCallCount()).toBe(2));
    });

    it("appends SSE update events after history loads", async () => {
      setupStreamingMocks({
        data: [logEntry({ content: "Initial" })],
      });

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", streaming: true }),
      );

      await waitFor(() => {
        expect(result.current.connected).toBe(true);
        expect(result.current.logs).toHaveLength(1);
      });

      act(() => {
        currentMockStream!.controller.push("update", [
          logEntry({
            content: "New log entry",
            timestamp: "2024-01-01T10:02:00Z",
          }),
        ]);
      });

      await waitFor(() => {
        expect(result.current.logs).toHaveLength(2);
        expect(result.current.logs[1].content).toBe("New log entry");
        expect(result.current.logs[1].timestamp).toBe("2024-01-01T10:02:00Z");
        expect(result.current.lastUpdate).not.toBeNull();
      });
    });

    it("shows a live line once when the history already holds it", async () => {
      const stored = {
        key: "1792713599999000000-0",
        date: "20261022",
        timestamp: "2026-10-22T23:59:59.999Z",
        content: '{"_key":"1792713599999000000-0","message":"filled"}',
      };
      setupStreamingMocks({ data: [stored] });

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", streaming: true }),
      );

      await waitFor(() => {
        expect(result.current.connected).toBe(true);
        expect(result.current.logs).toHaveLength(1);
      });

      act(() => {
        currentMockStream!.controller.push("update", [stored]);
        currentMockStream!.controller.push("update", [
          logEntry({ content: "after" }),
        ]);
      });

      await waitFor(() => {
        expect(result.current.logs.map((l) => l.key)).toEqual([
          "1792713599999000000-0",
          "after",
        ]);
      });
    });

    it("shows a repeated live chunk once", async () => {
      setupStreamingMocks();

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", streaming: true }),
      );

      await waitFor(() => {
        expect(result.current.connected).toBe(true);
        expect(result.current.loading).toBe(false);
      });

      const chunk = [
        logEntry({ content: "a", key: "1792713599999000000-0" }),
        logEntry({ content: "b", key: "1792713599999000000-1" }),
      ];
      act(() => {
        currentMockStream!.controller.push("update", chunk);
        currentMockStream!.controller.push("update", chunk);
        currentMockStream!.controller.push("update", [
          logEntry({ content: "c" }),
        ]);
      });

      await waitFor(() => {
        expect(result.current.logs.map((l) => l.content)).toEqual([
          "a",
          "b",
          "c",
        ]);
      });
    });

    it("ignores SSE history events (history comes from REST)", async () => {
      setupStreamingMocks({
        data: [logEntry({ content: "REST history" })],
      });

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", streaming: true }),
      );

      await waitFor(() => {
        expect(result.current.connected).toBe(true);
        expect(result.current.logs).toHaveLength(1);
      });

      act(() => {
        currentMockStream!.controller.push("history", [
          logEntry({ content: "SSE history dupe" }),
        ]);
      });

      await act(async () => {
        await new Promise((r) => setTimeout(r, 50));
      });

      expect(result.current.logs).toHaveLength(1);
      expect(result.current.logs[0].content).toBe("REST history");
    });

    it("buffers updates arriving before history resolves, then merges without duplicates", async () => {
      let resolveRest: (value: any) => void;
      mockAuthFetch.mockImplementation(async (url: any, opts?: any) => {
        if (typeof url === "string" && url.includes("/stream")) {
          currentMockStream = createMockSSEStream(opts?.signal);
          return { ok: true, body: currentMockStream.stream } as any;
        }
        return new Promise((resolve) => {
          resolveRest = resolve;
        });
      });

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", streaming: true }),
      );

      await waitFor(() => expect(result.current.connected).toBe(true));

      // Updates arrive while REST history is still in flight
      act(() => {
        currentMockStream!.controller.push("update", [
          logEntry({
            content: "overlap line",
            timestamp: "2024-01-01T10:01:00Z",
          }),
        ]);
        currentMockStream!.controller.push("update", [
          logEntry({
            content: "fresh line",
            timestamp: "2024-01-01T10:02:00Z",
          }),
        ]);
      });

      // History resolves already containing the overlapping line
      await act(async () => {
        resolveRest!(
          restResponse({
            data: [
              {
                content: "old line",
                timestamp: "2024-01-01T10:00:00Z",
              },
              {
                content: "overlap line",
                timestamp: "2024-01-01T10:01:00Z",
              },
            ],
          }),
        );
      });

      await waitFor(() => {
        expect(result.current.logs).toHaveLength(3);
      });
      expect(result.current.logs.map((l) => l.content)).toEqual([
        "old line",
        "overlap line",
        "fresh line",
      ]);
    });

    it("buffers SSE updates during a refresh replace fetch and merges them after", async () => {
      let liveStream: ReturnType<typeof createMockSSEStream> | null = null;
      const restResolvers: ((value: any) => void)[] = [];
      let restCall = 0;
      mockAuthFetch.mockImplementation(async (url: any, opts?: any) => {
        if (typeof url === "string" && url.includes("/stream")) {
          liveStream = createMockSSEStream(opts?.signal);
          return { ok: true, body: liveStream.stream } as any;
        }
        restCall++;
        if (restCall === 1) {
          return restResponse({
            data: [logEntry({ content: "history line" })],
          });
        }
        return new Promise((resolve) => {
          restResolvers.push(resolve);
        });
      });

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", streaming: true }),
      );

      await waitFor(() => {
        expect(result.current.connected).toBe(true);
        expect(result.current.logs).toHaveLength(1);
      });

      // refresh() starts a replace fetch (which hangs) and reconnects SSE
      act(() => {
        result.current.refresh();
      });

      await waitFor(() => expect(result.current.connected).toBe(true));

      // An update arrives while the replace fetch is still in flight
      act(() => {
        liveStream!.controller.push("update", [
          logEntry({
            content: "mid-refresh line",
            timestamp: "2024-01-01T10:05:00Z",
          }),
        ]);
      });

      await act(async () => {
        await new Promise((r) => setTimeout(r, 20));
      });

      // The replace resolves WITHOUT the mid-refresh line (it raced the read)
      await act(async () => {
        restResolvers.forEach((resolve) =>
          resolve(
            restResponse({
              data: [logEntry({ content: "history line" })],
            }),
          ),
        );
      });

      // The buffered update must survive the replace, not be overwritten
      await waitFor(() => {
        expect(result.current.logs.map((l) => l.content)).toEqual([
          "history line",
          "mid-refresh line",
        ]);
      });
    });

    it("starts fallback polling when the SSE stream ends cleanly", async () => {
      jest.useFakeTimers();
      try {
        let liveStream: ReturnType<typeof createMockSSEStream> | null = null;
        mockAuthFetch.mockImplementation(async (url: any, opts?: any) => {
          if (typeof url === "string" && url.includes("/stream")) {
            liveStream = createMockSSEStream(opts?.signal);
            return { ok: true, body: liveStream.stream } as any;
          }
          return restResponse();
        });

        const { result } = renderHook(() =>
          useBotLogs({
            botId: "bot-1",
            streaming: true,
            refreshInterval: 5000,
          }),
        );

        await act(async () => {
          await jest.advanceTimersByTimeAsync(100);
        });
        expect(result.current.connected).toBe(true);
        const restCallsBefore = restCallCount();

        // Server closes the stream cleanly (e.g. API restart, proxy recycle)
        act(() => {
          liveStream!.controller.close();
        });
        await act(async () => {
          await jest.advanceTimersByTimeAsync(100);
        });
        expect(result.current.connected).toBe(false);

        // Live updates are gone - polling must take over
        await act(async () => {
          await jest.advanceTimersByTimeAsync(11000);
        });
        expect(restCallCount()).toBeGreaterThanOrEqual(restCallsBefore + 2);
      } finally {
        jest.useRealTimers();
      }
    });

    it("falls back to polling when SSE fails with a network error", async () => {
      jest.useFakeTimers();
      try {
        mockAuthFetch.mockImplementation(async (url: any) => {
          if (typeof url === "string" && url.includes("/stream")) {
            throw new Error("Connection refused");
          }
          return restResponse({
            data: [logEntry({ content: "REST log" })],
          });
        });

        const { result } = renderHook(() =>
          useBotLogs({
            botId: "bot-1",
            streaming: true,
            refreshInterval: 5000,
          }),
        );

        await act(async () => {
          await jest.advanceTimersByTimeAsync(100);
        });
        expect(result.current.connected).toBe(false);
        const initial = restCallCount();
        expect(initial).toBeGreaterThan(0);

        await act(async () => {
          await jest.advanceTimersByTimeAsync(11000);
        });
        expect(restCallCount()).toBeGreaterThanOrEqual(initial + 2);
      } finally {
        jest.useRealTimers();
      }
    });

    it("does not start fallback polling when refreshInterval is 0", async () => {
      jest.useFakeTimers();
      try {
        mockAuthFetch.mockImplementation(async (url: any) => {
          if (typeof url === "string" && url.includes("/stream")) {
            throw new Error("Connection refused");
          }
          return restResponse();
        });

        renderHook(() =>
          useBotLogs({ botId: "bot-1", streaming: true, refreshInterval: 0 }),
        );

        await act(async () => {
          await jest.advanceTimersByTimeAsync(100);
        });
        const initial = restCallCount();

        await act(async () => {
          await jest.advanceTimersByTimeAsync(60000);
        });
        expect(restCallCount()).toBe(initial);
      } finally {
        jest.useRealTimers();
      }
    });

    it("does NOT start fallback polling when the SSE abort is intentional (unmount)", async () => {
      // The stream mock errors pending reads with TypeError("Failed to fetch")
      // on abort, mimicking real browsers. An intentional teardown must not
      // be mistaken for a connection failure that triggers fallback polling.
      jest.useFakeTimers();
      try {
        const { result, unmount } = renderHook(() =>
          useBotLogs({
            botId: "bot-1",
            streaming: true,
            refreshInterval: 5000,
          }),
        );

        await act(async () => {
          await jest.advanceTimersByTimeAsync(100);
        });
        expect(result.current.connected).toBe(true);

        unmount();
        const callsAtUnmount = restCallCount();

        await act(async () => {
          await jest.advanceTimersByTimeAsync(60000);
        });

        expect(restCallCount()).toBe(callsAtUnmount);
        expect(mockToast).not.toHaveBeenCalled();
      } finally {
        jest.useRealTimers();
      }
    });

    it("disconnects SSE and fetches via REST when a date filter is set", async () => {
      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", streaming: true }),
      );

      await waitFor(() => expect(result.current.connected).toBe(true));

      setupStreamingMocks({
        data: [logEntry({ content: "Historical log" })],
      });

      act(() => {
        result.current.setDateFilter("20240101");
      });

      await waitFor(() => {
        expect(result.current.connected).toBe(false);
        expect(
          result.current.logs.some((l) => l.content === "Historical log"),
        ).toBe(true);
      });
      expect(mockAuthFetch).toHaveBeenCalledWith(
        expect.stringContaining("date=20240101"),
        expect.any(Object),
      );
      // The intentional SSE teardown must not surface an error or toast
      expect(mockToast).not.toHaveBeenCalled();
      expect(result.current.error).toBeNull();
    });

    it("reconnects SSE when the date filter is cleared", async () => {
      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", streaming: true }),
      );

      await waitFor(() => expect(result.current.connected).toBe(true));

      act(() => {
        result.current.setDateFilter("20240101");
      });
      await waitFor(() => expect(result.current.connected).toBe(false));

      act(() => {
        result.current.setDateFilter(null);
      });
      await waitFor(() => expect(result.current.connected).toBe(true));
    });

    it("uses -- separator for ISO datetime ranges", async () => {
      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", streaming: true }),
      );

      await waitFor(() => expect(result.current.connected).toBe(true));

      act(() => {
        result.current.setDateRangeFilter(
          "2024-01-01T00:00:00Z",
          "2024-01-02T00:00:00Z",
        );
      });

      await waitFor(() => {
        const restUrls = mockAuthFetch.mock.calls
          .map((c) => c[0] as string)
          .filter((u) => !u.includes("/stream"));
        expect(
          restUrls.some((u) =>
            u.includes(
              `dateRange=${encodeURIComponent("2024-01-01T00:00:00Z--2024-01-02T00:00:00Z")}`,
            ),
          ),
        ).toBe(true);
      });
    });

    it("resets state when botId changes", async () => {
      setupStreamingMocks({
        data: [logEntry({ content: "Bot A log" })],
      });

      const { result, rerender } = renderHook(
        ({ id }) => useBotLogs({ botId: id, streaming: true }),
        { initialProps: { id: "bot-a" } },
      );

      await waitFor(() => expect(result.current.logs).toHaveLength(1));

      setupStreamingMocks({ data: [] });
      rerender({ id: "bot-b" });

      await waitFor(() => {
        expect(result.current.logs).toEqual([]);
      });
    });

    it("handles rapid botId changes without errors or toasts", async () => {
      const { result, rerender } = renderHook(
        ({ id }) => useBotLogs({ botId: id, streaming: true }),
        { initialProps: { id: "bot-a" } },
      );

      await waitFor(() => expect(result.current.connected).toBe(true));

      for (let i = 0; i < 5; i++) {
        rerender({ id: i % 2 === 0 ? "bot-b" : "bot-a" });
      }

      await act(async () => {
        await new Promise((r) => setTimeout(r, 50));
      });

      expect(result.current.error).toBeNull();
      expect(mockToast).not.toHaveBeenCalled();
    });

    it("does not reconnect SSE when refreshInterval changes", async () => {
      const { result, rerender } = renderHook(
        ({ interval }) =>
          useBotLogs({
            botId: "bot-1",
            streaming: true,
            refreshInterval: interval,
          }),
        { initialProps: { interval: 30000 } },
      );

      await waitFor(() => expect(result.current.connected).toBe(true));
      const before = streamCallCount();

      rerender({ interval: 10000 });

      await act(async () => {
        await new Promise((r) => setTimeout(r, 100));
      });

      expect(streamCallCount()).toBe(before);
      expect(result.current.connected).toBe(true);
    });

    it("caps entries at 10000 when updates exceed the cap", async () => {
      const entries = Array.from({ length: 10000 }, (_, i) =>
        logEntry({ content: `Entry ${i}` }),
      );
      setupStreamingMocks({ data: entries });

      const { result } = renderHook(() =>
        useBotLogs({
          botId: "bot-1",
          streaming: true,
          initialQuery: { limit: 10000, sort: "asc" },
        }),
      );

      await waitFor(() => expect(result.current.logs).toHaveLength(10000));

      act(() => {
        currentMockStream!.controller.push("update", [
          logEntry({ content: "New entry", timestamp: "2024-01-01T11:00:00Z" }),
        ]);
      });

      await waitFor(() => {
        expect(result.current.logs.length).toBeLessThanOrEqual(10000);
        expect(
          result.current.logs[result.current.logs.length - 1].content,
        ).toBe("New entry");
      });
    });

    it("exposes hasEarlierLogs when the initial live fetch has more", async () => {
      setupStreamingMocks({
        data: [logEntry({ content: "Log" })],
        hasMore: true,
        nextCursor: "20240101:0",
      });

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", streaming: true }),
      );

      await waitFor(() => {
        expect(result.current.hasEarlierLogs).toBe(true);
      });
    });

    it("prepends earlier logs without duplicating existing entries", async () => {
      setupStreamingMocks({
        data: [logEntry({ content: "Current log" })],
        hasMore: true,
        nextCursor: "20240101:0",
      });

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", streaming: true }),
      );

      await waitFor(() => expect(result.current.logs).toHaveLength(1));

      setupStreamingMocks({
        data: [
          logEntry({ content: "Current log" }),
          logEntry({ content: "Earlier log" }),
        ],
        hasMore: false,
      });

      await act(async () => {
        result.current.loadEarlierLogs();
      });

      await waitFor(() => {
        expect(result.current.logs).toHaveLength(2);
        expect(result.current.logs[0].content).toBe("Earlier log");
        expect(result.current.logs[1].content).toBe("Current log");
      });
    });

    it("filters SSE updates to metric lines when query type is metrics", async () => {
      // The REST history is server-filtered by type=metrics; live updates
      // must honor the same filter client-side or dashboards would see
      // plain log lines leaking into their metric event stream.
      setupStreamingMocks({ data: [] });

      const { result } = renderHook(() =>
        useBotLogs({
          botId: "bot-1",
          streaming: true,
          initialQuery: {
            limit: 100,
            sort: "desc",
            type: "metrics",
          },
        }),
      );

      await waitFor(() => {
        expect(result.current.connected).toBe(true);
        expect(result.current.loading).toBe(false);
      });

      act(() => {
        currentMockStream!.controller.push("update", [
          logEntry({
            content: JSON.stringify({ _metric: "trade", price: 100 }),
            timestamp: "2024-01-01T10:01:00Z",
          }),
        ]);
        currentMockStream!.controller.push("update", [
          logEntry({
            content: "plain non-metric log line",
            timestamp: "2024-01-01T10:02:00Z",
          }),
        ]);
      });

      await waitFor(() => {
        expect(result.current.logs).toHaveLength(1);
      });
      await act(async () => {
        await new Promise((r) => setTimeout(r, 50));
      });

      expect(result.current.logs).toHaveLength(1);
      expect(result.current.logs[0].content).toContain("_metric");
    });

    it("supports metrics type in initialQuery for the history fetch", async () => {
      renderHook(() =>
        useBotLogs({
          botId: "bot-1",
          streaming: true,
          initialQuery: {
            limit: 100,
            sort: "desc",
            type: "metrics",
          },
        }),
      );

      await waitFor(() => {
        const restUrls = mockAuthFetch.mock.calls
          .map((c) => c[0] as string)
          .filter((u) => !u.includes("/stream"));
        expect(restUrls.some((u) => u.includes("type=metrics"))).toBe(true);
      });
    });
  });

  describe("background refresh loading semantics", () => {
    const logsResponse = {
      ok: true,
      json: async () => ({
        data: [logEntry({ content: "Log" })],
        hasMore: false,
      }),
    } as Response;

    it("keeps loading false when a polling tick refetches after data has loaded", async () => {
      jest.useFakeTimers();
      try {
        // First fetch resolves immediately; the polling fetch hangs so we can
        // observe state while it is in flight
        let resolvePoll: (r: Response) => void;
        mockAuthFetch
          .mockResolvedValueOnce(logsResponse)
          .mockImplementationOnce(
            () => new Promise<Response>((r) => (resolvePoll = r)),
          );

        const { result } = renderHook(() =>
          useBotLogs({
            botId: "bot-1",
            autoRefresh: true,
            refreshInterval: 30000,
          }),
        );

        await act(async () => {
          await jest.advanceTimersByTimeAsync(0);
        });
        expect(result.current.loading).toBe(false);
        expect(result.current.logs.length).toBe(1);

        // Fire the polling tick; its fetch is now in flight
        await act(async () => {
          await jest.advanceTimersByTimeAsync(30000);
        });

        expect(result.current.loading).toBe(false);
        expect(result.current.isFetching).toBe(true);
        // Stale data must remain visible during the background refetch
        expect(result.current.logs.length).toBe(1);

        await act(async () => {
          resolvePoll!(logsResponse);
          await jest.advanceTimersByTimeAsync(0);
        });
        expect(result.current.isFetching).toBe(false);
      } finally {
        jest.useRealTimers();
      }
    });

    it("keeps loading false during manual refresh once data has loaded", async () => {
      mockAuthFetch.mockResolvedValue(logsResponse);

      const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });

      act(() => {
        result.current.refresh();
      });

      expect(result.current.loading).toBe(false);

      await waitFor(() => {
        expect(result.current.isFetching).toBe(false);
      });
    });

    it("keeps loading false on filter change once data has loaded (stale-while-revalidate)", async () => {
      mockAuthFetch.mockResolvedValue(logsResponse);

      const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });

      act(() => {
        result.current.setDateRangeFilter("20260401", "20260403");
      });

      expect(result.current.loading).toBe(false);

      await waitFor(() => {
        expect(result.current.isFetching).toBe(false);
      });
    });

    it("sets loading again when the bot changes (no stale data across bots)", async () => {
      mockAuthFetch.mockResolvedValue(logsResponse);

      const { result, rerender } = renderHook(
        ({ botId }) => useBotLogs({ botId }),
        { initialProps: { botId: "bot-1" } },
      );

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });

      rerender({ botId: "bot-2" });

      expect(result.current.loading).toBe(true);

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });
    });

    it("recovers full loading state when the initial fetch fails (no data to show)", async () => {
      mockAuthFetch.mockRejectedValueOnce(new Error("network down"));

      const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));

      await waitFor(() => {
        expect(result.current.error).toBe("network down");
      });

      // With no data yet, a retry is an initial load again, not a background one
      mockAuthFetch.mockResolvedValue(logsResponse);
      act(() => {
        result.current.refresh();
      });
      expect(result.current.loading).toBe(true);

      await waitFor(() => {
        expect(result.current.loading).toBe(false);
      });
      expect(result.current.logs.length).toBe(1);
    });
  });

  describe("exporting logs", () => {
    it("toasts instead of downloading when there are no logs", async () => {
      mockAuthFetch.mockResolvedValue(restResponse());
      const createSpy = jest.spyOn(document, "createElement");

      const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));
      await waitFor(() => expect(result.current.loading).toBe(false));

      act(() => {
        result.current.exportLogs();
      });

      expect(mockToast).toHaveBeenCalledWith({
        title: "No logs to export",
        description: "There are no logs available to export.",
        variant: "destructive",
      });
      expect(createSpy).not.toHaveBeenCalledWith("a");
      createSpy.mockRestore();
    });

    it("downloads the loaded lines as dated plain text", async () => {
      mockAuthFetch.mockResolvedValue(
        restResponse({
          data: [
            { content: "first", date: "20240101" },
            { content: "second", date: "20240101" },
          ],
        }),
      );
      const originalCreate = URL.createObjectURL;
      const originalRevoke = URL.revokeObjectURL;
      const blobs: Blob[] = [];
      URL.createObjectURL = jest.fn((b: Blob) => {
        blobs.push(b);
        return "blob:logs";
      });
      URL.revokeObjectURL = jest.fn();
      const anchor = document.createElement("a");
      const click = jest.spyOn(anchor, "click").mockImplementation(() => {});
      const createSpy = jest
        .spyOn(document, "createElement")
        .mockImplementation(() => anchor);

      try {
        const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));
        await waitFor(() => expect(result.current.logs).toHaveLength(2));

        act(() => {
          result.current.exportLogs();
        });

        const today = new Date().toISOString().split("T")[0];
        expect(anchor.download).toBe(`bot-bot-1-logs-${today}.txt`);
        expect(anchor.href).toBe("blob:logs");
        expect(click).toHaveBeenCalledTimes(1);
        expect(URL.revokeObjectURL).toHaveBeenCalledWith("blob:logs");
        expect(blobs[0].type).toBe("text/plain");
        expect(await blobs[0].text()).toBe(
          "[20240101] first\n[20240101] second",
        );
      } finally {
        createSpy.mockRestore();
        URL.createObjectURL = originalCreate;
        URL.revokeObjectURL = originalRevoke;
      }
    });
  });

  describe("pagination failures", () => {
    it("surfaces a loadMore failure as an error without a toast or query change", async () => {
      mockAuthFetch.mockResolvedValueOnce(
        restResponse({
          data: [logEntry({ content: "page one" })],
          hasMore: true,
          nextCursor: "20240101:0",
        }),
      );

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", initialQuery: { limit: 1 } }),
      );
      await waitFor(() => expect(result.current.hasMore).toBe(true));

      mockAuthFetch.mockRejectedValueOnce(new Error("page two failed"));
      await act(async () => {
        await result.current.loadMore();
      });

      expect(result.current.error).toBe("page two failed");
      expect(mockToast).not.toHaveBeenCalled();
      expect(result.current.logs.map((l) => l.content)).toEqual(["page one"]);
      expect(result.current.loadingMore).toBe(false);
    });

    it("ignores loadMore when the API reported no further pages", async () => {
      mockAuthFetch.mockResolvedValue(
        restResponse({
          data: [logEntry({ content: "only page" })],
        }),
      );

      const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));
      await waitFor(() => expect(result.current.logs).toHaveLength(1));

      await act(async () => {
        await result.current.loadMore();
      });

      expect(restCallCount()).toBe(1);
    });
  });

  describe("history order and page direction", () => {
    it("offers older pages at the bottom of a newest-first view", async () => {
      mockAuthFetch.mockResolvedValue(
        restResponse({
          data: [
            logEntry({ content: "newest" }),
            logEntry({ content: "oldest" }),
          ],
          hasMore: true,
        }),
      );

      const { result } = renderHook(() =>
        useBotLogs({
          botId: "bot-1",
          streaming: true,
          initialQuery: { limit: 2, sort: "desc" },
        }),
      );
      await waitFor(() => expect(result.current.logs).toHaveLength(2));

      expect(result.current.logs.map((l) => l.content)).toEqual([
        "newest",
        "oldest",
      ]);
      expect(result.current.hasMore).toBe(true);
      expect(result.current.hasEarlierLogs).toBe(false);

      mockAuthFetch.mockResolvedValue(
        restResponse({ data: [logEntry({ content: "older still" })] }),
      );
      await act(async () => {
        await result.current.loadMore();
      });

      expect(result.current.logs.map((l) => l.content)).toEqual([
        "newest",
        "oldest",
        "older still",
      ]);
    });

    it("offers older pages at the top of a chronological view", async () => {
      mockAuthFetch.mockResolvedValue(
        restResponse({
          data: [
            logEntry({ content: "oldest" }),
            logEntry({ content: "newest" }),
          ],
          hasMore: true,
        }),
      );

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", initialQuery: { limit: 2, sort: "asc" } }),
      );
      await waitFor(() => expect(result.current.logs).toHaveLength(2));

      expect(result.current.hasEarlierLogs).toBe(true);
      expect(result.current.hasMore).toBe(false);

      mockAuthFetch.mockResolvedValue(
        restResponse({ data: [logEntry({ content: "older still" })] }),
      );
      await act(async () => {
        await result.current.loadEarlierLogs();
      });

      expect(result.current.logs.map((l) => l.content)).toEqual([
        "older still",
        "oldest",
        "newest",
      ]);
    });

    it("puts live lines on the newest side of a newest-first view", async () => {
      let stream: ReturnType<typeof createMockSSEStream> | null = null;
      mockAuthFetch.mockImplementation(async (url: any, opts?: any) => {
        if (typeof url === "string" && url.includes("/stream")) {
          stream = createMockSSEStream(opts?.signal);
          return { ok: true, body: stream.stream } as any;
        }
        return restResponse({
          data: [
            logEntry({ content: "newer" }),
            logEntry({ content: "older" }),
          ],
        });
      });

      try {
        const { result } = renderHook(() =>
          useBotLogs({
            botId: "bot-1",
            streaming: true,
            initialQuery: { limit: 2, sort: "desc" },
          }),
        );
        await waitFor(() => {
          expect(result.current.connected).toBe(true);
          expect(result.current.logs).toHaveLength(2);
        });

        act(() => {
          stream!.controller.push("update", [
            logEntry({ content: "live 1" }),
            logEntry({ content: "live 2" }),
          ]);
        });

        await waitFor(() => {
          expect(result.current.logs.map((l) => l.content)).toEqual([
            "live 2",
            "live 1",
            "newer",
            "older",
          ]);
        });
      } finally {
        (
          stream as ReturnType<typeof createMockSSEStream> | null
        )?.controller.close();
      }
    });
  });

  describe("paging older entries", () => {
    it("pages older entries before each page's nextCursor", async () => {
      mockAuthFetch
        .mockResolvedValueOnce(
          restResponse({
            data: [logEntry({ content: "newest" })],
            hasMore: true,
            nextCursor: "20240101:200",
          }),
        )
        .mockResolvedValueOnce(
          restResponse({
            data: [logEntry({ content: "older" })],
            hasMore: true,
            nextCursor: "20240101:100",
          }),
        )
        .mockResolvedValueOnce(
          restResponse({
            data: [logEntry({ content: "oldest" })],
            hasMore: false,
            nextCursor: null,
          }),
        );

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", initialQuery: { limit: 1 } }),
      );
      await waitFor(() => expect(result.current.hasMore).toBe(true));

      await act(async () => {
        await result.current.loadMore();
      });
      await act(async () => {
        await result.current.loadMore();
      });

      const urls = mockAuthFetch.mock.calls.map((c) => c[0] as string);
      expect(urls[0]).not.toContain("before=");
      expect(urls[1]).toContain("before=20240101%3A200");
      expect(urls[2]).toContain("before=20240101%3A100");
      urls.forEach((url) => expect(url).not.toContain("offset="));
      expect(result.current.logs.map((l) => l.content)).toEqual([
        "newest",
        "older",
        "oldest",
      ]);
      expect(result.current.hasMore).toBe(false);
    });

    it("loads earlier logs before the history's nextCursor, never a live line's key", async () => {
      const streams: ReturnType<typeof createMockSSEStream>[] = [];
      mockAuthFetch.mockImplementation(async (url: any, opts?: any) => {
        if (typeof url === "string" && url.includes("/stream")) {
          const s = createMockSSEStream(opts?.signal);
          streams.push(s);
          return { ok: true, body: s.stream } as any;
        }
        return !String(url).includes("before=")
          ? restResponse({
              data: [logEntry({ content: "Current" })],
              hasMore: true,
              nextCursor: "20240101:200",
            })
          : restResponse({
              data: [logEntry({ content: "Earlier" })],
              hasMore: true,
              nextCursor: "20240101:100",
            });
      });

      try {
        const { result } = renderHook(() =>
          useBotLogs({
            botId: "bot-1",
            streaming: true,
            initialQuery: { limit: 2, sort: "asc" },
          }),
        );
        await waitFor(() => {
          expect(result.current.connected).toBe(true);
          expect(result.current.hasEarlierLogs).toBe(true);
        });
        expect(result.current.hasMore).toBe(false);

        act(() => {
          streams[0].controller.push("update", [
            logEntry({ content: "live", key: "1792713599999000000-0" }),
          ]);
        });
        await waitFor(() => expect(result.current.logs).toHaveLength(2));

        await act(async () => {
          await result.current.loadEarlierLogs();
        });
        await act(async () => {
          await result.current.loadEarlierLogs();
        });

        const restUrls = mockAuthFetch.mock.calls
          .map((c) => c[0] as string)
          .filter((u) => !u.includes("/stream"));
        const pageUrls = restUrls.filter((u) => u.includes("before="));
        expect(pageUrls[0]).toContain("before=20240101%3A200");
        expect(pageUrls[1]).toContain("before=20240101%3A100");
        restUrls.forEach((url) => expect(url).not.toContain("offset="));
        expect(result.current.loadingEarlier).toBe(false);
      } finally {
        streams.forEach((s) => s.controller.close());
      }
    });

    it("keeps live lines flowing when an older page loads during a refresh", async () => {
      let stream: ReturnType<typeof createMockSSEStream> | null = null;
      let restCalls = 0;
      let answerRefresh: (() => void) | null = null;
      const history = restResponse({
        data: [logEntry({ content: "current" })],
        hasMore: true,
        nextCursor: "20240101:200",
      });
      mockAuthFetch.mockImplementation(
        async (input: RequestInfo | URL, opts?: RequestInit) => {
          const url = String(input);
          if (url.includes("/stream")) {
            stream = createMockSSEStream(opts?.signal ?? undefined);
            return { ok: true, body: stream.stream } as Response;
          }
          restCalls++;
          if (restCalls === 2) {
            // The refresh: answers when told, or fails like fetch on abort
            return new Promise<Response>((resolve, reject) => {
              answerRefresh = () => resolve(history);
              opts?.signal?.addEventListener("abort", () =>
                reject(new DOMException("aborted", "AbortError")),
              );
            });
          }
          return restCalls === 1
            ? history
            : restResponse({ data: [logEntry({ content: "earlier" })] });
        },
      );

      try {
        const { result } = renderHook(() =>
          useBotLogs({
            botId: "bot-1",
            streaming: true,
            initialQuery: { limit: 1, sort: "asc" },
          }),
        );
        await waitFor(() => {
          expect(result.current.connected).toBe(true);
          expect(result.current.hasEarlierLogs).toBe(true);
        });

        act(() => {
          result.current.refresh();
        });
        await act(async () => {
          await result.current.loadEarlierLogs();
        });
        await act(async () => {
          answerRefresh?.();
        });
        act(() => {
          stream!.controller.push("update", [logEntry({ content: "live" })]);
        });

        await waitFor(() =>
          expect(result.current.logs.map((l) => l.content)).toContain("live"),
        );
      } finally {
        (
          stream as ReturnType<typeof createMockSSEStream> | null
        )?.controller.close();
      }
    });

    it("keeps loaded earlier pages when a poll fires", async () => {
      jest.useFakeTimers();
      try {
        mockAuthFetch.mockResolvedValueOnce(
          restResponse({
            data: [logEntry({ content: "current" })],
            hasMore: true,
          }),
        );
        const { result } = renderHook(() =>
          useBotLogs({
            botId: "bot-1",
            autoRefresh: true,
            refreshInterval: 5000,
            initialQuery: { limit: 1, sort: "asc" },
          }),
        );
        await act(async () => {
          await jest.advanceTimersByTimeAsync(0);
        });
        expect(result.current.hasEarlierLogs).toBe(true);

        mockAuthFetch.mockResolvedValueOnce(
          restResponse({ data: [logEntry({ content: "earlier" })] }),
        );
        await act(async () => {
          await result.current.loadEarlierLogs();
        });
        expect(result.current.logs).toHaveLength(2);

        mockAuthFetch.mockResolvedValue(
          restResponse({ data: [logEntry({ content: "current" })] }),
        );
        await act(async () => {
          await jest.advanceTimersByTimeAsync(5000);
        });

        expect(result.current.logs.map((l) => l.content)).toEqual([
          "earlier",
          "current",
        ]);
      } finally {
        jest.useRealTimers();
      }
    });

    it("starts again from the newest page when the query changes after paging", async () => {
      mockAuthFetch.mockResolvedValue(
        restResponse({
          data: [logEntry({ content: "row" })],
          hasMore: true,
          nextCursor: "20240101:100",
        }),
      );

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", initialQuery: { limit: 1 } }),
      );
      await waitFor(() => expect(result.current.hasMore).toBe(true));

      await act(async () => {
        await result.current.loadMore();
      });
      expect(mockAuthFetch.mock.calls.at(-1)![0]).toContain("before=");

      act(() => {
        result.current.updateQuery({ type: "metrics" });
      });

      const lastUrl = mockAuthFetch.mock.calls.at(-1)![0] as string;
      expect(lastUrl).not.toContain("before=");
      expect(lastUrl).toContain("type=metrics");
    });
  });

  describe("live update parsing", () => {
    it("drops an update event without log content and keeps streaming later ones", async () => {
      let stream: ReturnType<typeof createMockSSEStream> | null = null;
      mockAuthFetch.mockImplementation(async (url: any, opts?: any) => {
        if (typeof url === "string" && url.includes("/stream")) {
          stream = createMockSSEStream(opts?.signal);
          return { ok: true, body: stream.stream } as any;
        }
        return restResponse({
          data: [logEntry({ content: "history" })],
        });
      });
      const errorSpy = jest
        .spyOn(console, "error")
        .mockImplementation(() => {});

      try {
        const { result } = renderHook(() =>
          useBotLogs({ botId: "bot-1", streaming: true }),
        );
        await waitFor(() => {
          expect(result.current.connected).toBe(true);
          expect(result.current.logs).toHaveLength(1);
        });

        act(() => {
          stream!.controller.push("update", "{not json");
          stream!.controller.push("update", [
            logEntry({
              content: "after bad line",
              timestamp: "2024-01-01T10:05:00Z",
            }),
          ]);
        });

        await waitFor(() => {
          expect(result.current.logs.map((l) => l.content)).toEqual([
            "history",
            "after bad line",
          ]);
        });
        expect(errorSpy).toHaveBeenCalledWith(
          "Failed to parse update SSE event:",
          expect.any(Error),
        );
      } finally {
        errorSpy.mockRestore();
        (
          stream as ReturnType<typeof createMockSSEStream> | null
        )?.controller.close();
      }
    });
  });
});
