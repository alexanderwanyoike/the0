import { renderHook, waitFor, act } from "@testing-library/react";
import { useBotLogs } from "../use-bot-logs";
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

function restResponse({
  data = [],
  total = data.length,
  hasMore = false,
}: {
  data?: { date: string; content: string; timestamp?: string }[];
  total?: number;
  hasMore?: boolean;
} = {}) {
  return {
    ok: true,
    json: async () => ({ data, total, hasMore }),
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
    { date: "2024-01-01T10:00:00Z", content: "Log line 1\nLog line 2" },
    { date: "2024-01-01T10:01:00Z", content: "Log line 3" },
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
      json: async () => ({ data: mockLogs, total: 2, hasMore: false }),
    } as Response);

    const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));

    expect(result.current.loading).toBe(true);

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });
  });

  it("should fetch and expand logs successfully", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({ data: mockLogs, total: 3, hasMore: false }),
    } as Response);

    const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    // Logs should be expanded (each line becomes a separate entry)
    expect(result.current.logs.length).toBe(3); // "Log line 1", "Log line 2", "Log line 3"
    expect(result.current.logs[0].content).toBe("Log line 1");
    expect(result.current.logs[1].content).toBe("Log line 2");
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
          data: [{ date: "2024-01-01", content: "Page 1" }],
          total: 2,
          hasMore: true,
        }),
      } as Response)
      .mockResolvedValueOnce({
        ok: true,
        json: async () => ({
          data: [{ date: "2024-01-01", content: "Page 2" }],
          total: 2,
          hasMore: false,
        }),
      } as Response);

    const { result } = renderHook(() =>
      useBotLogs({ botId: "bot-1", initialQuery: { limit: 1, offset: 0 } }),
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
        data: [{ date: "2024-01-01", content: "Log entry" }],
        total: 1,
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
    const initialLogs = Array.from({ length: 8000 }, (_, i) => ({
      date: "2024-01-01",
      content: `Line ${i}`,
    }));
    mockAuthFetch
      .mockResolvedValueOnce({
        ok: true,
        json: async () => ({
          data: initialLogs,
          total: 11000,
          hasMore: true,
        }),
      } as Response)
      .mockResolvedValueOnce({
        ok: true,
        json: async () => ({
          data: Array.from({ length: 3000 }, (_, i) => ({
            date: "2024-01-01",
            content: `Extra ${i}`,
          })),
          total: 11000,
          hasMore: false,
        }),
      } as Response);

    const { result } = renderHook(() =>
      useBotLogs({ botId: "bot-1", initialQuery: { limit: 8000, offset: 0 } }),
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
    const hugeLogs = Array.from({ length: 12000 }, (_, i) => ({
      date: "2024-01-01",
      content: `Huge ${i}`,
    }));
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: hugeLogs,
        total: 12000,
        hasMore: false,
      }),
    } as Response);

    const { result } = renderHook(() =>
      useBotLogs({ botId: "bot-1", initialQuery: { limit: 15000, offset: 0 } }),
    );

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    // Capped at 10000 (MAX_LOG_ENTRIES), oldest trimmed
    expect(result.current.logs).toHaveLength(10000);
    expect(result.current.logs[0].content).toBe("Huge 2000");
  });

  it("should preserve timestamp field when expanding log entries", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [
          {
            date: "20260403",
            content: "line1\nline2",
            timestamp: "2026-04-03T10:00:00Z",
          },
        ],
        total: 2,
        hasMore: false,
      }),
    } as Response);

    const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    // Both expanded lines should carry the original timestamp
    expect(result.current.logs).toHaveLength(2);
    expect(result.current.logs[0].timestamp).toBe("2026-04-03T10:00:00Z");
    expect(result.current.logs[1].timestamp).toBe("2026-04-03T10:00:00Z");
  });

  // ---- Navigation bugs ----

  it("should reset state when botId changes", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [{ date: "20240101", content: "Bot A log" }],
        total: 1,
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
            data: [{ date: "20240101", content: "Bot A" }],
            total: 1,
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
        data: [{ date: "20240101", content: "Bot C" }],
        total: 1,
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
        data: [{ date: "20240101", content: "Log" }],
        total: 1,
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
      json: async () => ({ data: [], total: 0, hasMore: false }),
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
      json: async () => ({ data: [], total: 0, hasMore: false }),
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
        data: [{ date: "20260401", content: "Log" }],
        total: 1,
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
          data: [{ date: "20260401", content: "Log" }],
          total: 1,
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
    const initialLogs = Array.from({ length: 2000 }, (_, i) => ({
      date: "2024-01-01",
      content: `Line ${i}`,
    }));
    mockAuthFetch
      .mockResolvedValueOnce({
        ok: true,
        json: async () => ({
          data: initialLogs,
          total: 4000,
          hasMore: true,
        }),
      } as Response)
      .mockResolvedValueOnce({
        ok: true,
        json: async () => ({
          data: Array.from({ length: 2000 }, (_, i) => ({
            date: "2024-01-01",
            content: `Extra ${i}`,
          })),
          total: 4000,
          hasMore: false,
        }),
      } as Response);

    const { result } = renderHook(() =>
      useBotLogs({ botId: "bot-1", initialQuery: { limit: 2000, offset: 0 } }),
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

  it("should advance offset on consecutive loadMore calls", async () => {
    let callCount = 0;
    mockAuthFetch.mockImplementation(async () => {
      callCount++;
      return {
        ok: true,
        json: async () => ({
          data: [{ date: "2024-01-01", content: `Page ${callCount}` }],
          total: 10,
          hasMore: true,
        }),
      } as Response;
    });

    const { result } = renderHook(() =>
      useBotLogs({ botId: "bot-1", initialQuery: { limit: 1, offset: 0 } }),
    );

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    // First loadMore: offset should be 1
    await act(async () => {
      await result.current.loadMore();
    });

    const firstLoadMoreUrl = mockAuthFetch.mock.calls[1][0] as string;
    expect(firstLoadMoreUrl).toContain("offset=1");

    // Second loadMore: offset should be 2 (not 1 again)
    await act(async () => {
      await result.current.loadMore();
    });

    const secondLoadMoreUrl = mockAuthFetch.mock.calls[2][0] as string;
    expect(secondLoadMoreUrl).toContain("offset=2");
  });

  it("should not overwrite paginated data when auto-refresh fires", async () => {
    jest.useFakeTimers();
    try {
      mockAuthFetch.mockResolvedValue({
        ok: true,
        json: async () => ({
          data: [{ date: "2024-01-01", content: "Page 1" }],
          total: 2,
          hasMore: true,
        }),
      } as Response);

      const { result } = renderHook(() =>
        useBotLogs({
          botId: "bot-1",
          autoRefresh: true,
          refreshInterval: 5000,
          initialQuery: { limit: 1, offset: 0 },
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
          data: [{ date: "2024-01-01", content: "Page 2" }],
          total: 2,
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
      json: async () => ({ data: [], total: 0, hasMore: false }),
    } as Response);

    renderHook(() =>
      useBotLogs({
        botId: "bot-1",
        initialQuery: { limit: 50, offset: 10 },
      }),
    );

    await waitFor(() => {
      expect(mockAuthFetch).toHaveBeenCalled();
    });

    const calledUrl = mockAuthFetch.mock.calls[0][0] as string;
    expect(calledUrl).toContain("/api/logs/bot-1");
    expect(calledUrl).toContain("limit=50");
    expect(calledUrl).toContain("offset=10");
  });

  describe("latest mode (lookbackDays)", () => {
    beforeEach(() => {
      mockAuthFetch.mockResolvedValue({
        ok: true,
        json: async () => ({ data: [], total: 0, hasMore: false }),
      } as Response);
    });

    it("sends lookbackDays instead of defaulting date to today", async () => {
      renderHook(() =>
        useBotLogs({
          botId: "bot-1",
          initialQuery: { limit: 100, offset: 0, sort: "desc", lookbackDays: 30 },
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
          initialQuery: { limit: 100, offset: 0, lookbackDays: 30 },
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
          initialQuery: { limit: 100, offset: 0, lookbackDays: 30 },
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

  it("should include sort=desc in URL by default", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({ data: [], total: 0, hasMore: false }),
    } as Response);

    renderHook(() => useBotLogs({ botId: "bot-1" }));

    await waitFor(() => {
      expect(mockAuthFetch).toHaveBeenCalled();
    });

    const url = mockAuthFetch.mock.calls[0][0] as string;
    expect(url).toContain("sort=desc");
  });

  it("should pass sort=asc when query sort is changed", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({ data: [], total: 0, hasMore: false }),
    } as Response);

    const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    mockAuthFetch.mockClear();

    act(() => {
      result.current.updateQuery({ sort: "asc" });
    });

    await waitFor(() => {
      expect(mockAuthFetch).toHaveBeenCalled();
    });

    const url = mockAuthFetch.mock.calls[0][0] as string;
    expect(url).toContain("sort=asc");
  });

  it("should reconfigure polling when refreshInterval changes", async () => {
    jest.useFakeTimers();
    try {
      mockAuthFetch.mockResolvedValue({
        ok: true,
        json: async () => ({
          data: [{ date: "20260406", content: "Log" }],
          total: 1,
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
          data: [{ date: "20260406", content: "Log" }],
          total: 1,
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
            data: [{ date: "20240101", content: "Page 1" }],
            total: 2,
            hasMore: true,
          });
        }
        return new Promise((resolve) => {
          resolveSecond = resolve;
        });
      });

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", initialQuery: { limit: 1, offset: 0 } }),
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
            data: [{ date: "20240101", content: "Page 2" }],
            total: 2,
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
              data: [{ date: "20240101", content: "Page 1" }],
              total: 2,
              hasMore: true,
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
            initialQuery: { limit: 1, offset: 0 },
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
              data: [{ date: "20240101", content: "Page 2" }],
              total: 2,
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
          useBotLogs({ botId: "bot-1", autoRefresh: true, refreshInterval: 5000 }),
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

    function setupStreamingMocks(rest: {
      data?: { date: string; content: string; timestamp?: string }[];
      total?: number;
      hasMore?: boolean;
    } = {}) {
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
          { date: "2024-01-01T10:01:00Z", content: "newest" },
          { date: "2024-01-01T10:00:00Z", content: "oldest" },
        ],
        total: 2,
      });

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", streaming: true }),
      );

      await waitFor(() => {
        expect(result.current.connected).toBe(true);
        expect(result.current.logs).toHaveLength(2);
      });

      // History arrives desc from the API but is displayed chronologically
      // (oldest first) so live appends land at the bottom.
      expect(result.current.logs[0].content).toBe("oldest");
      expect(result.current.logs[1].content).toBe("newest");
      expect(streamCallCount()).toBe(1);
      expect(restCallCount()).toBe(1);
    });

    it("appends SSE update events after history loads", async () => {
      setupStreamingMocks({
        data: [{ date: "2024-01-01T10:00:00Z", content: "Initial" }],
      });

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", streaming: true }),
      );

      await waitFor(() => {
        expect(result.current.connected).toBe(true);
        expect(result.current.logs).toHaveLength(1);
      });

      act(() => {
        currentMockStream!.controller.push("update", {
          content: "New log entry",
          timestamp: "2024-01-01T10:02:00Z",
        });
      });

      await waitFor(() => {
        expect(result.current.logs).toHaveLength(2);
        expect(result.current.logs[1].content).toBe("New log entry");
        expect(result.current.logs[1].timestamp).toBe("2024-01-01T10:02:00Z");
        expect(result.current.lastUpdate).not.toBeNull();
      });
    });

    it("ignores SSE history events (history comes from REST)", async () => {
      setupStreamingMocks({
        data: [{ date: "2024-01-01T10:00:00Z", content: "REST history" }],
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
          { date: "2024-01-01T09:00:00Z", content: "SSE history dupe" },
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
        currentMockStream!.controller.push("update", {
          content: "overlap line",
          timestamp: "2024-01-01T10:01:00Z",
        });
        currentMockStream!.controller.push("update", {
          content: "fresh line",
          timestamp: "2024-01-01T10:02:00Z",
        });
      });

      // History resolves already containing the overlapping line
      await act(async () => {
        resolveRest!(
          restResponse({
            data: [
              {
                date: "2024-01-01T10:01:00Z",
                content: "overlap line",
                timestamp: "2024-01-01T10:01:00Z",
              },
              {
                date: "2024-01-01T10:00:00Z",
                content: "old line",
                timestamp: "2024-01-01T10:00:00Z",
              },
            ],
            total: 2,
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
            data: [{ date: "2024-01-01T10:00:00Z", content: "history line" }],
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
        liveStream!.controller.push("update", {
          content: "mid-refresh line",
          timestamp: "2024-01-01T10:05:00Z",
        });
      });

      await act(async () => {
        await new Promise((r) => setTimeout(r, 20));
      });

      // The replace resolves WITHOUT the mid-refresh line (it raced the read)
      await act(async () => {
        restResolvers.forEach((resolve) =>
          resolve(
            restResponse({
              data: [{ date: "2024-01-01T10:00:00Z", content: "history line" }],
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
          useBotLogs({ botId: "bot-1", streaming: true, refreshInterval: 5000 }),
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
            data: [{ date: "2024-01-01T10:00:00Z", content: "REST log" }],
          });
        });

        const { result } = renderHook(() =>
          useBotLogs({ botId: "bot-1", streaming: true, refreshInterval: 5000 }),
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
          useBotLogs({ botId: "bot-1", streaming: true, refreshInterval: 5000 }),
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
        data: [{ date: "20240101", content: "Historical log" }],
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
        data: [{ date: "2024-01-01T10:00:00Z", content: "Bot A log" }],
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
        expect(result.current.total).toBe(0);
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
      const entries = Array.from({ length: 10000 }, (_, i) => ({
        date: "2024-01-01T10:00:00Z",
        content: `Entry ${i}`,
      }));
      setupStreamingMocks({ data: entries, total: 10000 });

      const { result } = renderHook(() =>
        useBotLogs({
          botId: "bot-1",
          streaming: true,
          initialQuery: { limit: 10000, offset: 0, sort: "desc" },
        }),
      );

      await waitFor(() => expect(result.current.logs).toHaveLength(10000));

      act(() => {
        currentMockStream!.controller.push("update", {
          content: "New entry",
          timestamp: "2024-01-01T11:00:00Z",
        });
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
        data: [{ date: "2024-01-01T10:00:00Z", content: "Log" }],
        total: 500,
        hasMore: true,
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
        data: [{ date: "2024-01-01T10:00:00Z", content: "Current log" }],
        total: 2,
        hasMore: true,
      });

      const { result } = renderHook(() =>
        useBotLogs({ botId: "bot-1", streaming: true }),
      );

      await waitFor(() => expect(result.current.logs).toHaveLength(1));

      setupStreamingMocks({
        data: [
          { date: "2024-01-01T10:00:00Z", content: "Current log" },
          { date: "2024-01-01T09:00:00Z", content: "Earlier log" },
        ],
        total: 2,
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
          initialQuery: { limit: 100, offset: 0, sort: "desc", type: "metrics" },
        }),
      );

      await waitFor(() => {
        expect(result.current.connected).toBe(true);
        expect(result.current.loading).toBe(false);
      });

      act(() => {
        currentMockStream!.controller.push("update", {
          content: JSON.stringify({ _metric: "trade", price: 100 }),
          timestamp: "2024-01-01T10:01:00Z",
        });
        currentMockStream!.controller.push("update", {
          content: "plain non-metric log line",
          timestamp: "2024-01-01T10:02:00Z",
        });
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
          initialQuery: { limit: 100, offset: 0, sort: "desc", type: "metrics" },
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
});
