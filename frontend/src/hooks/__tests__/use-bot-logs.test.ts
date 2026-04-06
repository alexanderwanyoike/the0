import { renderHook, waitFor, act } from "@testing-library/react";
import { useBotLogs } from "../use-bot-logs";
import { useAuth } from "@/contexts/auth-context";
import { useToast } from "@/hooks/use-toast";
import { authFetch } from "@/lib/auth-fetch";

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

const mockUseAuth = useAuth as jest.MockedFunction<typeof useAuth>;
const mockUseToast = useToast as jest.MockedFunction<typeof useToast>;
const mockAuthFetch = authFetch as jest.MockedFunction<typeof authFetch>;

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
    // Initial fetch returns 1500 entries
    const initialLogs = Array.from({ length: 1500 }, (_, i) => ({
      date: "2024-01-01",
      content: `Line ${i}`,
    }));
    mockAuthFetch
      .mockResolvedValueOnce({
        ok: true,
        json: async () => ({
          data: initialLogs,
          total: 2100,
          hasMore: true,
        }),
      } as Response)
      .mockResolvedValueOnce({
        ok: true,
        json: async () => ({
          data: Array.from({ length: 600 }, (_, i) => ({
            date: "2024-01-01",
            content: `Extra ${i}`,
          })),
          total: 2100,
          hasMore: false,
        }),
      } as Response);

    const { result } = renderHook(() =>
      useBotLogs({ botId: "bot-1", initialQuery: { limit: 1500, offset: 0 } }),
    );

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.logs).toHaveLength(1500);

    await act(async () => {
      await result.current.loadMore();
    });

    // Should be capped at 2000, oldest trimmed
    expect(result.current.logs).toHaveLength(2000);
    // First entry should be Line 100 (100 oldest trimmed from 1500 + 600 = 2100)
    expect(result.current.logs[0].content).toBe("Line 100");
    expect(result.current.logs[result.current.logs.length - 1].content).toBe(
      "Extra 599",
    );
  });

  it("should cap logs on initial fetch if response is very large", async () => {
    const hugeLogs = Array.from({ length: 2500 }, (_, i) => ({
      date: "2024-01-01",
      content: `Huge ${i}`,
    }));
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: hugeLogs,
        total: 2500,
        hasMore: false,
      }),
    } as Response);

    const { result } = renderHook(() =>
      useBotLogs({ botId: "bot-1", initialQuery: { limit: 3000, offset: 0 } }),
    );

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.logs).toHaveLength(2000);
    expect(result.current.logs[0].content).toBe("Huge 500");
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
});
