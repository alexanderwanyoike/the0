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

  it("should handle authentication error", async () => {
    mockUseAuth.mockReturnValue({
      user: null,
    } as any);

    const { result } = renderHook(() => useBotLogs({ botId: "bot-1" }));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.error).toBe("User not authenticated");
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
