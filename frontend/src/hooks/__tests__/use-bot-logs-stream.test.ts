import { renderHook, waitFor, act } from "@testing-library/react";
import { useBotLogsStream } from "../use-bot-logs-stream";
import { useAuth } from "@/contexts/auth-context";
import { useToast } from "@/hooks/use-toast";
import { authFetch } from "@/lib/auth-fetch";
import {
  createAuthenticatedSSEUrl,
  validateSSEAuth,
} from "@/lib/sse/sse-auth";

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
  createAuthenticatedSSEUrl: jest.fn(),
  validateSSEAuth: jest.fn(),
}));

const mockUseAuth = useAuth as jest.MockedFunction<typeof useAuth>;
const mockUseToast = useToast as jest.MockedFunction<typeof useToast>;
const mockAuthFetch = authFetch as jest.MockedFunction<typeof authFetch>;
const mockCreateAuthenticatedSSEUrl =
  createAuthenticatedSSEUrl as jest.MockedFunction<
    typeof createAuthenticatedSSEUrl
  >;
const mockValidateSSEAuth = validateSSEAuth as jest.MockedFunction<
  typeof validateSSEAuth
>;

// ---- EventSource mock ----

type EventSourceListener = (event: MessageEvent) => void;

class MockEventSource {
  static instances: MockEventSource[] = [];

  url: string;
  withCredentials: boolean;
  readyState: number = 0; // CONNECTING
  onopen: (() => void) | null = null;
  onerror: ((event: Event) => void) | null = null;
  onmessage: ((event: MessageEvent) => void) | null = null;

  private listeners: Record<string, EventSourceListener[]> = {};

  constructor(url: string, options?: { withCredentials?: boolean }) {
    this.url = url;
    this.withCredentials = options?.withCredentials ?? false;
    MockEventSource.instances.push(this);
  }

  addEventListener(type: string, listener: EventSourceListener) {
    if (!this.listeners[type]) {
      this.listeners[type] = [];
    }
    this.listeners[type].push(listener);
  }

  removeEventListener(type: string, listener: EventSourceListener) {
    if (this.listeners[type]) {
      this.listeners[type] = this.listeners[type].filter(
        (l) => l !== listener,
      );
    }
  }

  close() {
    this.readyState = 2; // CLOSED
  }

  // Test helpers

  simulateOpen() {
    this.readyState = 1; // OPEN
    this.onopen?.();
  }

  simulateEvent(type: string, data: any) {
    const event = new MessageEvent(type, {
      data: JSON.stringify(data),
    });
    const handlers = this.listeners[type] || [];
    handlers.forEach((h) => h(event));
  }

  simulateError() {
    this.readyState = 2;
    this.onerror?.(new Event("error"));
  }
}

// Install the mock globally
(global as any).EventSource = MockEventSource;

describe("useBotLogsStream", () => {
  const mockToast = jest.fn();

  beforeEach(() => {
    jest.clearAllMocks();
    jest.useFakeTimers();
    MockEventSource.instances = [];

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

    mockCreateAuthenticatedSSEUrl.mockReturnValue({
      success: true,
      data: { url: "/api/logs/bot-1/stream", isAuthenticated: true },
    } as any);
  });

  afterEach(() => {
    jest.useRealTimers();
  });

  it("should return the full interface including connected and lastUpdate", () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    // Standard useBotLogs fields
    expect(result.current).toHaveProperty("logs");
    expect(result.current).toHaveProperty("loading");
    expect(result.current).toHaveProperty("error");
    expect(result.current).toHaveProperty("hasMore");
    expect(result.current).toHaveProperty("total");
    expect(result.current).toHaveProperty("refresh");
    expect(result.current).toHaveProperty("exportLogs");
    expect(result.current).toHaveProperty("setDateFilter");
    expect(result.current).toHaveProperty("setDateRangeFilter");

    // Streaming-specific fields
    expect(result.current).toHaveProperty("connected");
    expect(result.current).toHaveProperty("lastUpdate");
    expect(result.current.connected).toBe(false);
    expect(result.current.lastUpdate).toBeNull();
  });

  it("should connect via SSE and handle history event with line expansion", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    // An EventSource should have been created
    expect(MockEventSource.instances.length).toBe(1);
    const es = MockEventSource.instances[0];

    // Simulate SSE open
    act(() => {
      es.simulateOpen();
    });

    expect(result.current.connected).toBe(true);

    // Simulate history event with multi-line content
    act(() => {
      es.simulateEvent("history", [
        { date: "2024-01-01T10:00:00Z", content: "Line 1\nLine 2" },
        { date: "2024-01-01T10:01:00Z", content: "Line 3" },
      ]);
    });

    expect(result.current.logs).toHaveLength(3);
    expect(result.current.logs[0].content).toBe("Line 1");
    expect(result.current.logs[1].content).toBe("Line 2");
    expect(result.current.logs[2].content).toBe("Line 3");
    expect(result.current.lastUpdate).not.toBeNull();
    expect(result.current.total).toBe(3);
  });

  it("should handle update events with line expansion and append to existing logs", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    const es = MockEventSource.instances[0];

    act(() => {
      es.simulateOpen();
    });

    // Set initial logs via history
    act(() => {
      es.simulateEvent("history", [
        { date: "2024-01-01T10:00:00Z", content: "Initial log" },
      ]);
    });

    expect(result.current.logs).toHaveLength(1);

    // Simulate update event with multi-line content
    act(() => {
      es.simulateEvent("update", {
        content: "New line A\nNew line B",
        timestamp: "2024-01-01T10:02:00Z",
      });
    });

    expect(result.current.logs).toHaveLength(3);
    expect(result.current.logs[1].content).toBe("New line A");
    expect(result.current.logs[2].content).toBe("New line B");
    expect(result.current.logs[2].date).toBe("2024-01-01T10:02:00Z");
  });

  it("should fall back to REST polling after 4 second timeout", async () => {
    // Make SSE auth fail so no EventSource is created
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

    // No EventSource because SSE auth failed
    expect(MockEventSource.instances.length).toBe(0);

    // REST fetch should not have been called yet (waiting for timeout)
    expect(mockAuthFetch).not.toHaveBeenCalled();

    // Advance past the 4-second fallback timeout
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
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [{ date: "2024-01-01", content: "Fallback log" }],
        total: 1,
        hasMore: false,
      }),
    } as Response);

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    const es = MockEventSource.instances[0];

    // Simulate SSE error before any data is received
    await act(async () => {
      es.simulateError();
    });

    await waitFor(() => {
      expect(mockAuthFetch).toHaveBeenCalled();
    });

    await waitFor(() => {
      expect(result.current.logs).toHaveLength(1);
      expect(result.current.logs[0].content).toBe("Fallback log");
    });
  });

  it("should clean up EventSource on unmount", () => {
    const { unmount } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    expect(MockEventSource.instances.length).toBe(1);
    const es = MockEventSource.instances[0];

    act(() => {
      es.simulateOpen();
    });

    unmount();

    expect(es.readyState).toBe(2); // CLOSED
  });

  it("should export current log state", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    const es = MockEventSource.instances[0];

    act(() => {
      es.simulateOpen();
    });

    act(() => {
      es.simulateEvent("history", [
        { date: "2024-01-01T10:00:00Z", content: "Export me" },
      ]);
    });

    // Mock DOM APIs for export AFTER renderHook so the container is already created
    const mockClick = jest.fn();
    const originalCreateObjectURL = global.URL.createObjectURL;
    const originalRevokeObjectURL = global.URL.revokeObjectURL;
    const mockCreateObjectURL = jest.fn(() => "blob:test-url");
    const mockRevokeObjectURL = jest.fn();
    global.URL.createObjectURL = mockCreateObjectURL;
    global.URL.revokeObjectURL = mockRevokeObjectURL;

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

    expect(mockCreateObjectURL).toHaveBeenCalled();
    expect(mockClick).toHaveBeenCalled();
    expect(mockRevokeObjectURL).toHaveBeenCalledWith("blob:test-url");
    expect(mockToast).toHaveBeenCalledWith(
      expect.objectContaining({
        description: "Logs exported successfully",
      }),
    );

    // Restore only the specific spies (not all mocks, which would break module-level mocks)
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

  it("should switch to REST when date filter is set", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [{ date: "2024-01-15", content: "Historical log" }],
        total: 1,
        hasMore: false,
      }),
    } as Response);

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    const es = MockEventSource.instances[0];

    act(() => {
      es.simulateOpen();
    });

    expect(result.current.connected).toBe(true);

    // Set a date filter -- should close SSE and fetch via REST
    await act(async () => {
      result.current.setDateFilter("20240115");
    });

    // SSE should be closed
    expect(es.readyState).toBe(2);
    expect(result.current.connected).toBe(false);

    await waitFor(() => {
      expect(mockAuthFetch).toHaveBeenCalled();
    });

    const calledUrl = mockAuthFetch.mock.calls[0][0] as string;
    expect(calledUrl).toContain("date=20240115");
  });

  it("should reconnect SSE when clearing date filter", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [{ date: "2024-01-15", content: "Historical log" }],
        total: 1,
        hasMore: false,
      }),
    } as Response);

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    const es = MockEventSource.instances[0];

    act(() => {
      es.simulateOpen();
    });

    // Set a date filter -- should close SSE
    await act(async () => {
      result.current.setDateFilter("20240115");
    });

    expect(es.readyState).toBe(2);

    // Clear the date filter -- should reconnect SSE
    await act(async () => {
      result.current.setDateFilter(null);
    });

    // A new EventSource should have been created
    expect(MockEventSource.instances.length).toBe(2);
    const es2 = MockEventSource.instances[1];

    act(() => {
      es2.simulateOpen();
    });

    expect(result.current.connected).toBe(true);
  });

  it("should attempt SSE reconnection with backoff after post-load drop", async () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    const es = MockEventSource.instances[0];

    act(() => {
      es.simulateOpen();
    });

    // Deliver history so initialLoadCompleteRef is true
    act(() => {
      es.simulateEvent("history", [
        { date: "2024-01-01T10:00:00Z", content: "Log line" },
      ]);
    });

    expect(result.current.logs).toHaveLength(1);

    // Simulate SSE error after initial load
    act(() => {
      es.simulateError();
    });

    expect(result.current.connected).toBe(false);

    // Should NOT have called REST fetch (initial load was complete)
    expect(mockAuthFetch).not.toHaveBeenCalled();

    // Advance past the 1s backoff (first attempt: 1000 * 2^0 = 1000ms)
    act(() => {
      jest.advanceTimersByTime(1000);
    });

    // A new EventSource should have been created for reconnection
    expect(MockEventSource.instances.length).toBe(2);
  });

  it("should switch to REST when date range filter is set", async () => {
    mockAuthFetch.mockResolvedValue({
      ok: true,
      json: async () => ({
        data: [],
        total: 0,
        hasMore: false,
      }),
    } as Response);

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    const es = MockEventSource.instances[0];

    act(() => {
      es.simulateOpen();
    });

    await act(async () => {
      result.current.setDateRangeFilter("20240101", "20240115");
    });

    expect(es.readyState).toBe(2);

    await waitFor(() => {
      expect(mockAuthFetch).toHaveBeenCalled();
    });

    const calledUrl = mockAuthFetch.mock.calls[0][0] as string;
    expect(calledUrl).toContain("dateRange=20240101-20240115");
  });

  it("should not create EventSource when botId is empty", () => {
    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "" }),
    );

    expect(MockEventSource.instances.length).toBe(0);
    expect(result.current.loading).toBe(false);
    expect(result.current.logs).toEqual([]);
  });

  it("should not create EventSource when user is null", () => {
    mockUseAuth.mockReturnValue({
      user: null,
    } as any);

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    expect(MockEventSource.instances.length).toBe(0);
    expect(result.current.loading).toBe(false);
  });

  it("should handle REST fetch error with toast notification", async () => {
    // Make SSE auth fail so we go through the 4s fallback path
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

    // Advance past the 4-second fallback timeout to trigger REST fetch
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

  it("should gracefully handle malformed SSE history event data", () => {
    const consoleSpy = jest
      .spyOn(console, "error")
      .mockImplementation(() => {});

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    const es = MockEventSource.instances[0];

    act(() => {
      es.simulateOpen();
    });

    // Send malformed data directly via addEventListener handler
    const event = new MessageEvent("history", {
      data: "not valid json{{{",
    });
    const handlers = (es as any).listeners["history"] || [];
    act(() => {
      handlers.forEach((h: any) => h(event));
    });

    // Should not crash -- logs remain empty, error logged to console
    expect(result.current.logs).toEqual([]);
    expect(consoleSpy).toHaveBeenCalledWith(
      "Failed to parse history SSE event:",
      expect.any(Error),
    );

    consoleSpy.mockRestore();
  });

  it("should gracefully handle malformed SSE update event data", () => {
    const consoleSpy = jest
      .spyOn(console, "error")
      .mockImplementation(() => {});

    const { result } = renderHook(() =>
      useBotLogsStream({ botId: "bot-1" }),
    );

    const es = MockEventSource.instances[0];

    act(() => {
      es.simulateOpen();
    });

    // First send valid history
    act(() => {
      es.simulateEvent("history", [
        { date: "2024-01-01T10:00:00Z", content: "Initial" },
      ]);
    });

    expect(result.current.logs).toHaveLength(1);

    // Send malformed update data
    const event = new MessageEvent("update", {
      data: "{{invalid json",
    });
    const handlers = (es as any).listeners["update"] || [];
    act(() => {
      handlers.forEach((h: any) => h(event));
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

    mockCreateAuthenticatedSSEUrl.mockImplementation((endpoint: string) => ({
      success: true as const,
      data: { url: endpoint, isAuthenticated: true },
    }));

    renderHook(() =>
      useBotLogsStream({ botId: specialBotId }),
    );

    // The SSE URL should have the botId encoded
    expect(mockCreateAuthenticatedSSEUrl).toHaveBeenCalledWith(
      `/api/logs/${encodeURIComponent(specialBotId)}/stream`,
    );
  });
});
