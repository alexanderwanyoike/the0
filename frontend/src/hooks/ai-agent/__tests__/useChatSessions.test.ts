import { renderHook, act } from "@testing-library/react";
import { useChatSessions } from "../useChatSessions";
import { useChatStore } from "@/stores/ai-agent/chatStore";
import { chatService } from "@/lib/ai-agent/chatService";

// Mock the dependencies
jest.mock("@/stores/ai-agent/chatStore");
jest.mock("@/lib/ai-agent/chatService");
const mockPush = jest.fn();
jest.mock("next/navigation", () => ({
  useRouter: jest.fn(() => ({
    push: mockPush,
  })),
}));

const mockChatService = chatService as jest.Mocked<typeof chatService>;

describe("useChatSessions - Infinite Loop Prevention Tests", () => {
  const mockSetSessions = jest.fn();
  const mockSetCurrentSession = jest.fn();
  const mockLoadSessionMessages = jest.fn();
  const mockCreateNewSession = jest.fn();
  const mockSetSessionId = jest.fn();

  beforeEach(() => {
    jest.clearAllMocks();

    // Mock useChatStore
    (useChatStore as unknown as jest.Mock).mockReturnValue({
      sessions: [],
      currentSessionId: null,
      messages: [],
      setSessions: mockSetSessions,
      setCurrentSession: mockSetCurrentSession,
      loadSessionMessages: mockLoadSessionMessages,
      createNewSession: mockCreateNewSession,
      setSessionId: mockSetSessionId,
    });

    // Mock chatService
    mockChatService.getSessions.mockResolvedValue([
      {
        id: "session1",
        title: "Test Session",
        created_at: "2024-01-01",
        updated_at: "2024-01-01",
        is_active: true,
      },
    ]);
    mockChatService.getMostRecentSession.mockResolvedValue({
      id: "session1",
      title: "Test Session",
      created_at: "2024-01-01",
      updated_at: "2024-01-01",
      is_active: true,
    });
    mockChatService.getSession.mockResolvedValue({
      id: "session1",
      messages: [],
    });
    mockChatService.convertToMessages.mockReturnValue([]);
  });

  it("should not cause infinite loops from function references", () => {
    const { result, rerender } = renderHook(() => useChatSessions());

    // Get initial function references
    const initialLoadSessions = result.current.loadSessions;
    const initialLoadSessionsAndRestore = result.current.loadSessionsAndRestore;
    const initialSwitchToSession = result.current.switchToSession;

    // Re-render multiple times
    rerender({});
    rerender({});
    rerender({});

    // These functions should be stable when store functions don't change
    // Note: In real usage, they may change when store updates, but that's expected
    // This test ensures useCallback is working when dependencies are stable
    expect(result.current.loadSessions).toBe(initialLoadSessions);
    expect(result.current.loadSessionsAndRestore).toBe(
      initialLoadSessionsAndRestore,
    );
    expect(result.current.switchToSession).toBe(initialSwitchToSession);
  });

  it("should only load sessions once on mount", async () => {
    const { rerender } = renderHook(() => useChatSessions());

    // Wait for initial load
    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, 0));
    });

    // Should load sessions once
    expect(mockChatService.getSessions).toHaveBeenCalledTimes(1);

    // Re-render multiple times
    rerender({});
    rerender({});
    rerender({});

    // Should not load sessions again
    expect(mockChatService.getSessions).toHaveBeenCalledTimes(1);
  });

  it("should not cause loops when store functions change", async () => {
    const { rerender } = renderHook(() => useChatSessions());

    // Wait for initial load
    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, 0));
    });

    // Should load sessions once initially
    expect(mockChatService.getSessions).toHaveBeenCalledTimes(1);

    // Change store functions to simulate store updates
    (useChatStore as unknown as jest.Mock).mockReturnValue({
      sessions: [],
      currentSessionId: null,
      messages: [],
      setSessions: jest.fn(), // Different function reference
      setCurrentSession: jest.fn(), // Different function reference
      loadSessionMessages: jest.fn(), // Different function reference
      createNewSession: jest.fn(), // Different function reference
      setSessionId: jest.fn(), // Different function reference
    });

    // Re-render multiple times
    rerender({});
    rerender({});

    // Should cause one additional load due to useEffect re-running with new setSessions
    // This is expected behavior when dependencies change
    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, 0));
    });

    // The hook should be optimized to not re-run when store functions change
    // This prevents infinite loops and unnecessary API calls
    expect(mockChatService.getSessions).toHaveBeenCalledTimes(1);
  });

  it("should handle loadSessionsAndRestore without infinite loops", async () => {
    const { result } = renderHook(() => useChatSessions());

    // Call loadSessionsAndRestore multiple times
    await act(async () => {
      await result.current.loadSessionsAndRestore();
      await result.current.loadSessionsAndRestore();
      await result.current.loadSessionsAndRestore();
    });

    // Should not cause excessive API calls
    // Note: getSessions is called once on mount + once per loadSessionsAndRestore call
    expect(mockChatService.getSessions).toHaveBeenCalledTimes(4); // 1 mount + 3 calls
    expect(mockChatService.getMostRecentSession).toHaveBeenCalledTimes(3); // Only from explicit calls
  });

  it("should prevent infinite loops on session load errors", async () => {
    // Mock session loading to fail with 500 error
    mockChatService.getSession.mockRejectedValue(
      new Error("HTTP 500: Internal Server Error"),
    );

    const { result } = renderHook(() => useChatSessions());

    await act(async () => {
      await result.current.loadSession("failing-session");
    });

    // Should not create new session on server errors (prevents loops)
    expect(mockCreateNewSession).not.toHaveBeenCalled();

    // Should handle 404 errors by creating new session
    mockChatService.getSession.mockRejectedValue(
      new Error("HTTP 404: Not Found"),
    );

    await act(async () => {
      await result.current.loadSession("missing-session");
    });

    // Should create new session on client errors
    expect(mockCreateNewSession).toHaveBeenCalledTimes(1);
  });

  it("should not cause navigation loops", async () => {
    // Clear previous calls to mockPush
    mockPush.mockClear();

    // Mock window.location.pathname
    Object.defineProperty(window, "location", {
      value: {
        pathname: "/ai-agent",
      },
      writable: true,
    });

    const { result } = renderHook(() => useChatSessions());

    // Switch to same session multiple times
    await act(async () => {
      await result.current.switchToSession("session1");
      await result.current.switchToSession("session1");
      await result.current.switchToSession("session1");
    });

    // Should navigate each time since path is different
    expect(mockPush).toHaveBeenCalledTimes(3);
    expect(mockPush).toHaveBeenCalledWith("/ai-agent/session/session1");

    // Mock being on session page already
    Object.defineProperty(window, "location", {
      value: {
        pathname: "/ai-agent/session/session1",
      },
    });

    mockPush.mockClear();

    await act(async () => {
      await result.current.switchToSession("session1");
    });

    // Should not navigate if already on session page
    expect(mockPush).not.toHaveBeenCalled();
    // But should still load session data
    expect(mockChatService.getSession).toHaveBeenCalledWith("session1");
  });

  it("should provide stable function references across re-renders", () => {
    const { result, rerender } = renderHook(() => useChatSessions());

    const functions1 = {
      loadSessions: result.current.loadSessions,
      loadSession: result.current.loadSession,
      loadSessionsAndRestore: result.current.loadSessionsAndRestore,
      startNewChat: result.current.startNewChat,
      switchToSession: result.current.switchToSession,
      refreshSessions: result.current.refreshSessions,
    };

    // Re-render without changing the store mock
    rerender({});

    const functions2 = {
      loadSessions: result.current.loadSessions,
      loadSession: result.current.loadSession,
      loadSessionsAndRestore: result.current.loadSessionsAndRestore,
      startNewChat: result.current.startNewChat,
      switchToSession: result.current.switchToSession,
      refreshSessions: result.current.refreshSessions,
    };

    // All functions should maintain reference equality when dependencies don't change
    Object.keys(functions1).forEach((key) => {
      expect(functions2[key as keyof typeof functions2]).toBe(
        functions1[key as keyof typeof functions1],
      );
    });
  });
});
