import { render, screen, waitFor } from "@testing-library/react";
import { describe, it, expect, beforeEach, jest } from "@jest/globals";
import AIAgentPage from "../page";
import { useArtifactsStore } from "@/stores/ai-agent/artifactsStore";
import { useChatSessions } from "@/hooks/ai-agent/useChatSessions";
import { apiService } from "@/lib/ai-agent/api";

// Mock all dependencies
const mockUseArtifactsStore = jest.fn();
jest.mock("@/stores/ai-agent/artifactsStore", () => ({
  useArtifactsStore: mockUseArtifactsStore,
}));
jest.mock("@/hooks/ai-agent/useArtifacts", () => ({
  useArtifacts: () => ({
    files: [],
    isLoading: false,
    error: null,
  }),
}));
const mockUseChatSessions = jest.fn();
jest.mock("@/hooks/ai-agent/useChatSessions", () => ({
  useChatSessions: mockUseChatSessions,
}));
const mockCheckApiKeyStatus = jest.fn();
jest.mock("@/lib/ai-agent/api", () => ({
  apiService: {
    checkApiKeyStatus: mockCheckApiKeyStatus,
  },
}));
jest.mock("@/components/auth/with-auth", () => ({
  withAuth: (Component: any) => Component,
}));
jest.mock("@/components/layouts/dashboard-layout", () => {
  return function DashboardLayout({ children }: { children: React.ReactNode }) {
    return <div data-testid="dashboard-layout">{children}</div>;
  };
});
jest.mock("@/components/ai-agent/chat/ChatPanel", () => ({
  ChatPanel: () => <div data-testid="chat-panel">Chat Panel</div>,
}));
jest.mock("@/components/ai-agent/chat/ChatList", () => ({
  ChatList: ({ onSessionSelect, onNewChat }: any) => (
    <div data-testid="chat-list">
      <button onClick={() => onSessionSelect("test-session")}>
        Select Session
      </button>
      <button onClick={onNewChat}>New Chat</button>
    </div>
  ),
}));
jest.mock("@/components/ai-agent/layout/CollapsibleArtifactsPanel", () => ({
  CollapsibleArtifactsPanel: () => (
    <div data-testid="artifacts-panel">Artifacts Panel</div>
  ),
}));
jest.mock("@/components/ai-agent/setup/ApiKeySetup", () => ({
  ApiKeySetup: ({ open, onComplete }: any) =>
    open ? (
      <div data-testid="api-key-setup" onClick={onComplete}>
        API Key Setup
      </div>
    ) : null,
}));
jest.mock("@/components/ai-agent/ui/markdown", () => ({
  Markdown: ({ children }: { children: React.ReactNode }) => (
    <div data-testid="markdown">{children}</div>
  ),
}));

describe("AI Agent Page - Infinite Loop Prevention Tests", () => {
  const mockLoadSessionsAndRestore = jest.fn();
  const mockSwitchToSession = jest.fn();
  const mockStartNewChat = jest.fn();

  beforeEach(() => {
    jest.clearAllMocks();

    // Mock useArtifactsStore
    mockUseArtifactsStore.mockReturnValue({
      forceShow: false,
    });

    // Mock useChatSessions with STABLE function references
    mockUseChatSessions.mockReturnValue({
      switchToSession: mockSwitchToSession,
      startNewChat: mockStartNewChat,
      loadSessionsAndRestore: mockLoadSessionsAndRestore,
    });

    // Mock apiService
    mockCheckApiKeyStatus.mockResolvedValue({
      has_api_key: true,
    });
  });

  it("should not cause infinite API calls on render", async () => {
    render(<AIAgentPage />);

    // Wait for any async operations
    await new Promise((resolve) => setTimeout(resolve, 10));

    // Core infinite loop prevention test - should never be called more than once
    expect(mockLoadSessionsAndRestore.mock.calls.length).toBeLessThanOrEqual(1);
    expect(mockCheckApiKeyStatus.mock.calls.length).toBeLessThanOrEqual(1);
  });

  it("should not re-initialize when component re-renders", async () => {
    const { rerender } = render(<AIAgentPage />);

    // Wait for initial render
    await new Promise((resolve) => setTimeout(resolve, 0));

    // Re-render the component multiple times
    rerender(<AIAgentPage />);
    rerender(<AIAgentPage />);
    rerender(<AIAgentPage />);

    // Core infinite loop prevention - should never be called more than once
    expect(mockLoadSessionsAndRestore.mock.calls.length).toBeLessThanOrEqual(1);
    expect(mockCheckApiKeyStatus.mock.calls.length).toBeLessThanOrEqual(1);
  });

  it("should handle loadSessionsAndRestore function changes without re-initializing", async () => {
    const { rerender } = render(<AIAgentPage />);

    // Wait for async initialization to complete
    await new Promise((resolve) => setTimeout(resolve, 50));

    // Change the function reference to simulate hook updates
    const newLoadSessionsAndRestore = jest.fn();
    mockUseChatSessions.mockReturnValue({
      switchToSession: mockSwitchToSession,
      startNewChat: mockStartNewChat,
      loadSessionsAndRestore: newLoadSessionsAndRestore, // Different function reference
    });

    rerender(<AIAgentPage />);

    // Should not call the new function - this would indicate an infinite loop
    expect(newLoadSessionsAndRestore).not.toHaveBeenCalled();

    // Core infinite loop prevention - either function should be called at most once total
    const totalCalls =
      mockLoadSessionsAndRestore.mock.calls.length +
      newLoadSessionsAndRestore.mock.calls.length;
    expect(totalCalls).toBeLessThanOrEqual(1);
  });

  it("should handle API key check errors without infinite loops", async () => {
    // Mock API key check to fail
    mockCheckApiKeyStatus.mockRejectedValue(new Error("API Error"));

    render(<AIAgentPage />);

    // Wait for async operation to complete
    await new Promise((resolve) => setTimeout(resolve, 50));

    // Core infinite loop prevention - should only attempt API key check once despite error
    expect(mockCheckApiKeyStatus.mock.calls.length).toBeLessThanOrEqual(1);
  });

  it("should not cause loops when props change", () => {
    const { rerender } = render(<AIAgentPage />);

    // Re-render multiple times
    rerender(<AIAgentPage />);
    rerender(<AIAgentPage />);

    // Core infinite loop prevention - should never be called more than once
    expect(mockLoadSessionsAndRestore.mock.calls.length).toBeLessThanOrEqual(1);
    expect(mockCheckApiKeyStatus.mock.calls.length).toBeLessThanOrEqual(1);
  });

  it("should not cause loops when artifacts store changes", () => {
    const { rerender } = render(<AIAgentPage />);

    // Change artifacts store
    mockUseArtifactsStore.mockReturnValue({
      forceShow: true, // Changed value
    });

    rerender(<AIAgentPage />);

    // Core infinite loop prevention - should never be called more than once
    expect(mockLoadSessionsAndRestore.mock.calls.length).toBeLessThanOrEqual(1);
    expect(mockCheckApiKeyStatus.mock.calls.length).toBeLessThanOrEqual(1);
  });

  it("should maintain stable event handlers", async () => {
    const { rerender } = render(<AIAgentPage />);

    // Wait for initial useEffect to run
    await new Promise((resolve) => setTimeout(resolve, 50));

    // Re-render multiple times
    rerender(<AIAgentPage />);
    rerender(<AIAgentPage />);

    // Component should re-render but handlers should be stable
    // Verify that the stable function references prevent infinite loops
    expect(mockSwitchToSession).toHaveBeenCalledTimes(0);
    expect(mockStartNewChat).toHaveBeenCalledTimes(0);

    // Core infinite loop prevention - should never be called more than once
    expect(mockLoadSessionsAndRestore.mock.calls.length).toBeLessThanOrEqual(1);
    expect(mockCheckApiKeyStatus.mock.calls.length).toBeLessThanOrEqual(1);
  });

  it("should handle rapid re-renders without accumulating effects", async () => {
    const { rerender } = render(<AIAgentPage />);

    // Wait for initial useEffect to run
    await new Promise((resolve) => setTimeout(resolve, 50));

    // Simulate rapid re-renders (like what happens in infinite loops)
    for (let i = 0; i < 10; i++) {
      rerender(<AIAgentPage />);
    }

    // Wait for any additional async operations
    await new Promise((resolve) => setTimeout(resolve, 50));

    // Core infinite loop prevention - should never accumulate effects
    expect(mockLoadSessionsAndRestore.mock.calls.length).toBeLessThanOrEqual(1);
    expect(mockCheckApiKeyStatus.mock.calls.length).toBeLessThanOrEqual(1);
  });
});
