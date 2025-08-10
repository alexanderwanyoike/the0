import { renderHook, act } from "@testing-library/react";
import { useArtifacts } from "../useArtifacts";
import { useArtifactsStore } from "@/stores/ai-agent/artifactsStore";
import { useChatStore } from "@/stores/ai-agent/chatStore";
import { apiService } from "@/lib/ai-agent/api";

// Mock the stores and API service
jest.mock("@/stores/ai-agent/artifactsStore");
jest.mock("@/stores/ai-agent/chatStore");
jest.mock("@/lib/ai-agent/api");

const mockApiService = apiService as jest.Mocked<typeof apiService>;

describe("useArtifacts - Infinite Loop Prevention Tests", () => {
  const mockSetFiles = jest.fn();
  const mockSetActiveFile = jest.fn();

  beforeEach(() => {
    jest.clearAllMocks();

    // Mock useArtifactsStore
    (useArtifactsStore as unknown as jest.Mock).mockReturnValue({
      forceShow: false,
      files: [],
      setFiles: mockSetFiles,
      setActiveFile: mockSetActiveFile,
    });

    // Mock useChatStore
    (useChatStore as unknown as jest.Mock).mockReturnValue({
      sessionId: "test-session-123",
    });

    // Mock API service
    mockApiService.getSessionArtifacts.mockResolvedValue([
      "file1.py",
      "file2.yaml",
    ]);
    mockApiService.getArtifact.mockResolvedValue({
      filename: "file1.py",
      content: 'print("hello")',
      version: 1,
    });
  });

  it("should not make API calls when forceShow is false", () => {
    renderHook(() => useArtifacts());

    // Should not make any API calls
    expect(mockApiService.getSessionArtifacts).not.toHaveBeenCalled();
    expect(mockApiService.getArtifact).not.toHaveBeenCalled();
  });

  it("should not cause infinite loops when store functions change", () => {
    const { rerender } = renderHook(() => useArtifacts());

    // Change store functions to simulate re-renders
    (useArtifactsStore as unknown as jest.Mock).mockReturnValue({
      forceShow: false,
      files: [],
      setFiles: jest.fn(), // Different function reference
      setActiveFile: jest.fn(), // Different function reference
    });

    // Re-render multiple times
    rerender({});
    rerender({});
    rerender({});

    // Should still not make any API calls
    expect(mockApiService.getSessionArtifacts).not.toHaveBeenCalled();
  });

  it("should only fetch once per session when forceShow is true", async () => {
    // Set forceShow to true
    (useArtifactsStore as unknown as jest.Mock).mockReturnValue({
      forceShow: true,
      files: [],
      setFiles: mockSetFiles,
      setActiveFile: mockSetActiveFile,
    });

    const { rerender } = renderHook(() => useArtifacts());

    // Wait for initial fetch
    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, 0));
    });

    // Should fetch once
    expect(mockApiService.getSessionArtifacts).toHaveBeenCalledTimes(1);
    expect(mockApiService.getSessionArtifacts).toHaveBeenCalledWith(
      "test-session-123",
    );

    // Re-render multiple times with same session
    rerender({});
    rerender({});
    rerender({});

    // Should not fetch again for same session
    expect(mockApiService.getSessionArtifacts).toHaveBeenCalledTimes(1);
  });

  it("should fetch new data when sessionId changes", async () => {
    // Set forceShow to true
    (useArtifactsStore as unknown as jest.Mock).mockReturnValue({
      forceShow: true,
      files: [],
      setFiles: mockSetFiles,
      setActiveFile: mockSetActiveFile,
    });

    const { rerender } = renderHook(() => useArtifacts());

    // Wait for initial fetch
    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, 0));
    });

    expect(mockApiService.getSessionArtifacts).toHaveBeenCalledTimes(1);
    expect(mockApiService.getSessionArtifacts).toHaveBeenCalledWith(
      "test-session-123",
    );

    // Change session ID
    (useChatStore as unknown as jest.Mock).mockReturnValue({
      sessionId: "new-session-456",
    });

    rerender({});

    // Wait for new fetch
    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, 0));
    });

    // Should fetch for new session
    expect(mockApiService.getSessionArtifacts).toHaveBeenCalledTimes(2);
    expect(mockApiService.getSessionArtifacts).toHaveBeenCalledWith(
      "new-session-456",
    );
  });

  it("should prevent duplicate fetches for same session", async () => {
    // Set forceShow to true and mock session
    (useChatStore as unknown as jest.Mock).mockReturnValue({
      sessionId: "test-session",
    });
    (useArtifactsStore as unknown as jest.Mock).mockReturnValue({
      forceShow: true,
      files: [],
      setFiles: mockSetFiles,
      setActiveFile: mockSetActiveFile,
    });

    // Render single hook instance and test re-renders don't cause duplicate fetches
    const { rerender } = renderHook(() => useArtifacts());

    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, 0));
    });

    // Should fetch once initially
    expect(mockApiService.getSessionArtifacts).toHaveBeenCalledTimes(1);

    // Re-render should not trigger additional fetch for same session
    rerender();

    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, 0));
    });

    // Should still only have been called once
    expect(mockApiService.getSessionArtifacts).toHaveBeenCalledTimes(1);
  });

  it("should clear data when forceShow becomes false", () => {
    // Start with forceShow true
    (useArtifactsStore as unknown as jest.Mock).mockReturnValue({
      forceShow: true,
      files: [{ id: "file1", name: "file1.py", content: "test" }],
      setFiles: mockSetFiles,
      setActiveFile: mockSetActiveFile,
    });

    const { rerender } = renderHook(() => useArtifacts());

    // Change to forceShow false
    (useArtifactsStore as unknown as jest.Mock).mockReturnValue({
      forceShow: false,
      files: [{ id: "file1", name: "file1.py", content: "test" }],
      setFiles: mockSetFiles,
      setActiveFile: mockSetActiveFile,
    });

    rerender({});

    // Should clear data
    expect(mockSetFiles).toHaveBeenCalledWith([]);
    expect(mockSetActiveFile).toHaveBeenCalledWith(null);
  });

  it("should handle API errors without causing loops", async () => {
    mockApiService.getSessionArtifacts.mockRejectedValue(
      new Error("API Error"),
    );
    (useArtifactsStore as unknown as jest.Mock).mockReturnValue({
      forceShow: true,
      files: [],
      setFiles: mockSetFiles,
      setActiveFile: mockSetActiveFile,
    });

    const { result } = renderHook(() => useArtifacts());

    await act(async () => {
      await new Promise((resolve) => setTimeout(resolve, 0));
    });

    // Should handle error gracefully
    expect(result.current.error).toBeInstanceOf(Error);
    expect(result.current.error?.message).toBe("API Error");

    // Should not cause infinite retries
    expect(mockApiService.getSessionArtifacts).toHaveBeenCalledTimes(1);
  });
});
