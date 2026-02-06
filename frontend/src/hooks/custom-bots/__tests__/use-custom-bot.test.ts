import { renderHook, waitFor, act } from "@testing-library/react";
import { useCustomBot } from "../use-custom-bot";
import { CustomBotService } from "@/lib/api/custom-bots.service";
import { useAuth } from "@/contexts/auth-context";

// Mock dependencies
jest.mock("@/contexts/auth-context", () => ({
  useAuth: jest.fn(),
}));

jest.mock("@/lib/api/custom-bots.service", () => ({
  CustomBotService: {
    getCustomBot: jest.fn(),
  },
}));

const mockUseAuth = useAuth as jest.MockedFunction<typeof useAuth>;
const mockGetCustomBot = CustomBotService.getCustomBot as jest.MockedFunction<
  typeof CustomBotService.getCustomBot
>;

describe("useCustomBot", () => {
  const mockBot = {
    id: "bot-1",
    name: "test-bot",
    userId: "user-1",
    latestVersion: "1.0.0",
    createdAt: new Date(),
    updatedAt: new Date(),
    versions: [
      {
        id: "v-1",
        version: "1.0.0",
        userId: "user-1",
        createdAt: new Date(),
        status: "active" as const,
        config: { name: "test-bot" },
        filePath: "/path",
      },
    ],
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it("should return loading state initially", async () => {
    mockUseAuth.mockReturnValue({
      user: { id: "user-1" },
    } as any);
    mockGetCustomBot.mockResolvedValue({
      success: true,
      data: mockBot,
    });

    const { result } = renderHook(() => useCustomBot("test-bot"));

    expect(result.current.loading).toBe(true);

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });
  });

  it("should fetch and return bot successfully", async () => {
    mockUseAuth.mockReturnValue({
      user: { id: "user-1" },
    } as any);
    mockGetCustomBot.mockResolvedValue({
      success: true,
      data: mockBot,
    });

    const { result } = renderHook(() => useCustomBot("test-bot"));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.bot).toBeDefined();
    expect(result.current.bot?.name).toBe("test-bot");
    expect(result.current.error).toBeNull();
    expect(mockGetCustomBot).toHaveBeenCalledWith("test-bot");
  });

  it("should return error when no user is authenticated", async () => {
    mockUseAuth.mockReturnValue({
      user: null,
    } as any);

    const { result } = renderHook(() => useCustomBot("test-bot"));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.bot).toBeNull();
    expect(result.current.error).toBe("Missing user or bot name");
    expect(mockGetCustomBot).not.toHaveBeenCalled();
  });

  it("should return error when no bot name provided", async () => {
    mockUseAuth.mockReturnValue({
      user: { id: "user-1" },
    } as any);

    const { result } = renderHook(() => useCustomBot(""));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.bot).toBeNull();
    expect(result.current.error).toBe("Missing user or bot name");
  });

  it("should handle 404 not found error", async () => {
    mockUseAuth.mockReturnValue({
      user: { id: "user-1" },
    } as any);
    mockGetCustomBot.mockResolvedValue({
      success: false,
      error: {
        message: "Not found",
        statusCode: 404,
      },
    });

    const { result } = renderHook(() => useCustomBot("non-existent"));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.bot).toBeNull();
    expect(result.current.error).toBe("Bot not found");
  });

  it("should handle 401 unauthorized error", async () => {
    const consoleSpy = jest.spyOn(console, "warn").mockImplementation();
    mockUseAuth.mockReturnValue({
      user: { id: "user-1" },
    } as any);
    mockGetCustomBot.mockResolvedValue({
      success: false,
      error: {
        message: "Unauthorized",
        statusCode: 401,
      },
    });

    const { result } = renderHook(() => useCustomBot("test-bot"));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.error).toBe("Unauthorized");
    expect(consoleSpy).toHaveBeenCalledWith("User unauthorized for custom bot");
    consoleSpy.mockRestore();
  });

  it("should refetch data when refetch is called", async () => {
    mockUseAuth.mockReturnValue({
      user: { id: "user-1" },
    } as any);

    const updatedBot = { ...mockBot, latestVersion: "2.0.0" };
    mockGetCustomBot
      .mockResolvedValueOnce({
        success: true,
        data: mockBot,
      })
      .mockResolvedValueOnce({
        success: true,
        data: updatedBot,
      });

    const { result } = renderHook(() => useCustomBot("test-bot"));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.bot?.latestVersion).toBe("1.0.0");

    await act(async () => {
      await result.current.refetch();
    });

    expect(result.current.bot?.latestVersion).toBe("2.0.0");
    expect(mockGetCustomBot).toHaveBeenCalledTimes(2);
  });

  it("should handle network exception", async () => {
    const consoleSpy = jest.spyOn(console, "error").mockImplementation();
    mockUseAuth.mockReturnValue({
      user: { id: "user-1" },
    } as any);
    mockGetCustomBot.mockRejectedValue(new Error("Network error"));

    const { result } = renderHook(() => useCustomBot("test-bot"));

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.bot).toBeNull();
    expect(result.current.error).toBe("Network error");
    consoleSpy.mockRestore();
  });
});
