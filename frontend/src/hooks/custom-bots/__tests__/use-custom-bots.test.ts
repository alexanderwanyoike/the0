import { renderHook, waitFor, act } from "@testing-library/react";
import { useCustomBots } from "../use-custom-bots";
import { CustomBotService } from "@/lib/api/custom-bots.service";
import { useAuth } from "@/contexts/auth-context";

// Mock dependencies
jest.mock("@/contexts/auth-context", () => ({
  useAuth: jest.fn(),
}));

jest.mock("@/lib/api/custom-bots.service", () => ({
  CustomBotService: {
    getCustomBots: jest.fn(),
  },
}));

const mockUseAuth = useAuth as jest.MockedFunction<typeof useAuth>;
const mockGetCustomBots = CustomBotService.getCustomBots as jest.MockedFunction<
  typeof CustomBotService.getCustomBots
>;

describe("useCustomBots", () => {
  const mockBots = [
    {
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
    },
  ];

  beforeEach(() => {
    jest.clearAllMocks();
  });

  it("should return loading state initially", async () => {
    mockUseAuth.mockReturnValue({
      user: { id: "user-1" },
    } as any);
    mockGetCustomBots.mockResolvedValue({
      success: true,
      data: mockBots,
    });

    const { result } = renderHook(() => useCustomBots());

    // Initial state should be loading
    expect(result.current.loading).toBe(true);

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });
  });

  it("should fetch and return bots successfully", async () => {
    mockUseAuth.mockReturnValue({
      user: { id: "user-1" },
    } as any);
    mockGetCustomBots.mockResolvedValue({
      success: true,
      data: mockBots,
    });

    const { result } = renderHook(() => useCustomBots());

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.bots).toHaveLength(1);
    expect(result.current.bots[0].name).toBe("test-bot");
    expect(result.current.error).toBeNull();
  });

  it("should return empty array when no user is authenticated", async () => {
    mockUseAuth.mockReturnValue({
      user: null,
    } as any);

    const { result } = renderHook(() => useCustomBots());

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.bots).toEqual([]);
    expect(mockGetCustomBots).not.toHaveBeenCalled();
  });

  it("should handle API error", async () => {
    mockUseAuth.mockReturnValue({
      user: { id: "user-1" },
    } as any);
    mockGetCustomBots.mockResolvedValue({
      success: false,
      error: {
        message: "Failed to fetch",
        statusCode: 500,
      },
    });

    const { result } = renderHook(() => useCustomBots());

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.bots).toEqual([]);
    expect(result.current.error).toBe("Failed to fetch");
  });

  it("should handle 401 unauthorized error", async () => {
    const consoleSpy = jest.spyOn(console, "warn").mockImplementation();
    mockUseAuth.mockReturnValue({
      user: { id: "user-1" },
    } as any);
    mockGetCustomBots.mockResolvedValue({
      success: false,
      error: {
        message: "Unauthorized",
        statusCode: 401,
      },
    });

    const { result } = renderHook(() => useCustomBots());

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.error).toBe("Unauthorized");
    expect(consoleSpy).toHaveBeenCalledWith(
      "User unauthorized for custom bots",
    );
    consoleSpy.mockRestore();
  });

  it("should refetch data when refetch is called", async () => {
    mockUseAuth.mockReturnValue({
      user: { id: "user-1" },
    } as any);
    mockGetCustomBots
      .mockResolvedValueOnce({
        success: true,
        data: [],
      })
      .mockResolvedValueOnce({
        success: true,
        data: mockBots,
      });

    const { result } = renderHook(() => useCustomBots());

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.bots).toHaveLength(0);

    // Call refetch
    await act(async () => {
      await result.current.refetch();
    });

    expect(result.current.bots).toHaveLength(1);
    expect(mockGetCustomBots).toHaveBeenCalledTimes(2);
  });

  it("should handle network exception", async () => {
    const consoleSpy = jest.spyOn(console, "error").mockImplementation();
    mockUseAuth.mockReturnValue({
      user: { id: "user-1" },
    } as any);
    mockGetCustomBots.mockRejectedValue(new Error("Network error"));

    const { result } = renderHook(() => useCustomBots());

    await waitFor(() => {
      expect(result.current.loading).toBe(false);
    });

    expect(result.current.bots).toEqual([]);
    expect(result.current.error).toBe("Network error");
    consoleSpy.mockRestore();
  });
});
