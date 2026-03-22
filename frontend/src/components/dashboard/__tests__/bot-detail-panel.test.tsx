import { render, screen, waitFor, act } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { BotDetailPanel } from "../bot-detail-panel";
import { BotService } from "@/lib/api/api-client";
import { useAuth } from "@/contexts/auth-context";
import { useDashboardBots } from "@/contexts/dashboard-bots-context";

// Mock dependencies
const mockPush = jest.fn();
const mockReplace = jest.fn();
const stableRouter = { push: mockPush, replace: mockReplace };
jest.mock("next/navigation", () => ({
  useRouter: () => stableRouter,
}));

jest.mock("@/contexts/auth-context", () => ({
  useAuth: jest.fn(),
}));

const stableToastReturn = {
  toast: jest.fn(),
  toasts: [],
  dismiss: jest.fn(),
};
jest.mock("@/hooks/use-toast", () => ({
  useToast: () => stableToastReturn,
}));

jest.mock("@/contexts/dashboard-bots-context", () => ({
  useDashboardBots: jest.fn(),
}));

jest.mock("@/lib/api/api-client", () => ({
  BotService: {
    getBot: jest.fn(),
    deleteBot: jest.fn(),
    updateBot: jest.fn(),
  },
}));

jest.mock("@/hooks/use-bot-logs", () => ({
  useBotLogs: () => ({
    logs: [],
    loading: false,
    refresh: jest.fn(),
    setDateFilter: jest.fn(),
    setDateRangeFilter: jest.fn(),
    exportLogs: jest.fn(),
  }),
}));

jest.mock("@/hooks/use-bot-logs-stream", () => ({
  useBotLogsStream: () => ({
    logs: [],
    loading: false,
    refresh: jest.fn(),
    setDateFilter: jest.fn(),
    setDateRangeFilter: jest.fn(),
    exportLogs: jest.fn(),
    connected: false,
    lastUpdate: null,
    hasEarlierLogs: false,
    loadingEarlier: false,
    loadEarlierLogs: jest.fn(),
  }),
}));

jest.mock("@/lib/bot-utils", () => ({
  shouldUseLogStreaming: () => false,
}));

jest.mock("@/hooks/use-media-query", () => ({
  useMediaQuery: () => true, // default to desktop
}));

jest.mock("@/components/bot/bot-dashboard-loader", () => ({
  BotDashboardLoader: () => <div data-testid="bot-dashboard" />,
}));

jest.mock("@/components/bot/console-interface", () => ({
  ConsoleInterface: () => <div data-testid="console-interface" />,
}));

jest.mock("../mobile-bot-detail", () => ({
  MobileBotDetail: () => <div data-testid="mobile-bot-detail" />,
}));

const mockToast = stableToastReturn.toast;
const mockRemoveBotFromList = jest.fn();
const mockUseAuth = useAuth as jest.MockedFunction<typeof useAuth>;
const mockUseDashboardBots = useDashboardBots as jest.MockedFunction<
  typeof useDashboardBots
>;
const mockGetBot = BotService.getBot as jest.MockedFunction<
  typeof BotService.getBot
>;
const mockDeleteBot = BotService.deleteBot as jest.MockedFunction<
  typeof BotService.deleteBot
>;
const mockUpdateBot = BotService.updateBot as jest.MockedFunction<
  typeof BotService.updateBot
>;

// Suppress act() warnings from async state updates
const originalError = console.error;
beforeAll(() => {
  console.error = (...args: any[]) => {
    if (typeof args[0] === "string" && args[0].includes("act(")) return;
    originalError.call(console, ...args);
  };
});
afterAll(() => {
  console.error = originalError;
});

const mockBot: any = {
  id: "bot-123",
  config: {
    name: "Test Bot",
    symbol: "BTCUSD",
    type: "scheduled",
    schedule: "0 * * * *",
    enabled: true,
    hasFrontend: false,
    api_key: "secret-key-123",
    password: "hunter2",
  },
  userId: "user-1",
  user_id: "user-1",
  createdAt: "2024-01-01T00:00:00Z",
  updatedAt: "2024-01-02T00:00:00Z",
};

describe("BotDetailPanel", () => {
  beforeEach(() => {
    jest.clearAllMocks();
    mockUseAuth.mockReturnValue({
      user: { id: "user-1" },
    } as any);
    stableToastReturn.toast.mockClear();
    mockUseDashboardBots.mockReturnValue({
      bots: [mockBot as any],
      loading: false,
      error: null,
      refetchBots: jest.fn(),
      removeBotFromList: mockRemoveBotFromList,
    });
  });

  describe("loading and fetch", () => {
    it("shows loading spinner while fetching", () => {
      mockGetBot.mockReturnValue(new Promise(() => {})); // never resolves
      render(<BotDetailPanel botId="bot-123" />);
      expect(document.querySelector(".animate-spin")).toBeInTheDocument();
    });

    it("fetches bot data and renders detail view", async () => {
      mockGetBot.mockResolvedValue({ success: true, data: mockBot } as any);

      await act(async () => {
        render(<BotDetailPanel botId="bot-123" />);
      });

      // Wait for loading to finish and bot name to appear
      await waitFor(
        () => {
          expect(screen.getByText("Test Bot")).toBeInTheDocument();
        },
        { timeout: 3000 },
      );
    });

    it("shows error toast on fetch failure", async () => {
      mockGetBot.mockResolvedValue({
        success: false,
        error: { message: "Bot not found", statusCode: 404 },
      } as any);
      render(<BotDetailPanel botId="bot-123" />);

      await waitFor(() => {
        expect(mockToast).toHaveBeenCalledWith(
          expect.objectContaining({ variant: "destructive" }),
        );
      });
    });

    it("rejects unauthorized access", async () => {
      mockGetBot.mockResolvedValue({
        success: true,
        data: { ...mockBot, userId: "other-user" },
      } as any);
      render(<BotDetailPanel botId="bot-123" />);

      await waitFor(() => {
        expect(mockToast).toHaveBeenCalledWith(
          expect.objectContaining({
            description: expect.stringContaining("Unauthorized"),
          }),
        );
      });
    });
  });

  describe("getMaskedConfig", () => {
    it("masks sensitive fields in displayed config", async () => {
      mockGetBot.mockResolvedValue({ success: true, data: mockBot } as any);
      render(<BotDetailPanel botId="bot-123" />);

      await waitFor(() => {
        expect(screen.getByText("Test Bot")).toBeInTheDocument();
      });

      // The config pre block should NOT contain sensitive values
      const configSection = document.querySelector("pre");
      expect(configSection).not.toBeNull();
      expect(configSection!.textContent).not.toContain("secret-key-123");
      expect(configSection!.textContent).not.toContain("hunter2");
      // But should contain non-sensitive values
      expect(configSection!.textContent).toContain("BTCUSD");
      expect(configSection!.textContent).toContain("scheduled");
    });
  });

  describe("delete handler", () => {
    it("calls deleteBot and removes from list on success", async () => {
      mockGetBot.mockResolvedValue({ success: true, data: mockBot } as any);
      mockDeleteBot.mockResolvedValue({ success: true, data: {} } as any);
      mockUseDashboardBots.mockReturnValue({
        bots: [],
        loading: false,
        error: null,
        refetchBots: jest.fn(),
        removeBotFromList: mockRemoveBotFromList,
      });

      render(<BotDetailPanel botId="bot-123" />);

      await waitFor(() => {
        expect(screen.getByText("Test Bot")).toBeInTheDocument();
      });

      // Click delete button to open dialog
      const deleteButton = screen.getByRole("button", { name: /delete/i });
      await userEvent.click(deleteButton);

      // Confirm deletion in dialog
      const confirmButton = screen.getByRole("button", {
        name: /delete bot/i,
      });
      await userEvent.click(confirmButton);

      await waitFor(() => {
        expect(mockDeleteBot).toHaveBeenCalledWith("bot-123");
        expect(mockRemoveBotFromList).toHaveBeenCalledWith("bot-123");
      });
    });
  });

  describe("toggle enabled", () => {
    it("calls updateBot with toggled enabled state", async () => {
      mockGetBot.mockResolvedValue({ success: true, data: mockBot } as any);
      mockUpdateBot.mockResolvedValue({ success: true, data: {} } as any);

      render(<BotDetailPanel botId="bot-123" />);

      await waitFor(() => {
        expect(screen.getByText("Test Bot")).toBeInTheDocument();
      });

      // Find and click the switch (it's a button with role=switch)
      const toggleSwitch = screen.getByRole("switch");
      await userEvent.click(toggleSwitch);

      await waitFor(() => {
        expect(mockUpdateBot).toHaveBeenCalledWith(
          "bot-123",
          expect.objectContaining({ enabled: false }),
        );
      });
    });
  });
});
