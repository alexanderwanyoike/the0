import { render, screen, waitFor, act, fireEvent, within } from "@testing-library/react";
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
    error: null,
    hasMore: false,
    total: 0,
    query: {},
    refresh: jest.fn(),
    loadMore: jest.fn(),
    loadEarlierLogs: jest.fn(),
    updateQuery: jest.fn(),
    setDateFilter: jest.fn(),
    setDateRangeFilter: jest.fn(),
    exportLogs: jest.fn(),
    connected: false,
    lastUpdate: null,
    hasEarlierLogs: false,
    loadingEarlier: false,
  }),
}));

jest.mock("@/lib/bot-utils", () => ({
  // All loaded bots stream; these tests exercise a scheduled bot's defaults
  shouldUseLogStreaming: (bot: unknown) => bot !== null,
  isScheduledBot: () => true,
}));

let mockIsDesktop: boolean | null = true;
jest.mock("@/hooks/use-media-query", () => ({
  useMediaQuery: () => mockIsDesktop,
}));

jest.mock("@/components/bot/bot-dashboard-loader", () => ({
  BotDashboardLoader: () => <div data-testid="bot-dashboard" />,
}));

jest.mock("@/components/bot/console-interface", () => ({
  ConsoleInterface: () => <div data-testid="console-interface" />,
  ConnectionStatusIndicator: () => (
    <div data-testid="connection-status-indicator" />
  ),
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
    mockIsDesktop = true;
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

    it("shows Disabled once the update succeeds", async () => {
      mockGetBot.mockResolvedValue({ success: true, data: mockBot } as any);
      mockUpdateBot.mockResolvedValue({ success: true, data: {} } as any);

      render(<BotDetailPanel botId="bot-123" />);
      await screen.findByText("Enabled");

      await userEvent.click(screen.getByRole("switch"));

      expect(await screen.findByText("Disabled")).toBeInTheDocument();
      expect(screen.getByRole("switch")).not.toBeChecked();
    });

    it("keeps the previous state and toasts when the update fails", async () => {
      mockGetBot.mockResolvedValue({ success: true, data: mockBot } as any);
      mockUpdateBot.mockResolvedValue({
        success: false,
        error: { message: "nope" },
      } as any);
      const errorSpy = jest.spyOn(console, "error").mockImplementation(() => {});

      render(<BotDetailPanel botId="bot-123" />);
      await screen.findByText("Enabled");

      await userEvent.click(screen.getByRole("switch"));

      await waitFor(() => {
        expect(mockToast).toHaveBeenCalledWith(
          expect.objectContaining({
            title: "Update Failed",
            variant: "destructive",
          }),
        );
      });
      expect(screen.getByText("Enabled")).toBeInTheDocument();
      errorSpy.mockRestore();
    });
  });

  describe("delete navigation", () => {
    async function confirmDelete() {
      await screen.findByText("Test Bot");
      await userEvent.click(screen.getByRole("button", { name: /delete/i }));
      await userEvent.click(screen.getByRole("button", { name: /delete bot/i }));
    }

    it("moves to the next remaining bot", async () => {
      mockGetBot.mockResolvedValue({ success: true, data: mockBot } as any);
      mockDeleteBot.mockResolvedValue({ success: true, data: {} } as any);
      mockUseDashboardBots.mockReturnValue({
        bots: [mockBot, { ...mockBot, id: "bot-456" }],
        loading: false,
        error: null,
        refetchBots: jest.fn(),
        removeBotFromList: mockRemoveBotFromList,
      });

      render(<BotDetailPanel botId="bot-123" />);
      await confirmDelete();

      await waitFor(() => {
        expect(mockReplace).toHaveBeenCalledWith("/dashboard/bot-456");
      });
    });

    it("returns to the dashboard when no bots remain", async () => {
      mockGetBot.mockResolvedValue({ success: true, data: mockBot } as any);
      mockDeleteBot.mockResolvedValue({ success: true, data: {} } as any);

      render(<BotDetailPanel botId="bot-123" />);
      await confirmDelete();

      await waitFor(() => {
        expect(mockReplace).toHaveBeenCalledWith("/dashboard");
      });
    });

    it("toasts and stays put when the delete fails", async () => {
      mockGetBot.mockResolvedValue({ success: true, data: mockBot } as any);
      mockDeleteBot.mockResolvedValue({
        success: false,
        error: { message: "delete broke" },
      } as any);
      const errorSpy = jest.spyOn(console, "error").mockImplementation(() => {});

      render(<BotDetailPanel botId="bot-123" />);
      await confirmDelete();

      await waitFor(() => {
        expect(mockToast).toHaveBeenCalledWith({
          title: "Delete Failed",
          description: "delete broke",
          variant: "destructive",
        });
      });
      expect(mockRemoveBotFromList).not.toHaveBeenCalled();
      expect(mockReplace).not.toHaveBeenCalled();
      errorSpy.mockRestore();
    });
  });

  describe("desktop detail layout", () => {
    it("renders the details, console and dashboard placeholder", async () => {
      mockGetBot.mockResolvedValue({ success: true, data: mockBot } as any);
      render(<BotDetailPanel botId="bot-123" />);

      await screen.findByText("Test Bot");
      expect(screen.getByText("ot-123")).toBeInTheDocument();
      expect(screen.getByText("BTCUSD")).toBeInTheDocument();
      expect(screen.getByText("0 * * * *")).toBeInTheDocument();
      expect(screen.getByText("Bot Details")).toBeInTheDocument();
      expect(screen.getByText("Configuration")).toBeInTheDocument();
      expect(
        screen.getByText("No dashboard configured for this bot"),
      ).toBeInTheDocument();
      expect(screen.getByTestId("console-interface")).toBeInTheDocument();
      expect(
        screen.getByTestId("connection-status-indicator"),
      ).toBeInTheDocument();
      expect(screen.queryByTestId("bot-dashboard")).not.toBeInTheDocument();
    });

    it("renders the custom dashboard and Real-time schedule when configured", async () => {
      mockGetBot.mockResolvedValue({
        success: true,
        data: {
          ...mockBot,
          customBotId: "custom-1",
          config: { ...mockBot.config, hasFrontend: true, schedule: undefined },
        },
      } as any);
      render(<BotDetailPanel botId="bot-123" />);

      await screen.findByText("Test Bot");
      expect(screen.getByTestId("bot-dashboard")).toBeInTheDocument();
      expect(screen.getByText("Real-time")).toBeInTheDocument();
    });

    it("copies the masked configuration", async () => {
      const writeText = jest.fn().mockResolvedValue(undefined);
      Object.defineProperty(navigator, "clipboard", {
        value: { writeText },
        configurable: true,
      });
      mockGetBot.mockResolvedValue({ success: true, data: mockBot } as any);
      render(<BotDetailPanel botId="bot-123" />);
      await screen.findByText("Test Bot");

      fireEvent.click(screen.getByText("Copy").closest("button")!);

      const copied = JSON.parse(writeText.mock.calls[0][0]);
      expect(copied.symbol).toBe("BTCUSD");
      expect(copied.api_key).toBeUndefined();
      expect(copied.password).toBeUndefined();
      expect(mockToast).toHaveBeenCalledWith({
        description: "Bot configuration copied to clipboard",
        duration: 2000,
      });
    });
  });

  describe("CLI update dialog", () => {
    it("opens with the bot id and update command and copies them", async () => {
      const writeText = jest.fn().mockResolvedValue(undefined);
      Object.defineProperty(navigator, "clipboard", {
        value: { writeText },
        configurable: true,
      });
      mockGetBot.mockResolvedValue({ success: true, data: mockBot } as any);
      render(<BotDetailPanel botId="bot-123" />);
      await screen.findByText("Test Bot");

      fireEvent.click(screen.getByRole("button", { name: /update via cli/i }));

      const dialog = await screen.findByRole("dialog");
      expect(within(dialog).getByText("Update Bot via CLI")).toBeInTheDocument();
      expect(within(dialog).getByDisplayValue("bot-123")).toBeInTheDocument();
      expect(
        within(dialog).getByDisplayValue("the0 bot update bot-123 config.json"),
      ).toBeInTheDocument();
      expect(within(dialog).getByText("the0 bot logs bot-123")).toBeInTheDocument();

      const [copyId, copyCommand] = within(dialog).getAllByRole("button", {
        name: /copy/i,
      });
      fireEvent.click(copyId);
      expect(writeText).toHaveBeenLastCalledWith("bot-123");
      expect(mockToast).toHaveBeenLastCalledWith({
        title: "Bot ID Copied",
        description: "Bot ID copied to clipboard.",
      });

      fireEvent.click(copyCommand);
      expect(writeText).toHaveBeenLastCalledWith(
        "the0 bot update bot-123 config.json",
      );
      expect(mockToast).toHaveBeenLastCalledWith({
        title: "Command Copied",
        description: "CLI update command copied to clipboard.",
      });
    });
  });

  describe("responsive layout", () => {
    it("renders the mobile layout below the desktop breakpoint", async () => {
      mockIsDesktop = false;
      mockGetBot.mockResolvedValue({ success: true, data: mockBot } as any);
      render(<BotDetailPanel botId="bot-123" />);

      expect(await screen.findByTestId("mobile-bot-detail")).toBeInTheDocument();
      expect(screen.queryByText("Bot Details")).not.toBeInTheDocument();
    });

    it("shows a spinner until the media query resolves", async () => {
      mockIsDesktop = null;
      mockGetBot.mockResolvedValue({ success: true, data: mockBot } as any);
      render(<BotDetailPanel botId="bot-123" />);

      await waitFor(() => expect(mockGetBot).toHaveBeenCalled());
      await act(async () => {});
      expect(document.querySelector(".animate-spin")).toBeInTheDocument();
      expect(screen.queryByText("Test Bot")).not.toBeInTheDocument();
      expect(screen.queryByTestId("mobile-bot-detail")).not.toBeInTheDocument();
    });
  });
});
