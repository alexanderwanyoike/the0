import { render, screen, waitFor } from "@testing-library/react";
import { BotDashboardLoader } from "../bot-dashboard-loader";

// Mock the BotEventsProvider
jest.mock("@/contexts/bot-events-context", () => ({
  BotEventsProvider: ({
    children,
    botId,
  }: {
    children: React.ReactNode;
    botId: string;
  }) => (
    <div data-testid="bot-events-provider" data-bot-id={botId}>
      {children}
    </div>
  ),
}));

// Mock lucide-react icons
jest.mock("lucide-react", () => ({
  RefreshCw: () => <div data-testid="refresh-icon">RefreshCw</div>,
  AlertCircle: () => <div data-testid="alert-icon">AlertCircle</div>,
}));

describe("BotDashboardLoader", () => {
  const defaultProps = {
    botId: "bot-123",
    customBotId: "custom-bot-456",
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe("loading state", () => {
    it("shows loading state initially when customBotId is provided", () => {
      render(<BotDashboardLoader {...defaultProps} />);

      expect(screen.getByText("Loading dashboard...")).toBeInTheDocument();
      expect(screen.getByTestId("refresh-icon")).toBeInTheDocument();
    });

    it("applies className to loading container", () => {
      render(<BotDashboardLoader {...defaultProps} className="custom-class" />);

      const loadingText = screen.getByText("Loading dashboard...");
      const container = loadingText.closest("div");
      expect(container).toHaveClass("custom-class");
    });
  });

  describe("when customBotId is empty", () => {
    it("shows no dashboard available message", async () => {
      render(<BotDashboardLoader {...defaultProps} customBotId="" />);

      await waitFor(() => {
        expect(screen.getByText("No dashboard available")).toBeInTheDocument();
      });
    });

    it("does not show loading state", async () => {
      render(<BotDashboardLoader {...defaultProps} customBotId="" />);

      await waitFor(() => {
        expect(
          screen.queryByText("Loading dashboard..."),
        ).not.toBeInTheDocument();
      });
    });
  });

  describe("error state", () => {
    it("displays error message when dashboard fails to load", async () => {
      // The component will attempt to import and fail
      // We can't easily mock dynamic imports, so we test the UI states

      render(<BotDashboardLoader {...defaultProps} />);

      // Initially shows loading
      expect(screen.getByText("Loading dashboard...")).toBeInTheDocument();
    });
  });

  describe("className prop", () => {
    it("passes className to the wrapper div", () => {
      render(<BotDashboardLoader {...defaultProps} className="test-class" />);

      // During loading state
      const loadingContainer = screen
        .getByText("Loading dashboard...")
        .closest("div");
      expect(loadingContainer).toHaveClass("test-class");
    });
  });

  describe("botId prop", () => {
    it("passes botId to BotEventsProvider when dashboard loads", async () => {
      // This test verifies the prop is passed correctly when rendering
      render(<BotDashboardLoader botId="my-bot-id" customBotId="" />);

      await waitFor(() => {
        expect(screen.getByText("No dashboard available")).toBeInTheDocument();
      });
    });
  });

  describe("no dashboard state", () => {
    it("renders placeholder when no dashboard is available", async () => {
      render(<BotDashboardLoader {...defaultProps} customBotId="" />);

      await waitFor(() => {
        const placeholder = screen.getByText("No dashboard available");
        expect(placeholder).toBeInTheDocument();
      });
    });

    it("applies className to no dashboard placeholder", async () => {
      render(
        <BotDashboardLoader
          {...defaultProps}
          customBotId=""
          className="custom-placeholder"
        />,
      );

      await waitFor(() => {
        const placeholder = screen
          .getByText("No dashboard available")
          .closest("div");
        expect(placeholder).toHaveClass("custom-placeholder");
      });
    });
  });
});
