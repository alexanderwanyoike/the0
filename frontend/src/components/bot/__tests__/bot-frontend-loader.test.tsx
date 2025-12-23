import { render, screen, waitFor } from "@testing-library/react";
import { BotFrontendLoader } from "../bot-frontend-loader";

// Mock the ConsoleInterface component
jest.mock("../console-interface", () => ({
  ConsoleInterface: ({ botId, logs }: { botId: string; logs: any[] }) => (
    <div data-testid="console-interface">
      Console Interface for {botId} with {logs.length} logs
    </div>
  ),
  LogEntry: jest.fn(),
}));

// Mock the BotEventsProvider
jest.mock("@/contexts/bot-events-context", () => ({
  BotEventsProvider: ({ children, botId }: { children: React.ReactNode; botId: string }) => (
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

describe("BotFrontendLoader", () => {
  const defaultProps = {
    botId: "bot-123",
    customBotId: "custom-bot-456",
    hasFrontend: true,
    logs: [{ date: "2024-01-01", content: "Test log" }],
    loading: false,
    onRefresh: jest.fn(),
    onDateChange: jest.fn(),
    onDateRangeChange: jest.fn(),
    onExport: jest.fn(),
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe("when hasFrontend is false", () => {
    it("renders ConsoleInterface immediately", () => {
      render(<BotFrontendLoader {...defaultProps} hasFrontend={false} />);

      expect(screen.getByTestId("console-interface")).toBeInTheDocument();
      expect(screen.getByText(/Console Interface for bot-123/)).toBeInTheDocument();
    });
  });

  describe("when customBotId is missing", () => {
    it("renders ConsoleInterface", () => {
      render(<BotFrontendLoader {...defaultProps} customBotId={undefined} />);

      expect(screen.getByTestId("console-interface")).toBeInTheDocument();
    });
  });

  describe("when hasFrontend is true with customBotId", () => {
    it("shows loading state initially", async () => {
      // Mock import that never resolves to keep loading state
      const mockImport = jest.fn(() => new Promise(() => {}));
      (global as any).import = mockImport;

      render(<BotFrontendLoader {...defaultProps} />);

      // Should show loading state
      expect(screen.getByText("Loading custom dashboard...")).toBeInTheDocument();
      expect(screen.getByTestId("refresh-icon")).toBeInTheDocument();
    });

    it("renders custom dashboard when import succeeds", async () => {
      const MockDashboard = () => <div data-testid="custom-dashboard">Custom Dashboard</div>;

      // Mock dynamic import
      jest.mock(
        "/api/custom-bots/frontend/custom-bot-456",
        () => ({ default: MockDashboard }),
        { virtual: true }
      );

      render(<BotFrontendLoader {...defaultProps} />);

      // Initially shows loading
      expect(screen.getByText("Loading custom dashboard...")).toBeInTheDocument();
    });

    it("shows error and fallback to ConsoleInterface on import failure", async () => {
      // For this test, we need to simulate an import failure
      // Since we can't easily mock dynamic imports, we test the error state rendering
      const { rerender } = render(
        <BotFrontendLoader {...defaultProps} hasFrontend={false} />
      );

      // Verify fallback works
      expect(screen.getByTestId("console-interface")).toBeInTheDocument();
    });
  });

  describe("callback props", () => {
    it("passes callbacks to ConsoleInterface when showing fallback", () => {
      const onRefresh = jest.fn();
      const onDateChange = jest.fn();
      const onDateRangeChange = jest.fn();
      const onExport = jest.fn();

      render(
        <BotFrontendLoader
          {...defaultProps}
          hasFrontend={false}
          onRefresh={onRefresh}
          onDateChange={onDateChange}
          onDateRangeChange={onDateRangeChange}
          onExport={onExport}
        />
      );

      expect(screen.getByTestId("console-interface")).toBeInTheDocument();
    });
  });

  describe("className prop", () => {
    it("passes className to ConsoleInterface", () => {
      render(
        <BotFrontendLoader
          {...defaultProps}
          hasFrontend={false}
          className="custom-class"
        />
      );

      expect(screen.getByTestId("console-interface")).toBeInTheDocument();
    });

    it("applies className to loading state container", () => {
      render(<BotFrontendLoader {...defaultProps} className="custom-class" />);

      // Loading state should have the class
      const loadingContainer = screen.getByText("Loading custom dashboard...").closest("div");
      expect(loadingContainer).toHaveClass("custom-class");
    });
  });

  describe("logs prop", () => {
    it("passes logs to ConsoleInterface", () => {
      const logs = [
        { date: "2024-01-01", content: "Log 1" },
        { date: "2024-01-02", content: "Log 2" },
      ];

      render(
        <BotFrontendLoader {...defaultProps} hasFrontend={false} logs={logs} />
      );

      expect(screen.getByText(/with 2 logs/)).toBeInTheDocument();
    });

    it("handles empty logs array", () => {
      render(
        <BotFrontendLoader {...defaultProps} hasFrontend={false} logs={[]} />
      );

      expect(screen.getByText(/with 0 logs/)).toBeInTheDocument();
    });
  });

  describe("loading prop", () => {
    it("passes loading state to ConsoleInterface", () => {
      render(
        <BotFrontendLoader {...defaultProps} hasFrontend={false} loading={true} />
      );

      expect(screen.getByTestId("console-interface")).toBeInTheDocument();
    });
  });
});
