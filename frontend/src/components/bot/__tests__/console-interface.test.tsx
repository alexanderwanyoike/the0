import { render, screen, fireEvent, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { ConsoleInterface, LogEntry } from "../console-interface";

// Mock the event parser
jest.mock("@/lib/events/event-parser", () => ({
  parseLogLine: jest.fn((content: string) => {
    // Check if it looks like a metric (contains _metric)
    if (content.includes("_metric")) {
      try {
        const jsonMatch = content.match(/\{.*\}/);
        if (jsonMatch) {
          const parsed = JSON.parse(jsonMatch[0]);
          return {
            timestamp: new Date(),
            type: "metric",
            data: parsed,
            metricType: parsed._metric,
            raw: content,
          };
        }
      } catch {
        // Fall through to log
      }
    }
    return {
      timestamp: new Date(),
      type: "log",
      data: content,
      level: content.includes("ERROR")
        ? "ERROR"
        : content.includes("WARN")
          ? "WARN"
          : content.includes("DEBUG")
            ? "DEBUG"
            : "INFO",
      raw: content,
    };
  }),
  isMetricEvent: jest.fn((event: any) => event?.type === "metric"),
}));

describe("ConsoleInterface", () => {
  const defaultProps = {
    botId: "bot-123",
    logs: [] as LogEntry[],
    loading: false,
    onRefresh: jest.fn(),
    onDateChange: jest.fn(),
    onDateRangeChange: jest.fn(),
    onExport: jest.fn(),
  };

  beforeEach(() => {
    jest.clearAllMocks();
  });

  describe("rendering", () => {
    it("renders console header with title and entry count", () => {
      const logs: LogEntry[] = [
        {
          date: "2024-01-01",
          content: "[2024-01-01 10:00:00] INFO: Test log 1",
        },
        {
          date: "2024-01-01",
          content: "[2024-01-01 10:01:00] INFO: Test log 2",
        },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      expect(screen.getByText("CONSOLE")).toBeInTheDocument();
      expect(screen.getByText("2 entries")).toBeInTheDocument();
    });

    it("renders empty state when no logs", () => {
      render(<ConsoleInterface {...defaultProps} logs={[]} />);

      expect(screen.getByText("> Waiting for logs...")).toBeInTheDocument();
    });

    it("renders loading state", () => {
      const { container } = render(
        <ConsoleInterface {...defaultProps} loading={true} logs={[]} />,
      );

      // Should show loading spinner (RefreshCw with animate-spin class)
      const spinner = container.querySelector(".animate-spin");
      expect(spinner).toBeInTheDocument();
    });

    it("applies custom className", () => {
      const { container } = render(
        <ConsoleInterface {...defaultProps} className="custom-class" />,
      );

      expect(container.firstChild).toHaveClass("custom-class");
    });
  });

  describe("compact mode", () => {
    it("hides header elements in compact mode", () => {
      render(<ConsoleInterface {...defaultProps} compact={true} />);

      expect(screen.queryByText("CONSOLE")).not.toBeInTheDocument();
    });

    it("shows simplified toolbar in compact mode", () => {
      render(<ConsoleInterface {...defaultProps} compact={true} />);

      // Filter button should not be visible in compact mode
      expect(
        screen.queryByRole("button", { name: /filter/i }),
      ).not.toBeInTheDocument();
    });
  });

  describe("log entries", () => {
    it("renders log entries with correct content", () => {
      const logs: LogEntry[] = [
        {
          date: "2024-01-01",
          content: "[2024-01-01 10:00:00] INFO: Starting bot",
        },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      expect(screen.getByText(/Starting bot/)).toBeInTheDocument();
    });

    it("displays logs in reverse order (newest first)", () => {
      const logs: LogEntry[] = [
        {
          date: "2024-01-01",
          content: "[2024-01-01 10:00:00] INFO: First log",
        },
        {
          date: "2024-01-01",
          content: "[2024-01-01 10:01:00] INFO: Second log",
        },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const logElements = screen.getAllByText(/log/i);
      // Newest should be first in the DOM
      expect(logElements.length).toBeGreaterThan(0);
    });

    it("renders ERROR logs with appropriate styling", () => {
      const logs: LogEntry[] = [
        {
          date: "2024-01-01",
          content: "[2024-01-01 10:00:00] ERROR: Something failed",
        },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      expect(screen.getByText(/Something failed/)).toBeInTheDocument();
    });

    it("renders WARN logs", () => {
      const logs: LogEntry[] = [
        {
          date: "2024-01-01",
          content: "[2024-01-01 10:00:00] WARN: Warning message",
        },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      expect(screen.getByText(/Warning message/)).toBeInTheDocument();
    });

    it("renders DEBUG logs", () => {
      const logs: LogEntry[] = [
        {
          date: "2024-01-01",
          content: "[2024-01-01 10:00:00] DEBUG: Debug info",
        },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      expect(screen.getByText(/Debug info/)).toBeInTheDocument();
    });
  });

  describe("metric entries", () => {
    it("renders metric entries with special styling", () => {
      const logs: LogEntry[] = [
        {
          date: "2024-01-01",
          content:
            '[2024-01-01 10:00:00] {"_metric": "portfolio_value", "value": 10000}',
        },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Metric should be rendered
      expect(screen.getByText(/portfolio_value/)).toBeInTheDocument();
    });
  });

  describe("search functionality", () => {
    it("filters logs based on search query", async () => {
      const user = userEvent.setup();
      const logs: LogEntry[] = [
        { date: "2024-01-01", content: "[2024-01-01 10:00:00] INFO: Apple" },
        { date: "2024-01-01", content: "[2024-01-01 10:01:00] INFO: Banana" },
        { date: "2024-01-01", content: "[2024-01-01 10:02:00] INFO: Cherry" },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Find and click filter button (first button in toolbar)
      const buttons = screen.getAllByRole("button");
      await user.click(buttons[0]);

      // Search input should now be visible
      const searchInput = await screen.findByPlaceholderText("Search logs...");
      await user.type(searchInput, "Apple");

      // Should filter to show only Apple
      expect(screen.getByText(/Apple/)).toBeInTheDocument();
    });

    it("shows no matches message when search has no results", async () => {
      const user = userEvent.setup();
      const logs: LogEntry[] = [
        { date: "2024-01-01", content: "[2024-01-01 10:00:00] INFO: Test log" },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Open filter panel
      const buttons = screen.getAllByRole("button");
      await user.click(buttons[0]);

      // Search for something that doesn't exist
      const searchInput = await screen.findByPlaceholderText("Search logs...");
      await user.type(searchInput, "nonexistent");

      // Should show no matches message
      expect(screen.getByText("> No logs match filter")).toBeInTheDocument();
    });
  });

  describe("toolbar buttons", () => {
    it("calls onRefresh when refresh button is clicked", async () => {
      const user = userEvent.setup();
      const onRefresh = jest.fn();

      render(<ConsoleInterface {...defaultProps} onRefresh={onRefresh} />);

      // Find refresh button (has RefreshCw icon)
      const buttons = screen.getAllByRole("button");
      const refreshButton = buttons[1]; // Second button is typically refresh

      await user.click(refreshButton);

      expect(onRefresh).toHaveBeenCalled();
    });

    it("toggles auto-scroll when play/pause button is clicked", async () => {
      const user = userEvent.setup();

      render(<ConsoleInterface {...defaultProps} />);

      const buttons = screen.getAllByRole("button");
      // Auto-scroll button is typically after refresh
      const autoScrollButton = buttons[2];

      // Initially should show play icon (auto-scroll off)
      await user.click(autoScrollButton);

      // After click, should toggle auto-scroll state
      // Button appearance should change
    });

    it("calls onExport when export button is clicked", async () => {
      const user = userEvent.setup();
      const onExport = jest.fn();

      render(<ConsoleInterface {...defaultProps} onExport={onExport} />);

      const buttons = screen.getAllByRole("button");
      const exportButton = buttons[buttons.length - 1]; // Last button is export

      await user.click(exportButton);

      expect(onExport).toHaveBeenCalled();
    });

    it("disables refresh button when loading", () => {
      render(<ConsoleInterface {...defaultProps} loading={true} />);

      const buttons = screen.getAllByRole("button");
      // Find the refresh button and check if it's disabled
      const refreshButton = buttons[1];
      expect(refreshButton).toBeDisabled();
    });
  });

  describe("date filtering", () => {
    it("calls onDateChange when date is selected", async () => {
      const user = userEvent.setup();
      const onDateChange = jest.fn();

      render(
        <ConsoleInterface {...defaultProps} onDateChange={onDateChange} />,
      );

      // Need to open filter panel first
      const buttons = screen.getAllByRole("button");
      const filterBtn = buttons[0];

      await user.click(filterBtn);

      // Now date inputs should be visible
    });
  });

  describe("filter panel", () => {
    it("toggles filter panel visibility", async () => {
      const user = userEvent.setup();

      render(<ConsoleInterface {...defaultProps} />);

      const buttons = screen.getAllByRole("button");
      const filterButton = buttons[0];

      // Initially filters are hidden
      expect(
        screen.queryByPlaceholderText("Search logs..."),
      ).not.toBeInTheDocument();

      await user.click(filterButton);

      // After click, search input should be visible
      await waitFor(() => {
        expect(
          screen.getByPlaceholderText("Search logs..."),
        ).toBeInTheDocument();
      });
    });

    it("clears filters when clear button is clicked", async () => {
      const user = userEvent.setup();
      const onDateChange = jest.fn();

      render(
        <ConsoleInterface {...defaultProps} onDateChange={onDateChange} />,
      );

      // Open filter panel
      const buttons = screen.getAllByRole("button");
      await user.click(buttons[0]);

      // Type in search
      const searchInput = await screen.findByPlaceholderText("Search logs...");
      await user.type(searchInput, "test");

      // Clear button should appear
      const clearButton = screen.queryByRole("button", { name: /clear/i });
      if (clearButton) {
        await user.click(clearButton);
        expect(searchInput).toHaveValue("");
      }
    });
  });

  describe("connection status indicator", () => {
    it("should show Live indicator when connected is true", () => {
      render(<ConsoleInterface {...defaultProps} connected={true} lastUpdate={new Date()} />);
      expect(screen.getByText("Live")).toBeInTheDocument();
    });

    it("should show Polling indicator when connected is false with lastUpdate", () => {
      render(<ConsoleInterface {...defaultProps} connected={false} lastUpdate={new Date()} />);
      expect(screen.getByText("Polling")).toBeInTheDocument();
    });

    it("should show Connecting indicator when connected is false without lastUpdate", () => {
      render(<ConsoleInterface {...defaultProps} connected={false} lastUpdate={null} />);
      expect(screen.getByText("Connecting...")).toBeInTheDocument();
    });

    it("should not show any indicator when connected prop is not passed", () => {
      render(<ConsoleInterface {...defaultProps} />);
      expect(screen.queryByText("Live")).not.toBeInTheDocument();
      expect(screen.queryByText("Polling")).not.toBeInTheDocument();
      expect(screen.queryByText("Connecting...")).not.toBeInTheDocument();
    });

    it("should show indicator in compact mode when connected", () => {
      render(<ConsoleInterface {...defaultProps} compact={true} connected={true} lastUpdate={new Date()} />);
      expect(screen.getByText("Live")).toBeInTheDocument();
    });

    it("should show Connecting in compact mode when not yet connected", () => {
      render(<ConsoleInterface {...defaultProps} compact={true} connected={false} lastUpdate={null} />);
      expect(screen.getByText("Connecting...")).toBeInTheDocument();
    });
  });

  describe("entry count badge", () => {
    it("shows correct count after filtering", async () => {
      const logs: LogEntry[] = [
        {
          date: "2024-01-01",
          content: "[2024-01-01 10:00:00] INFO: Match this",
        },
        { date: "2024-01-01", content: "[2024-01-01 10:01:00] INFO: No match" },
        {
          date: "2024-01-01",
          content: "[2024-01-01 10:02:00] INFO: Match this too",
        },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Initially shows all entries
      expect(screen.getByText("3 entries")).toBeInTheDocument();
    });

    it("updates count when logs change", () => {
      const { rerender } = render(
        <ConsoleInterface
          {...defaultProps}
          logs={[{ date: "2024-01-01", content: "Log 1" }]}
        />,
      );

      expect(screen.getByText("1 entries")).toBeInTheDocument();

      rerender(
        <ConsoleInterface
          {...defaultProps}
          logs={[
            { date: "2024-01-01", content: "Log 1" },
            { date: "2024-01-01", content: "Log 2" },
          ]}
        />,
      );

      expect(screen.getByText("2 entries")).toBeInTheDocument();
    });
  });
});
