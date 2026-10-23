import React from "react";
import { render, screen, fireEvent, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { ConsoleInterface } from "./console-interface";
import type { LogEntry } from "@/types/logs";

/** A log line as the api sends it, keyed by its day and content. */
const line = (content: string, date = "20251227"): LogEntry => ({
  key: `${date}:${content}`,
  date,
  timestamp: null,
  content,
});

jest.mock("react-virtuoso", () => ({
  Virtuoso: React.forwardRef(
    (
      {
        data,
        itemContent,
        components,
        followOutput,
        atBottomStateChange,
        atTopStateChange,
        overscan,
        className,
      }: any,
      ref: any,
    ) => (
      <div data-testid="virtuoso-container" className={className}>
        {components?.Header && <components.Header />}
        {data?.map((item: any, index: number) => (
          <div key={index}>{itemContent(index, item)}</div>
        ))}
      </div>
    ),
  ),
}));

describe("ConsoleInterface", () => {
  const defaultProps = {
    botId: "test-bot",
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
    it("renders empty state when no logs", () => {
      render(<ConsoleInterface {...defaultProps} />);

      expect(screen.getByText("> Waiting for logs...")).toBeInTheDocument();
    });

    it("renders loading state", () => {
      render(<ConsoleInterface {...defaultProps} loading={true} />);

      // Should show loading spinner (RefreshCw with animate-spin)
      const spinner = document.querySelector(".animate-spin");
      expect(spinner).toBeInTheDocument();
    });

    it("renders log entries", () => {
      const logs: LogEntry[] = [
        line("First log message", "20251227"),
        line("Second log message", "20251227"),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      expect(screen.getByText("First log message")).toBeInTheDocument();
      expect(screen.getByText("Second log message")).toBeInTheDocument();
    });

    it("renders metrics with special styling", () => {
      const logs: LogEntry[] = [
        line(
          '{"_metric": "trade", "symbol": "BTC/USD", "price": 50000}',
          "20251227",
        ),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Should show the metric type badge
      expect(screen.getByText("trade")).toBeInTheDocument();
      // Should show the metric values
      expect(screen.getByText(/symbol: BTC\/USD/)).toBeInTheDocument();
    });

    it("renders structured JSON logs as messages", () => {
      const logs: LogEntry[] = [
        line(
          '{"level": "info", "msg": "Bot initialized successfully"}',
          "20251227",
        ),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Should show the message, not the raw JSON
      expect(
        screen.getByText("Bot initialized successfully"),
      ).toBeInTheDocument();
    });

    it("shows entry count in header", () => {
      const logs: LogEntry[] = [
        line("Log 1", "20251227"),
        line("Log 2", "20251227"),
        line("Log 3", "20251227"),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      expect(screen.getByText("3 entries")).toBeInTheDocument();
    });

    it("hides header in compact mode", () => {
      const logs: LogEntry[] = [line("Log message", "20251227")];

      render(<ConsoleInterface {...defaultProps} logs={logs} compact={true} />);

      expect(screen.queryByText("CONSOLE")).not.toBeInTheDocument();
    });

    it("displays timestamps correctly", () => {
      const logs: LogEntry[] = [
        line("[2025-12-27 14:30:45] Hello world", "20251227"),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Timestamp is split into date and time in separate spans
      expect(screen.getByText("12-27")).toBeInTheDocument();
      expect(screen.getByText("14:30:45")).toBeInTheDocument();
    });

    it("shows message for logs without timestamp", () => {
      const logs: LogEntry[] = [line("No timestamp here", "20251227")];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Message should be visible
      expect(screen.getByText("No timestamp here")).toBeInTheDocument();
    });
  });

  describe("log level indicators", () => {
    it("shows ERROR level badge", () => {
      const logs: LogEntry[] = [
        line("ERROR: Something went wrong", "20251227"),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      expect(screen.getByText("ERROR")).toBeInTheDocument();
    });

    it("shows WARN level badge", () => {
      const logs: LogEntry[] = [line("WARN: Be careful", "20251227")];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      expect(screen.getByText("WARN")).toBeInTheDocument();
    });

    it("shows INFO level badge", () => {
      const logs: LogEntry[] = [line("INFO: All good", "20251227")];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      expect(screen.getByText("INFO")).toBeInTheDocument();
    });

    it("shows DEBUG level badge", () => {
      const logs: LogEntry[] = [line("DEBUG: Debugging info", "20251227")];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      expect(screen.getByText("DEBUG")).toBeInTheDocument();
    });
  });

  describe("copy functionality", () => {
    it("has copy button in log entry", async () => {
      const logs: LogEntry[] = [line("Copy me", "20251227")];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const copyButton = screen.getByTitle("Copy");
      expect(copyButton).toBeInTheDocument();
    });

    it("copies log message to clipboard", async () => {
      // Mock clipboard API for this specific test
      const writeText = jest.fn().mockResolvedValue(undefined);
      Object.assign(navigator, {
        clipboard: { writeText },
      });

      const logs: LogEntry[] = [line("Copy this message", "20251227")];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const copyButton = screen.getByTitle("Copy");
      fireEvent.click(copyButton);

      await waitFor(() => {
        expect(writeText).toHaveBeenCalledWith("Copy this message");
      });
    });

    it("copies metric data to clipboard", async () => {
      // Mock clipboard API for this specific test
      const writeText = jest.fn().mockResolvedValue(undefined);
      Object.assign(navigator, {
        clipboard: { writeText },
      });

      const logs: LogEntry[] = [
        line(
          '{"_metric": "trade", "symbol": "ETH/USD", "amount": 10}',
          "20251227",
        ),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Metric copy button also has title="Copy"
      const copyButton = screen.getByTitle("Copy");
      fireEvent.click(copyButton);

      await waitFor(() => {
        // Metric data is copied as key: value lines
        expect(writeText).toHaveBeenCalledWith("symbol: ETH/USD\namount: 10");
      });
    });

    it("copies successfully when clicking copy button", async () => {
      // Mock clipboard API
      const writeText = jest.fn().mockResolvedValue(undefined);
      Object.assign(navigator, {
        clipboard: { writeText },
      });

      const logs: LogEntry[] = [line("Copy me", "20251227")];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const copyButton = screen.getByTitle("Copy");
      fireEvent.click(copyButton);

      await waitFor(() => {
        expect(writeText).toHaveBeenCalledWith("Copy me");
      });
    });
  });

  describe("search and filtering", () => {
    // Helper to find filter button by its icon
    const findFilterButton = () => {
      const buttons = screen.getAllByRole("button");
      return buttons.find((btn) =>
        btn.querySelector(".lucide-funnel, .lucide-filter"),
      );
    };

    it("filters logs by search query", async () => {
      const logs: LogEntry[] = [
        line("Apple message", "20251227"),
        line("Banana message", "20251227"),
        line("Cherry message", "20251227"),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Click filter button to show search
      const filterButton = findFilterButton();
      expect(filterButton).toBeDefined();
      fireEvent.click(filterButton!);

      // Find the search input and type
      const searchInput = screen.getByPlaceholderText("Search logs...");
      fireEvent.change(searchInput, { target: { value: "Banana" } });

      // Only Banana should be visible
      expect(screen.queryByText("Apple message")).not.toBeInTheDocument();
      expect(screen.getByText("Banana message")).toBeInTheDocument();
      expect(screen.queryByText("Cherry message")).not.toBeInTheDocument();
    });

    it("search is case insensitive", async () => {
      const logs: LogEntry[] = [
        line("UPPERCASE message", "20251227"),
        line("lowercase message", "20251227"),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Click filter button
      const filterButton = findFilterButton();
      fireEvent.click(filterButton!);

      const searchInput = screen.getByPlaceholderText("Search logs...");
      fireEvent.change(searchInput, { target: { value: "LOWERCASE" } });

      expect(screen.queryByText("UPPERCASE message")).not.toBeInTheDocument();
      expect(screen.getByText("lowercase message")).toBeInTheDocument();
    });

    it("searches the text a stored line shows, not its JSON", async () => {
      const stored = (key: string, message: string): LogEntry => ({
        key,
        date: "20261022",
        timestamp: "2026-10-22T10:00:00.000Z",
        content: JSON.stringify({
          _key: key,
          message,
          timestamp: "2026-10-22T10:00:00.000Z",
        }).replace("&", "\\u0026"),
      });
      const logs = [
        stored("1792713599999000000-0", "Bought BTC & ETH"),
        stored("1792713599999000000-1", "Sold"),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);
      fireEvent.click(findFilterButton()!);
      const searchInput = screen.getByPlaceholderText("Search logs...");

      fireEvent.change(searchInput, { target: { value: "btc & eth" } });
      expect(screen.getByText("1 entries")).toBeInTheDocument();

      fireEvent.change(searchInput, { target: { value: "_key" } });
      expect(screen.getByText("> No logs match filter")).toBeInTheDocument();
    });

    it("shows 'No logs match filter' when search has no results", async () => {
      const logs: LogEntry[] = [line("Some log", "20251227")];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Click filter button
      const filterButton = findFilterButton();
      fireEvent.click(filterButton!);

      const searchInput = screen.getByPlaceholderText("Search logs...");
      fireEvent.change(searchInput, { target: { value: "nonexistent" } });

      expect(screen.getByText("> No logs match filter")).toBeInTheDocument();
    });

    it("updates entry count based on filter", async () => {
      const logs: LogEntry[] = [
        line("Match this", "20251227"),
        line("Match this too", "20251227"),
        line("Ignore this", "20251227"),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Initially shows 3 entries
      expect(screen.getByText("3 entries")).toBeInTheDocument();

      // Click filter button
      const filterButton = findFilterButton();
      fireEvent.click(filterButton!);

      const searchInput = screen.getByPlaceholderText("Search logs...");
      fireEvent.change(searchInput, { target: { value: "Match" } });

      // Should show 2 entries after filter
      expect(screen.getByText("2 entries")).toBeInTheDocument();
    });
  });

  describe("toolbar actions", () => {
    // Helper to find buttons by their icon class
    const findButtonByIcon = (iconClass: string) => {
      const buttons = screen.getAllByRole("button");
      return buttons.find((btn) => btn.querySelector(`.${iconClass}`));
    };

    it("calls onRefresh when refresh button clicked", () => {
      const onRefresh = jest.fn();

      render(<ConsoleInterface {...defaultProps} onRefresh={onRefresh} />);

      // Find refresh button by its icon
      const refreshButton = findButtonByIcon("lucide-refresh-cw");
      expect(refreshButton).toBeDefined();

      fireEvent.click(refreshButton!);

      expect(onRefresh).toHaveBeenCalled();
    });

    it("calls onExport when export button clicked", () => {
      const onExport = jest.fn();

      render(<ConsoleInterface {...defaultProps} onExport={onExport} />);

      // Find export button by its download icon
      const exportButton = findButtonByIcon("lucide-download");
      expect(exportButton).toBeDefined();

      fireEvent.click(exportButton!);

      expect(onExport).toHaveBeenCalled();
    });

    it("has auto-scroll toggle button", async () => {
      render(<ConsoleInterface {...defaultProps} />);

      // Auto-scroll defaults to on, so Pause icon is shown
      const pauseButton = findButtonByIcon("lucide-pause");
      expect(pauseButton).toBeDefined();

      // Button should be clickable (toggles to Play)
      fireEvent.click(pauseButton!);
      // No error thrown means button works
    });

    it("disables refresh button while loading", () => {
      render(<ConsoleInterface {...defaultProps} loading={true} />);

      const refreshButton = findButtonByIcon("lucide-refresh-cw");
      expect(refreshButton).toBeDefined();
      expect(refreshButton).toBeDisabled();
    });
  });

  describe("compact mode", () => {
    it("shows inline search in compact mode", () => {
      render(<ConsoleInterface {...defaultProps} compact={true} />);

      const searchInput = screen.getByPlaceholderText("Search...");
      expect(searchInput).toBeInTheDocument();
    });

    it("hides filter button in compact mode", () => {
      render(<ConsoleInterface {...defaultProps} compact={true} />);

      // Filter button should not be present
      const buttons = screen.getAllByRole("button");
      // In compact mode, we have fewer buttons (no filter, no CONSOLE header)
      expect(buttons.length).toBeLessThan(5);
    });
  });

  describe("expand/collapse interactions", () => {
    it("displays long messages", () => {
      const longMessage = "A".repeat(200);
      const logs: LogEntry[] = [line(longMessage, "20251227")];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Message should be visible
      expect(screen.getByText(longMessage)).toBeInTheDocument();
    });

    it("expands message on click", async () => {
      const logs: LogEntry[] = [line("Click to expand me", "20251227")];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const messageSpan = screen.getByText("Click to expand me");
      const logEntry = messageSpan.closest(".group");

      if (logEntry) {
        // Click to expand
        fireEvent.click(logEntry);

        // After click, should show expanded content in a pre element
        await waitFor(() => {
          const expandedContent = document.querySelector("pre");
          expect(expandedContent).toBeInTheDocument();
        });
      }
    });
  });

  describe("timestamp and indicator display", () => {
    it("displays timestamp from log", () => {
      const logs: LogEntry[] = [
        line("[2025-12-27 10:00:00] Message", "20251227"),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Timestamp should be visible
      expect(screen.getByText("10:00:00")).toBeInTheDocument();
    });

    it("labels its times as UTC", () => {
      const logs: LogEntry[] = [
        {
          key: "20251227:0",
          date: "20251227",
          timestamp: null,
          content: "[2025-12-27 10:00:00] Message",
        },
      ];

      for (const compact of [false, true]) {
        const { unmount } = render(
          <ConsoleInterface {...defaultProps} logs={logs} compact={compact} />,
        );
        expect(screen.getByText("UTC")).toBeInTheDocument();
        expect(screen.getByText("10:00:00").closest("[title]")).toHaveAttribute(
          "title",
          "12-27 10:00:00 UTC",
        );
        unmount();
      }
    });

    it("displays level indicator", () => {
      const logs: LogEntry[] = [line("INFO: Message", "20251227")];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Level badge should be visible
      expect(screen.getByText("INFO")).toBeInTheDocument();
    });
  });

  describe("log order toggle", () => {
    const findSortToggle = () => {
      const buttons = screen.getAllByRole("button");
      return buttons.find((btn) => btn.querySelector(".lucide-arrow-down-up"));
    };

    it("displays logs in received order (no client-side reversal)", () => {
      const logs: LogEntry[] = [
        line("First log", "20251227"),
        line("Second log", "20251227"),
        line("Third log", "20251227"),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const container = screen.getByTestId("virtuoso-container");
      const items = container.querySelectorAll(":scope > div");
      const texts = Array.from(items).map((el) => el.textContent);
      const firstIdx = texts.findIndex((t) => t?.includes("First log"));
      const thirdIdx = texts.findIndex((t) => t?.includes("Third log"));
      // Logs displayed in the order received from the API (no reversal)
      expect(firstIdx).toBeLessThan(thirdIdx);
    });

    it("calls onSortChange when sort toggle is clicked", () => {
      const onSortChange = jest.fn();

      render(
        <ConsoleInterface
          {...defaultProps}
          logs={[line("Log", "20251227")]}
          sort="desc"
          onSortChange={onSortChange}
        />,
      );

      const sortButton = findSortToggle()!;
      fireEvent.click(sortButton);

      expect(onSortChange).toHaveBeenCalledWith("asc");
    });

    it("calls onSortChange with desc when currently asc", () => {
      const onSortChange = jest.fn();

      render(
        <ConsoleInterface
          {...defaultProps}
          logs={[line("Log", "20251227")]}
          sort="asc"
          onSortChange={onSortChange}
        />,
      );

      const sortButton = findSortToggle()!;
      fireEvent.click(sortButton);

      expect(onSortChange).toHaveBeenCalledWith("desc");
    });

    it("has correct tooltip for sort order", () => {
      const { rerender } = render(
        <ConsoleInterface
          {...defaultProps}
          logs={[line("Log", "20251227")]}
          sort="desc"
        />,
      );

      const sortButton = findSortToggle()!;
      expect(sortButton).toHaveAttribute("title", "Newest first");

      rerender(
        <ConsoleInterface
          {...defaultProps}
          logs={[line("Log", "20251227")]}
          sort="asc"
        />,
      );
      expect(sortButton).toHaveAttribute("title", "Oldest first");
    });

    it("renders sort toggle in compact mode", () => {
      render(
        <ConsoleInterface
          {...defaultProps}
          compact={true}
          logs={[line("Log", "20251227")]}
        />,
      );

      const sortButton = findSortToggle();
      expect(sortButton).toBeDefined();
    });
  });

  describe("metric rendering", () => {
    it("shows metric type as badge", () => {
      const logs: LogEntry[] = [
        line('{"_metric": "portfolio_value", "value": 10000}', "20251227"),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Metric type should be shown as a badge
      expect(screen.getByText("portfolio_value")).toBeInTheDocument();
    });

    it("excludes _metric key from display values", () => {
      const logs: LogEntry[] = [
        line(
          '{"_metric": "trade", "symbol": "BTC", "price": 50000}',
          "20251227",
        ),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Should show symbol and price, but not _metric
      expect(screen.getByText(/symbol: BTC/)).toBeInTheDocument();
      expect(screen.getByText(/price: 50000/)).toBeInTheDocument();
      // The metric type is shown as a badge, not in the values
    });

    it("formats nested objects in metrics", () => {
      const logs: LogEntry[] = [
        line(
          '{"_metric": "order", "details": {"id": 123, "status": "filled"}}',
          "20251227",
        ),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Nested object should be JSON stringified
      expect(screen.getByText(/details:/)).toBeInTheDocument();
    });

    it("displays metric value", () => {
      const logs: LogEntry[] = [
        line('{"_metric": "test", "value": 1}', "20251227"),
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Metric type and value should be visible
      expect(screen.getByText("test")).toBeInTheDocument();
      expect(screen.getByText(/value: 1/)).toBeInTheDocument();
    });
  });
});
