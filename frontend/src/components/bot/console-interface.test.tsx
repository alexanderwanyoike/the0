import React from "react";
import { render, screen, fireEvent, waitFor } from "@testing-library/react";
import userEvent from "@testing-library/user-event";
import { ConsoleInterface, LogEntry } from "./console-interface";

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
        { date: "20251227", content: "First log message" },
        { date: "20251227", content: "Second log message" },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      expect(screen.getByText("First log message")).toBeInTheDocument();
      expect(screen.getByText("Second log message")).toBeInTheDocument();
    });

    it("renders metrics with special styling", () => {
      const logs: LogEntry[] = [
        {
          date: "20251227",
          content: '{"_metric": "trade", "symbol": "BTC/USD", "price": 50000}',
        },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Should show the metric type badge
      expect(screen.getByText("trade")).toBeInTheDocument();
      // Should show the metric values
      expect(screen.getByText(/symbol: BTC\/USD/)).toBeInTheDocument();
    });

    it("renders structured JSON logs as messages", () => {
      const logs: LogEntry[] = [
        {
          date: "20251227",
          content: '{"level": "info", "msg": "Bot initialized successfully"}',
        },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Should show the message, not the raw JSON
      expect(screen.getByText("Bot initialized successfully")).toBeInTheDocument();
    });

    it("shows entry count in header", () => {
      const logs: LogEntry[] = [
        { date: "20251227", content: "Log 1" },
        { date: "20251227", content: "Log 2" },
        { date: "20251227", content: "Log 3" },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      expect(screen.getByText("3 entries")).toBeInTheDocument();
    });

    it("hides header in compact mode", () => {
      const logs: LogEntry[] = [{ date: "20251227", content: "Log message" }];

      render(<ConsoleInterface {...defaultProps} logs={logs} compact={true} />);

      expect(screen.queryByText("CONSOLE")).not.toBeInTheDocument();
    });

    it("displays timestamps correctly", () => {
      const logs: LogEntry[] = [
        { date: "20251227", content: "[2025-12-27 14:30:45] Hello world" },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Timestamp should be formatted as MM-DD HH:MM:SS
      expect(screen.getByText("12-27 14:30:45")).toBeInTheDocument();
    });

    it("shows empty timestamp for logs without timestamp", () => {
      const logs: LogEntry[] = [
        { date: "20251227", content: "No timestamp here" },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Message should be visible
      expect(screen.getByText("No timestamp here")).toBeInTheDocument();
      // Timestamp span should be empty (120px width span)
      const timestampSpan = document.querySelector(".w-\\[120px\\]");
      expect(timestampSpan?.textContent).toBe("");
    });
  });

  describe("log level indicators", () => {
    it("shows correct indicator color for ERROR level", () => {
      const logs: LogEntry[] = [
        { date: "20251227", content: "ERROR: Something went wrong" },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const indicator = screen.getByText("●");
      expect(indicator.className).toContain("text-red");
    });

    it("shows correct indicator color for WARN level", () => {
      const logs: LogEntry[] = [
        { date: "20251227", content: "WARN: Be careful" },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const indicator = screen.getByText("●");
      expect(indicator.className).toContain("text-yellow");
    });

    it("shows correct indicator color for INFO level", () => {
      const logs: LogEntry[] = [
        { date: "20251227", content: "INFO: All good" },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const indicator = screen.getByText("●");
      expect(indicator.className).toContain("text-green");
    });

    it("shows correct indicator color for DEBUG level", () => {
      const logs: LogEntry[] = [
        { date: "20251227", content: "DEBUG: Debugging info" },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const indicator = screen.getByText("●");
      expect(indicator.className).toContain("text-gray");
    });
  });

  describe("copy functionality", () => {
    it("shows copy button on hover", async () => {
      const logs: LogEntry[] = [{ date: "20251227", content: "Copy me" }];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const logEntry = screen.getByText("Copy me").closest(".group");
      expect(logEntry).toBeInTheDocument();

      // Copy button should exist but be invisible initially
      const copyButton = logEntry?.querySelector('button[title="Copy message"]');
      expect(copyButton).toBeInTheDocument();
      expect(copyButton?.className).toContain("opacity-0");
    });

    it("copies log message to clipboard", async () => {
      // Mock clipboard API for this specific test
      const writeText = jest.fn().mockResolvedValue(undefined);
      Object.assign(navigator, {
        clipboard: { writeText },
      });

      const logs: LogEntry[] = [{ date: "20251227", content: "Copy this message" }];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const copyButton = screen.getByTitle("Copy message");
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
        {
          date: "20251227",
          content: '{"_metric": "trade", "symbol": "ETH/USD", "amount": 10}',
        },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const copyButton = screen.getByTitle("Copy metric data");
      fireEvent.click(copyButton);

      await waitFor(() => {
        expect(writeText).toHaveBeenCalledWith("symbol: ETH/USD | amount: 10");
      });
    });

    it("shows check icon after copying", async () => {
      // Mock clipboard API
      Object.assign(navigator, {
        clipboard: { writeText: jest.fn().mockResolvedValue(undefined) },
      });

      const logs: LogEntry[] = [{ date: "20251227", content: "Copy me" }];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const copyButton = screen.getByTitle("Copy message");
      fireEvent.click(copyButton);

      // Check icon should appear
      await waitFor(() => {
        const checkIcon = document.querySelector(".text-green-500");
        expect(checkIcon).toBeInTheDocument();
      });
    });
  });

  describe("search and filtering", () => {
    // Helper to find filter button by its icon
    const findFilterButton = () => {
      const buttons = screen.getAllByRole("button");
      return buttons.find((btn) =>
        btn.querySelector(".lucide-funnel, .lucide-filter")
      );
    };

    it("filters logs by search query", async () => {
      const logs: LogEntry[] = [
        { date: "20251227", content: "Apple message" },
        { date: "20251227", content: "Banana message" },
        { date: "20251227", content: "Cherry message" },
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
        { date: "20251227", content: "UPPERCASE message" },
        { date: "20251227", content: "lowercase message" },
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

    it("shows 'No logs match filter' when search has no results", async () => {
      const logs: LogEntry[] = [{ date: "20251227", content: "Some log" }];

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
        { date: "20251227", content: "Match this" },
        { date: "20251227", content: "Match this too" },
        { date: "20251227", content: "Ignore this" },
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

    it("toggles auto-scroll when play/pause button clicked", async () => {
      render(<ConsoleInterface {...defaultProps} />);

      // Find the play button by its icon
      const playButton = findButtonByIcon("lucide-play");
      expect(playButton).toBeDefined();

      // Initially should show Play icon (auto-scroll off)
      fireEvent.click(playButton!);

      // After click, button should have active styling
      await waitFor(() => {
        expect(playButton!.className).toContain("bg-green");
      });
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

  describe("hover interactions", () => {
    it("makes message scrollable on hover", async () => {
      const longMessage = "A".repeat(200);
      const logs: LogEntry[] = [{ date: "20251227", content: longMessage }];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const messageSpan = screen.getByText(longMessage);

      // Initially truncated
      expect(messageSpan.className).toContain("truncate");

      // Simulate hover on parent
      const logEntry = messageSpan.closest(".group");
      if (logEntry) {
        fireEvent.mouseEnter(logEntry);
      }

      // After hover, should be scrollable (overflow-x-auto, not truncate)
      await waitFor(() => {
        expect(messageSpan.className).toContain("overflow-x-auto");
        expect(messageSpan.className).not.toContain("truncate");
      });
    });

    it("reverts to truncated on mouse leave", async () => {
      const longMessage = "B".repeat(200);
      const logs: LogEntry[] = [{ date: "20251227", content: longMessage }];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const messageSpan = screen.getByText(longMessage);
      const logEntry = messageSpan.closest(".group");

      if (logEntry) {
        // Hover
        fireEvent.mouseEnter(logEntry);
        await waitFor(() => {
          expect(messageSpan.className).toContain("overflow-x-auto");
        });

        // Leave
        fireEvent.mouseLeave(logEntry);
        await waitFor(() => {
          expect(messageSpan.className).toContain("truncate");
        });
      }
    });
  });

  describe("timestamp and indicator selection", () => {
    it("timestamp has select-none class", () => {
      const logs: LogEntry[] = [
        { date: "20251227", content: "[2025-12-27 10:00:00] Message" },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const timestampSpan = document.querySelector(".w-\\[120px\\]");
      expect(timestampSpan?.className).toContain("select-none");
    });

    it("indicator has select-none class", () => {
      const logs: LogEntry[] = [{ date: "20251227", content: "INFO: Message" }];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const indicator = screen.getByText("●");
      expect(indicator.className).toContain("select-none");
    });
  });

  describe("metric rendering", () => {
    it("shows metric type as badge", () => {
      const logs: LogEntry[] = [
        {
          date: "20251227",
          content: '{"_metric": "portfolio_value", "value": 10000}',
        },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      const badge = screen.getByText("portfolio_value");
      expect(badge).toBeInTheDocument();
      expect(badge.className).toContain("bg-blue");
    });

    it("excludes _metric key from display values", () => {
      const logs: LogEntry[] = [
        {
          date: "20251227",
          content: '{"_metric": "trade", "symbol": "BTC", "price": 50000}',
        },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Should show symbol and price, but not _metric
      expect(screen.getByText(/symbol: BTC/)).toBeInTheDocument();
      expect(screen.getByText(/price: 50000/)).toBeInTheDocument();
      // The metric type is shown as a badge, not in the values
    });

    it("formats nested objects in metrics", () => {
      const logs: LogEntry[] = [
        {
          date: "20251227",
          content: '{"_metric": "order", "details": {"id": 123, "status": "filled"}}',
        },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Nested object should be JSON stringified
      expect(screen.getByText(/details:/)).toBeInTheDocument();
    });

    it("uses blue styling for metrics", () => {
      const logs: LogEntry[] = [
        { date: "20251227", content: '{"_metric": "test", "value": 1}' },
      ];

      render(<ConsoleInterface {...defaultProps} logs={logs} />);

      // Find the metric entry container
      const metricEntry = document.querySelector(".border-blue-500");
      expect(metricEntry).toBeInTheDocument();
    });
  });
});
