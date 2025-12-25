import { render, screen, renderHook } from "@testing-library/react";
import { ReactNode } from "react";
import {
  BotEventsProvider,
  useBotEventsContext,
  useThe0Events,
} from "../bot-events-context";

// Mock the useBotEvents hook
const mockEvents = [
  {
    timestamp: new Date("2024-01-01T10:00:00"),
    type: "log" as const,
    data: "Test log message",
    level: "INFO" as const,
    raw: "[2024-01-01 10:00:00] INFO: Test log message",
  },
  {
    timestamp: new Date("2024-01-01T10:01:00"),
    type: "metric" as const,
    data: { _metric: "portfolio_value", value: 10000 },
    metricType: "portfolio_value",
    raw: '[2024-01-01 10:01:00] {"_metric": "portfolio_value", "value": 10000}',
  },
];

const mockUtils = {
  since: jest.fn(() => mockEvents),
  between: jest.fn(() => mockEvents),
  filterByType: jest.fn(() => [mockEvents[1]]),
  logs: jest.fn(() => [mockEvents[0]]),
  metrics: jest.fn(() => [mockEvents[1]]),
  latest: jest.fn(() => mockEvents[1]),
  getMetricTypes: jest.fn(() => ["portfolio_value"]),
  groupByMetricType: jest.fn(() => ({ portfolio_value: [mockEvents[1]] })),
  groupByTimeWindow: jest.fn(() => [[mockEvents[0]], [mockEvents[1]]]),
  groupByRun: jest.fn(() => [mockEvents]),
  latestByType: jest.fn(() => ({ portfolio_value: mockEvents[1] })),
  countByType: jest.fn(() => ({ portfolio_value: 1 })),
  extractTimeSeries: jest.fn(() => [
    { timestamp: new Date("2024-01-01T10:01:00"), value: 10000 },
  ]),
};

const mockRefresh = jest.fn();
const mockSetDateFilter = jest.fn();
const mockSetDateRangeFilter = jest.fn();
const mockExportLogs = jest.fn();

jest.mock("@/hooks/use-bot-events", () => ({
  useBotEvents: jest.fn(() => ({
    events: mockEvents,
    loading: false,
    error: null,
    utils: mockUtils,
    refresh: mockRefresh,
    setDateFilter: mockSetDateFilter,
    setDateRangeFilter: mockSetDateRangeFilter,
    exportLogs: mockExportLogs,
    rawLogs: [],
  })),
}));

describe("BotEventsContext", () => {
  beforeEach(() => {
    jest.clearAllMocks();
    // Reset window context
    if (typeof window !== "undefined") {
      delete (window as any).__THE0_EVENTS_CONTEXT__;
    }
  });

  describe("BotEventsProvider", () => {
    it("renders children", () => {
      render(
        <BotEventsProvider botId="bot-123">
          <div data-testid="child">Child content</div>
        </BotEventsProvider>,
      );

      expect(screen.getByTestId("child")).toBeInTheDocument();
      expect(screen.getByText("Child content")).toBeInTheDocument();
    });

    it("provides context value to children", () => {
      const TestComponent = () => {
        const context = useBotEventsContext();
        return (
          <div>
            <span data-testid="bot-id">{context.botId}</span>
            <span data-testid="event-count">{context.events.length}</span>
            <span data-testid="loading">{context.loading.toString()}</span>
          </div>
        );
      };

      render(
        <BotEventsProvider botId="test-bot-id">
          <TestComponent />
        </BotEventsProvider>,
      );

      expect(screen.getByTestId("bot-id")).toHaveTextContent("test-bot-id");
      expect(screen.getByTestId("event-count")).toHaveTextContent("2");
      expect(screen.getByTestId("loading")).toHaveTextContent("false");
    });

    it("passes autoRefresh prop to useBotEvents", () => {
      const { useBotEvents } = require("@/hooks/use-bot-events");

      render(
        <BotEventsProvider
          botId="bot-123"
          autoRefresh={true}
          refreshInterval={5000}
        >
          <div>Child</div>
        </BotEventsProvider>,
      );

      expect(useBotEvents).toHaveBeenCalledWith(
        expect.objectContaining({
          botId: "bot-123",
          autoRefresh: true,
          refreshInterval: 5000,
        }),
      );
    });

    it("passes dateRange prop to useBotEvents", () => {
      const { useBotEvents } = require("@/hooks/use-bot-events");
      const dateRange = { start: "20240101", end: "20240131" };

      render(
        <BotEventsProvider botId="bot-123" dateRange={dateRange}>
          <div>Child</div>
        </BotEventsProvider>,
      );

      expect(useBotEvents).toHaveBeenCalledWith(
        expect.objectContaining({
          botId: "bot-123",
          dateRange,
        }),
      );
    });

    it("uses default values for optional props", () => {
      const { useBotEvents } = require("@/hooks/use-bot-events");

      render(
        <BotEventsProvider botId="bot-123">
          <div>Child</div>
        </BotEventsProvider>,
      );

      expect(useBotEvents).toHaveBeenCalledWith(
        expect.objectContaining({
          botId: "bot-123",
          autoRefresh: true,
          refreshInterval: 30000,
        }),
      );
    });
  });

  describe("useBotEventsContext", () => {
    it("returns context value when used within provider", () => {
      const wrapper = ({ children }: { children: ReactNode }) => (
        <BotEventsProvider botId="bot-123">{children}</BotEventsProvider>
      );

      const { result } = renderHook(() => useBotEventsContext(), { wrapper });

      expect(result.current.botId).toBe("bot-123");
      expect(result.current.events).toEqual(mockEvents);
      expect(result.current.loading).toBe(false);
      expect(result.current.error).toBeNull();
      expect(result.current.utils).toBeDefined();
      expect(result.current.refresh).toBe(mockRefresh);
    });

    it("throws error when used outside provider", () => {
      // Suppress console.error for this test
      const consoleSpy = jest
        .spyOn(console, "error")
        .mockImplementation(() => {});

      expect(() => {
        renderHook(() => useBotEventsContext());
      }).toThrow("useBotEventsContext must be used within a BotEventsProvider");

      consoleSpy.mockRestore();
    });

    it("provides utils object with all methods", () => {
      const wrapper = ({ children }: { children: ReactNode }) => (
        <BotEventsProvider botId="bot-123">{children}</BotEventsProvider>
      );

      const { result } = renderHook(() => useBotEventsContext(), { wrapper });

      expect(result.current.utils.since).toBeDefined();
      expect(result.current.utils.between).toBeDefined();
      expect(result.current.utils.filterByType).toBeDefined();
      expect(result.current.utils.logs).toBeDefined();
      expect(result.current.utils.metrics).toBeDefined();
      expect(result.current.utils.latest).toBeDefined();
      expect(result.current.utils.getMetricTypes).toBeDefined();
      expect(result.current.utils.groupByMetricType).toBeDefined();
      expect(result.current.utils.groupByTimeWindow).toBeDefined();
      expect(result.current.utils.groupByRun).toBeDefined();
      expect(result.current.utils.latestByType).toBeDefined();
      expect(result.current.utils.countByType).toBeDefined();
      expect(result.current.utils.extractTimeSeries).toBeDefined();
    });
  });

  describe("useThe0Events (alias)", () => {
    it("is an alias for useBotEventsContext", () => {
      const wrapper = ({ children }: { children: ReactNode }) => (
        <BotEventsProvider botId="bot-123">{children}</BotEventsProvider>
      );

      const { result: result1 } = renderHook(() => useBotEventsContext(), {
        wrapper,
      });
      const { result: result2 } = renderHook(() => useThe0Events(), {
        wrapper,
      });

      expect(result1.current.botId).toBe(result2.current.botId);
      expect(result1.current.events).toEqual(result2.current.events);
    });

    it("throws same error when used outside provider", () => {
      const consoleSpy = jest
        .spyOn(console, "error")
        .mockImplementation(() => {});

      expect(() => {
        renderHook(() => useThe0Events());
      }).toThrow("useBotEventsContext must be used within a BotEventsProvider");

      consoleSpy.mockRestore();
    });
  });

  describe("shared context (window global)", () => {
    it("uses shared context mechanism for SDK compatibility", () => {
      // The context is created at module load time, not at render time
      // This test verifies the provider works correctly with the shared context pattern
      render(
        <BotEventsProvider botId="bot-123">
          <div>Child</div>
        </BotEventsProvider>,
      );

      // Provider should render successfully with shared context pattern
      expect(screen.getByText("Child")).toBeInTheDocument();
    });
  });

  describe("context value updates", () => {
    it("provides refresh function that can be called", () => {
      const TestComponent = () => {
        const { refresh } = useBotEventsContext();
        return <button onClick={refresh}>Refresh</button>;
      };

      render(
        <BotEventsProvider botId="bot-123">
          <TestComponent />
        </BotEventsProvider>,
      );

      const button = screen.getByRole("button", { name: "Refresh" });
      button.click();

      expect(mockRefresh).toHaveBeenCalled();
    });
  });
});
