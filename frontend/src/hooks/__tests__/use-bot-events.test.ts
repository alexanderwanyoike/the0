import { renderHook, waitFor, act } from "@testing-library/react";
import { useBotEvents } from "../use-bot-events";
import { useBotLogs } from "../use-bot-logs";

// Mock the useBotLogs hook
jest.mock("../use-bot-logs", () => ({
  useBotLogs: jest.fn(),
}));

// Mock event parser
jest.mock("@/lib/events/event-parser", () => ({
  parseEvents: jest.fn((logs) => {
    const events: any[] = [];
    for (const log of logs) {
      const lines = log.content.split("\n").filter((l: string) => l.trim());
      for (const line of lines) {
        if (line.includes("_metric")) {
          try {
            const jsonMatch = line.match(/\{.*\}/);
            if (jsonMatch) {
              const parsed = JSON.parse(jsonMatch[0]);
              events.push({
                timestamp: new Date(),
                type: "metric",
                data: parsed,
                metricType: parsed._metric,
                raw: line,
              });
              continue;
            }
          } catch {
            // Fall through
          }
        }
        events.push({
          timestamp: new Date(),
          type: "log",
          data: line,
          level: "INFO",
          raw: line,
        });
      }
    }
    return events;
  }),
  isMetricEvent: jest.fn((event) => event?.type === "metric"),
  isLogEvent: jest.fn((event) => event?.type === "log"),
}));

// Mock event utils
jest.mock("@/lib/events/event-utils", () => ({
  since: jest.fn((events) => events),
  between: jest.fn((events) => events),
  filterByType: jest.fn((events, type) =>
    events.filter((e: any) => e.metricType === type),
  ),
  logs: jest.fn((events) => events.filter((e: any) => e.type === "log")),
  metrics: jest.fn((events) => events.filter((e: any) => e.type === "metric")),
  latest: jest.fn(
    (events, type) =>
      events.filter((e: any) => e.metricType === type).pop() || null,
  ),
  getMetricTypes: jest.fn((events) => [
    ...new Set(
      events.filter((e: any) => e.metricType).map((e: any) => e.metricType),
    ),
  ]),
  groupByMetricType: jest.fn((events) => {
    const grouped: Record<string, any[]> = {};
    events.forEach((e: any) => {
      if (e.metricType) {
        if (!grouped[e.metricType]) grouped[e.metricType] = [];
        grouped[e.metricType].push(e);
      }
    });
    return grouped;
  }),
  groupByTimeWindow: jest.fn((events) => [events]),
  groupByRun: jest.fn((events) => [events]),
  latestByType: jest.fn((events) => {
    const latest: Record<string, any> = {};
    events.forEach((e: any) => {
      if (e.metricType) latest[e.metricType] = e;
    });
    return latest;
  }),
  countByType: jest.fn((events) => {
    const counts: Record<string, number> = {};
    events.forEach((e: any) => {
      if (e.metricType) {
        counts[e.metricType] = (counts[e.metricType] || 0) + 1;
      }
    });
    return counts;
  }),
  extractTimeSeries: jest.fn((events, type, valueKey = "value") =>
    events
      .filter((e: any) => e.metricType === type)
      .map((e: any) => ({
        timestamp: e.timestamp,
        value: e.data?.[valueKey] || 0,
      })),
  ),
}));

const mockUseBotLogs = useBotLogs as jest.MockedFunction<typeof useBotLogs>;

describe("useBotEvents", () => {
  const mockLogs = [
    { date: "20240101", content: "[2024-01-01 10:00:00] INFO: Starting bot" },
    {
      date: "20240101",
      content:
        '[2024-01-01 10:01:00] {"_metric": "portfolio_value", "value": 10000}',
    },
  ];

  const mockRefresh = jest.fn();
  const mockSetDateFilter = jest.fn();
  const mockSetDateRangeFilter = jest.fn();
  const mockExportLogs = jest.fn();

  beforeEach(() => {
    jest.clearAllMocks();
    mockUseBotLogs.mockReturnValue({
      logs: mockLogs,
      loading: false,
      error: null,
      hasMore: false,
      total: 2,
      refresh: mockRefresh,
      loadMore: jest.fn(),
      setDateFilter: mockSetDateFilter,
      setDateRangeFilter: mockSetDateRangeFilter,
      exportLogs: mockExportLogs,
    });
  });

  describe("initialization", () => {
    it("calls useBotLogs with correct parameters", () => {
      renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(mockUseBotLogs).toHaveBeenCalledWith({
        botId: "bot-123",
        autoRefresh: false,
        refreshInterval: 30000,
        initialQuery: { limit: 100, offset: 0 },
      });
    });

    it("passes autoRefresh option to useBotLogs", () => {
      renderHook(() =>
        useBotEvents({
          botId: "bot-123",
          autoRefresh: true,
          refreshInterval: 5000,
        }),
      );

      expect(mockUseBotLogs).toHaveBeenCalledWith(
        expect.objectContaining({
          autoRefresh: true,
          refreshInterval: 5000,
        }),
      );
    });

    it("builds dateRange query when provided", () => {
      renderHook(() =>
        useBotEvents({
          botId: "bot-123",
          dateRange: { start: "20240101", end: "20240131" },
        }),
      );

      expect(mockUseBotLogs).toHaveBeenCalledWith(
        expect.objectContaining({
          initialQuery: expect.objectContaining({
            dateRange: "20240101-20240131",
          }),
        }),
      );
    });
  });

  describe("event parsing", () => {
    it("returns parsed events", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.events).toBeDefined();
      expect(Array.isArray(result.current.events)).toBe(true);
    });

    it("parses log events correctly", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      const logEvents = result.current.events.filter((e) => e.type === "log");
      expect(logEvents.length).toBeGreaterThan(0);
    });

    it("parses metric events correctly", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      const metricEvents = result.current.events.filter(
        (e) => e.type === "metric",
      );
      expect(metricEvents.length).toBeGreaterThan(0);
    });
  });

  describe("loading state", () => {
    it("returns loading state from useBotLogs", () => {
      mockUseBotLogs.mockReturnValue({
        logs: [],
        loading: true,
        error: null,
        hasMore: false,
        total: 0,
        refresh: mockRefresh,
        loadMore: jest.fn(),
        setDateFilter: mockSetDateFilter,
        setDateRangeFilter: mockSetDateRangeFilter,
        exportLogs: mockExportLogs,
      });

      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.loading).toBe(true);
    });

    it("returns false when loading is complete", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.loading).toBe(false);
    });
  });

  describe("error state", () => {
    it("returns error from useBotLogs", () => {
      mockUseBotLogs.mockReturnValue({
        logs: [],
        loading: false,
        error: "Failed to fetch logs",
        hasMore: false,
        total: 0,
        refresh: mockRefresh,
        loadMore: jest.fn(),
        setDateFilter: mockSetDateFilter,
        setDateRangeFilter: mockSetDateRangeFilter,
        exportLogs: mockExportLogs,
      });

      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.error).toBe("Failed to fetch logs");
    });

    it("returns null when no error", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.error).toBeNull();
    });
  });

  describe("utilities", () => {
    it("provides since utility function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.utils.since).toBeDefined();
      expect(typeof result.current.utils.since).toBe("function");

      const filtered = result.current.utils.since("1h");
      expect(Array.isArray(filtered)).toBe(true);
    });

    it("provides between utility function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.utils.between).toBeDefined();
      const filtered = result.current.utils.between(new Date(), new Date());
      expect(Array.isArray(filtered)).toBe(true);
    });

    it("provides filterByType utility function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.utils.filterByType).toBeDefined();
      const filtered = result.current.utils.filterByType("portfolio_value");
      expect(Array.isArray(filtered)).toBe(true);
    });

    it("provides logs utility function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.utils.logs).toBeDefined();
      const logEvents = result.current.utils.logs();
      expect(Array.isArray(logEvents)).toBe(true);
    });

    it("provides metrics utility function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.utils.metrics).toBeDefined();
      const metricEvents = result.current.utils.metrics();
      expect(Array.isArray(metricEvents)).toBe(true);
    });

    it("provides latest utility function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.utils.latest).toBeDefined();
      const latest = result.current.utils.latest("portfolio_value");
      // Can be null or an event
      expect(latest === null || typeof latest === "object").toBe(true);
    });

    it("provides getMetricTypes utility function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.utils.getMetricTypes).toBeDefined();
      const types = result.current.utils.getMetricTypes();
      expect(Array.isArray(types)).toBe(true);
    });

    it("provides groupByMetricType utility function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.utils.groupByMetricType).toBeDefined();
      const grouped = result.current.utils.groupByMetricType();
      expect(typeof grouped).toBe("object");
    });

    it("provides groupByTimeWindow utility function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.utils.groupByTimeWindow).toBeDefined();
      const grouped = result.current.utils.groupByTimeWindow("1h");
      expect(Array.isArray(grouped)).toBe(true);
    });

    it("provides groupByRun utility function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.utils.groupByRun).toBeDefined();
      const grouped = result.current.utils.groupByRun();
      expect(Array.isArray(grouped)).toBe(true);
    });

    it("provides latestByType utility function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.utils.latestByType).toBeDefined();
      const latest = result.current.utils.latestByType();
      expect(typeof latest).toBe("object");
    });

    it("provides countByType utility function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.utils.countByType).toBeDefined();
      const counts = result.current.utils.countByType();
      expect(typeof counts).toBe("object");
    });

    it("provides extractTimeSeries utility function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.utils.extractTimeSeries).toBeDefined();
      const series = result.current.utils.extractTimeSeries("portfolio_value");
      expect(Array.isArray(series)).toBe(true);
    });
  });

  describe("passthrough functions", () => {
    it("exposes refresh function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.refresh).toBe(mockRefresh);

      result.current.refresh();
      expect(mockRefresh).toHaveBeenCalled();
    });

    it("exposes setDateFilter function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.setDateFilter).toBe(mockSetDateFilter);

      result.current.setDateFilter("20240101");
      expect(mockSetDateFilter).toHaveBeenCalledWith("20240101");
    });

    it("exposes setDateRangeFilter function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.setDateRangeFilter).toBe(mockSetDateRangeFilter);

      result.current.setDateRangeFilter("20240101", "20240131");
      expect(mockSetDateRangeFilter).toHaveBeenCalledWith(
        "20240101",
        "20240131",
      );
    });

    it("exposes exportLogs function", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.exportLogs).toBe(mockExportLogs);

      result.current.exportLogs();
      expect(mockExportLogs).toHaveBeenCalled();
    });

    it("exposes rawLogs from useBotLogs", () => {
      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.rawLogs).toEqual(mockLogs);
    });
  });

  describe("memoization", () => {
    it("memoizes utils object", () => {
      const { result, rerender } = renderHook(() =>
        useBotEvents({ botId: "bot-123" }),
      );

      const utils1 = result.current.utils;
      rerender();
      const utils2 = result.current.utils;

      // Utils should be the same reference if events haven't changed
      expect(utils1).toBe(utils2);
    });

    it("updates utils when events change", () => {
      const { result, rerender } = renderHook(() =>
        useBotEvents({ botId: "bot-123" }),
      );

      const utils1 = result.current.utils;

      // Simulate events change
      mockUseBotLogs.mockReturnValue({
        logs: [
          { date: "20240101", content: "[2024-01-01 12:00:00] INFO: New log" },
        ],
        loading: false,
        error: null,
        hasMore: false,
        total: 1,
        refresh: mockRefresh,
        loadMore: jest.fn(),
        setDateFilter: mockSetDateFilter,
        setDateRangeFilter: mockSetDateRangeFilter,
        exportLogs: mockExportLogs,
      });

      rerender();
      const utils2 = result.current.utils;

      // Utils should be different reference after events change
      expect(utils1).not.toBe(utils2);
    });
  });

  describe("empty state", () => {
    it("handles empty logs array", () => {
      mockUseBotLogs.mockReturnValue({
        logs: [],
        loading: false,
        error: null,
        hasMore: false,
        total: 0,
        refresh: mockRefresh,
        loadMore: jest.fn(),
        setDateFilter: mockSetDateFilter,
        setDateRangeFilter: mockSetDateRangeFilter,
        exportLogs: mockExportLogs,
      });

      const { result } = renderHook(() => useBotEvents({ botId: "bot-123" }));

      expect(result.current.events).toEqual([]);
      expect(result.current.utils.logs()).toEqual([]);
      expect(result.current.utils.metrics()).toEqual([]);
    });
  });
});
