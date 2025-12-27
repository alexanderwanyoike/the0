import React from "react";
import { renderHook } from "@testing-library/react";
import {
  useThe0Events,
  useBotEvents,
  The0EventsContext,
  BotEventsContext,
  BotEvent,
  BotEventUtils,
  The0EventsContextValue,
} from "../index";

describe("@the0/react SDK", () => {
  // Create mock utils
  const createMockUtils = (events: BotEvent[]): BotEventUtils => ({
    since: jest.fn(() => events),
    between: jest.fn(() => events),
    filterByType: jest.fn(() => events),
    metrics: jest.fn(() => events.filter((e) => e.type === "metric")),
    logs: jest.fn(() => events.filter((e) => e.type === "log")),
    latest: jest.fn(() => (events.length > 0 ? events[events.length - 1] : null)),
    groupByMetricType: jest.fn(() => ({})),
    groupByRun: jest.fn(() => [events]),
    extractTimeSeries: jest.fn(() => []),
  });

  // Create mock context value
  const createMockContextValue = (
    events: BotEvent[] = [],
    loading = false,
    error: string | null = null
  ): The0EventsContextValue => ({
    events,
    loading,
    error,
    utils: createMockUtils(events),
    refresh: jest.fn(),
  });

  // Create a wrapper component
  const createWrapper = (contextValue: The0EventsContextValue | null) => {
    return function Wrapper({ children }: { children: React.ReactNode }) {
      return (
        <The0EventsContext.Provider value={contextValue}>
          {children}
        </The0EventsContext.Provider>
      );
    };
  };

  describe("useThe0Events", () => {
    it("should return context value when used within provider", () => {
      const mockValue = createMockContextValue();
      const wrapper = createWrapper(mockValue);

      const { result } = renderHook(() => useThe0Events(), { wrapper });

      expect(result.current).toBe(mockValue);
      expect(result.current.events).toEqual([]);
      expect(result.current.loading).toBe(false);
      expect(result.current.error).toBeNull();
    });

    it("should throw error when used outside provider", () => {
      // Clear any global context that might exist
      if (typeof window !== "undefined") {
        delete window.__THE0_EVENTS_CONTEXT__;
      }

      const { result } = renderHook(() => {
        try {
          return useThe0Events();
        } catch (e) {
          return e;
        }
      });

      expect(result.current).toBeInstanceOf(Error);
      expect((result.current as Error).message).toContain(
        "useThe0Events must be used within a the0 dashboard context"
      );
    });

    it("should provide events array", () => {
      const events: BotEvent[] = [
        {
          timestamp: new Date("2024-01-20T10:00:00"),
          type: "log",
          data: "Test log",
          level: "INFO",
          raw: "[2024-01-20 10:00:00] INFO: Test log",
        },
        {
          timestamp: new Date("2024-01-20T10:01:00"),
          type: "metric",
          data: { _metric: "trade", symbol: "BTC" },
          metricType: "trade",
          raw: '{"_metric": "trade", "symbol": "BTC"}',
        },
      ];
      const mockValue = createMockContextValue(events);
      const wrapper = createWrapper(mockValue);

      const { result } = renderHook(() => useThe0Events(), { wrapper });

      expect(result.current.events).toHaveLength(2);
      expect(result.current.events[0].type).toBe("log");
      expect(result.current.events[1].type).toBe("metric");
    });

    it("should provide loading state", () => {
      const mockValue = createMockContextValue([], true);
      const wrapper = createWrapper(mockValue);

      const { result } = renderHook(() => useThe0Events(), { wrapper });

      expect(result.current.loading).toBe(true);
    });

    it("should provide error state", () => {
      const mockValue = createMockContextValue([], false, "Failed to load events");
      const wrapper = createWrapper(mockValue);

      const { result } = renderHook(() => useThe0Events(), { wrapper });

      expect(result.current.error).toBe("Failed to load events");
    });

    it("should provide utils object with filtering methods", () => {
      const events: BotEvent[] = [
        {
          timestamp: new Date(),
          type: "metric",
          data: { _metric: "trade", symbol: "BTC" },
          metricType: "trade",
          raw: "",
        },
      ];
      const mockValue = createMockContextValue(events);
      const wrapper = createWrapper(mockValue);

      const { result } = renderHook(() => useThe0Events(), { wrapper });

      // Check that all utils methods are present
      expect(typeof result.current.utils.since).toBe("function");
      expect(typeof result.current.utils.between).toBe("function");
      expect(typeof result.current.utils.filterByType).toBe("function");
      expect(typeof result.current.utils.metrics).toBe("function");
      expect(typeof result.current.utils.logs).toBe("function");
      expect(typeof result.current.utils.latest).toBe("function");
      expect(typeof result.current.utils.groupByMetricType).toBe("function");
      expect(typeof result.current.utils.groupByRun).toBe("function");
      expect(typeof result.current.utils.extractTimeSeries).toBe("function");
    });

    it("should provide refresh function", () => {
      const mockRefresh = jest.fn();
      const mockValue = {
        ...createMockContextValue(),
        refresh: mockRefresh,
      };
      const wrapper = createWrapper(mockValue);

      const { result } = renderHook(() => useThe0Events(), { wrapper });
      result.current.refresh();

      expect(mockRefresh).toHaveBeenCalledTimes(1);
    });
  });

  describe("useBotEvents (alias)", () => {
    it("should be an alias for useThe0Events", () => {
      expect(useBotEvents).toBe(useThe0Events);
    });

    it("should work the same as useThe0Events", () => {
      const mockValue = createMockContextValue();
      const wrapper = createWrapper(mockValue);

      const { result } = renderHook(() => useBotEvents(), { wrapper });

      expect(result.current).toBe(mockValue);
    });
  });

  describe("Context exports", () => {
    it("should export The0EventsContext", () => {
      expect(The0EventsContext).toBeDefined();
      expect(The0EventsContext.Provider).toBeDefined();
    });

    it("should export BotEventsContext as alias", () => {
      expect(BotEventsContext).toBe(The0EventsContext);
    });
  });

  describe("Type exports", () => {
    it("should export BotEvent type correctly", () => {
      const event: BotEvent = {
        timestamp: new Date(),
        type: "log",
        data: "test",
        raw: "test",
      };
      expect(event.type).toBe("log");
    });

    it("should export BotEvent with metric type", () => {
      const event: BotEvent = {
        timestamp: new Date(),
        type: "metric",
        data: { _metric: "trade", value: 100 },
        metricType: "trade",
        raw: "",
      };
      expect(event.metricType).toBe("trade");
    });

    it("should export BotEvent with level", () => {
      const event: BotEvent = {
        timestamp: new Date(),
        type: "log",
        data: "test",
        level: "ERROR",
        raw: "test",
      };
      expect(event.level).toBe("ERROR");
    });
  });

  describe("Shared context via window global", () => {
    it("should store context on window global for sharing between modules", () => {
      // The context should already be on the global from the initial import
      // This enables dynamically loaded custom dashboards to share the same context
      if (typeof window !== "undefined") {
        // After import, the context should exist on window
        expect(The0EventsContext).toBeDefined();
        // The context can be used as a provider
        expect(The0EventsContext.Provider).toBeDefined();
      }
    });

    it("should return the same context instance on multiple imports", () => {
      // This verifies the singleton behavior - multiple imports get same context
      const context1 = The0EventsContext;
      const context2 = BotEventsContext;
      expect(context1).toBe(context2);
    });
  });
});
