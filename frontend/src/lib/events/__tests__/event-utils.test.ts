import {
  since,
  between,
  filterByType,
  logs,
  metrics,
  latest,
  getMetricTypes,
  groupByMetricType,
  groupByTimeWindow,
  groupByRun,
  latestByType,
  countByType,
  extractTimeSeries,
} from "../event-utils";
import { BotEvent } from "../event-parser";

describe("event-utils", () => {
  // Helper to create test events
  const createEvent = (
    overrides: Partial<BotEvent> & { timestamp: Date },
  ): BotEvent => ({
    type: "log",
    data: "test",
    raw: "test",
    ...overrides,
  });

  const createMetricEvent = (
    metricType: string,
    data: Record<string, unknown>,
    timestamp: Date,
  ): BotEvent => ({
    timestamp,
    type: "metric",
    metricType,
    data: { _metric: metricType, ...data },
    raw: JSON.stringify({ _metric: metricType, ...data }),
  });

  describe("since", () => {
    it("should filter events from the last hour", () => {
      const now = Date.now();
      const events = [
        createEvent({ timestamp: new Date(now - 30 * 60 * 1000) }), // 30 min ago
        createEvent({ timestamp: new Date(now - 2 * 60 * 60 * 1000) }), // 2 hours ago
        createEvent({ timestamp: new Date(now - 10 * 60 * 1000) }), // 10 min ago
      ];

      const result = since(events, "1h");

      expect(result).toHaveLength(2);
    });

    it("should filter events from the last day", () => {
      const now = Date.now();
      const events = [
        createEvent({ timestamp: new Date(now - 12 * 60 * 60 * 1000) }), // 12 hours ago
        createEvent({ timestamp: new Date(now - 2 * 24 * 60 * 60 * 1000) }), // 2 days ago
      ];

      const result = since(events, "1d");

      expect(result).toHaveLength(1);
    });

    it("should support seconds duration", () => {
      const now = Date.now();
      const events = [
        createEvent({ timestamp: new Date(now - 10 * 1000) }), // 10 sec ago
        createEvent({ timestamp: new Date(now - 60 * 1000) }), // 60 sec ago
      ];

      const result = since(events, "30s");

      expect(result).toHaveLength(1);
    });

    it("should support minutes duration", () => {
      const now = Date.now();
      const events = [
        createEvent({ timestamp: new Date(now - 3 * 60 * 1000) }), // 3 min ago
        createEvent({ timestamp: new Date(now - 10 * 60 * 1000) }), // 10 min ago
      ];

      const result = since(events, "5m");

      expect(result).toHaveLength(1);
    });

    it("should support weeks duration", () => {
      const now = Date.now();
      const events = [
        createEvent({ timestamp: new Date(now - 3 * 24 * 60 * 60 * 1000) }), // 3 days ago
        createEvent({ timestamp: new Date(now - 10 * 24 * 60 * 60 * 1000) }), // 10 days ago
      ];

      const result = since(events, "1w");

      expect(result).toHaveLength(1);
    });

    it("should throw error for invalid duration format", () => {
      const events = [createEvent({ timestamp: new Date() })];

      expect(() => since(events, "invalid")).toThrow("Invalid duration format");
    });
  });

  describe("between", () => {
    it("should filter events between two dates", () => {
      const start = new Date("2024-01-20T10:00:00");
      const end = new Date("2024-01-20T12:00:00");
      const events = [
        createEvent({ timestamp: new Date("2024-01-20T09:00:00") }),
        createEvent({ timestamp: new Date("2024-01-20T11:00:00") }),
        createEvent({ timestamp: new Date("2024-01-20T13:00:00") }),
      ];

      const result = between(events, start, end);

      expect(result).toHaveLength(1);
      expect(result[0].timestamp).toEqual(new Date("2024-01-20T11:00:00"));
    });

    it("should include events at boundary timestamps", () => {
      const start = new Date("2024-01-20T10:00:00");
      const end = new Date("2024-01-20T12:00:00");
      const events = [
        createEvent({ timestamp: new Date("2024-01-20T10:00:00") }),
        createEvent({ timestamp: new Date("2024-01-20T12:00:00") }),
      ];

      const result = between(events, start, end);

      expect(result).toHaveLength(2);
    });
  });

  describe("filterByType", () => {
    it("should filter events by metric type", () => {
      const events = [
        createMetricEvent("trade", { symbol: "BTC" }, new Date()),
        createMetricEvent("portfolio_value", { value: 1000 }, new Date()),
        createMetricEvent("trade", { symbol: "ETH" }, new Date()),
        createEvent({ timestamp: new Date() }), // log event
      ];

      const result = filterByType(events, "trade");

      expect(result).toHaveLength(2);
      expect(result[0].metricType).toBe("trade");
      expect(result[1].metricType).toBe("trade");
    });

    it("should return empty array if no matching type", () => {
      const events = [
        createMetricEvent("trade", { symbol: "BTC" }, new Date()),
      ];

      const result = filterByType(events, "nonexistent");

      expect(result).toHaveLength(0);
    });
  });

  describe("logs", () => {
    it("should return only log events", () => {
      const events = [
        createEvent({ timestamp: new Date(), type: "log", data: "log 1" }),
        createMetricEvent("trade", {}, new Date()),
        createEvent({ timestamp: new Date(), type: "log", data: "log 2" }),
      ];

      const result = logs(events);

      expect(result).toHaveLength(2);
      expect(result.every((e) => e.type === "log")).toBe(true);
    });
  });

  describe("metrics", () => {
    it("should return only metric events", () => {
      const events = [
        createEvent({ timestamp: new Date() }),
        createMetricEvent("trade", {}, new Date()),
        createMetricEvent("signal", {}, new Date()),
      ];

      const result = metrics(events);

      expect(result).toHaveLength(2);
      expect(result.every((e) => e.type === "metric")).toBe(true);
    });
  });

  describe("latest", () => {
    it("should return the latest event of a specific type", () => {
      const events = [
        createMetricEvent(
          "portfolio_value",
          { value: 1000 },
          new Date("2024-01-20T10:00:00"),
        ),
        createMetricEvent(
          "portfolio_value",
          { value: 1100 },
          new Date("2024-01-20T11:00:00"),
        ),
        createMetricEvent(
          "portfolio_value",
          { value: 1200 },
          new Date("2024-01-20T12:00:00"),
        ),
      ];

      const result = latest(events, "portfolio_value");

      expect(result).not.toBeNull();
      expect((result!.data as Record<string, unknown>).value).toBe(1200);
    });

    it("should return null if no events of that type exist", () => {
      const events = [createMetricEvent("trade", {}, new Date())];

      const result = latest(events, "portfolio_value");

      expect(result).toBeNull();
    });

    it("should return null for empty events", () => {
      const result = latest([], "any_type");
      expect(result).toBeNull();
    });
  });

  describe("getMetricTypes", () => {
    it("should return all unique metric types", () => {
      const events = [
        createMetricEvent("trade", {}, new Date()),
        createMetricEvent("portfolio_value", {}, new Date()),
        createMetricEvent("trade", {}, new Date()),
        createMetricEvent("signal", {}, new Date()),
        createEvent({ timestamp: new Date() }), // log event
      ];

      const result = getMetricTypes(events);

      expect(result).toHaveLength(3);
      expect(result).toContain("trade");
      expect(result).toContain("portfolio_value");
      expect(result).toContain("signal");
    });

    it("should return empty array for no metrics", () => {
      const events = [createEvent({ timestamp: new Date() })];

      const result = getMetricTypes(events);

      expect(result).toHaveLength(0);
    });
  });

  describe("groupByMetricType", () => {
    it("should group events by metric type", () => {
      const events = [
        createMetricEvent("trade", { id: 1 }, new Date()),
        createMetricEvent("portfolio_value", { value: 1000 }, new Date()),
        createMetricEvent("trade", { id: 2 }, new Date()),
      ];

      const result = groupByMetricType(events);

      expect(Object.keys(result)).toHaveLength(2);
      expect(result["trade"]).toHaveLength(2);
      expect(result["portfolio_value"]).toHaveLength(1);
    });

    it("should ignore log events", () => {
      const events = [
        createMetricEvent("trade", {}, new Date()),
        createEvent({ timestamp: new Date() }),
      ];

      const result = groupByMetricType(events);

      expect(Object.keys(result)).toHaveLength(1);
      expect(result["trade"]).toHaveLength(1);
    });
  });

  describe("groupByTimeWindow", () => {
    it("should group events by 1 hour window", () => {
      const events = [
        createEvent({ timestamp: new Date("2024-01-20T10:15:00") }),
        createEvent({ timestamp: new Date("2024-01-20T10:30:00") }),
        createEvent({ timestamp: new Date("2024-01-20T11:15:00") }),
        createEvent({ timestamp: new Date("2024-01-20T11:45:00") }),
      ];

      const result = groupByTimeWindow(events, "1h");

      expect(result).toHaveLength(2);
      expect(result[0]).toHaveLength(2); // 10:00-11:00
      expect(result[1]).toHaveLength(2); // 11:00-12:00
    });

    it("should group events by 5 minute window", () => {
      const events = [
        createEvent({ timestamp: new Date("2024-01-20T10:01:00") }),
        createEvent({ timestamp: new Date("2024-01-20T10:03:00") }),
        createEvent({ timestamp: new Date("2024-01-20T10:06:00") }),
      ];

      const result = groupByTimeWindow(events, "5m");

      expect(result).toHaveLength(2);
      expect(result[0]).toHaveLength(2); // 10:00-10:05
      expect(result[1]).toHaveLength(1); // 10:05-10:10
    });

    it("should return empty array for empty events", () => {
      const result = groupByTimeWindow([], "1h");
      expect(result).toHaveLength(0);
    });

    it("should sort windows chronologically", () => {
      const events = [
        createEvent({ timestamp: new Date("2024-01-20T12:00:00") }),
        createEvent({ timestamp: new Date("2024-01-20T10:00:00") }),
        createEvent({ timestamp: new Date("2024-01-20T11:00:00") }),
      ];

      const result = groupByTimeWindow(events, "1h");

      expect(result).toHaveLength(3);
      expect(result[0][0].timestamp).toEqual(new Date("2024-01-20T10:00:00"));
      expect(result[1][0].timestamp).toEqual(new Date("2024-01-20T11:00:00"));
      expect(result[2][0].timestamp).toEqual(new Date("2024-01-20T12:00:00"));
    });
  });

  describe("groupByRun", () => {
    it("should group events into runs based on time gaps", () => {
      const events = [
        createEvent({ timestamp: new Date("2024-01-20T10:00:00") }),
        createEvent({ timestamp: new Date("2024-01-20T10:01:00") }),
        // 10 minute gap (default threshold is 5 minutes)
        createEvent({ timestamp: new Date("2024-01-20T10:11:00") }),
        createEvent({ timestamp: new Date("2024-01-20T10:12:00") }),
      ];

      const result = groupByRun(events);

      expect(result).toHaveLength(2);
      expect(result[0]).toHaveLength(2);
      expect(result[1]).toHaveLength(2);
    });

    it("should use custom gap threshold", () => {
      const events = [
        createEvent({ timestamp: new Date("2024-01-20T10:00:00") }),
        createEvent({ timestamp: new Date("2024-01-20T10:02:00") }), // 2 min gap
        createEvent({ timestamp: new Date("2024-01-20T10:05:00") }), // 3 min gap
      ];

      // With 1 minute threshold, each event is its own run
      const result = groupByRun(events, 60 * 1000);

      expect(result).toHaveLength(3);
    });

    it("should keep all events in one run if no large gaps", () => {
      const events = [
        createEvent({ timestamp: new Date("2024-01-20T10:00:00") }),
        createEvent({ timestamp: new Date("2024-01-20T10:01:00") }),
        createEvent({ timestamp: new Date("2024-01-20T10:02:00") }),
      ];

      const result = groupByRun(events);

      expect(result).toHaveLength(1);
      expect(result[0]).toHaveLength(3);
    });

    it("should return empty array for empty events", () => {
      const result = groupByRun([]);
      expect(result).toHaveLength(0);
    });

    it("should sort events by timestamp within runs", () => {
      const events = [
        createEvent({ timestamp: new Date("2024-01-20T10:02:00") }),
        createEvent({ timestamp: new Date("2024-01-20T10:00:00") }),
        createEvent({ timestamp: new Date("2024-01-20T10:01:00") }),
      ];

      const result = groupByRun(events);

      expect(result).toHaveLength(1);
      expect(result[0][0].timestamp).toEqual(new Date("2024-01-20T10:00:00"));
      expect(result[0][1].timestamp).toEqual(new Date("2024-01-20T10:01:00"));
      expect(result[0][2].timestamp).toEqual(new Date("2024-01-20T10:02:00"));
    });
  });

  describe("latestByType", () => {
    it("should return latest event for each metric type", () => {
      const events = [
        createMetricEvent(
          "trade",
          { id: 1 },
          new Date("2024-01-20T10:00:00"),
        ),
        createMetricEvent(
          "trade",
          { id: 2 },
          new Date("2024-01-20T11:00:00"),
        ),
        createMetricEvent(
          "portfolio_value",
          { value: 1000 },
          new Date("2024-01-20T10:30:00"),
        ),
      ];

      const result = latestByType(events);

      expect(Object.keys(result)).toHaveLength(2);
      expect((result["trade"].data as Record<string, unknown>).id).toBe(2);
      expect(
        (result["portfolio_value"].data as Record<string, unknown>).value,
      ).toBe(1000);
    });
  });

  describe("countByType", () => {
    it("should count events by metric type", () => {
      const events = [
        createMetricEvent("trade", {}, new Date()),
        createMetricEvent("trade", {}, new Date()),
        createMetricEvent("trade", {}, new Date()),
        createMetricEvent("portfolio_value", {}, new Date()),
        createEvent({ timestamp: new Date() }), // log, not counted
      ];

      const result = countByType(events);

      expect(result).toEqual({
        trade: 3,
        portfolio_value: 1,
      });
    });

    it("should return empty object for no metrics", () => {
      const events = [createEvent({ timestamp: new Date() })];

      const result = countByType(events);

      expect(result).toEqual({});
    });
  });

  describe("extractTimeSeries", () => {
    it("should extract time series data from metric events", () => {
      const events = [
        createMetricEvent(
          "portfolio_value",
          { value: 1000 },
          new Date("2024-01-20T10:00:00"),
        ),
        createMetricEvent(
          "portfolio_value",
          { value: 1100 },
          new Date("2024-01-20T11:00:00"),
        ),
        createMetricEvent(
          "portfolio_value",
          { value: 1200 },
          new Date("2024-01-20T12:00:00"),
        ),
      ];

      const result = extractTimeSeries(events, "portfolio_value");

      expect(result).toHaveLength(3);
      expect(result[0]).toEqual({
        timestamp: new Date("2024-01-20T10:00:00"),
        value: 1000,
      });
      expect(result[2].value).toBe(1200);
    });

    it("should use custom value key", () => {
      const events = [
        createMetricEvent(
          "custom",
          { amount: 500 },
          new Date("2024-01-20T10:00:00"),
        ),
      ];

      const result = extractTimeSeries(events, "custom", "amount");

      expect(result).toHaveLength(1);
      expect(result[0].value).toBe(500);
    });

    it("should filter out events without numeric value", () => {
      const events = [
        createMetricEvent(
          "portfolio_value",
          { value: 1000 },
          new Date("2024-01-20T10:00:00"),
        ),
        createMetricEvent(
          "portfolio_value",
          { value: "not a number" },
          new Date("2024-01-20T11:00:00"),
        ),
        createMetricEvent(
          "portfolio_value",
          { value: 1200 },
          new Date("2024-01-20T12:00:00"),
        ),
      ];

      const result = extractTimeSeries(events, "portfolio_value");

      expect(result).toHaveLength(2);
    });

    it("should return empty array for non-matching metric type", () => {
      const events = [createMetricEvent("trade", { value: 100 }, new Date())];

      const result = extractTimeSeries(events, "portfolio_value");

      expect(result).toHaveLength(0);
    });
  });
});
