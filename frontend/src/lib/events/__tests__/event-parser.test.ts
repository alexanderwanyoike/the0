import {
  parseLogLine,
  parseEvents,
  isMetricEvent,
  isLogEvent,
  BotEvent,
  RawLogEntry,
} from "../event-parser";

describe("event-parser", () => {
  describe("parseLogLine", () => {
    it("should parse a regular log line with timestamp", () => {
      const line = "[2024-01-20 10:30:00] INFO: Starting bot...";
      const event = parseLogLine(line);

      expect(event.type).toBe("log");
      expect(event.level).toBe("INFO");
      expect(event.data).toBe("INFO: Starting bot...");
      expect(event.timestamp).toEqual(new Date("2024-01-20T10:30:00"));
      expect(event.raw).toBe(line);
    });

    it("should parse DEBUG level logs", () => {
      const line = "[2024-01-20 10:30:00] DEBUG: Debug message";
      const event = parseLogLine(line);

      expect(event.type).toBe("log");
      expect(event.level).toBe("DEBUG");
    });

    it("should parse WARN level logs", () => {
      const line = "[2024-01-20 10:30:00] WARN: Warning message";
      const event = parseLogLine(line);

      expect(event.type).toBe("log");
      expect(event.level).toBe("WARN");
    });

    it("should parse WARNING as WARN level", () => {
      const line = "[2024-01-20 10:30:00] WARNING: Warning message";
      const event = parseLogLine(line);

      expect(event.type).toBe("log");
      expect(event.level).toBe("WARN");
    });

    it("should parse ERROR level logs", () => {
      const line = "[2024-01-20 10:30:00] ERROR: Error message";
      const event = parseLogLine(line);

      expect(event.type).toBe("log");
      expect(event.level).toBe("ERROR");
    });

    it("should default to INFO level for logs without level", () => {
      const line = "[2024-01-20 10:30:00] Some message without level";
      const event = parseLogLine(line);

      expect(event.type).toBe("log");
      expect(event.level).toBe("INFO");
    });

    it("should parse a JSON metric with _metric field", () => {
      const line =
        '[2024-01-20 10:30:01] {"_metric": "portfolio_value", "value": 10000}';
      const event = parseLogLine(line);

      expect(event.type).toBe("metric");
      expect(event.metricType).toBe("portfolio_value");
      expect(event.data).toEqual({ _metric: "portfolio_value", value: 10000 });
      expect(event.timestamp).toEqual(new Date("2024-01-20T10:30:01"));
    });

    it("should parse a metric with level prefix", () => {
      const line =
        '[2024-01-20 10:30:01] INFO: {"_metric": "trade", "symbol": "BTC", "side": "buy"}';
      const event = parseLogLine(line);

      expect(event.type).toBe("metric");
      expect(event.metricType).toBe("trade");
      expect(event.data).toEqual({
        _metric: "trade",
        symbol: "BTC",
        side: "buy",
      });
      expect(event.level).toBe("INFO");
    });

    it("should handle JSON without _metric as regular log", () => {
      const line =
        '[2024-01-20 10:30:00] {"some": "json", "without": "metric"}';
      const event = parseLogLine(line);

      expect(event.type).toBe("log");
      expect(event.data).toBe('{"some": "json", "without": "metric"}');
    });

    it("should handle malformed JSON as regular log", () => {
      const line = "[2024-01-20 10:30:00] {not valid json}";
      const event = parseLogLine(line);

      expect(event.type).toBe("log");
      expect(event.data).toBe("{not valid json}");
    });

    it("should handle lines without timestamp", () => {
      const line = "INFO: Message without timestamp";
      const event = parseLogLine(line);

      expect(event.type).toBe("log");
      expect(event.level).toBe("INFO");
      // Returns null when no timestamp found (display should show blank)
      expect(event.timestamp).toBeNull();
    });

    it("should preserve the raw log line", () => {
      const line = '[2024-01-20 10:30:00] {"_metric": "signal", "type": "buy"}';
      const event = parseLogLine(line);

      expect(event.raw).toBe(line);
    });
  });

  describe("parseEvents", () => {
    it("should parse multiple log entries from raw log data", () => {
      const logs: RawLogEntry[] = [
        {
          date: "20240120",
          content:
            "[2024-01-20 10:30:00] INFO: Starting bot...\n" +
            '[2024-01-20 10:30:01] {"_metric": "portfolio_value", "value": 10000}\n' +
            "[2024-01-20 10:30:05] INFO: Fetching market data",
        },
      ];

      const events = parseEvents(logs);

      expect(events).toHaveLength(3);
      expect(events[0].type).toBe("log");
      expect(events[0].level).toBe("INFO");
      expect(events[1].type).toBe("metric");
      expect(events[1].metricType).toBe("portfolio_value");
      expect(events[2].type).toBe("log");
    });

    it("should handle multiple raw log entries", () => {
      const logs: RawLogEntry[] = [
        {
          date: "20240119",
          content: "[2024-01-19 23:00:00] Day 1 log",
        },
        {
          date: "20240120",
          content: "[2024-01-20 01:00:00] Day 2 log",
        },
      ];

      const events = parseEvents(logs);

      expect(events).toHaveLength(2);
      expect(events[0].data).toContain("Day 1");
      expect(events[1].data).toContain("Day 2");
    });

    it("should skip empty lines", () => {
      const logs: RawLogEntry[] = [
        {
          date: "20240120",
          content:
            "[2024-01-20 10:30:00] Line 1\n\n[2024-01-20 10:31:00] Line 2\n   \n",
        },
      ];

      const events = parseEvents(logs);

      expect(events).toHaveLength(2);
    });

    it("should return empty array for empty input", () => {
      const events = parseEvents([]);
      expect(events).toHaveLength(0);
    });

    it("should handle entry with empty content", () => {
      const logs: RawLogEntry[] = [
        {
          date: "20240120",
          content: "",
        },
      ];

      const events = parseEvents(logs);
      expect(events).toHaveLength(0);
    });
  });

  describe("isMetricEvent", () => {
    it("should return true for metric events", () => {
      const event: BotEvent = {
        timestamp: new Date(),
        type: "metric",
        data: { _metric: "trade", symbol: "BTC" },
        metricType: "trade",
        raw: "test",
      };

      expect(isMetricEvent(event)).toBe(true);
    });

    it("should return false for log events", () => {
      const event: BotEvent = {
        timestamp: new Date(),
        type: "log",
        data: "Some log message",
        raw: "test",
      };

      expect(isMetricEvent(event)).toBe(false);
    });
  });

  describe("isLogEvent", () => {
    it("should return true for log events", () => {
      const event: BotEvent = {
        timestamp: new Date(),
        type: "log",
        data: "Some log message",
        raw: "test",
      };

      expect(isLogEvent(event)).toBe(true);
    });

    it("should return false for metric events", () => {
      const event: BotEvent = {
        timestamp: new Date(),
        type: "metric",
        data: { _metric: "trade" },
        metricType: "trade",
        raw: "test",
      };

      expect(isLogEvent(event)).toBe(false);
    });
  });
});
