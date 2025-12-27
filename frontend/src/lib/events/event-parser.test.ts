import {
  parseLogLine,
  parseEvents,
  isMetricEvent,
  isLogEvent,
  BotEvent,
  RawLogEntry,
} from "./event-parser";

describe("event-parser", () => {
  describe("parseLogLine", () => {
    describe("regular log lines", () => {
      it("parses a plain log line", () => {
        const result = parseLogLine("Hello world");

        expect(result.type).toBe("log");
        expect(result.data).toBe("Hello world");
        expect(result.timestamp).toBeNull();
        expect(result.level).toBe("INFO");
        expect(result.raw).toBe("Hello world");
      });

      it("parses a log line with timestamp", () => {
        const result = parseLogLine("[2025-12-27 14:30:45] Hello world");

        expect(result.type).toBe("log");
        expect(result.data).toBe("Hello world");
        expect(result.timestamp).toEqual(new Date("2025-12-27T14:30:45"));
        expect(result.level).toBe("INFO");
      });

      it("extracts DEBUG level from log line", () => {
        const result = parseLogLine("DEBUG: Some debug message");

        expect(result.type).toBe("log");
        expect(result.data).toBe("DEBUG: Some debug message");
        expect(result.level).toBe("DEBUG");
      });

      it("extracts INFO level from log line", () => {
        const result = parseLogLine("INFO: Some info message");

        expect(result.type).toBe("log");
        expect(result.level).toBe("INFO");
      });

      it("extracts WARN level from log line", () => {
        const result = parseLogLine("WARN: Some warning");

        expect(result.type).toBe("log");
        expect(result.level).toBe("WARN");
      });

      it("extracts WARNING level and normalizes to WARN", () => {
        const result = parseLogLine("WARNING: Some warning");

        expect(result.type).toBe("log");
        expect(result.level).toBe("WARN");
      });

      it("extracts ERROR level from log line", () => {
        const result = parseLogLine("ERROR: Something went wrong");

        expect(result.type).toBe("log");
        expect(result.level).toBe("ERROR");
      });

      it("handles level extraction case-insensitively", () => {
        const result = parseLogLine("error: lowercase error");

        expect(result.level).toBe("ERROR");
      });

      it("combines timestamp and level extraction", () => {
        const result = parseLogLine(
          "[2025-12-27 10:00:00] ERROR: Critical failure",
        );

        expect(result.timestamp).toEqual(new Date("2025-12-27T10:00:00"));
        expect(result.level).toBe("ERROR");
        expect(result.data).toBe("ERROR: Critical failure");
      });
    });

    describe("metric payloads", () => {
      it("parses a metric payload with _metric field", () => {
        const content =
          '{"_metric": "trade", "symbol": "BTC/USD", "price": 50000}';
        const result = parseLogLine(content);

        expect(result.type).toBe("metric");
        expect(result.metricType).toBe("trade");
        expect(result.data).toEqual({
          _metric: "trade",
          symbol: "BTC/USD",
          price: 50000,
        });
      });

      it("parses a metric with timestamp prefix", () => {
        const content =
          '[2025-12-27 15:00:00] {"_metric": "portfolio", "value": 10000}';
        const result = parseLogLine(content);

        expect(result.type).toBe("metric");
        expect(result.metricType).toBe("portfolio");
        expect(result.timestamp).toEqual(new Date("2025-12-27T15:00:00"));
      });

      it("parses a metric with level prefix", () => {
        const content = 'INFO: {"_metric": "signal", "action": "buy"}';
        const result = parseLogLine(content);

        expect(result.type).toBe("metric");
        expect(result.metricType).toBe("signal");
        expect(result.level).toBe("INFO");
      });

      it("returns log type for invalid JSON with _metric-like text", () => {
        const content = "This is not a metric: {_metric broken}";
        const result = parseLogLine(content);

        expect(result.type).toBe("log");
      });
    });

    describe("structured JSON logs (pino/winston format)", () => {
      it("parses pino-style log with msg field", () => {
        const content =
          '{"level": "info", "msg": "Bot started successfully", "time": 1735312800000}';
        const result = parseLogLine(content);

        expect(result.type).toBe("log");
        expect(result.data).toBe("Bot started successfully");
        expect(result.level).toBe("INFO");
        expect(result.timestamp).toEqual(new Date(1735312800000));
      });

      it("parses winston-style log with message field", () => {
        const content =
          '{"level": "error", "message": "Connection failed", "timestamp": "2025-12-27T10:30:00.000Z"}';
        const result = parseLogLine(content);

        expect(result.type).toBe("log");
        expect(result.data).toBe("Connection failed");
        expect(result.level).toBe("ERROR");
        expect(result.timestamp).toEqual(new Date("2025-12-27T10:30:00.000Z"));
      });

      it("parses log with numeric pino levels", () => {
        const testCases = [
          { level: "10", expected: "DEBUG" }, // trace
          { level: "20", expected: "DEBUG" }, // debug
          { level: "30", expected: "INFO" }, // info
          { level: "40", expected: "WARN" }, // warn
          { level: "50", expected: "ERROR" }, // error
          { level: "60", expected: "ERROR" }, // fatal
        ];

        for (const { level, expected } of testCases) {
          const content = `{"level": "${level}", "msg": "Test message"}`;
          const result = parseLogLine(content);
          expect(result.level).toBe(expected);
        }
      });

      it("extracts timestamp from time field as Unix ms", () => {
        const timestamp = 1735312800000; // 2025-12-27T14:00:00.000Z
        const content = `{"level": "info", "msg": "Test", "time": ${timestamp}}`;
        const result = parseLogLine(content);

        expect(result.timestamp).toEqual(new Date(timestamp));
      });

      it("extracts timestamp from time field as ISO string", () => {
        const content =
          '{"level": "info", "msg": "Test", "time": "2025-12-27T14:00:00.000Z"}';
        const result = parseLogLine(content);

        expect(result.timestamp).toEqual(new Date("2025-12-27T14:00:00.000Z"));
      });

      it("extracts timestamp from ts field (Unix seconds)", () => {
        const unixSeconds = 1735312800; // 2025-12-27T14:00:00.000Z
        const content = `{"level": "info", "msg": "Test", "ts": ${unixSeconds}}`;
        const result = parseLogLine(content);

        expect(result.timestamp).toEqual(new Date(unixSeconds * 1000));
      });

      it("extracts timestamp from ts field (Unix ms)", () => {
        const unixMs = 1735312800000;
        const content = `{"level": "info", "msg": "Test", "ts": ${unixMs}}`;
        const result = parseLogLine(content);

        expect(result.timestamp).toEqual(new Date(unixMs));
      });

      it("prefers structured log timestamp over prefix timestamp", () => {
        const content =
          '[2020-01-01 00:00:00] {"level": "info", "msg": "Test", "time": 1735312800000}';
        const result = parseLogLine(content);

        // Should use the structured log timestamp, not the prefix
        expect(result.timestamp).toEqual(new Date(1735312800000));
      });

      it("falls back to prefix timestamp when structured log has no timestamp", () => {
        const content =
          '[2025-12-27 14:00:00] {"level": "info", "msg": "Test"}';
        const result = parseLogLine(content);

        expect(result.timestamp).toEqual(new Date("2025-12-27T14:00:00"));
      });

      it("returns null timestamp when no timestamp found anywhere", () => {
        const content = '{"level": "info", "msg": "Test message"}';
        const result = parseLogLine(content);

        expect(result.timestamp).toBeNull();
      });

      it("maps TRACE level to DEBUG", () => {
        const content = '{"level": "trace", "msg": "Trace message"}';
        const result = parseLogLine(content);

        expect(result.level).toBe("DEBUG");
      });

      it("maps FATAL level to ERROR", () => {
        const content = '{"level": "fatal", "msg": "Fatal error"}';
        const result = parseLogLine(content);

        expect(result.level).toBe("ERROR");
      });

      it("does not parse JSON with _metric as structured log", () => {
        const content = '{"_metric": "trade", "msg": "This is a metric"}';
        const result = parseLogLine(content);

        // Should be parsed as metric, not structured log
        expect(result.type).toBe("metric");
        expect(result.metricType).toBe("trade");
      });

      it("stringifies structured log when no message field found", () => {
        const content = '{"level": "info", "status": "ok", "count": 5}';
        const result = parseLogLine(content);

        expect(result.type).toBe("log");
        // Falls back to JSON.stringify of the object
        expect(result.data).toBe('{"level":"info","status":"ok","count":5}');
      });

      it("handles log level prefix before structured JSON", () => {
        const content = 'INFO: {"level": "warn", "msg": "Overridden level"}';
        const result = parseLogLine(content);

        // Structured log level should take precedence
        expect(result.level).toBe("WARN");
        expect(result.data).toBe("Overridden level");
      });
    });

    describe("edge cases", () => {
      it("handles empty string", () => {
        const result = parseLogLine("");

        expect(result.type).toBe("log");
        expect(result.data).toBe("");
        expect(result.timestamp).toBeNull();
      });

      it("handles whitespace-only string", () => {
        const result = parseLogLine("   ");

        expect(result.type).toBe("log");
        expect(result.data).toBe("   ");
      });

      it("handles malformed JSON gracefully", () => {
        const result = parseLogLine("{this is not valid json}");

        expect(result.type).toBe("log");
        expect(result.data).toBe("{this is not valid json}");
      });

      it("handles JSON that is not an object", () => {
        const result = parseLogLine('"just a string"');

        expect(result.type).toBe("log");
      });

      it("handles JSON array", () => {
        const result = parseLogLine("[1, 2, 3]");

        expect(result.type).toBe("log");
      });

      it("preserves raw content", () => {
        const content = "[2025-12-27 10:00:00] INFO: Hello world";
        const result = parseLogLine(content);

        expect(result.raw).toBe(content);
      });
    });
  });

  describe("parseEvents", () => {
    it("parses multiple log entries", () => {
      const logs: RawLogEntry[] = [
        { date: "20251227", content: "First line\nSecond line" },
        { date: "20251227", content: "Third line" },
      ];

      const events = parseEvents(logs);

      expect(events).toHaveLength(3);
      expect(events[0].data).toBe("First line");
      expect(events[1].data).toBe("Second line");
      expect(events[2].data).toBe("Third line");
    });

    it("filters out empty lines", () => {
      const logs: RawLogEntry[] = [
        { date: "20251227", content: "Line 1\n\n\nLine 2\n  \n" },
      ];

      const events = parseEvents(logs);

      expect(events).toHaveLength(2);
    });

    it("handles empty logs array", () => {
      const events = parseEvents([]);

      expect(events).toHaveLength(0);
    });

    it("handles entry with only whitespace content", () => {
      const logs: RawLogEntry[] = [
        { date: "20251227", content: "   \n  \n  " },
      ];

      const events = parseEvents(logs);

      expect(events).toHaveLength(0);
    });

    it("parses mixed content (logs and metrics)", () => {
      const logs: RawLogEntry[] = [
        {
          date: "20251227",
          content:
            'Regular log\n{"_metric": "trade", "price": 100}\nAnother log',
        },
      ];

      const events = parseEvents(logs);

      expect(events).toHaveLength(3);
      expect(events[0].type).toBe("log");
      expect(events[1].type).toBe("metric");
      expect(events[2].type).toBe("log");
    });
  });

  describe("type guards", () => {
    describe("isMetricEvent", () => {
      it("returns true for metric events", () => {
        const event: BotEvent = {
          type: "metric",
          metricType: "trade",
          data: { _metric: "trade", price: 100 },
          timestamp: new Date(),
          raw: '{"_metric": "trade", "price": 100}',
        };

        expect(isMetricEvent(event)).toBe(true);
      });

      it("returns false for log events", () => {
        const event: BotEvent = {
          type: "log",
          data: "Hello world",
          timestamp: new Date(),
          level: "INFO",
          raw: "Hello world",
        };

        expect(isMetricEvent(event)).toBe(false);
      });
    });

    describe("isLogEvent", () => {
      it("returns true for log events", () => {
        const event: BotEvent = {
          type: "log",
          data: "Hello world",
          timestamp: new Date(),
          level: "INFO",
          raw: "Hello world",
        };

        expect(isLogEvent(event)).toBe(true);
      });

      it("returns false for metric events", () => {
        const event: BotEvent = {
          type: "metric",
          metricType: "trade",
          data: { _metric: "trade", price: 100 },
          timestamp: new Date(),
          raw: '{"_metric": "trade", "price": 100}',
        };

        expect(isLogEvent(event)).toBe(false);
      });
    });
  });
});
