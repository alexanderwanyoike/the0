import { LogsService, LogsQuery, LogEntry } from "../logs.service";
import { ConfigService } from "@nestjs/config";
import { BotService } from "@/bot/bot.service";
import { PinoLogger } from "nestjs-pino";
import { Ok, Failure } from "@/common/result";
import { Readable } from "stream";

/**
 * Helper: create a Readable stream from a string, optionally split into
 * multiple chunks at the given positions to exercise leftover logic.
 */
function stringStream(content: string, chunkSize?: number): Readable {
  const stream = new Readable({ read() {} });
  if (chunkSize) {
    for (let i = 0; i < content.length; i += chunkSize) {
      stream.push(Buffer.from(content.slice(i, i + chunkSize)));
    }
  } else {
    stream.push(Buffer.from(content));
  }
  stream.push(null);
  return stream;
}

describe("LogsService", () => {
  let service: LogsService;
  let mockMinioClient: any;
  let mockBotService: Partial<BotService>;
  let mockConfigService: Partial<ConfigService>;
  let mockLogger: Partial<PinoLogger>;
  let mockRequest: any;

  const userId = "user-abc";

  beforeEach(() => {
    mockRequest = { user: { uid: userId } };

    mockMinioClient = {
      statObject: jest.fn().mockResolvedValue({}),
      getObject: jest.fn(),
      getPartialObject: jest.fn(),
    };

    mockBotService = {
      findOneByUserId: jest.fn().mockResolvedValue(Ok({ id: "bot-1" })),
    };

    mockConfigService = {
      get: jest.fn().mockReturnValue("bot-logs"),
    };

    mockLogger = {
      info: jest.fn(),
      warn: jest.fn(),
      error: jest.fn(),
      debug: jest.fn(),
    };

    service = new LogsService(
      mockRequest,
      mockMinioClient,
      mockConfigService as ConfigService,
      mockBotService as BotService,
      mockLogger as PinoLogger,
    );
  });

  describe("getLogs - line-level streaming", () => {
    it("should return each line as a separate LogEntry", async () => {
      const lines = [
        '{"level":"info","msg":"started"}',
        '{"level":"info","msg":"running"}',
        '{"level":"info","msg":"done"}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(3);
      expect(result.data!.entries[0]).toEqual({
        date: "20260401",
        content: '{"level":"info","msg":"started"}',
        timestamp: null,
      });
      expect(result.data!.entries[1]).toEqual({
        date: "20260401",
        content: '{"level":"info","msg":"running"}',
        timestamp: null,
      });
      expect(result.data!.entries[2]).toEqual({
        date: "20260401",
        content: '{"level":"info","msg":"done"}',
        timestamp: null,
      });
    });

    it("should return only metric lines when type=metrics", async () => {
      const lines = [
        '{"level":"info","msg":"started"}',
        '{"_metric":true,"name":"pnl","value":42}',
        '{"level":"info","msg":"running"}',
        '{"_metric":true,"name":"sharpe","value":1.5}',
        '{"level":"info","msg":"done"}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
        type: "metrics",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(2);
      expect(result.data!.entries[0].content).toContain('"_metric"');
      expect(result.data!.entries[1].content).toContain('"_metric"');
    });

    it("should return all lines when type=all", async () => {
      const lines = [
        '{"level":"info","msg":"started"}',
        '{"_metric":true,"name":"pnl","value":42}',
        '{"level":"info","msg":"done"}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
        type: "all",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(3);
    });

    it("should respect limit at the line level (tail returns last N)", async () => {
      const lines = Array.from(
        { length: 50 },
        (_, i) => `{"level":"info","msg":"line-${i}"}`,
      ).join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 5,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(5);
      // Single-date tail reads return the LAST N lines
      expect(result.data!.entries[0].content).toContain("line-45");
      expect(result.data!.entries[4].content).toContain("line-49");
    });

    it("should respect offset at the line level (date range uses stream)", async () => {
      // Offset is meaningful for stream-from-start (multi-date range) queries
      const lines = Array.from(
        { length: 10 },
        (_, i) => `{"level":"info","msg":"line-${i}"}`,
      ).join("\n");

      // Use a two-day range so dates.length > 1 triggers the stream path
      mockMinioClient.getObject
        .mockResolvedValueOnce(stringStream(lines))
        .mockResolvedValueOnce(stringStream("")); // second day empty

      const result = await service.getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 3,
        offset: 5,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(3);
      expect(result.data!.entries[0].content).toContain("line-5");
      expect(result.data!.entries[1].content).toContain("line-6");
      expect(result.data!.entries[2].content).toContain("line-7");
    });

    it("should return all metrics ignoring limit when type=metrics", async () => {
      const lines = [
        '{"level":"info","msg":"noise"}',
        '{"_metric":true,"name":"m1","value":1}',
        '{"level":"info","msg":"noise"}',
        '{"_metric":true,"name":"m2","value":2}',
        '{"level":"info","msg":"noise"}',
        '{"_metric":true,"name":"m3","value":3}',
        '{"_metric":true,"name":"m4","value":4}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 2,
        offset: 0,
        type: "metrics",
      });

      expect(result.success).toBe(true);
      // All 4 metrics returned despite limit=2
      expect(result.data!.entries).toHaveLength(4);
    });

    it("should combine offset and type=metrics filtering", async () => {
      const lines = [
        '{"_metric":true,"name":"m1","value":1}',
        '{"level":"info","msg":"noise"}',
        '{"_metric":true,"name":"m2","value":2}',
        '{"_metric":true,"name":"m3","value":3}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 10,
        offset: 1,
        type: "metrics",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(2);
      expect(result.data!.entries[0].content).toContain("m2");
      expect(result.data!.entries[1].content).toContain("m3");
    });

    it("should stop reading early once limit is reached (stream path)", async () => {
      // Use date range to trigger stream-from-start path
      const lines = Array.from(
        { length: 1000 },
        (_, i) => `{"level":"info","msg":"line-${i}"}`,
      ).join("\n");

      const stream = stringStream(lines);
      const destroySpy = jest.spyOn(stream, "destroy");
      mockMinioClient.getObject.mockResolvedValue(stream);

      const result = await service.getLogs("bot-1", {
        dateRange: "20260401-20260401",
        limit: 2,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(2);
      expect(destroySpy).toHaveBeenCalled();
    });

    it("should handle multi-chunk streams correctly", async () => {
      const lines = [
        '{"level":"info","msg":"first"}',
        '{"level":"info","msg":"second"}',
        '{"level":"info","msg":"third"}',
      ].join("\n");

      // Split into 20-byte chunks to force leftover handling
      mockMinioClient.getObject.mockResolvedValue(stringStream(lines, 20));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(3);
      expect(result.data!.entries[0].content).toContain("first");
      expect(result.data!.entries[1].content).toContain("second");
      expect(result.data!.entries[2].content).toContain("third");
    });

    it("should handle empty lines gracefully", async () => {
      const content = '{"msg":"a"}\n\n\n{"msg":"b"}\n';

      mockMinioClient.getObject.mockResolvedValue(stringStream(content));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(2);
    });

    it("should return empty array when log file does not exist", async () => {
      // Single-date tail path calls statObject first
      mockMinioClient.statObject.mockRejectedValue({ code: "NoSuchKey" });

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toEqual([]);
    });

    it("should return empty array when log file does not exist (date range)", async () => {
      // Multi-date range triggers stream path, which handles NoSuchKey on getObject
      mockMinioClient.getObject.mockRejectedValue({ code: "NoSuchKey" });

      const result = await service.getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toEqual([]);
    });

    it("should span multiple date files with line-level pagination", async () => {
      // Two days, 3 lines each
      const day1 = '{"msg":"d1-l1"}\n{"msg":"d1-l2"}\n{"msg":"d1-l3"}';
      const day2 = '{"msg":"d2-l1"}\n{"msg":"d2-l2"}\n{"msg":"d2-l3"}';

      mockMinioClient.getObject
        .mockResolvedValueOnce(stringStream(day1))
        .mockResolvedValueOnce(stringStream(day2));

      const result = await service.getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 4,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(4);
      expect(result.data!.entries[0].content).toContain("d1-l1");
      expect(result.data!.entries[2].content).toContain("d1-l3");
      expect(result.data!.entries[3].content).toContain("d2-l1");
    });

    it("should apply offset across multiple date files", async () => {
      const day1 = '{"msg":"d1-l1"}\n{"msg":"d1-l2"}';
      const day2 = '{"msg":"d2-l1"}\n{"msg":"d2-l2"}';

      mockMinioClient.getObject
        .mockResolvedValueOnce(stringStream(day1))
        .mockResolvedValueOnce(stringStream(day2));

      const result = await service.getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 2,
        offset: 1,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(2);
      expect(result.data!.entries[0].content).toContain("d1-l2");
      expect(result.data!.entries[1].content).toContain("d2-l1");
    });
  });

  describe("getLogs - JSON-based metrics filtering", () => {
    it("should filter metrics using JSON parsing not string matching", async () => {
      // This line contains the text _metric in a message but is NOT a metric line
      const lines = [
        '{"timestamp":"2026-03-30T13:00:26Z","message":"check _metric docs"}',
        '{"_metric":true,"name":"pnl","value":42}',
        '{"timestamp":"2026-03-30T13:00:27Z","message":"see _metric reference"}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
        type: "metrics",
      });

      expect(result.success).toBe(true);
      // Only the actual metric line should be returned, not the ones that
      // merely contain the string "_metric" in their message text
      expect(result.data!.entries).toHaveLength(1);
      expect(result.data!.entries[0].content).toContain('"name":"pnl"');
    });

    it("should skip non-JSON lines when type=metrics", async () => {
      const lines = [
        "[2026-03-30 13:00:26] INFO:main:Starting bot",
        '{"_metric":true,"name":"pnl","value":42}',
        "plain text log line with no structure",
        "[2026-03-30 13:00:27] ERROR:main:Something failed",
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
        type: "metrics",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(1);
      expect(result.data!.entries[0].content).toContain('"name":"pnl"');
    });

    it("should return old format lines when type=all", async () => {
      const lines = [
        "[2026-03-30 13:00:26] INFO:main:Starting bot",
        '{"level":"info","msg":"json line"}',
        "plain text log line",
        '{"_metric":true,"name":"pnl","value":42}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
        type: "all",
      });

      expect(result.success).toBe(true);
      // All lines should be returned regardless of format
      expect(result.data!.entries).toHaveLength(4);
      expect(result.data!.entries[0].content).toBe(
        "[2026-03-30 13:00:26] INFO:main:Starting bot",
      );
      expect(result.data!.entries[0].timestamp).toBeNull();
      expect(result.data!.entries[1].content).toBe(
        '{"level":"info","msg":"json line"}',
      );
      expect(result.data!.entries[1].timestamp).toBeNull();
      expect(result.data!.entries[2].content).toBe("plain text log line");
      expect(result.data!.entries[2].timestamp).toBeNull();
      expect(result.data!.entries[3].content).toContain('"_metric"');
      expect(result.data!.entries[3].timestamp).toBeNull();
    });

    it("should handle leftover line with JSON-based metrics filtering", async () => {
      // No trailing newline, so the metric line becomes leftover
      const content =
        '{"level":"info","msg":"check _metric docs"}\n{"_metric":true,"name":"sharpe","value":1.5}';

      mockMinioClient.getObject.mockResolvedValue(stringStream(content));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
        type: "metrics",
      });

      expect(result.success).toBe(true);
      // The first line mentions _metric in text but isn't a metric;
      // the leftover (second line) IS a real metric
      expect(result.data!.entries).toHaveLength(1);
      expect(result.data!.entries[0].content).toContain('"name":"sharpe"');
    });

    it("should skip non-JSON leftover line when type=metrics", async () => {
      // The leftover is a plain text line
      const content =
        '{"_metric":true,"name":"pnl","value":42}\n[2026-03-30 13:00:26] INFO:main:done';

      mockMinioClient.getObject.mockResolvedValue(stringStream(content));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
        type: "metrics",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(1);
      expect(result.data!.entries[0].content).toContain('"name":"pnl"');
    });
  });

  describe("getLogs - line normalization", () => {
    it("should normalize NDJSON wrapped text with extracted message and timestamp", async () => {
      const lines = [
        '{"timestamp":"2026-03-30T13:00:26Z","message":"INFO:main:Starting bot"}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260330",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(1);
      expect(result.data!.entries[0]).toEqual({
        date: "20260330",
        content: "INFO:main:Starting bot",
        timestamp: "2026-03-30T13:00:26Z",
      });
    });

    it("should normalize metric lines keeping full JSON as content with timestamp", async () => {
      const metricLine =
        '{"_metric":"price","value":42,"timestamp":"2026-04-01T10:00:00Z"}';
      const lines = [metricLine].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(1);
      expect(result.data!.entries[0]).toEqual({
        date: "20260401",
        content: metricLine,
        timestamp: "2026-04-01T10:00:00Z",
      });
    });

    it("should handle old format plain text with null timestamp", async () => {
      const lines = [
        "[2026-03-30 13:00:26] INFO:main:Starting bot",
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260330",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(1);
      expect(result.data!.entries[0]).toEqual({
        date: "20260330",
        content: "[2026-03-30 13:00:26] INFO:main:Starting bot",
        timestamp: null,
      });
    });

    it("should filter metrics using JSON parsing not string matching", async () => {
      const lines = [
        '{"timestamp":"2026-03-30T13:00:26Z","message":"check _metric docs"}',
        '{"_metric":"price","value":42,"timestamp":"2026-04-01T10:00:00Z"}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
        type: "metrics",
      });

      expect(result.success).toBe(true);
      // Only the real metric line should be returned
      expect(result.data!.entries).toHaveLength(1);
      expect(result.data!.entries[0].content).toContain('"_metric"');
      expect(result.data!.entries[0].timestamp).toBe("2026-04-01T10:00:00Z");
    });

    it("should handle mixed old and new format lines", async () => {
      const metricLine =
        '{"_metric":"sharpe","value":1.5,"timestamp":"2026-04-01T10:05:00Z"}';
      const lines = [
        "[2026-03-30 13:00:26] INFO:main:Starting bot",
        '{"timestamp":"2026-03-30T13:00:27Z","message":"Running strategy"}',
        metricLine,
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260330",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(3);

      // Old format: content as-is, timestamp null
      expect(result.data!.entries[0]).toEqual({
        date: "20260330",
        content: "[2026-03-30 13:00:26] INFO:main:Starting bot",
        timestamp: null,
      });

      // NDJSON wrapped text: extracted message, extracted timestamp
      expect(result.data!.entries[1]).toEqual({
        date: "20260330",
        content: "Running strategy",
        timestamp: "2026-03-30T13:00:27Z",
      });

      // Metric line: full JSON as content, extracted timestamp
      expect(result.data!.entries[2]).toEqual({
        date: "20260330",
        content: metricLine,
        timestamp: "2026-04-01T10:05:00Z",
      });
    });
  });

  describe("getLogs - tail-based reads", () => {
    it("should return latest entries from end of file (tail)", async () => {
      // 100-line file; request limit=10 on a single date
      const lines = Array.from(
        { length: 100 },
        (_, i) =>
          `{"timestamp":"2026-04-01T${String(i).padStart(2, "0")}:00:00Z","message":"line-${i}"}`,
      );
      const fileContent = lines.join("\n") + "\n";
      const fileSize = Buffer.byteLength(fileContent, "utf-8");

      mockMinioClient.statObject.mockResolvedValue({ size: fileSize });
      // Tail window covers the whole file in this case, so getObject is used
      // But for a proper tail test, simulate a file larger than TAIL_BYTES
      // by using getPartialObject with the tail portion
      const tailContent = lines.slice(-15).join("\n") + "\n";
      const tailSize = Buffer.byteLength(tailContent, "utf-8");
      const bigFileSize = 512 * 1024 + 1000; // bigger than TAIL_BYTES

      mockMinioClient.statObject.mockResolvedValue({ size: bigFileSize });
      // getPartialObject returns the last TAIL_BYTES, which we simulate
      // as containing the last 15 lines (with a partial first line)
      const partialFirstLine = "artial-junk-from-previous-line";
      const tailWithPartial = partialFirstLine + "\n" + tailContent;
      mockMinioClient.getPartialObject.mockResolvedValue(
        stringStream(tailWithPartial),
      );

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 10,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(10);
      // The returned entries should be the LAST 10 lines, not the first 10
      expect(result.data!.entries[0].content).toBe("line-90");
      expect(result.data!.entries[9].content).toBe("line-99");
    });

    it("should return chronological entries for date range", async () => {
      // Two dates with a few lines each - should stream from start
      const day1 = [
        '{"timestamp":"2026-04-01T01:00:00Z","message":"d1-line1"}',
        '{"timestamp":"2026-04-01T02:00:00Z","message":"d1-line2"}',
      ].join("\n");
      const day2 = [
        '{"timestamp":"2026-04-02T01:00:00Z","message":"d2-line1"}',
        '{"timestamp":"2026-04-02T02:00:00Z","message":"d2-line2"}',
      ].join("\n");

      mockMinioClient.getObject
        .mockResolvedValueOnce(stringStream(day1))
        .mockResolvedValueOnce(stringStream(day2));

      const result = await service.getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 10,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(4);
      // Chronological order: day1 first, then day2
      expect(result.data!.entries[0].content).toBe("d1-line1");
      expect(result.data!.entries[1].content).toBe("d1-line2");
      expect(result.data!.entries[2].content).toBe("d2-line1");
      expect(result.data!.entries[3].content).toBe("d2-line2");
    });

    it("should stream from start for metrics filter on single date", async () => {
      const lines = [
        '{"level":"info","message":"noise"}',
        '{"_metric":true,"name":"pnl","value":42}',
        '{"level":"info","message":"more noise"}',
        '{"_metric":true,"name":"sharpe","value":1.5}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 10,
        offset: 0,
        type: "metrics",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(2);
      // Metrics from anywhere in the file, streamed from start
      expect(result.data!.entries[0].content).toContain('"name":"pnl"');
      expect(result.data!.entries[1].content).toContain('"name":"sharpe"');
      // getObject should have been called (not getPartialObject)
      expect(mockMinioClient.getObject).toHaveBeenCalled();
    });

    it("should return all metrics without limit when type=metrics", async () => {
      // 200 lines: 150 noise + 50 metrics scattered throughout
      const lines = Array.from({ length: 200 }, (_, i) =>
        i % 4 === 0
          ? `{"_metric":true,"name":"metric-${i}","value":${i}}`
          : `{"level":"info","message":"noise-${i}"}`,
      ).join("\n") + "\n";

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 10,
        offset: 0,
        type: "metrics",
      });

      expect(result.success).toBe(true);
      // Should return all 50 metrics, not just 10
      expect(result.data!.entries.length).toBe(50);
    });

    it("should still respect limit for non-metrics queries", async () => {
      const lines = Array.from(
        { length: 50 },
        (_, i) => `{"timestamp":"2026-04-01T10:00:00Z","message":"line-${i}"}`,
      ).join("\n") + "\n";

      // Tail path: single date, non-metrics
      mockMinioClient.statObject.mockResolvedValue({ size: 100 });
      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 10,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries.length).toBe(10);
    });

    it("should handle file smaller than tail window", async () => {
      // Small file (well under 512KB)
      const lines = [
        '{"timestamp":"2026-04-01T01:00:00Z","message":"line-1"}',
        '{"timestamp":"2026-04-01T02:00:00Z","message":"line-2"}',
        '{"timestamp":"2026-04-01T03:00:00Z","message":"line-3"}',
      ];
      const fileContent = lines.join("\n") + "\n";
      const fileSize = Buffer.byteLength(fileContent, "utf-8");

      mockMinioClient.statObject.mockResolvedValue({ size: fileSize });
      // File is smaller than TAIL_BYTES, so getObject is used
      mockMinioClient.getObject.mockResolvedValue(stringStream(fileContent));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      // All lines should be returned, nothing skipped
      expect(result.data!.entries).toHaveLength(3);
      expect(result.data!.entries[0].content).toBe("line-1");
      expect(result.data!.entries[1].content).toBe("line-2");
      expect(result.data!.entries[2].content).toBe("line-3");
    });

    it("should drop partial first line when tailing from mid-file", async () => {
      const bigFileSize = 512 * 1024 + 5000;
      mockMinioClient.statObject.mockResolvedValue({ size: bigFileSize });

      // Simulate getPartialObject returning content that starts mid-line
      const tailContent =
        'oken-json-from-previous-line"}\n' +
        '{"timestamp":"2026-04-01T10:00:00Z","message":"complete-line-1"}\n' +
        '{"timestamp":"2026-04-01T11:00:00Z","message":"complete-line-2"}\n';

      mockMinioClient.getPartialObject.mockResolvedValue(
        stringStream(tailContent),
      );

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      // The partial first line should be dropped
      expect(result.data!.entries).toHaveLength(2);
      expect(result.data!.entries[0].content).toBe("complete-line-1");
      expect(result.data!.entries[1].content).toBe("complete-line-2");
      // getPartialObject should have been called (not getObject)
      expect(mockMinioClient.getPartialObject).toHaveBeenCalled();
    });
  });

  describe("getLogs - datetime range filtering", () => {
    it("should filter lines by timestamp when datetime range is provided", async () => {
      const lines = [
        '{"timestamp":"2026-04-03T09:30:00Z","message":"before range"}',
        '{"timestamp":"2026-04-03T10:15:00Z","message":"in range 1"}',
        '{"timestamp":"2026-04-03T10:45:00Z","message":"in range 2"}',
        '{"timestamp":"2026-04-03T11:30:00Z","message":"after range"}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        dateRange: "2026-04-03T10:00:00Z--2026-04-03T11:00:00Z",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(2);
      expect(result.data!.entries[0].content).toBe("in range 1");
      expect(result.data!.entries[1].content).toBe("in range 2");
    });

    it("should still work with YYYYMMDD date range", async () => {
      const day1 = '{"timestamp":"2026-04-01T12:00:00Z","message":"day1 line"}';
      const day2 = '{"timestamp":"2026-04-02T12:00:00Z","message":"day2 line"}';
      const day3 = '{"timestamp":"2026-04-03T12:00:00Z","message":"day3 line"}';

      mockMinioClient.getObject
        .mockResolvedValueOnce(stringStream(day1))
        .mockResolvedValueOnce(stringStream(day2))
        .mockResolvedValueOnce(stringStream(day3));

      const result = await service.getLogs("bot-1", {
        dateRange: "20260401-20260403",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(3);
      expect(result.data!.entries[0].content).toBe("day1 line");
      expect(result.data!.entries[1].content).toBe("day2 line");
      expect(result.data!.entries[2].content).toBe("day3 line");
    });

    it("should handle datetime range spanning midnight", async () => {
      const day1Lines = [
        '{"timestamp":"2026-04-02T22:00:00Z","message":"before range"}',
        '{"timestamp":"2026-04-02T23:30:00Z","message":"late night"}',
      ].join("\n");
      const day2Lines = [
        '{"timestamp":"2026-04-03T00:30:00Z","message":"early morning"}',
        '{"timestamp":"2026-04-03T02:00:00Z","message":"after range"}',
      ].join("\n");

      mockMinioClient.getObject
        .mockResolvedValueOnce(stringStream(day1Lines))
        .mockResolvedValueOnce(stringStream(day2Lines));

      const result = await service.getLogs("bot-1", {
        dateRange: "2026-04-02T23:00:00Z--2026-04-03T01:00:00Z",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(2);
      expect(result.data!.entries[0].content).toBe("late night");
      expect(result.data!.entries[1].content).toBe("early morning");
    });

    it("should include lines without timestamps when filtering by datetime", async () => {
      const lines = [
        '{"timestamp":"2026-04-03T10:15:00Z","message":"in range"}',
        "[2026-04-03 09:00:00] INFO:main:old format no json timestamp",
        "plain text line with no timestamp at all",
        '{"timestamp":"2026-04-03T12:00:00Z","message":"after range"}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        dateRange: "2026-04-03T10:00:00Z--2026-04-03T11:00:00Z",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      // "in range" is in window, old format and plain text have no timestamp so included,
      // "after range" is outside the window so excluded
      expect(result.data!.entries).toHaveLength(3);
      expect(result.data!.entries[0].content).toBe("in range");
      expect(result.data!.entries[1].content).toBe(
        "[2026-04-03 09:00:00] INFO:main:old format no json timestamp",
      );
      expect(result.data!.entries[2].content).toBe(
        "plain text line with no timestamp at all",
      );
    });

    it("should use tail path for single-date datetime range and filter by time", async () => {
      // When the datetime range spans a single day, it should use the tail path
      // but still apply time filtering
      const lines = [
        '{"timestamp":"2026-04-03T08:00:00Z","message":"morning"}',
        '{"timestamp":"2026-04-03T10:30:00Z","message":"mid-morning"}',
        '{"timestamp":"2026-04-03T14:00:00Z","message":"afternoon"}',
      ];
      const fileContent = lines.join("\n") + "\n";
      const fileSize = Buffer.byteLength(fileContent, "utf-8");

      mockMinioClient.statObject.mockResolvedValue({ size: fileSize });
      mockMinioClient.getObject.mockResolvedValue(stringStream(fileContent));

      const result = await service.getLogs("bot-1", {
        dateRange: "2026-04-03T10:00:00Z--2026-04-03T11:00:00Z",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(1);
      expect(result.data!.entries[0].content).toBe("mid-morning");
    });
  });

  describe("getLogs - auth and validation", () => {
    it("should return failure when user is not authenticated", async () => {
      const unauthService = new LogsService(
        { user: null } as any,
        mockMinioClient,
        mockConfigService as ConfigService,
        mockBotService as BotService,
        mockLogger as PinoLogger,
      );

      const result = await unauthService.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(false);
      expect(result.error).toContain("Authentication required");
    });

    it("should return failure when bot is not found", async () => {
      (mockBotService.findOneByUserId as jest.Mock).mockResolvedValue(
        Failure("not found"),
      );

      const result = await service.getLogs("bot-999", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(false);
      expect(result.error).toContain("not found");
    });

    it("should return failure for invalid date format", async () => {
      const result = await service.getLogs("bot-1", {
        date: "not-a-date",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(false);
      expect(result.error).toContain("Invalid date");
    });
  });
});
