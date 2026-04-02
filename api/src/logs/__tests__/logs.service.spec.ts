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
      expect(result.data).toHaveLength(3);
      expect(result.data![0]).toEqual({
        date: "20260401",
        content: '{"level":"info","msg":"started"}',
        timestamp: null,
      });
      expect(result.data![1]).toEqual({
        date: "20260401",
        content: '{"level":"info","msg":"running"}',
        timestamp: null,
      });
      expect(result.data![2]).toEqual({
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
      expect(result.data).toHaveLength(2);
      expect(result.data![0].content).toContain('"_metric"');
      expect(result.data![1].content).toContain('"_metric"');
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
      expect(result.data).toHaveLength(3);
    });

    it("should respect limit at the line level", async () => {
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
      expect(result.data).toHaveLength(5);
      expect(result.data![0].content).toContain("line-0");
      expect(result.data![4].content).toContain("line-4");
    });

    it("should respect offset at the line level", async () => {
      const lines = Array.from(
        { length: 10 },
        (_, i) => `{"level":"info","msg":"line-${i}"}`,
      ).join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 3,
        offset: 5,
      });

      expect(result.success).toBe(true);
      expect(result.data).toHaveLength(3);
      expect(result.data![0].content).toContain("line-5");
      expect(result.data![1].content).toContain("line-6");
      expect(result.data![2].content).toContain("line-7");
    });

    it("should combine limit and type=metrics filtering", async () => {
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
      expect(result.data).toHaveLength(2);
      expect(result.data![0].content).toContain("m1");
      expect(result.data![1].content).toContain("m2");
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
      expect(result.data).toHaveLength(2);
      expect(result.data![0].content).toContain("m2");
      expect(result.data![1].content).toContain("m3");
    });

    it("should stop reading early once limit is reached", async () => {
      // Use a large file but only need 2 lines
      const lines = Array.from(
        { length: 1000 },
        (_, i) => `{"level":"info","msg":"line-${i}"}`,
      ).join("\n");

      const stream = stringStream(lines);
      const destroySpy = jest.spyOn(stream, "destroy");
      mockMinioClient.getObject.mockResolvedValue(stream);

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 2,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data).toHaveLength(2);
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
      expect(result.data).toHaveLength(3);
      expect(result.data![0].content).toContain("first");
      expect(result.data![1].content).toContain("second");
      expect(result.data![2].content).toContain("third");
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
      expect(result.data).toHaveLength(2);
    });

    it("should return empty array when log file does not exist", async () => {
      mockMinioClient.getObject.mockRejectedValue({ code: "NoSuchKey" });

      const result = await service.getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data).toEqual([]);
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
      expect(result.data).toHaveLength(4);
      expect(result.data![0].content).toContain("d1-l1");
      expect(result.data![2].content).toContain("d1-l3");
      expect(result.data![3].content).toContain("d2-l1");
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
      expect(result.data).toHaveLength(2);
      expect(result.data![0].content).toContain("d1-l2");
      expect(result.data![1].content).toContain("d2-l1");
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
      expect(result.data).toHaveLength(1);
      expect(result.data![0].content).toContain('"name":"pnl"');
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
      expect(result.data).toHaveLength(1);
      expect(result.data![0].content).toContain('"name":"pnl"');
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
      expect(result.data).toHaveLength(4);
      expect(result.data![0].content).toBe(
        "[2026-03-30 13:00:26] INFO:main:Starting bot",
      );
      expect(result.data![0].timestamp).toBeNull();
      expect(result.data![1].content).toBe(
        '{"level":"info","msg":"json line"}',
      );
      expect(result.data![1].timestamp).toBeNull();
      expect(result.data![2].content).toBe("plain text log line");
      expect(result.data![2].timestamp).toBeNull();
      expect(result.data![3].content).toContain('"_metric"');
      expect(result.data![3].timestamp).toBeNull();
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
      expect(result.data).toHaveLength(1);
      expect(result.data![0].content).toContain('"name":"sharpe"');
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
      expect(result.data).toHaveLength(1);
      expect(result.data![0].content).toContain('"name":"pnl"');
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
      expect(result.data).toHaveLength(1);
      expect(result.data![0]).toEqual({
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
      expect(result.data).toHaveLength(1);
      expect(result.data![0]).toEqual({
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
      expect(result.data).toHaveLength(1);
      expect(result.data![0]).toEqual({
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
      expect(result.data).toHaveLength(1);
      expect(result.data![0].content).toContain('"_metric"');
      expect(result.data![0].timestamp).toBe("2026-04-01T10:00:00Z");
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
      expect(result.data).toHaveLength(3);

      // Old format: content as-is, timestamp null
      expect(result.data![0]).toEqual({
        date: "20260330",
        content: "[2026-03-30 13:00:26] INFO:main:Starting bot",
        timestamp: null,
      });

      // NDJSON wrapped text: extracted message, extracted timestamp
      expect(result.data![1]).toEqual({
        date: "20260330",
        content: "Running strategy",
        timestamp: "2026-03-30T13:00:27Z",
      });

      // Metric line: full JSON as content, extracted timestamp
      expect(result.data![2]).toEqual({
        date: "20260330",
        content: metricLine,
        timestamp: "2026-04-01T10:05:00Z",
      });
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
