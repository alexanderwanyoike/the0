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

/** The text a stored line carries, whether wrapped as NDJSON or plain. */
function messageOf(entry: { content: string }): string {
  try {
    const parsed = JSON.parse(entry.content);
    return typeof parsed?.message === "string" ? parsed.message : entry.content;
  } catch {
    return entry.content;
  }
}

describe("LogsService", () => {
  let service: LogsService;
  let mockMinioClient: any;
  let mockBotService: Partial<BotService>;
  let mockConfigService: Partial<ConfigService>;
  let mockLogger: Partial<PinoLogger>;
  let mockRequest: any;

  const userId = "user-abc";
  const getLogs = (
    botId: string,
    query: Parameters<LogsService["getLogs"]>[1],
    uid: string = userId,
  ) => service.getLogs(botId, query, uid);

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

  /** Serves consecutive daily files from `start` by path, in any read order. */
  function serveDaysFrom(start: string, ...days: string[]) {
    const files: Record<string, string> = {};
    const day = new Date(
      Date.UTC(+start.slice(0, 4), +start.slice(4, 6) - 1, +start.slice(6, 8)),
    );
    for (const content of days) {
      const name = day.toISOString().slice(0, 10).replace(/-/g, "");
      files[`logs/bot-1/${name}.log`] = content;
      day.setUTCDate(day.getUTCDate() + 1);
    }
    const read = (path: string) => {
      if (files[path] === undefined) throw { code: "NoSuchKey" };
      return files[path];
    };
    mockMinioClient.getObject.mockImplementation(
      async (_bucket: string, path: string) => stringStream(read(path)),
    );
    mockMinioClient.statObject.mockImplementation(
      async (_bucket: string, path: string) => ({
        size: Buffer.byteLength(read(path)),
      }),
    );
  }

  describe("getLogs - line-level streaming", () => {
    it("should return each line as a separate LogEntry", async () => {
      const lines = [
        '{"level":"info","msg":"started"}',
        '{"level":"info","msg":"running"}',
        '{"level":"info","msg":"done"}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(3);
      expect(result.data!.entries[0]).toEqual({
        key: "20260401:0",
        date: "20260401",
        content: '{"level":"info","msg":"started"}',
        timestamp: null,
      });
      expect(result.data!.entries[1]).toEqual({
        key: "20260401:33",
        date: "20260401",
        content: '{"level":"info","msg":"running"}',
        timestamp: null,
      });
      expect(result.data!.entries[2]).toEqual({
        key: "20260401:66",
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

      const result = await getLogs("bot-1", {
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

      const result = await getLogs("bot-1", {
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

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 5,
        offset: 0,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(5);
      // Single-date tail reads return the LAST N lines
      expect(result.data!.entries[0].content).toContain("line-45");
      expect(result.data!.entries[4].content).toContain("line-49");
    });

    it("should apply offset counting back from the newest entry", async () => {
      const lines = Array.from(
        { length: 10 },
        (_, i) => `{"level":"info","msg":"line-${i}"}`,
      ).join("\n");

      serveDaysFrom("20260401", lines, ""); // second day empty

      const result = await getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 3,
        offset: 5,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(3);
      // The newest three after skipping five, oldest of them first
      expect(result.data!.entries[0].content).toContain("line-2");
      expect(result.data!.entries[1].content).toContain("line-3");
      expect(result.data!.entries[2].content).toContain("line-4");
    });

    it("should return the newest `limit` metrics and report hasMore when type=metrics", async () => {
      const lines = [
        '{"level":"info","msg":"noise"}',
        '{"_metric":true,"name":"m1","value":1}',
        '{"level":"info","msg":"noise"}',
        '{"_metric":true,"name":"m2","value":2}',
        '{"level":"info","msg":"noise"}',
        '{"_metric":true,"name":"m3","value":3}',
        '{"_metric":true,"name":"m4","value":4}',
      ].join("\n");

      mockMinioClient.getObject.mockImplementation(() => stringStream(lines));

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 2,
        offset: 0,
        type: "metrics",
      });

      expect(result.success).toBe(true);
      // Default sort is desc: newest 2 of the 4 metrics, with hasMore
      // signalling the older ones are reachable via offset
      expect(result.data!.entries.map(messageOf)).toEqual([
        '{"_metric":true,"name":"m4","value":4}',
        '{"_metric":true,"name":"m3","value":3}',
      ]);
      expect(result.data!.hasMore).toBe(true);
    });

    it("should combine offset and type=metrics filtering", async () => {
      const lines = [
        '{"_metric":true,"name":"m1","value":1}',
        '{"level":"info","msg":"noise"}',
        '{"_metric":true,"name":"m2","value":2}',
        '{"_metric":true,"name":"m3","value":3}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 10,
        offset: 1,
        type: "metrics",
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(2);
      expect(result.data!.entries[0].content).toContain("m1");
      expect(result.data!.entries[1].content).toContain("m2");
    });

    it("should handle multi-chunk streams correctly", async () => {
      const lines = [
        '{"level":"info","msg":"first"}',
        '{"level":"info","msg":"second"}',
        '{"level":"info","msg":"third"}',
      ].join("\n");

      // Split into 20-byte chunks to force leftover handling
      mockMinioClient.getObject.mockResolvedValue(stringStream(lines, 20));

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
        sort: "asc",
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

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(2);
    });

    it("should return empty array when log file does not exist", async () => {
      // A day's read starts by statting its file
      mockMinioClient.statObject.mockRejectedValue({ code: "NoSuchKey" });

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toEqual([]);
    });

    it("should return empty array when log file does not exist (date range)", async () => {
      mockMinioClient.getObject.mockRejectedValue({ code: "NoSuchKey" });

      const result = await getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toEqual([]);
    });

    it("should treat an R2 NotFound from statObject as a missing file", async () => {
      // Cloudflare R2 returns "NotFound" instead of "NoSuchKey"
      mockMinioClient.statObject.mockRejectedValue({ code: "NotFound" });

      const result = await getLogs("bot-1", {
        date: "20260405",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toEqual([]);
    });

    it("should treat an R2 NotFound from getObject as a missing file", async () => {
      mockMinioClient.getObject.mockRejectedValue({ code: "NotFound" });

      const result = await getLogs("bot-1", {
        dateRange: "20260405-20260405",
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

      serveDaysFrom("20260401", day1, day2);

      const result = await getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 4,
        offset: 0,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(4);
      expect(result.data!.entries[0].content).toContain("d1-l3");
      expect(result.data!.entries[1].content).toContain("d2-l1");
      expect(result.data!.entries[3].content).toContain("d2-l3");
    });

    it("should apply offset across multiple date files", async () => {
      const day1 = '{"msg":"d1-l1"}\n{"msg":"d1-l2"}';
      const day2 = '{"msg":"d2-l1"}\n{"msg":"d2-l2"}';

      serveDaysFrom("20260401", day1, day2);

      const result = await getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 2,
        offset: 1,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(2);
      expect(result.data!.entries[0].content).toContain("d1-l2");
      expect(result.data!.entries[1].content).toContain("d2-l1");
    });
    it("should skip blank lines", async () => {
      const content = '{"msg":"a"}\n\n   \n{"msg":"b"}\n';

      serveDaysFrom("20260401", content, "");

      const result = await getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 100,
        offset: 0,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries.map(messageOf)).toEqual([
        '{"msg":"a"}',
        '{"msg":"b"}',
      ]);
    });

    it("should count a trailing line without a newline toward the offset", async () => {
      const day1 = '{"msg":"d1-l1"}\n{"msg":"d1-l2"}';
      const day2 = '{"msg":"d2-l1"}\n{"msg":"d2-l2"}';

      serveDaysFrom("20260401", day1, day2);

      const result = await getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 10,
        offset: 2,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries.map(messageOf)).toEqual([
        '{"msg":"d1-l1"}',
        '{"msg":"d1-l2"}',
      ]);
    });

    it("should return the newest entries when an older day holds 10000 lines", async () => {
      const files: Record<string, string> = {
        "logs/bot-1/20260401.log":
          Array.from({ length: 10000 }, (_, i) => `{"msg":"d1-${i}"}`).join(
            "\n",
          ) + "\n",
        "logs/bot-1/20260402.log": '{"msg":"d2-0"}\n',
      };
      mockMinioClient.getObject.mockImplementation(
        async (_bucket: string, path: string) => stringStream(files[path]),
      );
      mockMinioClient.statObject.mockImplementation(
        async (_bucket: string, path: string) => ({
          size: Buffer.byteLength(files[path]),
        }),
      );

      const result = await getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 2,
        offset: 0,
        sort: "desc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries.map(messageOf)).toEqual([
        '{"msg":"d2-0"}',
        '{"msg":"d1-9999"}',
      ]);
      expect(result.data!.hasMore).toBe(true);
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

      const result = await getLogs("bot-1", {
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

      const result = await getLogs("bot-1", {
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

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
        sort: "asc",
        type: "all",
      });

      expect(result.success).toBe(true);
      // All lines should be returned regardless of format
      expect(result.data!.entries).toHaveLength(4);
      expect(messageOf(result.data!.entries[0])).toBe(
        "[2026-03-30 13:00:26] INFO:main:Starting bot",
      );
      expect(result.data!.entries[0].timestamp).toBeNull();
      expect(messageOf(result.data!.entries[1])).toBe(
        '{"level":"info","msg":"json line"}',
      );
      expect(result.data!.entries[1].timestamp).toBeNull();
      expect(messageOf(result.data!.entries[2])).toBe("plain text log line");
      expect(result.data!.entries[2].timestamp).toBeNull();
      expect(result.data!.entries[3].content).toContain('"_metric"');
      expect(result.data!.entries[3].timestamp).toBeNull();
    });

    it("should handle leftover line with JSON-based metrics filtering", async () => {
      // No trailing newline, so the metric line becomes leftover
      const content =
        '{"level":"info","msg":"check _metric docs"}\n{"_metric":true,"name":"sharpe","value":1.5}';

      mockMinioClient.getObject.mockResolvedValue(stringStream(content));

      const result = await getLogs("bot-1", {
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

      const result = await getLogs("bot-1", {
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

  describe("getLogs - entry shape", () => {
    it("should pass a runtime line through verbatim, keyed by its _key", async () => {
      const line =
        '{"_key":"1792674000123456789-0","message":"Bought 1 BTC","timestamp":"2026-10-22T10:00:00.123Z"}';
      mockMinioClient.getObject.mockResolvedValue(stringStream(line + "\n"));

      const result = await getLogs("bot-1", {
        date: "20261022",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toEqual([
        {
          key: "1792674000123456789-0",
          date: "20261022",
          timestamp: "2026-10-22T10:00:00.123Z",
          content: line,
        },
      ]);
    });

    it("should key a line without _key by its file date and byte offset", async () => {
      const wrapped =
        '{"timestamp":"2026-03-30T13:00:27Z","message":"Running strategy"}';
      const content = `plain one\n${wrapped}\nlast\n`;
      mockMinioClient.getObject.mockResolvedValue(stringStream(content));

      const result = await getLogs("bot-1", {
        date: "20260330",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(3);
      expect(result.data!.entries).toEqual(
        expect.arrayContaining([
          {
            key: "20260330:0",
            date: "20260330",
            timestamp: null,
            content: "plain one",
          },
          {
            key: "20260330:10",
            date: "20260330",
            timestamp: "2026-03-30T13:00:27Z",
            content: wrapped,
          },
          {
            key: `20260330:${11 + wrapped.length}`,
            date: "20260330",
            timestamp: null,
            content: "last",
          },
        ]),
      );
    });

    it("should give a line the same key whether it was tailed or streamed", async () => {
      const content = "first\nsecond\nthird\n";
      mockMinioClient.getObject.mockImplementation(
        async (_bucket: string, path: string) => {
          if (!path.endsWith("20260401.log")) throw { code: "NoSuchKey" };
          return stringStream(content, 4);
        },
      );

      const tailed = await getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });
      const streamed = await getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 100,
        offset: 0,
        type: "all",
        sort: "asc",
      });

      const keysByContent = (entries: { key: string; content: string }[]) =>
        Object.fromEntries(entries.map((e) => [e.content, e.key]));
      expect(keysByContent(tailed.data!.entries)).toEqual({
        first: "20260401:0",
        second: "20260401:6",
        third: "20260401:13",
      });
      expect(keysByContent(streamed.data!.entries)).toEqual(
        keysByContent(tailed.data!.entries),
      );
    });

    it("should count key offsets from the start of the file when tailing mid-file", async () => {
      const tailBytes = 512 * 1024;
      mockMinioClient.statObject.mockResolvedValue({ size: tailBytes + 100 });
      mockMinioClient.getPartialObject.mockResolvedValue(
        stringStream("partial\nline-a\nline-b\nline-c\n"),
      );

      // The tail holds limit + 1 whole lines, so the day is not read whole
      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 2,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(
        result.data!.entries.map((e) => [e.content, e.key]).sort(),
      ).toEqual([
        ["line-b", "20260401:115"],
        ["line-c", "20260401:122"],
      ]);
    });

    it("should count key offsets in bytes, not characters", async () => {
      mockMinioClient.getObject.mockResolvedValue(
        stringStream("prix: 10€\nnext\n"),
      );

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });

      const next = result.data!.entries.find((e) => e.content === "next");
      expect(next!.key).toBe("20260401:12");
    });

    it("should keep a bot's own string timestamp as written", async () => {
      const line = '{"timestamp":"2026-10-22 12:00:00+02:00","message":"x"}';
      mockMinioClient.getObject.mockResolvedValue(stringStream(line));

      const result = await getLogs("bot-1", {
        date: "20261022",
        limit: 100,
        offset: 0,
      });

      expect(result.data!.entries[0].timestamp).toBe(
        "2026-10-22 12:00:00+02:00",
      );
    });

    it("should report a null timestamp for plain text and non-string timestamps", async () => {
      mockMinioClient.getObject.mockResolvedValue(
        stringStream(
          '[2026-03-30 13:00:26] INFO:main:Starting bot\n{"timestamp":1792674000,"message":"x"}\n',
        ),
      );

      const result = await getLogs("bot-1", {
        date: "20260330",
        limit: 100,
        offset: 0,
      });

      expect(result.data!.entries.map((e) => e.timestamp)).toEqual([
        null,
        null,
      ]);
    });

    it.each(["order-1", "2024-10", "12345-1"])(
      "should ignore a _key the runtime did not write (%s)",
      async (botKey) => {
        // Before the runtime stamped keys, a bot could log its own _key
        const lines = [
          `{"_key":"${botKey}","message":"a"}`,
          `{"_key":"${botKey}","message":"b"}`,
        ];
        mockMinioClient.getObject.mockResolvedValue(
          stringStream(lines.join("\n") + "\n"),
        );

        const result = await getLogs("bot-1", {
          date: "20260401",
          limit: 100,
          offset: 0,
          sort: "asc",
        });

        expect(result.data!.entries.map((e) => [e.key, e.content])).toEqual([
          ["20260401:0", lines[0]],
          [`20260401:${lines[0].length + 1}`, lines[1]],
        ]);
      },
    );
    it("should keep a metric line's full JSON as content", async () => {
      const metricLine =
        '{"_metric":"price","value":42,"timestamp":"2026-04-01T10:00:00Z"}';
      mockMinioClient.getObject.mockResolvedValue(stringStream(metricLine));

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });

      expect(result.data!.entries[0]).toEqual({
        key: "20260401:0",
        date: "20260401",
        content: metricLine,
        timestamp: "2026-04-01T10:00:00Z",
      });
    });

    it("should filter metrics using JSON parsing not string matching", async () => {
      const lines = [
        '{"timestamp":"2026-03-30T13:00:26Z","message":"check _metric docs"}',
        '{"_metric":"price","value":42,"timestamp":"2026-04-01T10:00:00Z"}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await getLogs("bot-1", {
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

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 10,
        offset: 0,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(10);
      // The returned entries should be the LAST 10 lines, not the first 10
      expect(messageOf(result.data!.entries[0])).toBe("line-90");
      expect(messageOf(result.data!.entries[9])).toBe("line-99");
    });

    it("should return chronological entries for date range", async () => {
      const day1 = [
        '{"timestamp":"2026-04-01T01:00:00Z","message":"d1-line1"}',
        '{"timestamp":"2026-04-01T02:00:00Z","message":"d1-line2"}',
      ].join("\n");
      const day2 = [
        '{"timestamp":"2026-04-02T01:00:00Z","message":"d2-line1"}',
        '{"timestamp":"2026-04-02T02:00:00Z","message":"d2-line2"}',
      ].join("\n");

      serveDaysFrom("20260401", day1, day2);

      const result = await getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 10,
        offset: 0,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(4);
      // Chronological order: day1 first, then day2
      expect(messageOf(result.data!.entries[0])).toBe("d1-line1");
      expect(messageOf(result.data!.entries[1])).toBe("d1-line2");
      expect(messageOf(result.data!.entries[2])).toBe("d2-line1");
      expect(messageOf(result.data!.entries[3])).toBe("d2-line2");
    });

    it("should find metrics anywhere in a single day", async () => {
      const lines = [
        '{"level":"info","message":"noise"}',
        '{"_metric":true,"name":"pnl","value":42}',
        '{"level":"info","message":"more noise"}',
        '{"_metric":true,"name":"sharpe","value":1.5}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 10,
        offset: 0,
        type: "metrics",
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(2);
      expect(result.data!.entries[0].content).toContain('"name":"pnl"');
      expect(result.data!.entries[1].content).toContain('"name":"sharpe"');
      // Metrics are read from the whole day, not its tail
      expect(mockMinioClient.getObject).toHaveBeenCalled();
    });

    it("should respect limit for metrics scattered through a large file", async () => {
      // 200 lines: 150 noise + 50 metrics scattered throughout
      const lines =
        Array.from({ length: 200 }, (_, i) =>
          i % 4 === 0
            ? `{"_metric":true,"name":"metric-${i}","value":${i}}`
            : `{"level":"info","message":"noise-${i}"}`,
        ).join("\n") + "\n";

      mockMinioClient.getObject.mockImplementation(() => stringStream(lines));

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 10,
        offset: 0,
        type: "metrics",
      });

      expect(result.success).toBe(true);
      // Newest 10 of the 50 metrics; the rest are reachable via offset
      expect(result.data!.entries.length).toBe(10);
      expect(result.data!.entries[0].content).toContain("metric-196");
      expect(result.data!.hasMore).toBe(true);
    });

    it("should still respect limit for non-metrics queries", async () => {
      const lines =
        Array.from(
          { length: 50 },
          (_, i) =>
            `{"timestamp":"2026-04-01T10:00:00Z","message":"line-${i}"}`,
        ).join("\n") + "\n";

      mockMinioClient.statObject.mockResolvedValue({ size: 100 });
      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await getLogs("bot-1", {
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

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      // All lines should be returned, nothing skipped
      expect(result.data!.entries).toHaveLength(3);
      expect(messageOf(result.data!.entries[0])).toBe("line-1");
      expect(messageOf(result.data!.entries[1])).toBe("line-2");
      expect(messageOf(result.data!.entries[2])).toBe("line-3");
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

      // The tail holds limit + 1 whole lines, so the day is not read whole
      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 1,
        offset: 0,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries.map(messageOf)).toEqual(["complete-line-2"]);
      expect(result.data!.hasMore).toBe(true);
      expect(mockMinioClient.getPartialObject).toHaveBeenCalled();
      expect(mockMinioClient.getObject).not.toHaveBeenCalled();
    });
  });

  describe("getLogs - datetime range filtering", () => {
    it("should stop reading a day once its lines pass the end of the window", async () => {
      // 10000 lines a second apart from midnight; the window holds the first 600
      const at = (i: number) =>
        new Date(Date.UTC(2026, 3, 1) + i * 1000).toISOString();
      const lines = Array.from(
        { length: 10000 },
        (_, i) => `{"message":"m${i}","timestamp":"${at(i)}"}\n`,
      );
      const file = Buffer.from(lines.join(""));
      let linesRead = 0;
      mockMinioClient.statObject.mockResolvedValue({ size: file.length });
      mockMinioClient.getPartialObject.mockImplementation(
        async (_b: string, _p: string, start: number, length = 0) =>
          Readable.from([
            file.subarray(start, length ? start + length : file.length),
          ]),
      );
      mockMinioClient.getObject.mockImplementation(async () =>
        Readable.from(
          (function* () {
            for (const line of lines) {
              linesRead++;
              yield Buffer.from(line);
            }
          })(),
        ),
      );

      const result = await getLogs("bot-1", {
        dateRange: `${at(0)}--${at(599)}`,
        limit: 2,
        offset: 0,
      });

      expect(result.data!.entries.map(messageOf)).toEqual(["m599", "m598"]);
      expect(linesRead).toBeLessThan(1000);
    });

    it("should keep reading past lines without a timestamp", async () => {
      const lines = [
        '{"message":"a","timestamp":"2026-04-01T00:00:00Z"}',
        "no timestamp here",
        '{"message":"b","timestamp":"2026-04-01T00:00:01Z"}',
        '{"message":"late","timestamp":"2026-04-01T05:00:00Z"}',
      ];
      mockMinioClient.getObject.mockResolvedValue(
        stringStream(lines.join("\n") + "\n"),
      );

      const result = await getLogs("bot-1", {
        dateRange: "2026-04-01T00:00:00Z--2026-04-01T00:00:01Z",
        limit: 10,
        offset: 0,
        sort: "asc",
      });

      expect(result.data!.entries.map(messageOf)).toEqual([
        "a",
        "no timestamp here",
        "b",
      ]);
    });
    it("should filter lines by timestamp when datetime range is provided", async () => {
      const lines = [
        '{"timestamp":"2026-04-03T09:30:00Z","message":"before range"}',
        '{"timestamp":"2026-04-03T10:15:00Z","message":"in range 1"}',
        '{"timestamp":"2026-04-03T10:45:00Z","message":"in range 2"}',
        '{"timestamp":"2026-04-03T11:30:00Z","message":"after range"}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await getLogs("bot-1", {
        dateRange: "2026-04-03T10:00:00Z--2026-04-03T11:00:00Z",
        limit: 100,
        offset: 0,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(2);
      expect(messageOf(result.data!.entries[0])).toBe("in range 1");
      expect(messageOf(result.data!.entries[1])).toBe("in range 2");
    });

    it("should still work with YYYYMMDD date range", async () => {
      const day1 = '{"timestamp":"2026-04-01T12:00:00Z","message":"day1 line"}';
      const day2 = '{"timestamp":"2026-04-02T12:00:00Z","message":"day2 line"}';
      const day3 = '{"timestamp":"2026-04-03T12:00:00Z","message":"day3 line"}';

      serveDaysFrom("20260401", day1, day2, day3);

      const result = await getLogs("bot-1", {
        dateRange: "20260401-20260403",
        limit: 100,
        offset: 0,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(3);
      expect(messageOf(result.data!.entries[0])).toBe("day1 line");
      expect(messageOf(result.data!.entries[1])).toBe("day2 line");
      expect(messageOf(result.data!.entries[2])).toBe("day3 line");
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

      serveDaysFrom("20260402", day1Lines, day2Lines);

      const result = await getLogs("bot-1", {
        dateRange: "2026-04-02T23:00:00Z--2026-04-03T01:00:00Z",
        limit: 100,
        offset: 0,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(2);
      expect(messageOf(result.data!.entries[0])).toBe("late night");
      expect(messageOf(result.data!.entries[1])).toBe("early morning");
    });

    it("should include lines without timestamps when filtering by datetime", async () => {
      const lines = [
        '{"timestamp":"2026-04-03T10:15:00Z","message":"in range"}',
        "[2026-04-03 09:00:00] INFO:main:old format no json timestamp",
        "plain text line with no timestamp at all",
        '{"timestamp":"2026-04-03T12:00:00Z","message":"after range"}',
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await getLogs("bot-1", {
        dateRange: "2026-04-03T10:00:00Z--2026-04-03T11:00:00Z",
        limit: 100,
        offset: 0,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      // "in range" is in window, old format and plain text have no timestamp so included,
      // "after range" is outside the window so excluded
      expect(result.data!.entries).toHaveLength(3);
      expect(messageOf(result.data!.entries[0])).toBe("in range");
      expect(messageOf(result.data!.entries[1])).toBe(
        "[2026-04-03 09:00:00] INFO:main:old format no json timestamp",
      );
      expect(messageOf(result.data!.entries[2])).toBe(
        "plain text line with no timestamp at all",
      );
    });

    it("should include a trailing line without a timestamp when filtering by datetime", async () => {
      const lines = [
        '{"timestamp":"2026-04-03T10:15:00Z","message":"in range"}',
        "plain trailing line",
      ].join("\n");

      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await getLogs("bot-1", {
        dateRange: "2026-04-03T10:00:00Z--2026-04-03T11:00:00Z",
        limit: 100,
        offset: 0,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries.map(messageOf)).toEqual([
        "in range",
        "plain trailing line",
      ]);
    });

    it("should filter a single-day datetime range by time", async () => {
      const lines = [
        '{"timestamp":"2026-04-03T08:00:00Z","message":"morning"}',
        '{"timestamp":"2026-04-03T10:30:00Z","message":"mid-morning"}',
        '{"timestamp":"2026-04-03T14:00:00Z","message":"afternoon"}',
      ];
      const fileContent = lines.join("\n") + "\n";
      const fileSize = Buffer.byteLength(fileContent, "utf-8");

      mockMinioClient.statObject.mockResolvedValue({ size: fileSize });
      mockMinioClient.getObject.mockResolvedValue(stringStream(fileContent));

      const result = await getLogs("bot-1", {
        dateRange: "2026-04-03T10:00:00Z--2026-04-03T11:00:00Z",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(1);
      expect(messageOf(result.data!.entries[0])).toBe("mid-morning");
    });
  });

  describe("getLogs - sort parameter", () => {
    it("should return newest-first by default for a single date", async () => {
      const lines =
        Array.from(
          { length: 10 },
          (_, i) =>
            `{"timestamp":"2026-04-01T${String(i).padStart(2, "0")}:00:00Z","message":"line-${i}"}`,
        ).join("\n") + "\n";

      mockMinioClient.statObject.mockResolvedValue({ size: 100 });
      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 5,
        offset: 0,
        sort: "desc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(5);
      expect(result.data!.entries.map(messageOf)).toEqual([
        "line-9",
        "line-8",
        "line-7",
        "line-6",
        "line-5",
      ]);
    });

    it("should return oldest-first when sort=asc for single date", async () => {
      const lines =
        Array.from(
          { length: 10 },
          (_, i) =>
            `{"timestamp":"2026-04-01T${String(i).padStart(2, "0")}:00:00Z","message":"line-${i}"}`,
        ).join("\n") + "\n";

      mockMinioClient.statObject.mockResolvedValue({ size: 100 });
      mockMinioClient.getObject.mockResolvedValue(stringStream(lines));

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 5,
        offset: 0,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(5);
      // The newest five, oldest of them first
      expect(result.data!.entries.map(messageOf)).toEqual([
        "line-5",
        "line-6",
        "line-7",
        "line-8",
        "line-9",
      ]);
    });

    it("should return newest-first for date range when sort=desc", async () => {
      const day1 = '{"msg":"d1-l1"}\n{"msg":"d1-l2"}\n{"msg":"d1-l3"}';
      const day2 = '{"msg":"d2-l1"}\n{"msg":"d2-l2"}\n{"msg":"d2-l3"}';

      serveDaysFrom("20260401", day1, day2);

      const result = await getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 6,
        offset: 0,
        sort: "desc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(6);
      // Reversed: newest (d2-l3) first, oldest (d1-l1) last
      expect(result.data!.entries[0].content).toContain("d2-l3");
      expect(result.data!.entries[5].content).toContain("d1-l1");
    });

    it("should handle offset with sort=desc on date range", async () => {
      // 6 entries total, sort=desc, offset=2, limit=2
      // desc order: d2-l3, d2-l2, d2-l1, d1-l3, d1-l2, d1-l1
      // offset=2 skips first 2, returns d2-l1 and d1-l3
      const day1 = '{"msg":"d1-l1"}\n{"msg":"d1-l2"}\n{"msg":"d1-l3"}';
      const day2 = '{"msg":"d2-l1"}\n{"msg":"d2-l2"}\n{"msg":"d2-l3"}';

      serveDaysFrom("20260401", day1, day2);

      const result = await getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 2,
        offset: 2,
        sort: "desc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(2);
      expect(result.data!.entries[0].content).toContain("d2-l1");
      expect(result.data!.entries[1].content).toContain("d1-l3");
    });

    it("should default to desc when sort is not specified", async () => {
      const day1 = '{"msg":"d1-l1"}\n{"msg":"d1-l2"}';
      const day2 = '{"msg":"d2-l1"}\n{"msg":"d2-l2"}';

      serveDaysFrom("20260401", day1, day2);

      const result = await getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 10,
        offset: 0,
        // no sort specified
      });

      expect(result.success).toBe(true);
      // Should default to desc (newest-first)
      expect(result.data!.entries[0].content).toContain("d2-l2");
      expect(result.data!.entries[3].content).toContain("d1-l1");
    });
  });

  describe("getLogs - lookbackDays (latest mode)", () => {
    /** UTC YYYYMMDD for N days ago, matching the daily log file names. */
    function utcDateStr(daysAgo: number): string {
      const d = new Date();
      d.setUTCDate(d.getUTCDate() - daysAgo);
      return d.toISOString().slice(0, 10).replace(/-/g, "");
    }

    /** Mock MinIO so each logs/bot-1/<date>.log resolves to the given content. */
    function mockFiles(files: Record<string, string>) {
      const notFound = () => {
        const err: any = new Error("NoSuchKey");
        err.code = "NoSuchKey";
        return Promise.reject(err);
      };
      mockMinioClient.getObject.mockImplementation(
        (_bucket: string, path: string) =>
          files[path] !== undefined
            ? Promise.resolve(stringStream(files[path]))
            : notFound(),
      );
      mockMinioClient.statObject.mockImplementation(
        (_bucket: string, path: string) =>
          files[path] !== undefined
            ? Promise.resolve({
                size: Buffer.byteLength(files[path], "utf-8"),
              })
            : notFound(),
      );
    }

    it("should return newest-first entries across day files without a date param", async () => {
      const today = utcDateStr(0);
      const older = utcDateStr(2);
      mockFiles({
        [`logs/bot-1/${older}.log`]: '{"message":"old-1"}\n{"message":"old-2"}',
        [`logs/bot-1/${today}.log`]: '{"message":"new-1"}\n{"message":"new-2"}',
      });

      const result = await getLogs("bot-1", {
        lookbackDays: 7,
        limit: 10,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries.map(messageOf)).toEqual([
        "new-2",
        "new-1",
        "old-2",
        "old-1",
      ]);
      // Days with no log file are skipped, not errors
      expect(result.data!.hasMore).toBe(false);
    });

    it("should return the newest `limit` metric entries across days", async () => {
      const metric = (day: string, n: number) =>
        `{"_metric":"signal","run":"${day}-${n}","timestamp":"2026-01-01T0${n}:00:00Z"}`;
      const today = utcDateStr(0);
      const yesterday = utcDateStr(1);
      const older = utcDateStr(2);
      mockFiles({
        [`logs/bot-1/${today}.log`]: [metric("t", 1), metric("t", 2)].join(
          "\n",
        ),
        [`logs/bot-1/${yesterday}.log`]: [metric("y", 1), metric("y", 2)].join(
          "\n",
        ),
        [`logs/bot-1/${older}.log`]: [metric("o", 1), metric("o", 2)].join(
          "\n",
        ),
      });

      const result = await getLogs("bot-1", {
        lookbackDays: 7,
        limit: 3,
        offset: 0,
        type: "metrics",
      });

      expect(result.success).toBe(true);
      // Unlike date-range metric queries, latest mode respects limit
      expect(result.data!.entries.map(messageOf)).toEqual([
        metric("t", 2),
        metric("t", 1),
        metric("y", 2),
      ]);
      expect(result.data!.hasMore).toBe(true);
    });

    it("should stop scanning older days once enough entries are collected", async () => {
      const today = utcDateStr(0);
      const yesterday = utcDateStr(1);
      mockFiles({
        [`logs/bot-1/${today}.log`]:
          '{"message":"a"}\n{"message":"b"}\n{"message":"c"}',
        [`logs/bot-1/${yesterday}.log`]: '{"message":"y"}',
      });

      const result = await getLogs("bot-1", {
        lookbackDays: 7,
        limit: 2,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries.map(messageOf)).toEqual(["c", "b"]);
      expect(result.data!.hasMore).toBe(true);
      // Only today's file was opened; older days were never touched
      const touchedPaths = [
        ...mockMinioClient.statObject.mock.calls,
        ...mockMinioClient.getObject.mock.calls,
      ].map((call: any[]) => call[1]);
      expect(touchedPaths).not.toContain(`logs/bot-1/${yesterday}.log`);
    });

    it("should apply offset across the lookback window for pagination", async () => {
      const today = utcDateStr(0);
      const yesterday = utcDateStr(1);
      mockFiles({
        [`logs/bot-1/${today}.log`]: '{"message":"t1"}\n{"message":"t2"}',
        [`logs/bot-1/${yesterday}.log`]: '{"message":"y1"}\n{"message":"y2"}',
      });

      const result = await getLogs("bot-1", {
        lookbackDays: 7,
        limit: 2,
        offset: 2,
      });

      expect(result.success).toBe(true);
      // Newest-first flat order is [t2, t1, y2, y1]; offset 2 -> [y2, y1]
      expect(result.data!.entries.map(messageOf)).toEqual(["y2", "y1"]);
      expect(result.data!.hasMore).toBe(false);
    });

    it("should return chronological order when sort=asc", async () => {
      const today = utcDateStr(0);
      const yesterday = utcDateStr(1);
      mockFiles({
        [`logs/bot-1/${today}.log`]: '{"message":"t1"}\n{"message":"t2"}',
        [`logs/bot-1/${yesterday}.log`]: '{"message":"y1"}\n{"message":"y2"}',
      });

      const result = await getLogs("bot-1", {
        lookbackDays: 7,
        limit: 10,
        offset: 0,
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries.map(messageOf)).toEqual([
        "y1",
        "y2",
        "t1",
        "t2",
      ]);
    });

    it("should fall back to a full read when the tail window holds too few lines", async () => {
      const today = utcDateStr(0);
      const todayPath = `logs/bot-1/${today}.log`;
      // File larger than the 512KB tail window whose tail only contains 2
      // lines (very long lines); the full file has 5
      const fullLines = Array.from(
        { length: 5 },
        (_, i) => `{"message":"line-${i + 1}"}`,
      );
      const notFound = () => {
        const err: any = new Error("NoSuchKey");
        err.code = "NoSuchKey";
        return Promise.reject(err);
      };
      mockMinioClient.statObject.mockImplementation(
        (_bucket: string, path: string) =>
          path === todayPath
            ? Promise.resolve({ size: 512 * 1024 + 1000 })
            : notFound(),
      );
      mockMinioClient.getPartialObject.mockImplementation(
        (_bucket: string, path: string) =>
          path === todayPath
            ? Promise.resolve(
                stringStream("partial-junk\n" + fullLines.slice(-2).join("\n")),
              )
            : notFound(),
      );
      mockMinioClient.getObject.mockImplementation(
        (_bucket: string, path: string) =>
          path === todayPath
            ? Promise.resolve(stringStream(fullLines.join("\n")))
            : notFound(),
      );

      const result = await getLogs("bot-1", {
        lookbackDays: 7,
        limit: 3,
        offset: 0,
      });

      expect(result.success).toBe(true);
      // Tail yielded only 2 of the needed 4 entries, so the day is re-read
      // in full and the newest 3 lines are returned
      expect(result.data!.entries.map(messageOf)).toEqual([
        "line-5",
        "line-4",
        "line-3",
      ]);
      expect(result.data!.hasMore).toBe(true);
    });

    it("should prefer an explicit date over lookbackDays", async () => {
      mockFiles({
        "logs/bot-1/20260401.log": '{"message":"explicit-date"}',
      });

      const result = await getLogs("bot-1", {
        date: "20260401",
        lookbackDays: 7,
        limit: 10,
        offset: 0,
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(1);
      expect(messageOf(result.data!.entries[0])).toBe("explicit-date");
      expect(result.data!.entries[0].date).toBe("20260401");
    });

    it("should return empty entries when no day in the window has logs", async () => {
      mockFiles({});

      const result = await getLogs("bot-1", {
        lookbackDays: 7,
        limit: 10,
        offset: 0,
        type: "metrics",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toEqual([]);
      expect(result.data!.hasMore).toBe(false);
    });
  });

  describe("getLogs - metrics pagination", () => {
    const metricLine = (n: number) =>
      `{"_metric":true,"name":"m${n}","value":${n}}`;

    it("should page backwards through metric history with offset (desc)", async () => {
      const lines = Array.from({ length: 5 }, (_, i) => metricLine(i + 1)).join(
        "\n",
      );
      mockMinioClient.getObject.mockImplementation(() => stringStream(lines));

      const page1 = await getLogs("bot-1", {
        date: "20260401",
        limit: 2,
        offset: 0,
        type: "metrics",
      });
      expect(page1.success).toBe(true);
      expect(page1.data!.entries.map(messageOf)).toEqual([
        metricLine(5),
        metricLine(4),
      ]);
      expect(page1.data!.hasMore).toBe(true);

      const page2 = await getLogs("bot-1", {
        date: "20260401",
        limit: 2,
        offset: 2,
        type: "metrics",
      });
      expect(page2.data!.entries.map(messageOf)).toEqual([
        metricLine(3),
        metricLine(2),
      ]);
      expect(page2.data!.hasMore).toBe(true);

      const page3 = await getLogs("bot-1", {
        date: "20260401",
        limit: 2,
        offset: 4,
        type: "metrics",
      });
      expect(page3.data!.entries.map(messageOf)).toEqual([metricLine(1)]);
      expect(page3.data!.hasMore).toBe(false);
    });

    it("should report hasMore false when all metrics fit in one page", async () => {
      const lines = [metricLine(1), metricLine(2), metricLine(3)].join("\n");
      mockMinioClient.getObject.mockImplementation(() => stringStream(lines));

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 10,
        offset: 0,
        type: "metrics",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries).toHaveLength(3);
      expect(result.data!.hasMore).toBe(false);
    });

    it("should respect limit and report hasMore for asc metric queries", async () => {
      const lines = Array.from({ length: 4 }, (_, i) => metricLine(i + 1)).join(
        "\n",
      );
      mockMinioClient.getObject.mockImplementation(() => stringStream(lines));

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 2,
        offset: 0,
        type: "metrics",
        sort: "asc",
      });

      expect(result.success).toBe(true);
      expect(result.data!.entries.map(messageOf)).toEqual([
        metricLine(3),
        metricLine(4),
      ]);
      expect(result.data!.hasMore).toBe(true);
    });

    it("should paginate metrics across a date range", async () => {
      const day1 = ['{"level":"info","message":"noise"}', metricLine(1)].join(
        "\n",
      );
      const day2 = [metricLine(2), metricLine(3)].join("\n");
      mockMinioClient.getObject.mockImplementation(
        (_bucket: string, path: string) =>
          Promise.resolve(
            stringStream(path.includes("20260401") ? day1 : day2),
          ),
      );

      const result = await getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 2,
        offset: 0,
        type: "metrics",
      });

      expect(result.success).toBe(true);
      // Newest-first across days: day2's metrics come first
      expect(result.data!.entries.map(messageOf)).toEqual([
        metricLine(3),
        metricLine(2),
      ]);
      expect(result.data!.hasMore).toBe(true);
    });
  });

  describe("getLogs - cursor paging", () => {
    /** Serves logs/bot-1/<date>.log from `files`, ranged reads included. */
    function serveFiles(files: Record<string, string>) {
      const content = (path: string) => {
        const date = path.slice("logs/bot-1/".length, -".log".length);
        if (files[date] === undefined) throw { code: "NoSuchKey" };
        return Buffer.from(files[date]);
      };
      mockMinioClient.statObject.mockImplementation(
        async (_bucket: string, path: string) => ({
          size: content(path).length,
        }),
      );
      mockMinioClient.getObject.mockImplementation(
        async (_bucket: string, path: string) => Readable.from([content(path)]),
      );
      mockMinioClient.getPartialObject.mockImplementation(
        async (_bucket: string, path: string, start: number, length = 0) => {
          const bytes = content(path);
          return Readable.from([
            bytes.subarray(start, length ? start + length : bytes.length),
          ]);
        },
      );
    }

    const page = async (query: Partial<LogsQuery>) => {
      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
        ...query,
      });
      expect(result.success).toBe(true);
      return result.data!;
    };

    it("should return the entries strictly older than the cursor entry", async () => {
      serveFiles({ "20260401": "a\nb\nc\nd\n" });

      const older = await page({ before: "20260401:4" });

      expect(older.entries.map((e) => e.content)).toEqual(["b", "a"]);
      expect(older.hasMore).toBe(false);
      expect(older.nextCursor).toBeNull();
    });

    it("should page backwards with nextCursor until no older entries remain", async () => {
      serveFiles({ "20260401": "a\nb\nc\nd\ne\n" });

      const seen: string[] = [];
      let result = await page({ limit: 2 });
      seen.push(...result.entries.map((e) => e.content));
      while (result.nextCursor) {
        result = await page({ limit: 2, before: result.nextCursor });
        seen.push(...result.entries.map((e) => e.content));
      }

      expect(seen).toEqual(["e", "d", "c", "b", "a"]);
      expect(result.hasMore).toBe(false);
    });

    it("should not shift a page when new lines are appended after the cursor", async () => {
      serveFiles({ "20260401": "a\nb\nc\nd\n" });
      const first = await page({ limit: 2 });

      serveFiles({ "20260401": "a\nb\nc\nd\nnew-1\nnew-2\n" });
      const second = await page({ limit: 2, before: first.nextCursor! });

      expect(first.entries.map((e) => e.content)).toEqual(["d", "c"]);
      expect(second.entries.map((e) => e.content)).toEqual(["b", "a"]);
    });

    it("should accept a runtime _key as the cursor", async () => {
      const line = (i: number) =>
        `{"_key":"1775001600000000000-${i}","message":"m${i}"}`;
      serveFiles({ "20260401": [0, 1, 2, 3].map(line).join("\n") + "\n" });

      const older = await page({ before: "1775001600000000000-2" });

      expect(older.entries.map((e) => e.key)).toEqual([
        "1775001600000000000-1",
        "1775001600000000000-0",
      ]);
    });

    it("should continue into earlier days of the window", async () => {
      serveFiles({ "20260401": "d1-a\nd1-b\n", "20260402": "d2-a\nd2-b\n" });

      const older = await page({
        date: undefined,
        dateRange: "20260401-20260402",
        before: "20260402:5",
      });

      expect(older.entries.map((e) => e.content)).toEqual([
        "d2-a",
        "d1-b",
        "d1-a",
      ]);
    });

    it("should return the entries just before the cursor oldest-first when sort=asc", async () => {
      serveFiles({ "20260401": "a\nb\nc\nd\ne\n" });

      const older = await page({ before: "20260401:8", limit: 2, sort: "asc" });

      expect(older.entries.map((e) => e.content)).toEqual(["c", "d"]);
      expect(older.hasMore).toBe(true);
      expect(older.nextCursor).toBe("20260401:4");
    });

    it("should keep only metric lines older than the cursor when type=metrics", async () => {
      serveFiles({
        "20260401":
          '{"_metric":"m1"}\nlog\n{"_metric":"m2"}\n{"_metric":"m3"}\n',
      });

      const older = await page({ before: "20260401:38", type: "metrics" });

      expect(older.entries.map((e) => e.content)).toEqual([
        '{"_metric":"m2"}',
        '{"_metric":"m1"}',
      ]);
    });

    it("should reject a cursor that matches no entry", async () => {
      serveFiles({ "20260401": "a\n" });

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 10,
        offset: 0,
        before: "1775001600000000000-9",
      });

      expect(result.success).toBe(false);
      expect(result.error).toMatch(/cursor/i);
      expect(result.error).not.toMatch(/not found/i);
    });

    it("should reject a position cursor whose offset is not a safe byte offset", async () => {
      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 10,
        offset: 0,
        before: `20260401:${"9".repeat(22)}`,
      });

      expect(result.success).toBe(false);
      expect(result.error).toMatch(/offset is too large/i);
      expect(mockMinioClient.getPartialObject).not.toHaveBeenCalled();
    });
    it("should reject a malformed cursor", async () => {
      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 10,
        offset: 0,
        before: "not-a-key",
      });

      expect(result.success).toBe(false);
      expect(result.error).toMatch(/cursor/i);
    });

    it("should refuse before combined with offset", async () => {
      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 10,
        offset: 5,
        before: "20260401:4",
      });

      expect(result.success).toBe(false);
      expect(result.error).toMatch(/offset/);
    });

    it("should report nextCursor on an offset page that has more", async () => {
      serveFiles({ "20260401": "a\nb\nc\n" });

      const first = await page({ limit: 2 });

      expect(first.hasMore).toBe(true);
      expect(first.nextCursor).toBe("20260401:2");
    });

    describe("ascending pages", () => {
      it("should return the newest entries of a multi-day window oldest-first and page back with nextCursor", async () => {
        serveFiles({ "20260401": "a\nb\n", "20260402": "c\nd\n" });
        const window: Partial<LogsQuery> = {
          date: undefined,
          dateRange: "20260401-20260402",
          limit: 2,
          sort: "asc",
        };

        const newest = await page(window);
        const older = await page({ ...window, before: newest.nextCursor! });

        expect(newest.entries.map((e) => e.content)).toEqual(["c", "d"]);
        expect(newest.nextCursor).toBe("20260402:0");
        expect(older.entries.map((e) => e.content)).toEqual(["a", "b"]);
        expect(older.nextCursor).toBeNull();
      });

      it("should return the newest metrics oldest-first with nextCursor", async () => {
        const metric = (n: number) => `{"_metric":"m${n}"}`;
        serveFiles({ "20260401": [1, 2, 3].map(metric).join("\n") + "\n" });

        const newest = await page({ limit: 2, type: "metrics", sort: "asc" });

        expect(newest.entries.map((e) => e.content)).toEqual([
          metric(2),
          metric(3),
        ]);
        expect(newest.nextCursor).toBe(`20260401:${metric(1).length + 1}`);
      });

      it("should return the newest entries of a datetime window oldest-first", async () => {
        const at = (hour: string) =>
          `{"message":"${hour}","timestamp":"2026-04-01T${hour}:00:00Z"}`;
        serveFiles({
          "20260401": ["01", "02", "03", "04"].map(at).join("\n") + "\n",
        });

        const newest = await page({
          date: undefined,
          dateRange: "2026-04-01T00:00:00Z--2026-04-01T03:30:00Z",
          limit: 2,
          sort: "asc",
        });

        expect(newest.entries.map(messageOf)).toEqual(["02", "03"]);
        expect(newest.hasMore).toBe(true);
        expect(newest.nextCursor).toBe(`20260401:${at("01").length + 1}`);
      });
    });

    it("should read the whole day when its tail holds fewer than limit lines", async () => {
      // Five 200 KB lines: the last 512 KB holds only two whole lines
      const lines = ["a", "b", "c", "d", "e"].map((c) => c.repeat(200 * 1024));
      serveFiles({ "20260401": lines.join("\n") + "\n" });

      const newest = await page({ limit: 3 });

      expect(newest.entries.map((e) => e.content[0])).toEqual(["e", "d", "c"]);
      expect(newest.hasMore).toBe(true);
      expect(newest.nextCursor).toBe(`20260401:${2 * (200 * 1024 + 1)}`);
    });
    describe("windows larger than 10000 entries", () => {
      // A realtime bot writing a metric every 5s fills ~17k lines a day
      const metric = (day: string, i: number) => `{"_metric":"${day}-${i}"}`;
      const metrics = (day: string, count: number) =>
        Array.from({ length: count }, (_, i) => metric(day, i)).join("\n") +
        "\n";

      it("should return the newest metrics of the window and continue with before", async () => {
        serveFiles({
          "20260401": metrics("d1", 10000),
          "20260402": metrics("d2", 3),
        });
        const window: Partial<LogsQuery> = {
          date: undefined,
          dateRange: "20260401-20260402",
          type: "metrics",
          limit: 2,
        };

        const newest = await page(window);
        const older = await page({ ...window, before: newest.nextCursor! });

        expect(newest.entries.map((e) => e.content)).toEqual([
          metric("d2", 2),
          metric("d2", 1),
        ]);
        expect(older.entries.map((e) => e.content)).toEqual([
          metric("d2", 0),
          metric("d1", 9999),
        ]);
      });

      it("should return the newest metrics oldest-first when sort=asc", async () => {
        serveFiles({
          "20260401": metrics("d1", 10000),
          "20260402": metrics("d2", 3),
        });

        const newest = await page({
          date: undefined,
          dateRange: "20260401-20260402",
          type: "metrics",
          limit: 2,
          sort: "asc",
        });

        expect(newest.entries.map((e) => e.content)).toEqual([
          metric("d2", 1),
          metric("d2", 2),
        ]);
      });

      it("should return the newest metrics of one day holding 17000", async () => {
        serveFiles({ "20260401": metrics("d1", 17000) });

        const newest = await page({ type: "metrics", limit: 2 });
        const older = await page({
          type: "metrics",
          limit: 2,
          before: newest.nextCursor!,
        });

        expect(newest.entries.map((e) => e.content)).toEqual([
          metric("d1", 16999),
          metric("d1", 16998),
        ]);
        expect(older.entries.map((e) => e.content)).toEqual([
          metric("d1", 16997),
          metric("d1", 16996),
        ]);
      });

      it("should page a day by a deep offset", async () => {
        const count = 160000;
        serveFiles({
          "20260401":
            Array.from({ length: count }, (_, i) => `l${i}`).join("\n") + "\n",
        });

        const deep = await page({ limit: 2, offset: 150000 });

        expect(deep.entries.map((e) => e.content)).toEqual([
          `l${count - 150001}`,
          `l${count - 150002}`,
        ]);
      });
      it("should continue a raw log window with before past a 10000-line day", async () => {
        const lines = (day: string, count: number) =>
          Array.from({ length: count }, (_, i) => `${day}-${i}`).join("\n") +
          "\n";
        serveFiles({
          "20260401": lines("d1", 10000),
          "20260402": lines("d2", 1),
        });
        const window: Partial<LogsQuery> = {
          date: undefined,
          dateRange: "20260401-20260402",
          limit: 2,
        };

        const newest = await page(window);
        const older = await page({ ...window, before: newest.nextCursor! });

        expect(newest.entries.map((e) => e.content)).toEqual([
          "d2-0",
          "d1-9999",
        ]);
        expect(older.entries.map((e) => e.content)).toEqual([
          "d1-9998",
          "d1-9997",
        ]);
      });
    });

    describe("repeated runtime keys", () => {
      const keyed = (i: number) =>
        `{"_key":"1775001600000000000-${i}","message":"m${i}"}`;
      const file = [keyed(1), keyed(2), keyed(1), keyed(3)].join("\n") + "\n";

      it("should return a repeated key once", async () => {
        serveFiles({ "20260401": file });

        const all = await page({});

        expect(all.entries.map((e) => e.key)).toEqual([
          "1775001600000000000-3",
          "1775001600000000000-2",
          "1775001600000000000-1",
        ]);
      });

      it("should page across a repeated key without skipping or looping", async () => {
        serveFiles({ "20260401": file });

        const keys: string[] = [];
        let result = await page({ limit: 1 });
        keys.push(...result.entries.map((e) => e.key));
        for (let i = 0; result.nextCursor && i < 10; i++) {
          result = await page({ limit: 1, before: result.nextCursor });
          keys.push(...result.entries.map((e) => e.key));
        }

        expect(result.nextCursor).toBeNull();
        expect(new Set(keys)).toEqual(
          new Set([
            "1775001600000000000-1",
            "1775001600000000000-2",
            "1775001600000000000-3",
          ]),
        );
      });
    });
  });

  describe("getLogs - UTC day files", () => {
    // Tests run in Pacific/Kiritimati (UTC+14), where this is Oct 23
    beforeEach(() => {
      jest.useFakeTimers({
        now: new Date("2026-10-22T12:00:00Z"),
        doNotFake: ["nextTick", "setImmediate", "queueMicrotask"],
      });
      mockMinioClient.statObject.mockRejectedValue({ code: "NoSuchKey" });
      mockMinioClient.getObject.mockRejectedValue({ code: "NoSuchKey" });
    });

    afterEach(() => {
      jest.useRealTimers();
    });

    const readPaths = () =>
      [
        ...mockMinioClient.statObject.mock.calls,
        ...mockMinioClient.getObject.mock.calls,
      ].map((call: string[]) => call[1]);

    it("should start a lookback at today's UTC day", async () => {
      await getLogs("bot-1", {
        lookbackDays: 2,
        limit: 10,
        offset: 0,
      });

      expect(new Set(readPaths())).toEqual(
        new Set(["logs/bot-1/20261022.log", "logs/bot-1/20261021.log"]),
      );
    });

    it("should read the UTC days a datetime range with an offset covers", async () => {
      await getLogs("bot-1", {
        dateRange: "2026-10-22T01:00:00+05:00--2026-10-22T03:00:00+05:00",
        limit: 10,
        offset: 0,
      });

      expect(new Set(readPaths())).toEqual(
        new Set(["logs/bot-1/20261021.log"]),
      );
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

      const result = await getLogs("bot-999", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(false);
      expect(result.error).toContain("not found");
    });

    it("should check ownership against an explicit userId", async () => {
      mockMinioClient.statObject.mockRejectedValue({ code: "NoSuchKey" });

      const result = await getLogs(
        "bot-1",
        { date: "20260401", limit: 100, offset: 0 },
        "other-user",
      );

      expect(result.success).toBe(true);
      expect(mockBotService.findOneByUserId).toHaveBeenCalledWith(
        "other-user",
        "bot-1",
      );
    });

    it("should return failure for invalid date format", async () => {
      const result = await getLogs("bot-1", {
        date: "not-a-date",
        limit: 100,
        offset: 0,
      });

      expect(result.success).toBe(false);
      expect(result.error).toContain("Invalid date");
    });
  });

  describe("getLogs - storage failures", () => {
    const failure = () =>
      Object.assign(new Error("access denied"), { code: "AccessDenied" });

    it("should report a failure when tailing a log file fails", async () => {
      const error = failure();
      mockMinioClient.statObject.mockRejectedValue(error);

      const result = await getLogs("bot-1", {
        date: "20260401",
        limit: 100,
        offset: 0,
      });

      expect(result).toEqual(Failure("Failed to fetch logs: access denied"));
      expect(mockLogger.error).toHaveBeenCalledWith(
        { err: error },
        "Error fetching logs",
      );
    });

    it("should report a failure when streaming a log file fails", async () => {
      const error = failure();
      mockMinioClient.getObject.mockRejectedValue(error);

      const result = await getLogs("bot-1", {
        dateRange: "20260401-20260402",
        limit: 100,
        offset: 0,
      });

      expect(result).toEqual(Failure("Failed to fetch logs: access denied"));
      expect(mockLogger.error).toHaveBeenCalledWith(
        { err: error },
        "Error fetching logs",
      );
    });

    it("should report a failure when a latest-mode read fails", async () => {
      const error = failure();
      mockMinioClient.statObject.mockRejectedValue(error);

      const result = await getLogs("bot-1", {
        lookbackDays: 3,
        limit: 100,
        offset: 0,
      });

      expect(result).toEqual(Failure("Failed to fetch logs: access denied"));
      expect(mockLogger.error).toHaveBeenCalledWith(
        { err: error },
        "Error fetching latest logs",
      );
    });
  });
});
