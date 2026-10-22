import { LogStreamService, ACCESS_DENIED_MESSAGE } from "../log-stream.service";
import { LogFanOut } from "../log-fan-out";
import { LogsService } from "../logs.service";
import { NatsService } from "@/nats/nats.service";
import { PinoLogger } from "nestjs-pino";
import { Ok, Failure } from "@/common/result";
import { Readable } from "stream";

describe("LogStreamService", () => {
  let service: LogStreamService;
  let mockLogsService: Partial<LogsService>;
  let mockNatsService: Partial<NatsService>;
  let mockLogger: Partial<PinoLogger>;
  let mockUnsubscribe: jest.Mock;
  const userId = "user123";

  beforeEach(() => {
    mockUnsubscribe = jest.fn();

    mockLogsService = {
      getLogs: jest.fn().mockResolvedValue(
        Ok({
          entries: [{ date: "20260210", content: "test log line" }],
          hasMore: false,
        }),
      ),
    };

    mockNatsService = {
      subscribe: jest.fn().mockReturnValue(Ok(mockUnsubscribe)),
    };

    mockLogger = {
      info: jest.fn(),
      warn: jest.fn(),
      error: jest.fn(),
      debug: jest.fn(),
    };

    service = new LogStreamService(
      mockLogsService as LogsService,
      new LogFanOut(mockNatsService as NatsService, mockLogger as PinoLogger),
    );
  });

  describe("stream", () => {
    let mockReq: any;
    let mockRes: any;
    let closeHandler: (() => void) | null;

    beforeEach(() => {
      closeHandler = null;
      mockReq = {
        on: jest.fn((event: string, handler: () => void) => {
          if (event === "close") closeHandler = handler;
        }),
      };
      mockRes = {
        status: jest.fn(),
        setHeader: jest.fn(),
        flushHeaders: jest.fn(),
        write: jest.fn(),
        end: jest.fn(),
        writableEnded: false,
      };
    });

    afterEach(() => {
      // Trigger cleanup to clear keepalive intervals
      if (closeHandler) closeHandler();
    });

    it("should set SSE headers", async () => {
      await service.stream("bot-headers", userId, mockReq, mockRes);

      expect(mockRes.setHeader).toHaveBeenCalledWith(
        "Content-Type",
        "text/event-stream",
      );
      expect(mockRes.setHeader).toHaveBeenCalledWith(
        "Cache-Control",
        "no-cache",
      );
      expect(mockRes.setHeader).toHaveBeenCalledWith(
        "Connection",
        "keep-alive",
      );
      expect(mockRes.flushHeaders).toHaveBeenCalled();
    });

    it("should send history event on connect", async () => {
      await service.stream("bot-history", userId, mockReq, mockRes);

      expect(mockLogsService.getLogs).toHaveBeenCalledWith(
        "bot-history",
        {
          date: expect.any(String),
          limit: 1000,
          offset: 0,
        },
        "user123",
      );

      const historyWrite = mockRes.write.mock.calls.find((call: string[]) =>
        call[0].startsWith("event: history"),
      );
      expect(historyWrite).toBeDefined();
    });

    it("should subscribe to NATS for live updates", async () => {
      await service.stream("bot-nats", userId, mockReq, mockRes);

      expect(mockNatsService.subscribe).toHaveBeenCalledWith(
        "the0.bot.logs.bot-nats",
        expect.any(Function),
      );
    });

    it("should forward NATS messages as update events", async () => {
      await service.stream("bot-forward", userId, mockReq, mockRes);

      const natsCallback = (mockNatsService.subscribe as jest.Mock).mock
        .calls[0][1];
      natsCallback("new log line");

      const updateWrite = mockRes.write.mock.calls.find((call: string[]) =>
        call[0].startsWith("event: update"),
      );
      expect(updateWrite).toBeDefined();
      expect(updateWrite[0]).toContain("new log line");
    });

    it("should register close handler for cleanup", async () => {
      await service.stream("bot-close", userId, mockReq, mockRes);

      expect(mockReq.on).toHaveBeenCalledWith("close", expect.any(Function));
    });

    it("should unsubscribe from NATS on client disconnect", async () => {
      await service.stream("bot-unsub", userId, mockReq, mockRes);

      expect(closeHandler).not.toBeNull();
      closeHandler!();
      closeHandler = null; // Prevent afterEach double-call

      expect(mockUnsubscribe).toHaveBeenCalled();
    });

    it("should terminate SSE when bot access is denied", async () => {
      (mockLogsService.getLogs as jest.Mock).mockResolvedValue(
        Failure("Bot not found or access denied"),
      );

      await service.stream("bot-denied", userId, mockReq, mockRes);

      const errorWrite = mockRes.write.mock.calls.find((call: string[]) =>
        call[0].startsWith("event: error"),
      );
      expect(errorWrite).toBeDefined();
      expect(errorWrite[0]).toContain(ACCESS_DENIED_MESSAGE);
      expect(mockRes.end).toHaveBeenCalled();
      expect(mockNatsService.subscribe).not.toHaveBeenCalled();
    });

    it("should terminate SSE when bot is not found", async () => {
      (mockLogsService.getLogs as jest.Mock).mockResolvedValue(
        Failure("Bot not found"),
      );

      await service.stream("bot-missing", userId, mockReq, mockRes);

      const errorWrite = mockRes.write.mock.calls.find((call: string[]) =>
        call[0].startsWith("event: error"),
      );
      expect(errorWrite).toBeDefined();
      expect(errorWrite[0]).toContain("not found");
      expect(mockRes.end).toHaveBeenCalled();
      expect(mockNatsService.subscribe).not.toHaveBeenCalled();
    });

    it("should send warning event when getLogs fails for non-access reasons", async () => {
      (mockLogsService.getLogs as jest.Mock).mockResolvedValue(
        Failure("Failed to fetch logs: MinIO unavailable"),
      );

      await service.stream("bot-minio-fail", userId, mockReq, mockRes);

      const warningWrite = mockRes.write.mock.calls.find((call: string[]) =>
        call[0].startsWith("event: warning"),
      );
      expect(warningWrite).toBeDefined();
      expect(warningWrite[0]).toContain("History unavailable");
      // Should still subscribe to NATS: the user is authorized, just can't fetch history
      expect(mockNatsService.subscribe).toHaveBeenCalled();
    });

    it("should share NATS subscription for same botId", async () => {
      let closeHandler2: (() => void) | null = null;
      const mockReq2: any = {
        on: jest.fn((event: string, handler: () => void) => {
          if (event === "close") closeHandler2 = handler;
        }),
      };
      const mockRes2: any = {
        status: jest.fn(),
        setHeader: jest.fn(),
        flushHeaders: jest.fn(),
        write: jest.fn(),
        end: jest.fn(),
        writableEnded: false,
      };

      await service.stream("bot-shared", userId, mockReq, mockRes);
      await service.stream("bot-shared", userId, mockReq2, mockRes2);

      // NATS subscribe should only be called once for the same botId
      const subscribeCalls = (
        mockNatsService.subscribe as jest.Mock
      ).mock.calls.filter(
        (call: string[]) => call[0] === "the0.bot.logs.bot-shared",
      );
      expect(subscribeCalls.length).toBe(1);

      // But messages should fan-out to both clients
      const natsCallback = subscribeCalls[0][1];
      natsCallback("shared message");

      expect(mockRes.write).toHaveBeenCalledWith(
        expect.stringContaining("shared message"),
      );
      expect(mockRes2.write).toHaveBeenCalledWith(
        expect.stringContaining("shared message"),
      );

      // Cleanup second client
      if (closeHandler2) closeHandler2();
    });

    it("should reject botId with NATS wildcard characters with 400 status", async () => {
      for (const unsafeId of ["bot.with.dots", "bot*wildcard", "bot>arrow"]) {
        mockRes.write.mockClear();
        mockRes.end.mockClear();
        mockRes.status.mockClear();

        await service.stream(unsafeId, userId, mockReq, mockRes);

        expect(mockRes.status).toHaveBeenCalledWith(400);
        const errorWrite = mockRes.write.mock.calls.find((call: string[]) =>
          call[0].startsWith("event: error"),
        );
        expect(errorWrite).toBeDefined();
        expect(errorWrite[0]).toContain("Invalid bot ID");
        expect(mockRes.end).toHaveBeenCalled();
      }

      // Should never reach NATS subscribe for unsafe IDs
      expect(mockNatsService.subscribe).not.toHaveBeenCalled();
    });

    it("should handle concurrent subscriptions for same botId without duplicate NATS subs", async () => {
      let closeHandler2: (() => void) | null = null;
      const mockReq2: any = {
        on: jest.fn((event: string, handler: () => void) => {
          if (event === "close") closeHandler2 = handler;
        }),
      };
      const mockRes2: any = {
        status: jest.fn(),
        setHeader: jest.fn(),
        flushHeaders: jest.fn(),
        write: jest.fn(),
        end: jest.fn(),
        writableEnded: false,
      };

      // Fire both requests concurrently: exercises the pendingSubscriptions lock
      await Promise.all([
        service.stream("bot-concurrent", userId, mockReq, mockRes),
        service.stream("bot-concurrent", userId, mockReq2, mockRes2),
      ]);

      // Should only create one NATS subscription despite concurrent requests
      const subscribeCalls = (
        mockNatsService.subscribe as jest.Mock
      ).mock.calls.filter(
        (call: string[]) => call[0] === "the0.bot.logs.bot-concurrent",
      );
      expect(subscribeCalls.length).toBe(1);

      // Cleanup second client to clear keepalive interval
      if (closeHandler2) closeHandler2();
    });

    it("should handle NATS subscribe failure gracefully", async () => {
      (mockNatsService.subscribe as jest.Mock).mockReturnValue(
        Failure("NATS connection failed"),
      );

      await service.stream("bot-nats-fail", userId, mockReq, mockRes);

      // Should not throw: client still gets SSE connection
      expect(mockRes.flushHeaders).toHaveBeenCalled();
      expect(mockLogger.error).toHaveBeenCalled();

      // Should send warning to client
      const warningWrite = mockRes.write.mock.calls.find((call: string[]) =>
        call[0].includes("Live streaming unavailable"),
      );
      expect(warningWrite).toBeDefined();

      // Keepalive should still be registered
      expect(mockReq.on).toHaveBeenCalledWith("close", expect.any(Function));

      // Dead subscription should NOT be stored: next client retries
      (mockNatsService.subscribe as jest.Mock).mockReturnValue(
        Ok(mockUnsubscribe),
      );
      let closeHandler2: (() => void) | null = null;
      const mockReq2: any = {
        on: jest.fn((event: string, handler: () => void) => {
          if (event === "close") closeHandler2 = handler;
        }),
      };
      const mockRes2: any = {
        status: jest.fn(),
        setHeader: jest.fn(),
        flushHeaders: jest.fn(),
        write: jest.fn(),
        end: jest.fn(),
        writableEnded: false,
      };
      await service.stream("bot-nats-fail", userId, mockReq2, mockRes2);

      // Should retry subscribe since the dead one wasn't stored
      expect(mockNatsService.subscribe).toHaveBeenCalledTimes(2);

      // Cleanup second client to clear keepalive interval
      if (closeHandler2) closeHandler2();
    });

    describe("UTC day files", () => {
      afterEach(() => {
        jest.useRealTimers();
      });

      it("should read today's history from the UTC day file", async () => {
        // Tests run in Pacific/Kiritimati (UTC+14), where this is Oct 23
        jest.useFakeTimers({
          now: new Date("2026-10-22T12:00:00Z"),
          doNotFake: ["nextTick", "setImmediate", "queueMicrotask"],
        });

        await service.stream("bot-utc", userId, mockReq, mockRes);

        expect(mockLogsService.getLogs).toHaveBeenCalledWith(
          "bot-utc",
          expect.objectContaining({ date: "20261022" }),
          userId,
        );
      });
    });

    describe("history opt-out", () => {
      const events = (): string[] =>
        mockRes.write.mock.calls.map((call: string[]) => call[0]);

      /** A stream service over a real LogsService, so ownership is real. */
      const withOwner = (ownerId: string, request: any = { user: null }) => {
        const botService = {
          findOneByUserId: jest.fn(async (uid: string) =>
            uid === ownerId ? Ok({ id: "bot-1" }) : Failure("Bot not found"),
          ),
        };
        const logsService = new LogsService(
          request,
          { getObject: jest.fn(), statObject: jest.fn() } as any,
          { get: jest.fn().mockReturnValue("bot-logs") } as any,
          botService as any,
          mockLogger as PinoLogger,
        );
        const streamService = new LogStreamService(
          logsService,
          new LogFanOut(
            mockNatsService as NatsService,
            mockLogger as PinoLogger,
          ),
        );
        return { streamService, botService };
      };

      it("should skip the history read when history=false", async () => {
        const { streamService } = withOwner(userId);

        await streamService.stream("bot-1", userId, mockReq, mockRes, {
          history: false,
        });

        expect(events().some((e) => e.startsWith("event: history"))).toBe(
          false,
        );
        expect(mockRes.end).not.toHaveBeenCalled();
        expect(mockNatsService.subscribe).toHaveBeenCalled();
      });

      it("should still reject a user who does not own the bot when history=false", async () => {
        const { streamService, botService } = withOwner("someone-else");

        await streamService.stream("bot-1", userId, mockReq, mockRes, {
          history: false,
        });

        expect(botService.findOneByUserId).toHaveBeenCalledWith(
          userId,
          "bot-1",
        );
        const error = events().find((e) => e.startsWith("event: error"));
        expect(error).toContain(ACCESS_DENIED_MESSAGE);
        expect(mockRes.end).toHaveBeenCalled();
        expect(mockNatsService.subscribe).not.toHaveBeenCalled();
      });

      it("should still reject a stream without a user when history=false", async () => {
        const { streamService } = withOwner(userId);

        await streamService.stream("bot-1", "", mockReq, mockRes, {
          history: false,
        });

        const error = events().find((e) => e.startsWith("event: error"));
        expect(error).toContain("Authentication required");
        expect(mockNatsService.subscribe).not.toHaveBeenCalled();
      });
    });
  });

  describe("live entries", () => {
    let mockReq: any;
    let mockRes: any;
    let closeHandler: (() => void) | null;

    const runtimeLine = (index: number, message: string) =>
      `{"_key":"1792713599999000000-${index}","message":"${message}","timestamp":"2026-10-22T23:59:59.999Z"}`;

    const updates = (): unknown[] =>
      mockRes.write.mock.calls
        .map((call: string[]) => call[0])
        .filter((event: string) => event.startsWith("event: update"))
        .map((event: string) =>
          JSON.parse(event.slice(event.indexOf("data: ") + 6)),
        );

    const publish = async (botId: string, payload: string) => {
      await service.stream(botId, userId, mockReq, mockRes);
      const natsCallback = (mockNatsService.subscribe as jest.Mock).mock
        .calls[0][1];
      natsCallback(payload);
    };

    beforeEach(() => {
      closeHandler = null;
      mockReq = {
        on: jest.fn((event: string, handler: () => void) => {
          if (event === "close") closeHandler = handler;
        }),
      };
      mockRes = {
        status: jest.fn(),
        setHeader: jest.fn(),
        flushHeaders: jest.fn(),
        write: jest.fn(),
        end: jest.fn(),
        writableEnded: false,
      };
    });

    afterEach(() => {
      if (closeHandler) closeHandler();
    });

    it("should send a chunk as one update event with an entry per line", async () => {
      const first = runtimeLine(0, "Bought 1 BTC");
      const second = runtimeLine(1, "Sold 1 BTC");

      await publish("bot-live", `${first}\n${second}\n`);

      expect(updates()).toEqual([
        [
          {
            key: "1792713599999000000-0",
            date: "20261022",
            timestamp: "2026-10-22T23:59:59.999Z",
            content: first,
          },
          {
            key: "1792713599999000000-1",
            date: "20261022",
            timestamp: "2026-10-22T23:59:59.999Z",
            content: second,
          },
        ],
      ]);
    });

    it("should send a live line as the same entry REST returns for the stored line", async () => {
      const line = runtimeLine(0, "Bought 1 BTC");
      const minio = {
        statObject: jest.fn().mockResolvedValue({ size: line.length + 1 }),
        getObject: jest.fn().mockResolvedValue(Readable.from([`${line}\n`])),
        getPartialObject: jest.fn(),
      };
      const logsService = new LogsService(
        { user: { uid: userId } } as any,
        minio as any,
        { get: jest.fn().mockReturnValue("bot-logs") } as any,
        {
          findOneByUserId: jest.fn().mockResolvedValue(Ok({ id: "bot-live" })),
        } as any,
        mockLogger as PinoLogger,
      );
      const stored = await logsService.getLogs(
        "bot-live",
        { date: "20261022", limit: 10, offset: 0 },
        userId,
      );

      await publish("bot-live", `${line}\n`);

      expect(stored.success).toBe(true);
      expect(updates()).toEqual([stored.data!.entries]);
    });

    it("should give a live line with a bot-written _key a process-unique key", async () => {
      await publish("bot-raw", '{"_key":"order-1","message":"a"}\n');

      const [entries] = updates() as { key: string }[][];
      expect(entries).toHaveLength(1);
      expect(entries[0].key).toMatch(/^live:/);
    });
    it("should still deliver a raw chunk from a runtime that does not format lines", async () => {
      await publish("bot-raw", "plain text\nsecond line");

      const [entries] = updates() as {
        key: string;
        date: string;
        timestamp: string;
        content: string;
      }[][];
      expect(entries.map((e) => e.content)).toEqual([
        "plain text",
        "second line",
      ]);
      expect(new Set(entries.map((e) => e.key)).size).toBe(2);
      for (const entry of entries) {
        expect(entry.key).toMatch(/^live:/);
        expect(new Date(entry.timestamp).toISOString()).toBe(entry.timestamp);
        expect(entry.date).toBe(entry.timestamp.slice(0, 10).replace(/-/g, ""));
      }
    });
  });
});
