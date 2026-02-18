import { LogsController, ACCESS_DENIED_MESSAGE } from "../logs.controller";
import { LogsService } from "../logs.service";
import { NatsService } from "@/nats/nats.service";
import { PinoLogger } from "nestjs-pino";
import { Ok, Failure } from "@/common/result";

describe("LogsController", () => {
  let controller: LogsController;
  let mockLogsService: Partial<LogsService>;
  let mockNatsService: Partial<NatsService>;
  let mockLogger: Partial<PinoLogger>;
  let mockUnsubscribe: jest.Mock;

  beforeEach(() => {
    LogsController._resetForTest();

    mockUnsubscribe = jest.fn();

    mockLogsService = {
      getLogs: jest.fn().mockResolvedValue(
        Ok([{ date: "20260210", content: "test log line" }]),
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

    controller = new LogsController(
      mockLogsService as LogsService,
      mockNatsService as NatsService,
      mockLogger as PinoLogger,
    );
  });

  afterEach(() => {
    LogsController._resetForTest();
  });

  describe("streamLogs", () => {
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
      await controller.streamLogs("bot-headers", mockReq, mockRes);

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
      await controller.streamLogs("bot-history", mockReq, mockRes);

      expect(mockLogsService.getLogs).toHaveBeenCalledWith("bot-history", {
        date: expect.any(String),
        limit: 1000,
        offset: 0,
      });

      const historyWrite = mockRes.write.mock.calls.find((call: string[]) =>
        call[0].startsWith("event: history"),
      );
      expect(historyWrite).toBeDefined();
    });

    it("should subscribe to NATS for live updates", async () => {
      await controller.streamLogs("bot-nats", mockReq, mockRes);

      expect(mockNatsService.subscribe).toHaveBeenCalledWith(
        "the0.bot.logs.bot-nats",
        expect.any(Function),
      );
    });

    it("should forward NATS messages as update events", async () => {
      await controller.streamLogs("bot-forward", mockReq, mockRes);

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
      await controller.streamLogs("bot-close", mockReq, mockRes);

      expect(mockReq.on).toHaveBeenCalledWith("close", expect.any(Function));
    });

    it("should unsubscribe from NATS on client disconnect", async () => {
      await controller.streamLogs("bot-unsub", mockReq, mockRes);

      expect(closeHandler).not.toBeNull();
      closeHandler!();
      closeHandler = null; // Prevent afterEach double-call

      expect(mockUnsubscribe).toHaveBeenCalled();
    });

    it("should terminate SSE when bot access is denied", async () => {
      (mockLogsService.getLogs as jest.Mock).mockResolvedValue(
        Failure("Bot not found or access denied"),
      );

      await controller.streamLogs("bot-denied", mockReq, mockRes);

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

      await controller.streamLogs("bot-missing", mockReq, mockRes);

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

      await controller.streamLogs("bot-minio-fail", mockReq, mockRes);

      const warningWrite = mockRes.write.mock.calls.find((call: string[]) =>
        call[0].startsWith("event: warning"),
      );
      expect(warningWrite).toBeDefined();
      expect(warningWrite[0]).toContain("History unavailable");
      // Should still subscribe to NATS — the user is authorized, just can't fetch history
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

      await controller.streamLogs("bot-shared", mockReq, mockRes);
      await controller.streamLogs("bot-shared", mockReq2, mockRes2);

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

        await controller.streamLogs(unsafeId, mockReq, mockRes);

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

      // Fire both requests concurrently — exercises the pendingSubscriptions lock
      await Promise.all([
        controller.streamLogs("bot-concurrent", mockReq, mockRes),
        controller.streamLogs("bot-concurrent", mockReq2, mockRes2),
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

      await controller.streamLogs("bot-nats-fail", mockReq, mockRes);

      // Should not throw — client still gets SSE connection
      expect(mockRes.flushHeaders).toHaveBeenCalled();
      expect(mockLogger.error).toHaveBeenCalled();

      // Should send warning to client
      const warningWrite = mockRes.write.mock.calls.find((call: string[]) =>
        call[0].includes("Live streaming unavailable"),
      );
      expect(warningWrite).toBeDefined();

      // Keepalive should still be registered
      expect(mockReq.on).toHaveBeenCalledWith("close", expect.any(Function));

      // Dead subscription should NOT be stored — next client retries
      (mockNatsService.subscribe as jest.Mock).mockReturnValue(Ok(mockUnsubscribe));
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
      await controller.streamLogs("bot-nats-fail", mockReq2, mockRes2);

      // Should retry subscribe since the dead one wasn't stored
      expect(mockNatsService.subscribe).toHaveBeenCalledTimes(2);

      // Cleanup second client to clear keepalive interval
      if (closeHandler2) closeHandler2();
    });
  });
});
