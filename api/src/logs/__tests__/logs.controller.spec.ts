import { LogsController, _clearActiveSubscriptions } from "../logs.controller";
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
    _clearActiveSubscriptions();

    mockUnsubscribe = jest.fn();

    mockLogsService = {
      getLogs: jest.fn().mockResolvedValue(
        Ok([{ date: "20260210", content: "test log line" }]),
      ),
    };

    mockNatsService = {
      subscribe: jest.fn().mockReturnValue(mockUnsubscribe),
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
    _clearActiveSubscriptions();
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
        setHeader: jest.fn(),
        flushHeaders: jest.fn(),
        write: jest.fn(),
        end: jest.fn(),
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

    it("should handle getLogs failure gracefully", async () => {
      (mockLogsService.getLogs as jest.Mock).mockResolvedValue(
        Failure("not found"),
      );

      await controller.streamLogs("bot-fail", mockReq, mockRes);

      const historyWrite = mockRes.write.mock.calls.find((call: string[]) =>
        call[0].startsWith("event: history"),
      );
      expect(historyWrite).toBeUndefined();
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
        setHeader: jest.fn(),
        flushHeaders: jest.fn(),
        write: jest.fn(),
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

    it("should reject botId with NATS wildcard characters", async () => {
      for (const unsafeId of ["bot.with.dots", "bot*wildcard", "bot>arrow"]) {
        mockRes.write.mockClear();
        mockRes.end.mockClear();

        await controller.streamLogs(unsafeId, mockReq, mockRes);

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

    it("should handle NATS subscribe failure gracefully", async () => {
      (mockNatsService.subscribe as jest.Mock).mockImplementation(() => {
        throw new Error("NATS connection failed");
      });

      await controller.streamLogs("bot-nats-fail", mockReq, mockRes);

      // Should not throw — client still gets SSE connection
      expect(mockRes.flushHeaders).toHaveBeenCalled();
      expect(mockLogger.error).toHaveBeenCalled();

      // Keepalive should still be registered
      expect(mockReq.on).toHaveBeenCalledWith("close", expect.any(Function));
    });
  });
});
