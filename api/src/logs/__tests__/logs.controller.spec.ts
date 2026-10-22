import { LogsController } from "../logs.controller";
import { LogsService } from "../logs.service";
import { LogStreamService } from "../log-stream.service";
import { Ok, Failure } from "@/common/result";
import { AuthenticatedUser } from "@/auth/auth.types";

describe("LogsController", () => {
  let controller: LogsController;
  let mockLogsService: Partial<LogsService>;
  let mockLogStream: Partial<LogStreamService>;
  const mockUser = { uid: "user123" } as AuthenticatedUser;

  beforeEach(() => {
    mockLogsService = {
      getLogs: jest.fn().mockResolvedValue(
        Ok({
          entries: [{ date: "20260210", content: "test log line" }],
          hasMore: false,
        }),
      ),
    };

    mockLogStream = {
      stream: jest.fn().mockResolvedValue(undefined),
    };

    controller = new LogsController(
      mockLogsService as LogsService,
      mockLogStream as LogStreamService,
    );
  });

  describe("getLogs", () => {
    it("should return logs successfully", async () => {
      const mockLogs = [{ date: "20260210", content: "test log" }];
      (mockLogsService.getLogs as jest.Mock).mockResolvedValue(
        Ok({ entries: mockLogs, hasMore: false }),
      );

      const result = await controller.getLogs(
        "bot-123",
        { limit: 100, offset: 0 } as any,
        mockUser,
      );

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockLogs);
      expect(result.hasMore).toBe(false);
      expect(mockLogsService.getLogs).toHaveBeenCalledWith(
        "bot-123",
        {
          date: undefined,
          dateRange: undefined,
          limit: 100,
          offset: 0,
          sort: "desc",
        },
        "user123",
      );
    });

    it("should throw NotFoundException for not found errors", async () => {
      (mockLogsService.getLogs as jest.Mock).mockResolvedValue(
        Failure("Bot not found or access denied"),
      );

      await expect(
        controller.getLogs(
          "bot-123",
          { limit: 100, offset: 0 } as any,
          mockUser,
        ),
      ).rejects.toThrow("Bot not found or access denied");
    });

    it("should throw BadRequestException for other errors", async () => {
      (mockLogsService.getLogs as jest.Mock).mockResolvedValue(
        Failure("Invalid date format"),
      );

      await expect(
        controller.getLogs(
          "bot-123",
          { limit: 100, offset: 0 } as any,
          mockUser,
        ),
      ).rejects.toThrow("Invalid date format");
    });

    it("should reject invalid bot IDs before querying logs", async () => {
      await expect(
        controller.getLogs(
          "../../etc/passwd",
          { limit: 100, offset: 0 } as any,
          mockUser,
        ),
      ).rejects.toThrow("Invalid bot ID");

      expect(mockLogsService.getLogs).not.toHaveBeenCalled();
    });

    it("should pass type parameter to service", async () => {
      const mockLogs = [
        { date: "20260210", content: '{"_metric":true,"value":42}' },
      ];
      (mockLogsService.getLogs as jest.Mock).mockResolvedValue(
        Ok({ entries: mockLogs, hasMore: false }),
      );

      await controller.getLogs(
        "bot-123",
        { limit: 100, offset: 0, type: "metrics" } as any,
        mockUser,
      );

      expect(mockLogsService.getLogs).toHaveBeenCalledWith(
        "bot-123",
        {
          date: undefined,
          dateRange: undefined,
          limit: 100,
          offset: 0,
          type: "metrics",
          sort: "desc",
        },
        "user123",
      );
    });

    it("should pass lookbackDays parameter to service", async () => {
      (mockLogsService.getLogs as jest.Mock).mockResolvedValue(
        Ok({ entries: [], hasMore: false }),
      );

      await controller.getLogs(
        "bot-123",
        { limit: 100, offset: 0, lookbackDays: 30, type: "metrics" } as any,
        mockUser,
      );

      expect(mockLogsService.getLogs).toHaveBeenCalledWith(
        "bot-123",
        expect.objectContaining({ lookbackDays: 30, type: "metrics" }),
        "user123",
      );
    });

    it("should return hasMore=true when entries equal limit", async () => {
      const mockLogs = Array.from({ length: 100 }, (_, i) => ({
        date: "20260210",
        content: `log-${i}`,
      }));
      (mockLogsService.getLogs as jest.Mock).mockResolvedValue(
        Ok({ entries: mockLogs, hasMore: true }),
      );

      const result = await controller.getLogs(
        "bot-123",
        { limit: 100, offset: 0 } as any,
        mockUser,
      );

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockLogs);
      expect(result.hasMore).toBe(true);
    });

    it("should return hasMore=false when entries less than limit", async () => {
      const mockLogs = [{ date: "20260210", content: "only one" }];
      (mockLogsService.getLogs as jest.Mock).mockResolvedValue(
        Ok({ entries: mockLogs, hasMore: false }),
      );

      const result = await controller.getLogs(
        "bot-123",
        { limit: 100, offset: 0 } as any,
        mockUser,
      );

      expect(result.success).toBe(true);
      expect(result.data).toEqual(mockLogs);
      expect(result.hasMore).toBe(false);
    });

    it("should return hasMore=false for metrics queries", async () => {
      const mockLogs = Array.from({ length: 100 }, (_, i) => ({
        date: "20260210",
        content: `{"_metric":true,"value":${i}}`,
      }));
      (mockLogsService.getLogs as jest.Mock).mockResolvedValue(
        Ok({ entries: mockLogs, hasMore: false }),
      );

      const result = await controller.getLogs(
        "bot-123",
        { limit: 100, offset: 0, type: "metrics" } as any,
        mockUser,
      );

      expect(result.success).toBe(true);
      expect(result.hasMore).toBe(false);
    });

    it("should pass type=all parameter to service", async () => {
      const mockLogs = [{ date: "20260210", content: "some log" }];
      (mockLogsService.getLogs as jest.Mock).mockResolvedValue(
        Ok({ entries: mockLogs, hasMore: false }),
      );

      await controller.getLogs(
        "bot-123",
        { limit: 50, offset: 0, type: "all" } as any,
        mockUser,
      );

      expect(mockLogsService.getLogs).toHaveBeenCalledWith(
        "bot-123",
        {
          date: undefined,
          dateRange: undefined,
          limit: 50,
          offset: 0,
          type: "all",
          sort: "desc",
        },
        "user123",
      );
    });
  });

  describe("streamLogs", () => {
    it("should hand the stream to LogStreamService for the current user", async () => {
      const req = {} as any;
      const res = {} as any;

      await controller.streamLogs("bot-123", mockUser, req, res);

      expect(mockLogStream.stream).toHaveBeenCalledWith(
        "bot-123",
        "user123",
        req,
        res,
      );
    });
  });
});
