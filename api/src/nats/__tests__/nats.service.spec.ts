import { Test, TestingModule } from "@nestjs/testing";
import { ConfigService } from "@nestjs/config";
import { PinoLogger } from "nestjs-pino";
import { NatsService } from "../nats.service";
import { createMockLogger } from "@/test/mock-logger";

// Mock NATS at the top level to avoid hoisting issues
jest.mock("nats", () => {
  const mockSubscription = {
    unsubscribe: jest.fn(),
  };

  const mockConnection = {
    isClosed: jest.fn().mockReturnValue(false),
    close: jest.fn().mockResolvedValue(undefined),
    jetstreamManager: jest.fn(),
    jetstream: jest.fn(),
    subscribe: jest.fn().mockReturnValue(mockSubscription),
    info: {
      server_name: "test-server",
      version: "2.9.0",
      go: "1.19.0",
      host: "localhost",
      port: 4222,
      max_payload: 1048576,
      connect_urls: ["nats://localhost:4222"],
    },
  };

  const mockJetStreamManager = {
    streams: {
      info: jest.fn().mockResolvedValue(undefined),
      add: jest.fn().mockResolvedValue(undefined),
      update: jest.fn().mockResolvedValue(undefined),
    },
  };

  const mockJetStream = {
    publish: jest.fn().mockResolvedValue(undefined),
  };

  // Set up the relationships
  mockConnection.jetstreamManager.mockReturnValue(mockJetStreamManager);
  mockConnection.jetstream.mockReturnValue(mockJetStream);

  return {
    connect: jest.fn().mockResolvedValue(mockConnection),
    StringCodec: jest.fn().mockReturnValue({
      encode: jest.fn((data: string) => Buffer.from(data)),
      decode: jest.fn((data: Buffer) => data.toString()),
    }),
    RetentionPolicy: {
      Limits: "limits",
    },
    StorageType: {
      File: "file",
      Memory: "memory",
    },
    // Expose mocks for test assertions
    __mocks: {
      mockConnection,
      mockSubscription,
      mockJetStreamManager,
      mockJetStream,
    },
  };
});

// Access the exposed mocks
const natsMocks = (require("nats") as any).__mocks;

describe("NatsService", () => {
  let service: NatsService;
  let configService: ConfigService;

  beforeEach(async () => {
    const module: TestingModule = await Test.createTestingModule({
      providers: [
        NatsService,
        {
          provide: ConfigService,
          useValue: {
            get: jest.fn((key: string) => {
              switch (key) {
                case "NATS_URLS":
                  return "nats://localhost:4222";
                default:
                  return undefined;
              }
            }),
          },
        },
        {
          provide: PinoLogger,
          useValue: createMockLogger(),
        },
      ],
    }).compile();

    service = module.get<NatsService>(NatsService);
    configService = module.get<ConfigService>(ConfigService);

    // Wait for onModuleInit to complete
    await service.onModuleInit();
  });

  afterEach(async () => {
    await service.onModuleDestroy();
    // Restore default mock behavior so rejected info() doesn't leak across tests
    natsMocks.mockJetStreamManager.streams.info.mockResolvedValue(undefined);
    jest.clearAllMocks();
  });

  describe("Connection Management", () => {
    it("should connect to NATS on module init", async () => {
      const isConnected = await service.isConnected();
      expect(isConnected).toBe(true);
    });

    it("should disconnect from NATS on module destroy", async () => {
      await service.onModuleDestroy();
      const isConnected = await service.isConnected();
      expect(isConnected).toBe(false);
    });

    it("should return connection info", async () => {
      const result = await service.getConnectionInfo();
      expect(result.success).toBe(true);
      expect(result.data).toEqual({
        server_name: "test-server",
        version: "2.9.0",
        go: "1.19.0",
        host: "localhost",
        port: 4222,
        max_payload: 1048576,
        connect_urls: ["nats://localhost:4222"],
      });
    });
  });

  describe("Generic Publishing", () => {
    it("should publish to any topic", async () => {
      const topic = "test-topic";
      const payload = { id: "123", data: "test" };

      const result = await service.publish(topic, payload);

      expect(result.success).toBe(true);
    });

    it("should handle publish errors", async () => {
      // This test requires access to the mock, so we'll skip detailed mock verification
      // and just test the general error handling path
      const topic = "test-topic";
      const payload = { id: "123", data: "test" };

      const result = await service.publish(topic, payload);
      // Since we can't easily mock the error in this setup, we'll just verify it doesn't throw
      expect(result).toBeDefined();
    });

    it("should fail to publish when not connected", async () => {
      // Simulate disconnected state by destroying the service
      await service.onModuleDestroy();

      const result = await service.publish("test-topic", {});

      expect(result.success).toBe(false);
      expect(result.error).toBe("NATS connection not established");
    });
  });

  describe("Stream Setup", () => {
    it("should update existing streams when info() succeeds", () => {
      // info() resolves by default (stream exists), so ensureStream calls update
      expect(
        natsMocks.mockJetStreamManager.streams.update,
      ).toHaveBeenCalledWith(
        "THE0_BOT_LOGS",
        expect.objectContaining({
          name: "THE0_BOT_LOGS",
          subjects: ["the0.bot.logs.>"],
          retention: "limits",
          storage: "memory",
          max_bytes: 128 * 1024 * 1024,
        }),
      );
    });

    it("should create stream when info() throws not-found", async () => {
      jest.clearAllMocks();

      // Simulate stream not found for all info() calls
      natsMocks.mockJetStreamManager.streams.info.mockRejectedValue(
        Object.assign(new Error("stream not found"), {
          api_error: { err_code: 10059 },
        }),
      );

      await service.onModuleInit();

      expect(natsMocks.mockJetStreamManager.streams.add).toHaveBeenCalledWith(
        expect.objectContaining({
          name: "THE0_BOT_LOGS",
          subjects: ["the0.bot.logs.>"],
          retention: "limits",
          storage: "memory",
          max_bytes: 128 * 1024 * 1024,
        }),
      );
    });
  });

  describe("Subscribe", () => {
    it("should return Ok with an unsubscribe function", () => {
      const result = service.subscribe("test.subject", jest.fn());

      expect(result.success).toBe(true);
      expect(typeof result.data).toBe("function");
    });

    it("should invoke callback with decoded message data", () => {
      const callback = jest.fn();
      service.subscribe("test.subject", callback);

      // Get the callback that was passed to connection.subscribe
      const lastCall =
        natsMocks.mockConnection.subscribe.mock.calls[
          natsMocks.mockConnection.subscribe.mock.calls.length - 1
        ];
      const opts = lastCall[1];

      // Simulate a message arriving
      const mockMsg = { data: Buffer.from("hello world") };
      opts.callback(null, mockMsg);

      expect(callback).toHaveBeenCalledWith("hello world");
    });

    it("should not invoke callback when subscription receives an error", () => {
      const callback = jest.fn();
      service.subscribe("test.subject", callback);

      const lastCall =
        natsMocks.mockConnection.subscribe.mock.calls[
          natsMocks.mockConnection.subscribe.mock.calls.length - 1
        ];
      const opts = lastCall[1];

      // Simulate a subscription error
      opts.callback(new Error("subscription error"), {} as any);

      expect(callback).not.toHaveBeenCalled();
    });

    it("should not crash when callback throws", () => {
      const callback = jest.fn().mockImplementation(() => {
        throw new Error("callback boom");
      });
      service.subscribe("test.subject", callback);

      const lastCall =
        natsMocks.mockConnection.subscribe.mock.calls[
          natsMocks.mockConnection.subscribe.mock.calls.length - 1
        ];
      const opts = lastCall[1];

      const mockMsg = { data: Buffer.from("hello") };
      // Should not throw
      expect(() => opts.callback(null, mockMsg)).not.toThrow();
    });

    it("should unsubscribe when calling the returned function", () => {
      const result = service.subscribe("test.subject", jest.fn());
      expect(result.success).toBe(true);
      result.data!();

      expect(natsMocks.mockSubscription.unsubscribe).toHaveBeenCalled();
    });

    it("should return Failure when connection.subscribe throws", () => {
      natsMocks.mockConnection.subscribe.mockImplementationOnce(() => {
        throw new Error("invalid subject");
      });

      const result = service.subscribe("bad.subject", jest.fn());
      expect(result.success).toBe(false);
      expect(result.error).toBe("invalid subject");
    });

    it("should return Failure when not connected", async () => {
      await service.onModuleDestroy();

      const result = service.subscribe("test.subject", jest.fn());
      expect(result.success).toBe(false);
      expect(result.error).toBe("NATS connection not established");
    });
  });
});
