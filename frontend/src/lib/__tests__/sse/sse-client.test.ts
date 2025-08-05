/**
 * SSE Client Tests
 *
 * Basic validation tests for the SSE client functionality
 */

import { SSEClient, SSEMessage } from "../../sse/sse-client";

// Define EventSource constants first
const EventSourceStates = {
  CONNECTING: 0,
  OPEN: 1,
  CLOSED: 2,
};

// Mock EventSource
const mockEventSource = {
  onopen: null as ((event: Event) => void) | null,
  onmessage: null as ((event: MessageEvent) => void) | null,
  onerror: null as ((event: Event) => void) | null,
  close: jest.fn(),
  readyState: EventSourceStates.OPEN,
};

// Mock global EventSource
global.EventSource = jest.fn(() => mockEventSource) as any;
global.EventSource.CONNECTING = EventSourceStates.CONNECTING;
global.EventSource.OPEN = EventSourceStates.OPEN;
global.EventSource.CLOSED = EventSourceStates.CLOSED;

describe("SSEClient", () => {
  let sseClient: SSEClient;

  beforeEach(() => {
    jest.clearAllMocks();
    sseClient = new SSEClient();
    mockEventSource.readyState = EventSourceStates.OPEN;
  });

  afterEach(() => {
    sseClient.disconnectAll();
  });

  describe("connect", () => {
    it("should create EventSource connection", () => {
      const onMessage = jest.fn();
      const testUrl = "/test-stream";

      sseClient.connect(testUrl, onMessage);

      expect(global.EventSource).toHaveBeenCalledWith(testUrl, {
        withCredentials: true,
      });
    });

    it("should handle message parsing correctly", () => {
      const onMessage = jest.fn();
      const testUrl = "/test-stream";
      const testData = { id: "1", name: "test" };

      sseClient.connect(testUrl, onMessage);

      // Simulate SSE message
      const sseMessage: SSEMessage = {
        type: "update",
        data: testData,
        timestamp: new Date().toISOString(),
      };

      const messageEvent = {
        data: JSON.stringify(sseMessage),
      } as MessageEvent;

      if (mockEventSource.onmessage) {
        mockEventSource.onmessage(messageEvent);
      }

      expect(onMessage).toHaveBeenCalledWith(testData);
    });

    it("should handle keepalive messages without calling onMessage", () => {
      const onMessage = jest.fn();
      const testUrl = "/test-stream";

      sseClient.connect(testUrl, onMessage);

      // Simulate keepalive message
      const keepaliveMessage: SSEMessage = {
        type: "keepalive",
        timestamp: new Date().toISOString(),
      };

      const messageEvent = {
        data: JSON.stringify(keepaliveMessage),
      } as MessageEvent;

      if (mockEventSource.onmessage) {
        mockEventSource.onmessage(messageEvent);
      }

      expect(onMessage).not.toHaveBeenCalled();
    });

    it("should handle connection errors", () => {
      const onMessage = jest.fn();
      const onError = jest.fn();
      const testUrl = "/test-stream";

      sseClient.connect(testUrl, onMessage, { onError });

      // Simulate connection error
      const errorEvent = new Event("error");
      if (mockEventSource.onerror) {
        mockEventSource.onerror(errorEvent);
      }

      expect(onError).toHaveBeenCalledWith(errorEvent);
    });

    it("should return cleanup function", () => {
      const onMessage = jest.fn();
      const testUrl = "/test-stream";

      const cleanup = sseClient.connect(testUrl, onMessage);

      expect(typeof cleanup).toBe("function");

      // Verify cleanup works
      cleanup();
      expect(mockEventSource.close).toHaveBeenCalled();
    });
  });

  describe("disconnect", () => {
    it("should close EventSource and clean up", () => {
      const onMessage = jest.fn();
      const testUrl = "/test-stream";

      sseClient.connect(testUrl, onMessage);
      sseClient.disconnect(testUrl);

      expect(mockEventSource.close).toHaveBeenCalled();
    });
  });

  describe("disconnectAll", () => {
    it("should close all connections", () => {
      const onMessage = jest.fn();

      sseClient.connect("/test-stream-1", onMessage);
      sseClient.connect("/test-stream-2", onMessage);

      sseClient.disconnectAll();

      // Should be called at least twice (once for each connection)
      expect(mockEventSource.close).toHaveBeenCalledTimes(2);
    });
  });

  describe("isConnected", () => {
    it("should return true for connected EventSource", () => {
      const onMessage = jest.fn();
      const testUrl = "/test-stream";

      sseClient.connect(testUrl, onMessage);

      expect(sseClient.isConnected(testUrl)).toBe(true);
    });

    it("should return false for disconnected EventSource", () => {
      const testUrl = "/test-stream";

      expect(sseClient.isConnected(testUrl)).toBe(false);
    });
  });

  describe("getConnectionState", () => {
    it("should return connection state information", () => {
      const onMessage = jest.fn();
      const testUrl = "/test-stream";

      sseClient.connect(testUrl, onMessage);

      const state = sseClient.getConnectionState(testUrl);

      expect(state).toEqual({
        connected: true,
        lastUpdate: null,
        error: null,
        retryCount: 0,
      });
    });
  });
});

console.log(
  "SSE Client tests created successfully. Run with: yarn test src/lib/__tests__/sse/sse-client.test.ts",
);
