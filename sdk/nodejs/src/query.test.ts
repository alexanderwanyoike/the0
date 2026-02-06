import { describe, it, expect, beforeEach, afterEach, vi } from "vitest";
import * as query from "./query";
import * as state from "./state";

// Save original env
const originalEnv = { ...process.env };

describe("query module", () => {
  beforeEach(() => {
    // Clear handlers before each test
    // @ts-expect-error - accessing private for testing
    query.handlers?.clear?.() ||
      // eslint-disable-next-line @typescript-eslint/no-explicit-any
      ((query as any).handlers = new Map());

    // Reset environment
    process.env = { ...originalEnv };
  });

  afterEach(() => {
    process.env = { ...originalEnv };
    vi.restoreAllMocks();
  });

  describe("handler registration", () => {
    it("should register a handler", () => {
      const testHandler = vi.fn().mockReturnValue({ test: true });
      query.handler("/test", testHandler);

      // Verify handler is accessible (via internal map)
      // We'll test execution in integration tests
      expect(testHandler).not.toHaveBeenCalled();
    });

    it("should register multiple handlers", () => {
      const handler1 = vi.fn().mockReturnValue({ id: 1 });
      const handler2 = vi.fn().mockReturnValue({ id: 2 });

      query.handler("/one", handler1);
      query.handler("/two", handler2);

      // Handlers should not be called during registration
      expect(handler1).not.toHaveBeenCalled();
      expect(handler2).not.toHaveBeenCalled();
    });
  });

  describe("getParams", () => {
    it("should return empty object by default", () => {
      const params = query.getParams();
      expect(params).toEqual({});
    });

    it("should return a copy of params", () => {
      const params1 = query.getParams();
      params1.newKey = "value";

      const params2 = query.getParams();
      expect(params2).not.toHaveProperty("newKey");
    });
  });

  describe("getConfig", () => {
    it("should return empty object by default", () => {
      const config = query.getConfig();
      expect(config).toEqual({});
    });

    it("should return a copy of config", () => {
      const config1 = query.getConfig();
      config1.newKey = "value";

      const config2 = query.getConfig();
      expect(config2).not.toHaveProperty("newKey");
    });
  });

  describe("ReadOnlyStateError", () => {
    it("should create error with message", () => {
      const error = new query.ReadOnlyStateError("Test message");
      expect(error.message).toBe("Test message");
      expect(error.name).toBe("ReadOnlyStateError");
    });

    it("should be an instance of Error", () => {
      const error = new query.ReadOnlyStateError("Test");
      expect(error).toBeInstanceOf(Error);
    });
  });

  describe("isQueryMode", () => {
    it("should return false when QUERY_PATH is not set", () => {
      delete process.env.QUERY_PATH;
      expect(query.isQueryMode()).toBe(false);
    });

    it("should return true when QUERY_PATH is set", () => {
      process.env.QUERY_PATH = "/test";
      expect(query.isQueryMode()).toBe(true);
    });
  });
});

describe("state read-only enforcement", () => {
  beforeEach(() => {
    process.env = { ...originalEnv };
  });

  afterEach(() => {
    process.env = { ...originalEnv };
  });

  it("should throw ReadOnlyStateError when calling set in query mode", () => {
    process.env.QUERY_PATH = "/test";

    expect(() => state.set("key", "value")).toThrow(
      state.ReadOnlyStateError
    );
  });

  it("should throw ReadOnlyStateError when calling delete in query mode", () => {
    process.env.QUERY_PATH = "/test";

    expect(() => state.remove("key")).toThrow(state.ReadOnlyStateError);
  });

  it("should throw ReadOnlyStateError when calling clear in query mode", () => {
    process.env.QUERY_PATH = "/test";

    expect(() => state.clear()).toThrow(state.ReadOnlyStateError);
  });

  it("should allow get in query mode", () => {
    process.env.QUERY_PATH = "/test";

    // This should not throw
    const result = state.get("nonexistent", "default");
    expect(result).toBe("default");
  });

  it("should allow list in query mode", () => {
    process.env.QUERY_PATH = "/test";

    // This should not throw
    const keys = state.list();
    expect(Array.isArray(keys)).toBe(true);
  });

  it("should allow exists in query mode", () => {
    process.env.QUERY_PATH = "/test";

    // This should not throw
    const result = state.exists("nonexistent");
    expect(result).toBe(false);
  });
});

describe("QueryRequest interface", () => {
  it("should have correct structure", () => {
    // Type checking - this is compile-time verification
    const req: query.QueryRequest = {
      path: "/test",
      params: { key: "value" },
      get: (key: string, defaultValue?: string) =>
        ({ key: "value" }[key] ?? defaultValue),
    };

    expect(req.path).toBe("/test");
    expect(req.params).toEqual({ key: "value" });
    expect(req.get("key")).toBe("value");
    expect(req.get("missing", "default")).toBe("default");
  });
});

describe("QueryHandler type", () => {
  it("should accept sync handlers", () => {
    const syncHandler: query.QueryHandler = (req) => {
      return { value: req.get("key") };
    };

    const mockReq: query.QueryRequest = {
      path: "/test",
      params: { key: "value" },
      get: (k) => ({ key: "value" }[k]),
    };

    const result = syncHandler(mockReq);
    expect(result).toEqual({ value: "value" });
  });

  it("should accept async handlers", async () => {
    const asyncHandler: query.QueryHandler = async (req) => {
      await new Promise((resolve) => setTimeout(resolve, 1));
      return { value: req.get("key") };
    };

    const mockReq: query.QueryRequest = {
      path: "/test",
      params: { key: "async-value" },
      get: (k) => ({ key: "async-value" }[k]),
    };

    const result = await asyncHandler(mockReq);
    expect(result).toEqual({ value: "async-value" });
  });
});
