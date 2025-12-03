import "@testing-library/jest-dom";

// Polyfill for userEvent
import "whatwg-fetch";
import { TextEncoder, TextDecoder } from "util";
global.TextEncoder = TextEncoder;
//@ts-ignore
global.TextDecoder = TextDecoder;

// Mock matchMedia for tests
Object.defineProperty(window, "matchMedia", {
  writable: true,
  value: jest.fn().mockImplementation((query) => ({
    matches: false,
    media: query,
    onchange: null,
    addListener: jest.fn(), // deprecated
    removeListener: jest.fn(), // deprecated
    addEventListener: jest.fn(),
    removeEventListener: jest.fn(),
    dispatchEvent: jest.fn(),
  })),
});

// Setup MSW for API mocking
import { setupServer } from "msw/node";
import { http, HttpResponse } from "msw";

// Mock data for tests
const mockCustomBot = {
  id: "bot-1",
  name: "test-bot",
  userId: "user-1",
  latestVersion: "1.0.0",
  createdAt: new Date().toISOString(),
  updatedAt: new Date().toISOString(),
  versions: [
    {
      id: "version-1",
      version: "1.0.0",
      userId: "user-1",
      createdAt: new Date().toISOString(),
      status: "active",
      config: { name: "test-bot", description: "Test bot" },
      filePath: "/bots/test-bot/1.0.0",
    },
  ],
};

const mockApiKey = {
  id: "key-1",
  userId: "user-1",
  name: "Test API Key",
  key: "the0_test_key_123",
  isActive: true,
  createdAt: new Date().toISOString(),
  updatedAt: new Date().toISOString(),
};

const mockBotLog = {
  id: "log-1",
  botId: "bot-1",
  level: "info",
  message: "Bot executed successfully",
  timestamp: new Date().toISOString(),
};

// API mocking setup
const server = setupServer(
  // User endpoints
  http.get("/api/users/:id", ({ params }) => {
    return HttpResponse.json({
      success: true,
      data: {
        id: params.id,
        email: "test@example.com",
        displayName: "Test User",
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
        isActive: true,
        isEmailVerified: true,
      },
    });
  }),

  http.post("/api/users", () => {
    return HttpResponse.json({
      success: true,
      data: {
        id: "new-user-id",
        email: "test@example.com",
        displayName: "Test User",
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
        isActive: true,
        isEmailVerified: true,
      },
    });
  }),

  http.put("/api/users/:id", () => {
    return HttpResponse.json({
      success: true,
      data: {
        id: "user-id",
        email: "test@example.com",
        displayName: "Updated User",
        createdAt: new Date().toISOString(),
        updatedAt: new Date().toISOString(),
        isActive: true,
        isEmailVerified: true,
      },
    });
  }),

  http.delete("/api/users/:id", () => {
    return HttpResponse.json({ success: true });
  }),

  // Custom bots endpoints
  http.get("/api/custom-bots", () => {
    return HttpResponse.json({
      success: true,
      data: [mockCustomBot],
      message: "Custom bots fetched successfully",
    });
  }),

  http.get("/api/custom-bots/:name/:version", ({ params }) => {
    return HttpResponse.json({
      ...mockCustomBot.versions[0],
      name: params.name,
      version: params.version,
    });
  }),

  http.get("/api/custom-bots/:name/versions", () => {
    return HttpResponse.json(["1.0.0", "0.9.0"]);
  }),

  // API keys endpoints
  http.get("/api/api-keys", () => {
    return HttpResponse.json([mockApiKey]);
  }),

  http.get("/api/api-keys/stats/summary", () => {
    return HttpResponse.json({ total: 1, active: 1 });
  }),

  http.get("/api/api-keys/:id", ({ params }) => {
    return HttpResponse.json({ ...mockApiKey, id: params.id });
  }),

  http.post("/api/api-keys", async ({ request }) => {
    const body = (await request.json()) as { name: string };
    return HttpResponse.json({
      ...mockApiKey,
      id: "new-key-id",
      name: body.name,
    });
  }),

  http.delete("/api/api-keys/:id", () => {
    return HttpResponse.json({ message: "API key deleted successfully" });
  }),

  // User bots endpoints
  http.get("/api/user-bots", () => {
    return HttpResponse.json([
      {
        id: "user-bot-1",
        botName: "test-bot",
        installedAt: new Date().toISOString(),
      },
    ]);
  }),

  // Bot logs endpoints
  http.get("/api/logs/:botId", ({ request }) => {
    const url = new URL(request.url);
    const page = parseInt(url.searchParams.get("page") || "1");
    const limit = parseInt(url.searchParams.get("limit") || "20");
    return HttpResponse.json({
      logs: [mockBotLog],
      total: 1,
      page,
      limit,
      hasMore: false,
    });
  }),
);

// Export server for use in tests that need custom handlers
export { server };

beforeAll(() => server.listen());
afterEach(() => server.resetHandlers());
afterAll(() => server.close());

// Mock Next.js router
jest.mock("next/navigation", () => ({
  useRouter: () => ({
    push: jest.fn(),
    replace: jest.fn(),
    refresh: jest.fn(),
    back: jest.fn(),
    forward: jest.fn(),
    prefetch: jest.fn(),
  }),
  usePathname: () => "/test-path",
  useSearchParams: () => new URLSearchParams(),
  useParams: () => ({}),
}));

// Mock Next.js Image component
jest.mock("next/image", () => ({
  __esModule: true,
  default: function Image(props: any) {
    return props;
  },
}));

// Mock environment variables commonly used in tests
process.env.NEXT_PUBLIC_API_URL = "http://localhost:3001";
process.env.JWT_SECRET = "test-jwt-secret-key-for-testing-only";

// Mock console methods to reduce noise in tests
const originalError = console.error;
const originalWarn = console.warn;

beforeAll(() => {
  console.error = jest.fn((...args) => {
    // Only show errors that aren't React act() warnings
    if (
      typeof args[0] === "string" &&
      !args[0].includes("Warning: An update to") &&
      !args[0].includes("act()")
    ) {
      originalError(...args);
    }
  });
  console.warn = jest.fn((...args) => {
    // Filter out specific warnings if needed
    originalWarn(...args);
  });
});

afterAll(() => {
  console.error = originalError;
  console.warn = originalWarn;
});
