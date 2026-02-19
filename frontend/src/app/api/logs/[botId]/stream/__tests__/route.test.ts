import { NextRequest } from "next/server";

// Mock withAdminAuth to pass through the handler
jest.mock("@/lib/middleware/admin-auth", () => ({
  withAdminAuth: jest.fn(
    async (req: NextRequest, handler: (req: NextRequest) => Promise<any>) => {
      const token = req.headers.get("Authorization");
      if (!token) {
        const { NextResponse } = await import("next/server");
        return NextResponse.json(
          { error: "Missing authentication token" },
          { status: 401 },
        );
      }
      return handler(req);
    },
  ),
}));

// Mock global fetch
const mockFetch = jest.fn();
global.fetch = mockFetch;

// Suppress console.error in tests
jest.spyOn(console, "error").mockImplementation(() => {});

import { GET } from "../route";

function makeRequest(
  botId: string,
  { token }: { token?: string } = {},
): { req: NextRequest; params: Promise<{ botId: string }> } {
  const headers: Record<string, string> = {};
  if (token) headers["Authorization"] = token;

  const req = new NextRequest("http://localhost:3001/api/logs/" + botId + "/stream", {
    headers,
  });
  const params = Promise.resolve({ botId });
  return { req, params };
}

describe("GET /api/logs/[botId]/stream", () => {
  beforeEach(() => {
    jest.clearAllMocks();
    process.env.BOT_API_URL = "http://localhost:3000";
  });

  it("proxies SSE stream from backend with correct headers", async () => {
    const sseBody = new ReadableStream({
      start(controller) {
        controller.enqueue(new TextEncoder().encode("data: {\"type\":\"log\"}\n\n"));
        controller.close();
      },
    });

    mockFetch.mockResolvedValueOnce(
      new Response(sseBody, { status: 200 }),
    );

    const { req, params } = makeRequest("bot-123", { token: "Bearer test-token" });
    const response = await GET(req, { params });

    expect(response.status).toBe(200);
    expect(response.headers.get("Content-Type")).toBe(
      "text/event-stream; charset=utf-8",
    );
    expect(response.headers.get("Cache-Control")).toBe("no-cache, no-transform");
    expect(response.headers.get("X-Accel-Buffering")).toBe("no");

    // Verify the body is piped through
    const text = await response.text();
    expect(text).toBe("data: {\"type\":\"log\"}\n\n");
  });

  it("returns 401 when no auth token is provided", async () => {
    const { req, params } = makeRequest("bot-123");
    const response = await GET(req, { params });

    expect(response.status).toBe(401);
    expect(mockFetch).not.toHaveBeenCalled();
  });

  it("returns error when upstream fails", async () => {
    mockFetch.mockResolvedValueOnce(
      new Response(null, { status: 503 }),
    );

    const { req, params } = makeRequest("bot-123", { token: "Bearer test-token" });
    const response = await GET(req, { params });

    expect(response.status).toBe(503);
    const body = await response.json();
    expect(body.error).toBe("Error connecting to log stream");
  });

  it("encodes botId in upstream URL", async () => {
    mockFetch.mockResolvedValueOnce(
      new Response(new ReadableStream(), { status: 200 }),
    );

    const { req, params } = makeRequest("bot/special chars", {
      token: "Bearer test-token",
    });
    await GET(req, { params });

    expect(mockFetch).toHaveBeenCalledTimes(1);
    // MSW may transform fetch(url, opts) into fetch(Request), so extract the URL
    // from whichever form was used
    const call = mockFetch.mock.calls[0];
    const calledUrl =
      typeof call[0] === "string" ? call[0] : call[0]?.url ?? String(call[0]);
    expect(calledUrl).toBe(
      "http://localhost:3000/logs/bot%2Fspecial%20chars/stream",
    );
  });

  it("propagates abort signal to upstream fetch", async () => {
    mockFetch.mockResolvedValueOnce(
      new Response(new ReadableStream(), { status: 200 }),
    );

    const { req, params } = makeRequest("bot-123", { token: "Bearer test-token" });
    await GET(req, { params });

    expect(mockFetch).toHaveBeenCalledTimes(1);
    // The signal should be forwarded — check via the Request or options object
    const call = mockFetch.mock.calls[0];
    const passedSignal =
      call[1]?.signal ?? call[0]?.signal;
    expect(passedSignal).toBeDefined();
  });
});
