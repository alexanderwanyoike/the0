import { NextRequest } from "next/server";

jest.mock("@/lib/middleware/admin-auth", () => ({
  withAdminAuth: jest.fn(
    async (req: NextRequest, handler: (req: NextRequest) => Promise<any>) =>
      handler(req),
  ),
}));

const mockFetch = jest.fn();
global.fetch = mockFetch;

import { GET as listGET, POST as createPOST } from "../route";
import { GET as getByIdGET, DELETE as deleteByIdDELETE } from "../[id]/route";
import { GET as statsGET } from "../stats/summary/route";

const TOKEN = "Bearer test-token";

function makeRequest(
  path: string,
  { method = "GET", body }: { method?: string; body?: string } = {},
) {
  return new NextRequest(`http://localhost:3001${path}`, {
    method,
    headers: { Authorization: TOKEN, "Content-Type": "application/json" },
    body,
  });
}

function jsonResponse(data: unknown, status = 200) {
  return new Response(JSON.stringify(data), {
    status,
    headers: { "content-type": "application/json" },
  });
}

const idParams = (id: string) => ({ params: Promise.resolve({ id }) });

// Next's fetch instrumentation can hand the mock a Request instead of (url, init).
function upstreamRequest(): Request {
  const [input, init] = mockFetch.mock.calls[0];
  return input instanceof Request ? input : new Request(input, init);
}

type Case = {
  name: string;
  upstreamPath: string;
  method: string;
  failureMessage: string;
  call: () => Promise<Response>;
};

const cases: Case[] = [
  {
    name: "GET /api/api-keys",
    upstreamPath: "/api-keys",
    method: "GET",
    failureMessage: "Error fetching API keys",
    call: () => listGET(makeRequest("/api/api-keys")),
  },
  {
    name: "POST /api/api-keys",
    upstreamPath: "/api-keys",
    method: "POST",
    failureMessage: "Error creating API key",
    call: () =>
      createPOST(
        makeRequest("/api/api-keys", {
          method: "POST",
          body: JSON.stringify({ name: "ci" }),
        }),
      ),
  },
  {
    name: "GET /api/api-keys/[id]",
    upstreamPath: "/api-keys/key-1",
    method: "GET",
    failureMessage: "Error fetching API key",
    call: () =>
      getByIdGET(makeRequest("/api/api-keys/key-1"), idParams("key-1")),
  },
  {
    name: "DELETE /api/api-keys/[id]",
    upstreamPath: "/api-keys/key-1",
    method: "DELETE",
    failureMessage: "Error deleting API key",
    call: () =>
      deleteByIdDELETE(
        makeRequest("/api/api-keys/key-1", { method: "DELETE" }),
        idParams("key-1"),
      ),
  },
  {
    name: "GET /api/api-keys/stats/summary",
    upstreamPath: "/api-keys/stats/summary",
    method: "GET",
    failureMessage: "Error fetching API key stats",
    call: () => statsGET(makeRequest("/api/api-keys/stats/summary")),
  },
];

describe.each(cases)(
  "$name",
  ({ upstreamPath, method, failureMessage, call }) => {
    beforeEach(() => {
      jest.clearAllMocks();
      process.env.BOT_API_URL = "http://bot-api:3000";
    });

    it("forwards the method and Authorization header to the bot API", async () => {
      mockFetch.mockResolvedValueOnce(jsonResponse({ ok: true }));

      await call();

      expect(mockFetch).toHaveBeenCalledTimes(1);
      const upstream = upstreamRequest();
      expect(upstream.url).toBe(`http://bot-api:3000${upstreamPath}`);
      expect(upstream.method).toBe(method);
      expect([...upstream.headers.keys()].sort()).toEqual([
        "authorization",
        "content-type",
      ]);
      expect(upstream.headers.get("Authorization")).toBe(TOKEN);
      expect(upstream.headers.get("Content-Type")).toBe("application/json");
    });

    it("answers 200 with the upstream body, whatever the upstream success status", async () => {
      mockFetch.mockResolvedValueOnce(jsonResponse({ id: "key-1" }, 201));

      const response = await call();

      expect(response.status).toBe(200);
      expect(await response.json()).toEqual({ id: "key-1" });
    });

    it("wraps an upstream error body in { error } and keeps its status", async () => {
      mockFetch.mockResolvedValueOnce(
        jsonResponse({ message: "Forbidden", statusCode: 403 }, 403),
      );

      const response = await call();

      expect(response.status).toBe(403);
      expect(await response.json()).toEqual({
        error: { message: "Forbidden", statusCode: 403 },
      });
    });

    it("answers 500 with an error envelope when the upstream error body is not JSON", async () => {
      mockFetch.mockResolvedValueOnce(
        new Response("Bad Gateway", { status: 502 }),
      );

      const response = await call();

      expect(response.status).toBe(500);
      expect(await response.json()).toEqual({
        error: {
          message: failureMessage,
          statusCode: 500,
          error: "Internal Server Error",
        },
      });
    });

    it("answers 500 with an error envelope and logs when the bot API is unreachable", async () => {
      const consoleError = jest
        .spyOn(console, "error")
        .mockImplementation(() => {});
      const failure = new Error("connect ECONNREFUSED");
      mockFetch.mockRejectedValueOnce(failure);

      const response = await call();

      expect(response.status).toBe(500);
      expect(await response.json()).toEqual({
        error: {
          message: failureMessage,
          statusCode: 500,
          error: "Internal Server Error",
        },
      });
      expect(consoleError).toHaveBeenCalledWith(`${failureMessage}:`, failure);
      consoleError.mockRestore();
    });
  },
);

describe("request bodies", () => {
  beforeEach(() => {
    jest.clearAllMocks();
    process.env.BOT_API_URL = "http://bot-api:3000";
  });

  it("forwards the POST body as JSON", async () => {
    mockFetch.mockResolvedValueOnce(jsonResponse({ id: "key-1" }, 201));

    await createPOST(
      makeRequest("/api/api-keys", {
        method: "POST",
        body: JSON.stringify({ name: "ci" }),
      }),
    );

    expect(await upstreamRequest().text()).toBe(JSON.stringify({ name: "ci" }));
  });

  it("answers 500 without calling the bot API when the POST body is not JSON", async () => {
    const response = await createPOST(
      makeRequest("/api/api-keys", { method: "POST", body: "{not json" }),
    );

    expect(mockFetch).not.toHaveBeenCalled();
    expect(response.status).toBe(500);
    expect(await response.json()).toEqual({
      error: {
        message: "Error creating API key",
        statusCode: 500,
        error: "Internal Server Error",
      },
    });
  });

  it.each([
    ["GET /api/api-keys", () => listGET(makeRequest("/api/api-keys"))],
    [
      "DELETE /api/api-keys/[id]",
      () =>
        deleteByIdDELETE(
          makeRequest("/api/api-keys/key-1", { method: "DELETE" }),
          idParams("key-1"),
        ),
    ],
  ])("%s sends no body", async (_name, call) => {
    mockFetch.mockResolvedValueOnce(jsonResponse({ ok: true }));

    await call();

    expect(await upstreamRequest().text()).toBe("");
  });
});
