import { NextRequest } from "next/server";

// Mock global fetch
const mockFetch = jest.fn();
global.fetch = mockFetch;

// Suppress console.error in tests
jest.spyOn(console, "error").mockImplementation(() => {});

import { POST as loginPOST } from "../login/route";
import { POST as validatePOST } from "../validate/route";
import { GET as meGET } from "../me/route";

function getCalledUrl(call: unknown[]): string {
  return typeof call[0] === "string"
    ? call[0]
    : ((call[0] as { url?: string })?.url ?? String(call[0]));
}

function upstreamRequest(): Request {
  const [input, init] = mockFetch.mock.calls[0];
  return input instanceof Request ? input : new Request(input, init);
}

describe.each([
  { path: "/auth/login", handler: () => loginPOST, label: "auth login" },
  {
    path: "/auth/validate",
    handler: () => validatePOST,
    label: "auth validate",
  },
])("POST /api$path request forwarding", ({ path, handler, label }) => {
  beforeEach(() => {
    jest.clearAllMocks();
    process.env.BOT_API_URL = "http://localhost:3000";
  });

  const makeRequest = (body: string) =>
    new NextRequest(`http://localhost:3001/api${path}`, {
      method: "POST",
      body,
      headers: {
        "Content-Type": "application/json",
        Authorization: "Bearer caller-token",
      },
    });

  it("forwards the JSON body but not the caller's Authorization header", async () => {
    mockFetch.mockResolvedValueOnce(
      new Response(JSON.stringify({ success: true }), { status: 200 }),
    );

    await handler()(makeRequest(JSON.stringify({ token: "t" })));

    const upstream = upstreamRequest();
    expect(upstream.method).toBe("POST");
    expect([...upstream.headers.keys()]).toEqual(["content-type"]);
    expect(await upstream.text()).toBe(JSON.stringify({ token: "t" }));
  });

  it("answers 500 'Authentication service unavailable' and logs when the bot API is unreachable", async () => {
    const consoleError = jest
      .spyOn(console, "error")
      .mockImplementation(() => {});
    const failure = new Error("Network error");
    mockFetch.mockRejectedValueOnce(failure);

    const response = await handler()(makeRequest("{}"));

    expect(response.status).toBe(500);
    expect(await response.json()).toEqual({
      success: false,
      message: "Authentication service unavailable",
    });
    expect(consoleError).toHaveBeenCalledWith(
      `Error proxying ${label}:`,
      failure,
    );
    consoleError.mockRestore();
  });

  it("answers 500 without calling the bot API when the body is not JSON", async () => {
    const response = await handler()(makeRequest("{not json"));

    expect(mockFetch).not.toHaveBeenCalled();
    expect(response.status).toBe(500);
    expect(await response.json()).toEqual({
      success: false,
      message: "Authentication service unavailable",
    });
  });

  it("answers 500 when the upstream body is not JSON", async () => {
    mockFetch.mockResolvedValueOnce(
      new Response("Bad Gateway", { status: 502 }),
    );

    const response = await handler()(makeRequest("{}"));

    expect(response.status).toBe(500);
  });

  it("names the authentication service when BOT_API_URL is missing", async () => {
    delete process.env.BOT_API_URL;

    const response = await handler()(makeRequest("{}"));

    expect(await response.json()).toEqual({
      success: false,
      message: "Authentication service misconfigured",
    });
  });
});

describe("POST /api/auth/login", () => {
  beforeEach(() => {
    jest.clearAllMocks();
    process.env.BOT_API_URL = "http://localhost:3000";
  });

  it("proxies POST body and returns success response", async () => {
    const mockData = { success: true, data: { token: "abc123" } };
    mockFetch.mockResolvedValueOnce(
      new Response(JSON.stringify(mockData), { status: 200 }),
    );

    const req = new NextRequest("http://localhost:3001/api/auth/login", {
      method: "POST",
      body: JSON.stringify({ email: "test@example.com", password: "pass" }),
      headers: { "Content-Type": "application/json" },
    });

    const response = await loginPOST(req);
    const body = await response.json();

    expect(response.status).toBe(200);
    expect(body).toEqual(mockData);
    expect(mockFetch).toHaveBeenCalledTimes(1);
    expect(getCalledUrl(mockFetch.mock.calls[0])).toBe(
      "http://localhost:3000/auth/login",
    );
  });

  it("forwards error status from upstream", async () => {
    mockFetch.mockResolvedValueOnce(
      new Response(JSON.stringify({ message: "Invalid credentials" }), {
        status: 401,
      }),
    );

    const req = new NextRequest("http://localhost:3001/api/auth/login", {
      method: "POST",
      body: JSON.stringify({ email: "test@example.com", password: "wrong" }),
      headers: { "Content-Type": "application/json" },
    });

    const response = await loginPOST(req);
    expect(response.status).toBe(401);
  });

  it("returns 500 on network/fetch error", async () => {
    mockFetch.mockRejectedValueOnce(new Error("Network error"));

    const req = new NextRequest("http://localhost:3001/api/auth/login", {
      method: "POST",
      body: JSON.stringify({ email: "test@example.com", password: "pass" }),
      headers: { "Content-Type": "application/json" },
    });

    const response = await loginPOST(req);
    expect(response.status).toBe(500);
    const body = await response.json();
    expect(body.success).toBe(false);
  });

  it("returns 500 when BOT_API_URL is not configured", async () => {
    delete process.env.BOT_API_URL;
    const req = new NextRequest("http://localhost:3001/api/auth/login", {
      method: "POST",
      body: JSON.stringify({ email: "test@test.com", password: "password" }),
      headers: { "Content-Type": "application/json" },
    });
    const response = await loginPOST(req);
    expect(response.status).toBe(500);
    const data = await response.json();
    expect(data.message).toContain("misconfigured");
  });
});

describe("POST /api/auth/validate", () => {
  beforeEach(() => {
    jest.clearAllMocks();
    process.env.BOT_API_URL = "http://localhost:3000";
  });

  it("proxies POST body with token and returns success", async () => {
    const mockData = {
      success: true,
      data: { id: "1", username: "testuser" },
    };
    mockFetch.mockResolvedValueOnce(
      new Response(JSON.stringify(mockData), { status: 200 }),
    );

    const req = new NextRequest("http://localhost:3001/api/auth/validate", {
      method: "POST",
      body: JSON.stringify({ token: "valid-token" }),
      headers: { "Content-Type": "application/json" },
    });

    const response = await validatePOST(req);
    const body = await response.json();

    expect(response.status).toBe(200);
    expect(body).toEqual(mockData);
    expect(mockFetch).toHaveBeenCalledTimes(1);
    expect(getCalledUrl(mockFetch.mock.calls[0])).toBe(
      "http://localhost:3000/auth/validate",
    );
  });

  it("forwards error status from upstream", async () => {
    mockFetch.mockResolvedValueOnce(
      new Response(JSON.stringify({ message: "Invalid token" }), {
        status: 401,
      }),
    );

    const req = new NextRequest("http://localhost:3001/api/auth/validate", {
      method: "POST",
      body: JSON.stringify({ token: "invalid-token" }),
      headers: { "Content-Type": "application/json" },
    });

    const response = await validatePOST(req);
    expect(response.status).toBe(401);
  });

  it("returns 500 on network/fetch error", async () => {
    mockFetch.mockRejectedValueOnce(new Error("network"));
    const req = new NextRequest("http://localhost:3001/api/auth/validate", {
      method: "POST",
      body: JSON.stringify({ token: "test-token" }),
      headers: { "Content-Type": "application/json" },
    });
    const response = await validatePOST(req);
    expect(response.status).toBe(500);
  });

  it("returns 500 when BOT_API_URL is not configured", async () => {
    delete process.env.BOT_API_URL;
    const req = new NextRequest("http://localhost:3001/api/auth/validate", {
      method: "POST",
      body: JSON.stringify({ token: "test-token" }),
      headers: { "Content-Type": "application/json" },
    });
    const response = await validatePOST(req);
    expect(response.status).toBe(500);
    const data = await response.json();
    expect(data.message).toContain("misconfigured");
  });
});

describe("GET /api/auth/me", () => {
  beforeEach(() => {
    jest.clearAllMocks();
    process.env.BOT_API_URL = "http://localhost:3000";
  });

  it("forwards Authorization header to upstream", async () => {
    const mockData = {
      success: true,
      data: { id: "1", username: "testuser" },
    };
    mockFetch.mockResolvedValueOnce(
      new Response(JSON.stringify(mockData), { status: 200 }),
    );

    const req = new NextRequest("http://localhost:3001/api/auth/me", {
      method: "GET",
      headers: { Authorization: "Bearer valid-token" },
    });

    const response = await meGET(req);
    const body = await response.json();

    expect(response.status).toBe(200);
    expect(body).toEqual(mockData);
    expect(mockFetch).toHaveBeenCalledTimes(1);
    expect(getCalledUrl(mockFetch.mock.calls[0])).toBe(
      "http://localhost:3000/auth/me",
    );
  });

  it("forwards 401 from upstream", async () => {
    mockFetch.mockResolvedValueOnce(
      new Response(JSON.stringify({ message: "Unauthorized" }), {
        status: 401,
      }),
    );

    const req = new NextRequest("http://localhost:3001/api/auth/me", {
      method: "GET",
      headers: { Authorization: "Bearer invalid-token" },
    });

    const response = await meGET(req);
    expect(response.status).toBe(401);
  });

  it("returns 500 on network error", async () => {
    mockFetch.mockRejectedValueOnce(new Error("Network error"));

    const req = new NextRequest("http://localhost:3001/api/auth/me", {
      method: "GET",
      headers: { Authorization: "Bearer valid-token" },
    });

    const response = await meGET(req);
    expect(response.status).toBe(500);
    const body = await response.json();
    expect(body.success).toBe(false);
  });

  it("returns 500 when BOT_API_URL is not configured", async () => {
    delete process.env.BOT_API_URL;
    const req = new NextRequest("http://localhost:3001/api/auth/me", {
      method: "GET",
      headers: { Authorization: "Bearer valid-token" },
    });
    const response = await meGET(req);
    expect(response.status).toBe(500);
    const data = await response.json();
    expect(data.message).toContain("misconfigured");
  });
});
