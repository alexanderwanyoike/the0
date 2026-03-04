import { NextRequest } from "next/server";

// Mock global fetch
const mockFetch = jest.fn();
global.fetch = mockFetch;

// Suppress console.error in tests
jest.spyOn(console, "error").mockImplementation(() => {});

import { POST as loginPOST } from "../login/route";
import { POST as registerPOST } from "../register/route";
import { POST as validatePOST } from "../validate/route";
import { GET as meGET } from "../me/route";

function getCalledUrl(call: unknown[]): string {
  return typeof call[0] === "string"
    ? call[0]
    : (call[0] as { url?: string })?.url ?? String(call[0]);
}

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

describe("POST /api/auth/register", () => {
  beforeEach(() => {
    jest.clearAllMocks();
    process.env.BOT_API_URL = "http://localhost:3000";
  });

  it("proxies POST body and returns success response", async () => {
    const mockData = { success: true, data: { token: "abc123" } };
    mockFetch.mockResolvedValueOnce(
      new Response(JSON.stringify(mockData), { status: 200 }),
    );

    const req = new NextRequest("http://localhost:3001/api/auth/register", {
      method: "POST",
      body: JSON.stringify({
        username: "testuser",
        email: "test@example.com",
        password: "pass",
      }),
      headers: { "Content-Type": "application/json" },
    });

    const response = await registerPOST(req);
    const body = await response.json();

    expect(response.status).toBe(200);
    expect(body).toEqual(mockData);
    expect(mockFetch).toHaveBeenCalledTimes(1);
    expect(getCalledUrl(mockFetch.mock.calls[0])).toBe(
      "http://localhost:3000/auth/register",
    );
  });

  it("forwards error status from upstream", async () => {
    mockFetch.mockResolvedValueOnce(
      new Response(JSON.stringify({ message: "User already exists" }), {
        status: 400,
      }),
    );

    const req = new NextRequest("http://localhost:3001/api/auth/register", {
      method: "POST",
      body: JSON.stringify({ email: "existing@example.com", password: "pass" }),
      headers: { "Content-Type": "application/json" },
    });

    const response = await registerPOST(req);
    expect(response.status).toBe(400);
  });

  it("returns 500 on network/fetch error", async () => {
    mockFetch.mockRejectedValueOnce(new Error("Network error"));

    const req = new NextRequest("http://localhost:3001/api/auth/register", {
      method: "POST",
      body: JSON.stringify({ email: "test@example.com", password: "pass" }),
      headers: { "Content-Type": "application/json" },
    });

    const response = await registerPOST(req);
    expect(response.status).toBe(500);
    const body = await response.json();
    expect(body.success).toBe(false);
  });

  it("returns 500 when BOT_API_URL is not configured", async () => {
    delete process.env.BOT_API_URL;
    const req = new NextRequest("http://localhost:3001/api/auth/register", {
      method: "POST",
      body: JSON.stringify({
        username: "testuser",
        email: "test@test.com",
        password: "password",
      }),
      headers: { "Content-Type": "application/json" },
    });
    const response = await registerPOST(req);
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
