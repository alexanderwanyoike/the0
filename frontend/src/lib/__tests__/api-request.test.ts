import { apiRequest, setAuthFetchService } from "../auth-fetch";

const authService = { getToken: jest.fn(), logout: jest.fn() };
const logout = jest.fn();
let fetchMock: jest.SpyInstance;

beforeEach(() => {
  authService.getToken.mockReturnValue("jwt");
  setAuthFetchService(authService, logout);
  fetchMock = jest.spyOn(globalThis, "fetch");
});

afterEach(() => {
  jest.restoreAllMocks();
  jest.useRealTimers();
});

function json(data: unknown, status = 200) {
  return new Response(JSON.stringify(data), {
    status,
    headers: { "content-type": "application/json" },
  });
}

function sent(): { url: string; init: RequestInit; headers: Headers } {
  const [url, init] = fetchMock.mock.calls[0];
  return { url, init, headers: new Headers(init.headers) };
}

describe("requests", () => {
  it("sends a GET with the caller's token", async () => {
    fetchMock.mockResolvedValueOnce(json([]));

    await apiRequest("/api/bot");

    expect(sent().url).toBe("/api/bot");
    expect(sent().init.method).toBe("GET");
    expect(sent().headers.get("authorization")).toBe("Bearer jwt");
  });

  it("sends a body as JSON", async () => {
    fetchMock.mockResolvedValueOnce(json({ id: "k" }, 201));

    await apiRequest("/api/api-keys", { method: "POST", body: { name: "ci" } });

    expect(sent().init.method).toBe("POST");
    expect(sent().init.body).toBe(JSON.stringify({ name: "ci" }));
    expect(sent().headers.get("content-type")).toBe("application/json");
  });
});

describe("successful responses", () => {
  it("returns the parsed JSON body", async () => {
    fetchMock.mockResolvedValueOnce(json({ id: "k" }, 201));

    const result = await apiRequest<{ id: string }>("/api/api-keys/k");

    expect(result).toEqual({ success: true, data: { id: "k" } });
  });

  it("returns null for an empty body", async () => {
    fetchMock.mockResolvedValueOnce(new Response(null, { status: 204 }));

    const result = await apiRequest("/api/bot/b-1", { method: "DELETE" });

    expect(result).toEqual({ success: true, data: null });
  });
});

describe("failed responses", () => {
  it("returns the api's message and status", async () => {
    fetchMock.mockResolvedValueOnce(
      json(
        { message: "Bot not found", error: "Not Found", statusCode: 404 },
        404,
      ),
    );

    const result = await apiRequest("/api/bot/missing");

    expect(result).toEqual({
      success: false,
      error: { message: "Bot not found", statusCode: 404 },
    });
  });

  it("joins validation messages", async () => {
    fetchMock.mockResolvedValueOnce(
      json(
        {
          message: ["name must be a string", "config is required"],
          statusCode: 400,
        },
        400,
      ),
    );

    const result = await apiRequest("/api/bot", { method: "POST", body: {} });

    expect(result).toEqual({
      success: false,
      error: {
        message: "name must be a string, config is required",
        statusCode: 400,
      },
    });
  });

  it("falls back to the status text when the body has no message", async () => {
    fetchMock.mockResolvedValueOnce(
      new Response("<html>bad gateway</html>", {
        status: 502,
        statusText: "Bad Gateway",
      }),
    );

    const result = await apiRequest("/api/bot");

    expect(result).toEqual({
      success: false,
      error: { message: "Bad Gateway", statusCode: 502 },
    });
  });

  it("logs out and reports 401 when the api rejects the token", async () => {
    fetchMock.mockResolvedValueOnce(
      json({ message: "Unauthorized", statusCode: 401 }, 401),
    );

    const result = await apiRequest("/api/bot");

    expect(logout).toHaveBeenCalled();
    expect(result).toEqual({
      success: false,
      error: { message: "Authentication failed", statusCode: 401 },
    });
  });

  it("reports 401 without calling the api when there is no token", async () => {
    authService.getToken.mockReturnValue(null);

    const result = await apiRequest("/api/bot");

    expect(fetchMock).not.toHaveBeenCalled();
    expect(result).toEqual({
      success: false,
      error: { message: "Not authenticated", statusCode: 401 },
    });
  });
});

describe("when no response arrives", () => {
  it("reports a network failure", async () => {
    fetchMock.mockRejectedValueOnce(new TypeError("Failed to fetch"));

    const result = await apiRequest("/api/bot");

    expect(result).toEqual({
      success: false,
      error: { message: "No response received from server", statusCode: 500 },
    });
  });

  it("gives up after the timeout", async () => {
    jest.useFakeTimers();
    fetchMock.mockImplementationOnce(
      (_url: string, init: RequestInit) =>
        new Promise((_resolve, reject) => {
          init.signal!.addEventListener("abort", () =>
            reject(new DOMException("aborted", "AbortError")),
          );
        }),
    );

    const pending = apiRequest("/api/api-keys", { timeoutMs: 10_000 });
    await jest.advanceTimersByTimeAsync(10_000);

    expect(await pending).toEqual({
      success: false,
      error: { message: "Request timed out", statusCode: 504 },
    });
  });
});
