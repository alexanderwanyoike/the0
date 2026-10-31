import { NextRequest } from "next/server";
import { DELETE, GET, PATCH, POST, PUT } from "../route";

const API = "http://bot-api:3000";

let fetchMock: jest.SpyInstance;

beforeEach(() => {
  process.env.BOT_API_URL = API;
  // Spied after MSW's listen() so the route calls this mock directly.
  fetchMock = jest.spyOn(globalThis, "fetch");
  jest.spyOn(console, "error").mockImplementation(() => {});
});

afterEach(() => {
  jest.restoreAllMocks();
  jest.useRealTimers();
});

function request(
  path: string,
  init: ConstructorParameters<typeof NextRequest>[1] = {},
) {
  return new NextRequest(`http://localhost:3001${path}`, init);
}

function upstream(): { url: string; init: RequestInit } {
  const [input, init] = fetchMock.mock.calls[0];
  return { url: String(input), init: init ?? {} };
}

function upstreamHeaders(): Headers {
  return new Headers(upstream().init.headers);
}

function json(data: unknown, status = 200, headers: HeadersInit = {}) {
  return new Response(JSON.stringify(data), {
    status,
    headers: { "content-type": "application/json", ...headers },
  });
}

function openStream(...chunks: string[]): ReadableStream<Uint8Array> {
  const encoder = new TextEncoder();
  return new ReadableStream<Uint8Array>({
    start(controller) {
      for (const chunk of chunks) controller.enqueue(encoder.encode(chunk));
    },
  });
}

async function readChunk(
  reader: ReadableStreamDefaultReader<Uint8Array>,
): Promise<string> {
  const { value } = await reader.read();
  return new TextDecoder().decode(value);
}

describe("forwarding requests", () => {
  it.each([
    ["GET", GET],
    ["POST", POST],
    ["PUT", PUT],
    ["PATCH", PATCH],
    ["DELETE", DELETE],
  ])("forwards %s with the path after /api", async (method, handler) => {
    fetchMock.mockResolvedValueOnce(json({ ok: true }));

    await handler(request("/api/admin/users/u-1", { method }));

    expect(upstream().url).toBe(`${API}/admin/users/u-1`);
    expect(upstream().init.method).toBe(method);
  });

  it("keeps the path's encoding and the query string unchanged", async () => {
    fetchMock.mockResolvedValueOnce(json({ data: [] }));

    await GET(request("/api/logs/bot%2Fspecial%20chars?date=20261031&limit=5"));

    expect(upstream().url).toBe(
      `${API}/logs/bot%2Fspecial%20chars?date=20261031&limit=5`,
    );
  });

  it("forwards the caller's Authorization and x-api-key headers", async () => {
    fetchMock.mockResolvedValueOnce(json({}));

    await GET(
      request("/api/bot", {
        headers: { Authorization: "Bearer jwt", "x-api-key": "the0_key" },
      }),
    );

    expect(upstreamHeaders().get("authorization")).toBe("Bearer jwt");
    expect(upstreamHeaders().get("x-api-key")).toBe("the0_key");
  });

  it("does not forward the caller's cookies or host", async () => {
    fetchMock.mockResolvedValueOnce(json({}));

    await GET(
      request("/api/bot", {
        headers: { cookie: "auth-token=secret", host: "the0.example" },
      }),
    );

    expect(upstreamHeaders().has("cookie")).toBe(false);
    expect(upstreamHeaders().has("host")).toBe(false);
  });

  it("forwards a JSON body with its content type", async () => {
    fetchMock.mockResolvedValueOnce(json({}, 201));
    const body = JSON.stringify({ name: "ci" });

    await POST(
      request("/api/api-keys", {
        method: "POST",
        body,
        headers: { "content-type": "application/json" },
      }),
    );

    expect(upstreamHeaders().get("content-type")).toBe("application/json");
    expect(new TextDecoder().decode(upstream().init.body as ArrayBuffer)).toBe(
      body,
    );
  });

  it("forwards a multipart body byte for byte", async () => {
    fetchMock.mockResolvedValueOnce(json({}, 201));
    const form = new FormData();
    form.append("file", new Blob([new Uint8Array([0, 159, 255])]), "bot.zip");
    const sent = new Request("http://x", { method: "POST", body: form });
    const contentType = sent.headers.get("content-type")!;
    const bytes = new Uint8Array(await sent.arrayBuffer());

    await POST(
      request("/api/custom-bots/my-bot/upload", {
        method: "POST",
        body: bytes,
        headers: { "content-type": contentType },
      }),
    );

    expect(upstreamHeaders().get("content-type")).toBe(contentType);
    expect(new Uint8Array(upstream().init.body as ArrayBuffer)).toEqual(bytes);
  });

  it("leaves redirects for the browser instead of following them", async () => {
    fetchMock.mockResolvedValueOnce(
      new Response(null, { status: 302, headers: { location: "/elsewhere" } }),
    );

    const response = await GET(request("/api/docs"));

    expect(upstream().init.redirect).toBe("manual");
    expect(response.status).toBe(302);
    expect(response.headers.get("location")).toBe("/elsewhere");
  });
});

describe("forwarding responses", () => {
  it("keeps the api's status and body on success", async () => {
    fetchMock.mockResolvedValueOnce(json({ id: "key-1" }, 201));

    const response = await POST(request("/api/api-keys", { method: "POST" }));

    expect(response.status).toBe(201);
    expect(await response.json()).toEqual({ id: "key-1" });
  });

  it("keeps the api's status and error body unchanged", async () => {
    const error = {
      message: ["name must be a string"],
      error: "Bad Request",
      statusCode: 400,
    };
    fetchMock.mockResolvedValueOnce(json(error, 400));

    const response = await PUT(request("/api/bot/b-1", { method: "PUT" }));

    expect(response.status).toBe(400);
    expect(await response.json()).toEqual(error);
  });

  it("keeps the api's content headers", async () => {
    fetchMock.mockResolvedValueOnce(
      new Response("console.log(1)", {
        headers: {
          "content-type": "application/javascript",
          "cache-control": "public, max-age=60",
          "content-disposition": 'attachment; filename="bundle.js"',
        },
      }),
    );

    const response = await GET(request("/api/custom-bots/x/1.0.0/bundle"));

    expect(response.headers.get("content-type")).toBe("application/javascript");
    expect(response.headers.get("cache-control")).toBe("public, max-age=60");
    expect(response.headers.get("content-disposition")).toBe(
      'attachment; filename="bundle.js"',
    );
    expect(await response.text()).toBe("console.log(1)");
  });

  it("drops encoding headers that no longer describe the body it sends", async () => {
    fetchMock.mockResolvedValueOnce(
      json({ ok: true }, 200, {
        "content-encoding": "gzip",
        "content-length": "999",
      }),
    );

    const response = await GET(request("/api/bot"));

    expect(response.headers.has("content-encoding")).toBe(false);
    expect(response.headers.has("content-length")).toBe(false);
  });

  it("answers 204 with no body", async () => {
    fetchMock.mockResolvedValueOnce(new Response(null, { status: 204 }));

    const response = await DELETE(
      request("/api/bot/b-1", { method: "DELETE" }),
    );

    expect(response.status).toBe(204);
    expect(await response.text()).toBe("");
  });
});

describe("streaming responses", () => {
  const sse = (body: ReadableStream<Uint8Array>) =>
    new Response(body, { headers: { "content-type": "text/event-stream" } });

  it("passes events on as they arrive while the api keeps the stream open", async () => {
    fetchMock.mockResolvedValueOnce(
      sse(openStream("event: update\ndata: [1]\n\n")),
    );

    const response = await GET(request("/api/logs/b-1/stream?history=false"));
    const reader = response.body!.getReader();

    expect(await readChunk(reader)).toBe(": connected\n\n");
    expect(await readChunk(reader)).toBe("event: update\ndata: [1]\n\n");
    await reader.cancel();
  });

  it("opens the stream before the api sends its first event", async () => {
    fetchMock.mockResolvedValueOnce(sse(openStream()));

    const response = await GET(request("/api/logs/b-1/stream"));
    const reader = response.body!.getReader();

    expect(await readChunk(reader)).toBe(": connected\n\n");
    await reader.cancel();
  });

  it("tells compressing and buffering proxies to leave the stream alone", async () => {
    fetchMock.mockResolvedValueOnce(sse(openStream()));

    const response = await GET(request("/api/logs/b-1/stream"));

    expect(response.headers.get("content-type")).toBe("text/event-stream");
    expect(response.headers.get("cache-control")).toBe(
      "no-cache, no-transform",
    );
    expect(response.headers.get("x-accel-buffering")).toBe("no");
    await response.body!.cancel();
  });

  it("cancels the api request when the caller goes away", async () => {
    fetchMock.mockResolvedValueOnce(sse(openStream()));
    const caller = new AbortController();

    const response = await GET(
      request("/api/logs/b-1/stream", { signal: caller.signal }),
    );
    caller.abort();

    expect((upstream().init.signal as AbortSignal).aborted).toBe(true);
    await response.body!.cancel();
  });

  it("does not time out a stream that outlives the response timeout", async () => {
    jest.useFakeTimers();
    fetchMock.mockResolvedValueOnce(sse(openStream()));

    const response = await GET(request("/api/logs/b-1/stream"));
    jest.advanceTimersByTime(10 * 60_000);

    expect((upstream().init.signal as AbortSignal).aborted).toBe(false);
    await response.body!.cancel();
  });
});

describe("when the api cannot answer", () => {
  it("answers 500 when BOT_API_URL is not set", async () => {
    delete process.env.BOT_API_URL;

    const response = await GET(request("/api/bot"));

    expect(response.status).toBe(500);
    expect(await response.json()).toEqual({
      success: false,
      message: "API service misconfigured",
    });
    expect(fetchMock).not.toHaveBeenCalled();
  });

  it("answers 502 when the api is unreachable", async () => {
    fetchMock.mockRejectedValueOnce(new TypeError("fetch failed"));

    const response = await GET(request("/api/bot"));

    expect(response.status).toBe(502);
    expect(await response.json()).toEqual({
      success: false,
      message: "API service unavailable",
    });
  });

  it("answers 504 when the api does not respond in time", async () => {
    jest.useFakeTimers();
    fetchMock.mockImplementationOnce(
      (_input: unknown, init: RequestInit) =>
        new Promise((_resolve, reject) => {
          init.signal!.addEventListener("abort", () =>
            reject(new DOMException("aborted", "AbortError")),
          );
        }),
    );

    const pending = GET(request("/api/logs/b-1?date=20261031"));
    await jest.advanceTimersByTimeAsync(30_000);
    const response = await pending;

    expect(response.status).toBe(504);
    expect(await response.json()).toEqual({
      success: false,
      message: "API service timed out",
    });
  });
});
