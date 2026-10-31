import { NextRequest, NextResponse } from "next/server";

/** Bounds the wait for the api's response head, not the body, so streams can outlive it. */
const RESPONSE_HEAD_TIMEOUT_MS = 30_000;

const REQUEST_HEADERS_NOT_FORWARDED = new Set([
  "connection",
  "keep-alive",
  "proxy-authorization",
  "te",
  "trailer",
  "transfer-encoding",
  "upgrade",
  "host",
  "content-length",
  "accept-encoding",
  "cookie",
]);

// fetch has already decoded the body, so the api's encoding and length no
// longer describe what is sent on.
const RESPONSE_HEADERS_NOT_FORWARDED = new Set([
  "connection",
  "keep-alive",
  "transfer-encoding",
  "content-encoding",
  "content-length",
]);

const SSE_OPENING_COMMENT = new TextEncoder().encode(": connected\n\n");

function proxyError(status: number, message: string) {
  return NextResponse.json({ success: false, message }, { status });
}

/**
 * Forwards a request under /api to the same path on the api and its answer
 * back, unchanged apart from hop-by-hop headers.
 */
export async function forwardToApi(req: NextRequest): Promise<Response> {
  const apiUrl = process.env.BOT_API_URL;
  if (!apiUrl) return proxyError(500, "API service misconfigured");

  const { pathname, search } = new URL(req.url);
  const target = `${apiUrl}${pathname.replace(/^\/api/, "")}${search}`;

  const headers = new Headers();
  req.headers.forEach((value, key) => {
    if (!REQUEST_HEADERS_NOT_FORWARDED.has(key)) headers.set(key, value);
  });

  const hasBody = req.method !== "GET" && req.method !== "HEAD";
  const body = hasBody ? await req.arrayBuffer() : undefined;

  const upstream = new AbortController();
  const abortUpstream = () => upstream.abort();
  req.signal.addEventListener("abort", abortUpstream);
  let timedOut = false;
  const headTimeout = setTimeout(() => {
    timedOut = true;
    upstream.abort();
  }, RESPONSE_HEAD_TIMEOUT_MS);

  let response: Response;
  try {
    response = await fetch(target, {
      method: req.method,
      headers,
      body: body && body.byteLength > 0 ? body : undefined,
      redirect: "manual",
      cache: "no-store",
      signal: upstream.signal,
    });
  } catch (error) {
    req.signal.removeEventListener("abort", abortUpstream);
    if (timedOut) return proxyError(504, "API service timed out");
    console.error(`Error proxying ${req.method} ${pathname}:`, error);
    return proxyError(502, "API service unavailable");
  } finally {
    clearTimeout(headTimeout);
  }

  const responseHeaders = new Headers();
  response.headers.forEach((value, key) => {
    if (!RESPONSE_HEADERS_NOT_FORWARDED.has(key)) {
      responseHeaders.set(key, value);
    }
  });

  const isEventStream = (response.headers.get("content-type") ?? "").startsWith(
    "text/event-stream",
  );
  if (!isEventStream || !response.body) {
    const noBody = response.status === 204 || response.status === 304;
    return new Response(noBody ? null : response.body, {
      status: response.status,
      statusText: response.statusText,
      headers: responseHeaders,
    });
  }

  // no-transform keeps Next's compression from buffering the stream.
  responseHeaders.set("cache-control", "no-cache, no-transform");
  responseHeaders.set("x-accel-buffering", "no");
  return new Response(openingAtOnce(response.body), {
    status: response.status,
    headers: responseHeaders,
  });
}

/**
 * Next sends the response head with the first body chunk. The api may have
 * nothing to send until its first keepalive, so an SSE comment goes first
 * and the client sees the stream open at once.
 */
function openingAtOnce(
  body: ReadableStream<Uint8Array>,
): ReadableStream<Uint8Array> {
  const reader = body.getReader();
  return new ReadableStream<Uint8Array>({
    start(controller) {
      controller.enqueue(SSE_OPENING_COMMENT);
    },
    async pull(controller) {
      const { done, value } = await reader.read();
      if (done) controller.close();
      else controller.enqueue(value);
    },
    cancel(reason) {
      return reader.cancel(reason);
    },
  });
}

export async function proxyBotApi(
  req: NextRequest,
  path: string,
  method: string,
  body?: unknown,
) {
  const botApiUrl = process.env.BOT_API_URL;
  if (!botApiUrl) {
    return NextResponse.json(
      { success: false, message: "API service misconfigured" },
      { status: 500 },
    );
  }

  const authHeader = req.headers.get("Authorization");
  const headers: Record<string, string> = {
    "Content-Type": "application/json",
  };
  if (authHeader) {
    headers.Authorization = authHeader;
  }

  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), 8000);

  try {
    const response = await fetch(`${botApiUrl}${path}`, {
      method,
      headers,
      body: body === undefined ? undefined : JSON.stringify(body),
      signal: controller.signal,
    });

    if (response.status === 204) {
      return new NextResponse(null, { status: response.status });
    }

    const contentType = response.headers.get("content-type") || "";
    if (contentType.includes("application/json")) {
      const data = await response.json();
      return NextResponse.json(data, { status: response.status });
    }

    const text = await response.text();
    return new NextResponse(text, {
      status: response.status,
      headers: contentType ? { "content-type": contentType } : undefined,
    });
  } catch (error) {
    console.error(`Error proxying ${method} ${path}:`, error);
    return NextResponse.json(
      { success: false, message: "API service unavailable" },
      { status: 500 },
    );
  } finally {
    clearTimeout(timeout);
  }
}

/**
 * Keeps the older response contract the api-keys clients still depend on
 * (upstream errors wrapped in `{ error }`, 200 on any success, a 500
 * envelope on failure), which proxyBotApi does not. Prefer proxyBotApi for
 * new routes.
 */
export async function proxyBotApiWithErrorEnvelope(
  req: NextRequest,
  path: string,
  method: string,
  failureMessage: string,
  { forwardJsonBody = false }: { forwardJsonBody?: boolean } = {},
) {
  try {
    const body = forwardJsonBody ? JSON.stringify(await req.json()) : undefined;
    const response = await fetch(`${process.env.BOT_API_URL}${path}`, {
      method,
      headers: {
        "Content-Type": "application/json",
        Authorization: req.headers.get("Authorization"),
      } as HeadersInit,
      body,
    });

    if (!response.ok) {
      return NextResponse.json(
        { error: await response.json() },
        { status: response.status },
      );
    }

    return NextResponse.json(await response.json());
  } catch (error) {
    console.error(`${failureMessage}:`, error);
    return NextResponse.json(
      {
        error: {
          message: failureMessage,
          statusCode: 500,
          error: "Internal Server Error",
        },
      },
      { status: 500 },
    );
  }
}

/**
 * Login and token validation carry their credentials in the body, so the
 * caller's Authorization header is deliberately not forwarded. The error
 * messages name the authentication service because JwtAuthService shows
 * them on the login form.
 */
export async function proxyAuthPost(
  req: NextRequest,
  endpoint: "login" | "validate",
) {
  const botApiUrl = process.env.BOT_API_URL;
  if (!botApiUrl) {
    return NextResponse.json(
      { success: false, message: "Authentication service misconfigured" },
      { status: 500 },
    );
  }
  try {
    const body = await req.json();
    const controller = new AbortController();
    const timeout = setTimeout(() => controller.abort(), 8000);
    const response = await fetch(`${botApiUrl}/auth/${endpoint}`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body),
      signal: controller.signal,
    });
    clearTimeout(timeout);
    const data = await response.json();
    return NextResponse.json(data, { status: response.status });
  } catch (error) {
    console.error(`Error proxying auth ${endpoint}:`, error);
    return NextResponse.json(
      { success: false, message: "Authentication service unavailable" },
      { status: 500 },
    );
  }
}

export async function readJsonRequest(req: NextRequest): Promise<unknown> {
  try {
    return await req.json();
  } catch {
    return malformedJsonResponse();
  }
}

function malformedJsonResponse() {
  return NextResponse.json(
    { success: false, message: "Malformed JSON payload" },
    { status: 400 },
  );
}

export function isResponse(value: unknown): value is NextResponse {
  return value instanceof NextResponse;
}
