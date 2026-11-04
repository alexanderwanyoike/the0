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
