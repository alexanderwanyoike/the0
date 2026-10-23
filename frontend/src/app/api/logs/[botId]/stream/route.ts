import { NextRequest, NextResponse } from "next/server";
import { withAdminAuth } from "@/lib/middleware/admin-auth";

export const dynamic = "force-dynamic";

const OPENING_COMMENT = new TextEncoder().encode(": connected\n\n");

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
      controller.enqueue(OPENING_COMMENT);
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

export async function GET(
  req: NextRequest,
  { params }: { params: Promise<{ botId: string }> },
) {
  return withAdminAuth(req, async (req: NextRequest) => {
    const apiUrl = process.env.BOT_API_URL;
    if (!apiUrl) {
      return NextResponse.json(
        { error: "API server not configured" },
        { status: 500 },
      );
    }

    const { botId } = await params;
    const token = req.headers.get("Authorization");
    const headers: HeadersInit = { Accept: "text/event-stream" };
    if (token) {
      headers.Authorization = token;
    }

    const { search } = new URL(req.url);

    try {
      const upstream = await fetch(
        `${apiUrl}/logs/${encodeURIComponent(botId)}/stream${search}`,
        {
          headers,
          signal: req.signal,
        },
      );

      if (!upstream.ok) {
        return NextResponse.json(
          { error: "Error connecting to log stream" },
          { status: upstream.status },
        );
      }

      const body = upstream.body ? openingAtOnce(upstream.body) : null;
      return new NextResponse(body, {
        headers: {
          "Content-Type": "text/event-stream; charset=utf-8",
          "Cache-Control": "no-cache, no-transform",
          Connection: "keep-alive",
          "X-Accel-Buffering": "no",
        },
      });
    } catch (error) {
      console.error("Error proxying log stream:", error);
      return NextResponse.json(
        { error: "Error connecting to log stream" },
        { status: 502 },
      );
    }
  });
}
