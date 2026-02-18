import { NextRequest, NextResponse } from "next/server";
import { withAdminAuth } from "@/lib/middleware/admin-auth";

export const dynamic = "force-dynamic";

export async function GET(
  req: NextRequest,
  { params }: { params: Promise<{ botId: string }> },
) {
  return withAdminAuth(req, async (req: NextRequest) => {
    const { botId } = await params;
    const token = req.headers.get("Authorization");

    try {
      const upstream = await fetch(
        `${process.env.BOT_API_URL}/logs/${encodeURIComponent(botId)}/stream`,
        {
          headers: {
            Authorization: token,
          } as HeadersInit,
          signal: req.signal,
        },
      );

      if (!upstream.ok) {
        return NextResponse.json(
          { error: "Error connecting to log stream" },
          { status: upstream.status },
        );
      }

      return new NextResponse(upstream.body, {
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
