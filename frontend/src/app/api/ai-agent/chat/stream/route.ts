import { NextRequest } from "next/server";
import { getAiAgentUrl } from "@/lib/config";

export async function POST(request: NextRequest) {
  try {
    const body = await request.json();
    const aiAgentUrl = getAiAgentUrl();

    const response = await fetch(`${aiAgentUrl}/chat/stream`, {
      method: "POST",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify(body),
    });

    if (!response.ok) {
      throw new Error(`AI Agent API error: ${response.statusText}`);
    }

    // Stream the response back to the client
    return new Response(response.body, {
      status: response.status,
      headers: {
        "Content-Type": "text/event-stream",
        "Cache-Control": "no-cache",
        Connection: "keep-alive",
        "X-Accel-Buffering": "no",
      },
    });
  } catch (error) {
    console.error("AI Agent stream error:", error);
    return new Response(
      JSON.stringify({ error: "Failed to communicate with AI agent" }),
      {
        status: 500,
        headers: { "Content-Type": "application/json" },
      },
    );
  }
}
