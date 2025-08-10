import { NextRequest, NextResponse } from "next/server";
import { getAiAgentUrl } from "@/lib/config";

export async function GET(
  request: NextRequest,
  context: { params: Promise<{ filename: string }> },
) {
  try {
    const params = await context.params;
    const { searchParams } = new URL(request.url);
    const sessionId = searchParams.get("session_id");
    const aiAgentUrl = getAiAgentUrl();

    let url = `${aiAgentUrl}/artifacts/${params.filename}`;
    if (sessionId) {
      url = `${aiAgentUrl}/artifacts/session/${sessionId}/${params.filename}`;
    }

    const response = await fetch(url, {
      method: "GET",
      headers: {
        "Content-Type": "application/json",
      },
    });

    if (!response.ok) {
      throw new Error(`AI Agent API error: ${response.statusText}`);
    }

    const data = await response.json();
    return NextResponse.json(data);
  } catch (error) {
    console.error("AI Agent artifact error:", error);
    return NextResponse.json(
      { error: "Failed to fetch artifact" },
      { status: 500 },
    );
  }
}
