import { NextRequest, NextResponse } from "next/server";
import { getAiAgentUrl } from "@/lib/config";

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const userId = searchParams.get("user_id") || "default-user";
    const aiAgentUrl = getAiAgentUrl();

    const response = await fetch(
      `${aiAgentUrl}/chat/sessions?user_id=${userId}`,
      {
        method: "GET",
        headers: {
          "Content-Type": "application/json",
        },
      },
    );

    if (!response.ok) {
      throw new Error(`AI Agent API error: ${response.statusText}`);
    }

    const data = await response.json();
    return NextResponse.json(data);
  } catch (error) {
    console.error("AI Agent sessions error:", error);
    return NextResponse.json(
      { error: "Failed to fetch sessions" },
      { status: 500 },
    );
  }
}
