import { NextRequest, NextResponse } from "next/server";
import { getAiAgentUrl } from "@/lib/config";

export async function GET(
  request: NextRequest,
  context: { params: Promise<{ sessionId: string }> },
) {
  try {
    const params = await context.params;
    const aiAgentUrl = getAiAgentUrl();

    const response = await fetch(
      `${aiAgentUrl}/chat/sessions/${params.sessionId}`,
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
    console.error("AI Agent session error:", error);
    return NextResponse.json(
      { error: "Failed to fetch session" },
      { status: 500 },
    );
  }
}

export async function PUT(
  request: NextRequest,
  context: { params: Promise<{ sessionId: string }> },
) {
  try {
    const params = await context.params;
    const body = await request.json();
    const aiAgentUrl = getAiAgentUrl();

    const response = await fetch(
      `${aiAgentUrl}/chat/sessions/${params.sessionId}/title`,
      {
        method: "PUT",
        headers: {
          "Content-Type": "application/json",
        },
        body: JSON.stringify(body),
      },
    );

    if (!response.ok) {
      throw new Error(`AI Agent API error: ${response.statusText}`);
    }

    const data = await response.json();
    return NextResponse.json(data);
  } catch (error) {
    console.error("AI Agent session update error:", error);
    return NextResponse.json(
      { error: "Failed to update session" },
      { status: 500 },
    );
  }
}

export async function DELETE(
  request: NextRequest,
  context: { params: Promise<{ sessionId: string }> },
) {
  try {
    const params = await context.params;
    const aiAgentUrl = getAiAgentUrl();

    const response = await fetch(
      `${aiAgentUrl}/chat/sessions/${params.sessionId}`,
      {
        method: "DELETE",
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
    console.error("AI Agent session delete error:", error);
    return NextResponse.json(
      { error: "Failed to delete session" },
      { status: 500 },
    );
  }
}
