import { NextRequest, NextResponse } from "next/server";
import { cookies } from "next/headers";

export async function GET(
  req: NextRequest,
  { params }: { params: Promise<{ id: string }> },
) {
  const { id: customBotId } = await params;

  // Get auth token from cookies (set by the auth context)
  const cookieStore = await cookies();
  const authToken = cookieStore.get("auth-token")?.value;

  if (!authToken) {
    return NextResponse.json(
      { error: "Authentication required" },
      { status: 401 },
    );
  }

  try {
    const response = await fetch(
      `${process.env.BOT_API_URL}/custom-bots/by-id/${encodeURIComponent(customBotId)}/frontend`,
      {
        method: "GET",
        headers: {
          Authorization: `Bearer ${authToken}`,
        },
      },
    );

    if (!response.ok) {
      const errorText = await response.text();
      console.error("Backend error:", response.status, errorText);
      return NextResponse.json(
        { error: "Failed to fetch frontend bundle" },
        { status: response.status },
      );
    }

    // Get the JavaScript bundle (already built as IIFE by CLI)
    const bundle = await response.text();

    // Return as JavaScript - no caching to ensure fresh bundles after deploys
    return new NextResponse(bundle, {
      status: 200,
      headers: {
        "Content-Type": "application/javascript",
        "Cache-Control": "no-store, must-revalidate",
      },
    });
  } catch (error) {
    console.error("Error fetching frontend bundle:", error);
    return NextResponse.json(
      { error: "Internal server error" },
      { status: 500 },
    );
  }
}
