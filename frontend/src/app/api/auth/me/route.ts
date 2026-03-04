import { NextRequest, NextResponse } from "next/server";

export async function GET(req: NextRequest) {
  try {
    const authHeader = req.headers.get("Authorization");
    const headers: Record<string, string> = {
      "Content-Type": "application/json",
    };
    if (authHeader) {
      headers["Authorization"] = authHeader;
    }
    const response = await fetch(`${process.env.BOT_API_URL}/auth/me`, {
      method: "GET",
      headers,
    });
    const data = await response.json();
    return NextResponse.json(data, { status: response.status });
  } catch (error) {
    console.error("Error proxying auth me:", error);
    return NextResponse.json(
      { success: false, message: "Authentication service unavailable" },
      { status: 500 },
    );
  }
}
