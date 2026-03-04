import { NextRequest, NextResponse } from "next/server";

export async function POST(req: NextRequest) {
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
    const response = await fetch(`${botApiUrl}/auth/validate`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify(body),
      signal: controller.signal,
    });
    clearTimeout(timeout);
    const data = await response.json();
    return NextResponse.json(data, { status: response.status });
  } catch (error) {
    console.error("Error proxying auth validate:", error);
    return NextResponse.json(
      { success: false, message: "Authentication service unavailable" },
      { status: 500 },
    );
  }
}
