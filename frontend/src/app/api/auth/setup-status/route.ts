import { NextResponse } from "next/server";

export async function GET() {
  const botApiUrl = process.env.BOT_API_URL;
  if (!botApiUrl) {
    return NextResponse.json(
      { success: false, message: "Authentication service misconfigured" },
      { status: 500 },
    );
  }

  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), 8000);

  try {
    const response = await fetch(`${botApiUrl}/auth/setup-status`, {
      method: "GET",
      signal: controller.signal,
    });
    const data = await response.json();
    return NextResponse.json(data, { status: response.status });
  } catch (error) {
    console.error("Error proxying auth setup status:", error);
    return NextResponse.json(
      { success: false, message: "Authentication service unavailable" },
      { status: 500 },
    );
  } finally {
    clearTimeout(timeout);
  }
}
