import { NextRequest, NextResponse } from "next/server";

export async function proxyBotApi(
  req: NextRequest,
  path: string,
  method: string,
  body?: unknown,
) {
  const botApiUrl = process.env.BOT_API_URL;
  if (!botApiUrl) {
    return NextResponse.json(
      { success: false, message: "API service misconfigured" },
      { status: 500 },
    );
  }

  const authHeader = req.headers.get("Authorization");
  const headers: Record<string, string> = {
    "Content-Type": "application/json",
  };
  if (authHeader) {
    headers.Authorization = authHeader;
  }

  const controller = new AbortController();
  const timeout = setTimeout(() => controller.abort(), 8000);

  try {
    const response = await fetch(`${botApiUrl}${path}`, {
      method,
      headers,
      body: body === undefined ? undefined : JSON.stringify(body),
      signal: controller.signal,
    });

    if (response.status === 204) {
      return new NextResponse(null, { status: response.status });
    }

    const contentType = response.headers.get("content-type") || "";
    if (contentType.includes("application/json")) {
      const data = await response.json();
      return NextResponse.json(data, { status: response.status });
    }

    const text = await response.text();
    return new NextResponse(text, {
      status: response.status,
      headers: contentType ? { "content-type": contentType } : undefined,
    });
  } catch (error) {
    console.error(`Error proxying ${method} ${path}:`, error);
    return NextResponse.json(
      { success: false, message: "API service unavailable" },
      { status: 500 },
    );
  } finally {
    clearTimeout(timeout);
  }
}

export async function readJsonRequest(req: NextRequest): Promise<unknown> {
  try {
    return await req.json();
  } catch {
    return malformedJsonResponse();
  }
}

export function malformedJsonResponse() {
  return NextResponse.json(
    { success: false, message: "Malformed JSON payload" },
    { status: 400 },
  );
}

export function isResponse(value: unknown): value is NextResponse {
  return value instanceof NextResponse;
}
