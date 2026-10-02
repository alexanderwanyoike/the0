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

/**
 * Keeps the older response contract the api-keys clients still depend on
 * (upstream errors wrapped in `{ error }`, 200 on any success, a 500
 * envelope on failure), which proxyBotApi does not. Prefer proxyBotApi for
 * new routes.
 */
export async function proxyBotApiWithErrorEnvelope(
  req: NextRequest,
  path: string,
  method: string,
  failureMessage: string,
  { forwardJsonBody = false }: { forwardJsonBody?: boolean } = {},
) {
  try {
    const body = forwardJsonBody ? JSON.stringify(await req.json()) : undefined;
    const response = await fetch(`${process.env.BOT_API_URL}${path}`, {
      method,
      headers: {
        "Content-Type": "application/json",
        Authorization: req.headers.get("Authorization"),
      } as HeadersInit,
      body,
    });

    if (!response.ok) {
      return NextResponse.json(
        { error: await response.json() },
        { status: response.status },
      );
    }

    return NextResponse.json(await response.json());
  } catch (error) {
    console.error(`${failureMessage}:`, error);
    return NextResponse.json(
      {
        error: {
          message: failureMessage,
          statusCode: 500,
          error: "Internal Server Error",
        },
      },
      { status: 500 },
    );
  }
}

export async function readJsonRequest(req: NextRequest): Promise<unknown> {
  try {
    return await req.json();
  } catch {
    return malformedJsonResponse();
  }
}

function malformedJsonResponse() {
  return NextResponse.json(
    { success: false, message: "Malformed JSON payload" },
    { status: 400 },
  );
}

export function isResponse(value: unknown): value is NextResponse {
  return value instanceof NextResponse;
}
