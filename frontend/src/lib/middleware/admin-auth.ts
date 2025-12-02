import { NextRequest, NextResponse } from "next/server";
import { jwtVerify } from "jose";

const adminAuthMiddleware = async (
  req: NextRequest,
): Promise<NextResponse | { requestHeaders: Headers }> => {
  try {
    // Get the token from the auth header
    const authHeader = req.headers.get("Authorization");

    if (!authHeader?.startsWith("Bearer ")) {
      return NextResponse.json(
        { error: "Missing authentication token" },
        { status: 401 },
      );
    }

    const token = authHeader.split("Bearer ")[1];
    if (!token) {
      return NextResponse.json(
        { error: "Missing authentication token" },
        { status: 401 },
      );
    }

    // Get JWT secret from environment
    const jwtSecret = process.env.JWT_SECRET;
    if (!jwtSecret) {
      console.error("JWT_SECRET environment variable not set");
      return NextResponse.json(
        { error: "Server configuration error" },
        { status: 500 },
      );
    }

    // Verify the JWT token
    const secret = new TextEncoder().encode(jwtSecret);

    const { payload } = await jwtVerify(token, secret, {
      algorithms: ["HS256"],
      issuer: "the0-oss-api",
      audience: "the0-oss-clients",
    });

    if (!payload.sub) {
      return NextResponse.json(
        { error: "Invalid token payload" },
        { status: 401 },
      );
    }

    // Add the user ID to the request headers
    const requestHeaders = new Headers(req.headers);
    requestHeaders.set("x-user-id", payload.sub);
    requestHeaders.set("x-user-email", (payload.email as string) || "");

    // Return the modified request
    return {
      requestHeaders,
    };
  } catch (error: any) {
    // Handle JWT-specific errors
    if (error.name === "JWTExpired") {
      return NextResponse.json(
        {
          error: "Token expired",
          code: "TOKEN_EXPIRED",
          message:
            "Your authentication token has expired. Please refresh and try again.",
        },
        {
          status: 401,
          headers: {
            "X-Auth-Error": "TOKEN_EXPIRED",
          },
        },
      );
    }

    if (error.name === "JWTInvalid" || error.name === "JWTMalformed") {
      return NextResponse.json(
        {
          error: "Authentication failed",
          code: "AUTH_ERROR",
          message: "Invalid authentication token",
        },
        {
          status: 401,
          headers: {
            "X-Auth-Error": "INVALID_TOKEN",
          },
        },
      );
    }

    // Generic error fallback
    return NextResponse.json(
      {
        error: "Unauthorized",
        code: "AUTH_ERROR",
        message: "Authentication verification failed",
      },
      { status: 401 },
    );
  }
};

export const withAdminAuth = async (
  req: NextRequest,
  handler: (req: NextRequest) => Promise<NextResponse>,
) => {
  const middlewareResponse = await adminAuthMiddleware(req);

  // If the middleware returns a response, return it
  if (middlewareResponse instanceof NextResponse) {
    return middlewareResponse;
  }

  // Create a new request with the modified headers
  const newRequest = new NextRequest(req, {
    headers: middlewareResponse.requestHeaders,
    method: req.method,
    body: req.body,
    signal: req.signal,
  });

  return handler(newRequest);
};
