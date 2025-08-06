import { NextRequest, NextResponse } from "next/server";
import { jwtVerify } from "jose";

const adminAuthMiddleware = async (
  req: NextRequest,
): Promise<NextResponse | { requestHeaders: Headers }> => {
  try {
    console.log("🔐 Admin auth middleware - checking request to:", req.url);

    // Get the token from the auth header
    const authHeader = req.headers.get("Authorization");
    console.log("🔑 Auth header present:", !!authHeader);

    if (!authHeader?.startsWith("Bearer ")) {
      console.log("❌ Missing or invalid auth header format");
      return NextResponse.json(
        { error: "Missing authentication token" },
        { status: 401 },
      );
    }

    const token = authHeader.split("Bearer ")[1];
    if (!token) {
      console.log("❌ No token in auth header");
      return NextResponse.json(
        { error: "Missing authentication token" },
        { status: 401 },
      );
    }

    console.log(
      "🎫 Token received (first 20 chars):",
      token.substring(0, 20) + "...",
    );

    // Get JWT secret from environment
    const jwtSecret = process.env.JWT_SECRET;
    if (!jwtSecret) {
      console.error("❌ JWT_SECRET environment variable not set");
      return NextResponse.json(
        { error: "Server configuration error" },
        { status: 500 },
      );
    }

    console.log("🔒 JWT_SECRET is configured:", !!jwtSecret);

    // Verify the JWT token
    const secret = new TextEncoder().encode(jwtSecret);
    console.log("🔍 Attempting to verify JWT token...");

    const { payload } = await jwtVerify(token, secret, {
      algorithms: ["HS256"],
      issuer: "the0-oss-api",
      audience: "the0-oss-clients",
    });

    console.log("✅ JWT verification successful");
    console.log("👤 Payload sub:", payload.sub);
    console.log("📧 Payload email:", payload.email);

    if (!payload.sub) {
      console.log("❌ Missing sub in token payload");
      return NextResponse.json(
        { error: "Invalid token payload" },
        { status: 401 },
      );
    }

    // Add the user ID to the request headers
    const requestHeaders = new Headers(req.headers);
    requestHeaders.set("x-user-id", payload.sub);
    requestHeaders.set("x-user-email", (payload.email as string) || "");

    console.log("✅ Admin auth middleware passed");

    // Return the modified request
    return {
      requestHeaders,
    };
  } catch (error: any) {
    console.error("❌ Error verifying JWT token:", error);
    console.error("❌ Error name:", error.name);
    console.error("❌ Error message:", error.message);

    // Handle JWT-specific errors
    if (error.name === "JWTExpired") {
      console.log("❌ JWT token expired");
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
      console.log("❌ JWT token invalid or malformed");
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
    console.log("❌ Generic JWT auth error");
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
