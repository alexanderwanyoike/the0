import { NextRequest, NextResponse } from "next/server";
import { withAdminAuth } from "@/lib/middleware/admin-auth";

export async function GET(req: NextRequest) {
  return withAdminAuth(req, async (req: NextRequest) => {
    try {
      const token = req.headers.get("Authorization");
      console.log(
        "🚀 API proxy - fetching from backend:",
        `${process.env.BOT_API_URL}/api-keys`,
      );
      console.log(
        "🎫 Forwarding token:",
        token ? token.substring(0, 20) + "..." : "No token",
      );

      const response = await fetch(`${process.env.BOT_API_URL}/api-keys`, {
        method: "GET",
        headers: {
          "Content-Type": "application/json",
          Authorization: token,
        } as HeadersInit,
      });

      console.log("📡 Backend response status:", response.status);
      console.log("📡 Backend response ok:", response.ok);

      if (!response.ok) {
        const errorData = await response.json();
        console.log("❌ Backend error:", errorData);
        return NextResponse.json(
          { error: errorData },
          { status: response.status },
        );
      }

      let data = await response.json();
      return NextResponse.json(data);
    } catch (error: any) {
      console.error("Error fetching API keys:", error);
      return NextResponse.json(
        {
          error: {
            message: "Error fetching API keys",
            statusCode: 500,
            error: "Internal Server Error",
          },
        },
        { status: 500 },
      );
    }
  });
}

export async function POST(req: NextRequest) {
  return withAdminAuth(req, async (req: NextRequest) => {
    try {
      const token = req.headers.get("Authorization");
      const body = await req.json();

      const response = await fetch(`${process.env.BOT_API_URL}/api-keys`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          Authorization: token,
        } as HeadersInit,
        body: JSON.stringify(body),
      });

      if (!response.ok) {
        return NextResponse.json(
          { error: await response.json() },
          { status: response.status },
        );
      }

      let data = await response.json();
      return NextResponse.json(data);
    } catch (error: any) {
      console.error("Error creating API key:", error);
      return NextResponse.json(
        {
          error: {
            message: "Error creating API key",
            statusCode: 500,
            error: "Internal Server Error",
          },
        },
        { status: 500 },
      );
    }
  });
}
