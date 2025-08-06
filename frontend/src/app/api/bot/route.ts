import { NextRequest, NextResponse } from "next/server";
import { withAdminAuth } from "@/lib/middleware/admin-auth";

export async function GET(req: NextRequest) {
  return withAdminAuth(req, async (req: NextRequest) => {
    try {
      const token = req.headers.get("Authorization");
      console.log("Fetching bots list");
      const response = await fetch(`${process.env.BOT_API_URL}/bot`, {
        method: "GET",
        headers: {
          "Content-Type": "application/json",
          Authorization: token,
        } as HeadersInit,
      });
      console.log("Response:", response);
      if (!response.ok) {
        return NextResponse.json(
          { error: await response.json() },
          { status: response.status },
        );
      }
      const data = await response.json();
      return NextResponse.json(data);
    } catch (error: any) {
      console.error("Error fetching bots:", error);
      return NextResponse.json(
        {
          error: {
            message: "Error fetching bots",
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
      console.log("Creating bot with body:", body);
      const response = await fetch(`${process.env.BOT_API_URL}/bot`, {
        method: "POST",
        headers: {
          "Content-Type": "application/json",
          Authorization: token,
        } as HeadersInit,
        body: JSON.stringify(body),
      });
      console.log("Response:", response);
      if (!response.ok) {
        return NextResponse.json(
          { error: await response.json() },
          { status: response.status },
        );
      }
      const data = await response.json();
      return NextResponse.json(data);
    } catch (error: any) {
      console.error("Error creating bot:", error);
      return NextResponse.json(
        {
          error: {
            message: "Error creating bot",
            statusCode: 500,
            error: "Internal Server Error",
          },
        },
        { status: 500 },
      );
    }
  });
}
