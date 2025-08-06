import { NextRequest, NextResponse } from "next/server";
import { withAdminAuth } from "@/lib/middleware/admin-auth";

export async function GET(req: NextRequest) {
  return withAdminAuth(req, async (req: NextRequest) => {
    try {
      const token = req.headers.get("Authorization");

      const response = await fetch(
        `${process.env.BOT_API_URL}/api-keys/stats/summary`,
        {
          method: "GET",
          headers: {
            "Content-Type": "application/json",
            Authorization: token,
          } as HeadersInit,
        },
      );

      if (!response.ok) {
        return NextResponse.json(
          { error: await response.json() },
          { status: response.status },
        );
      }

      let data = await response.json();
      return NextResponse.json(data);
    } catch (error: any) {
      console.error("Error fetching API key stats:", error);
      return NextResponse.json(
        {
          error: {
            message: "Error fetching API key stats",
            statusCode: 500,
            error: "Internal Server Error",
          },
        },
        { status: 500 },
      );
    }
  });
}
