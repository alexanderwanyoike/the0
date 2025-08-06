import { NextRequest, NextResponse } from "next/server";
import { withAdminAuth } from "@/lib/middleware/admin-auth";

export async function GET(req: NextRequest) {
  return withAdminAuth(req, async (req: NextRequest) => {
    const token = req.headers.get("Authorization");

    try {
      const response = await fetch(`${process.env.BOT_API_URL}/custom-bots`, {
        method: "GET",
        headers: {
          "Content-Type": "application/json",
          Authorization: token,
        } as HeadersInit,
      });

      if (!response.ok) {
        return NextResponse.json(
          { error: "Failed to fetch custom bots" },
          { status: response.status },
        );
      }

      const result = await response.json();
      return NextResponse.json(result);
    } catch (error) {
      console.error("Error fetching custom bots:", error);
      return NextResponse.json(
        { error: "Internal server error" },
        { status: 500 },
      );
    }
  });
}
