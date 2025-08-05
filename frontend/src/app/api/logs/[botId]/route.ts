import { NextRequest, NextResponse } from "next/server";
import { withAdminAuth } from "@/lib/middleware/admin-auth";

export async function GET(
  req: NextRequest,
  { params }: { params: Promise<{ botId: string }> },
) {
  return withAdminAuth(req, async (req: NextRequest) => {
    try {
      const { botId } = await params;
      const { searchParams } = new URL(req.url);

      // Extract query parameters
      const date = searchParams.get("date");
      const dateRange = searchParams.get("dateRange");
      const limit = searchParams.get("limit") || "100";
      const offset = searchParams.get("offset") || "0";

      // Build query string for backend API
      const queryParams = new URLSearchParams();
      if (date) queryParams.set("date", date);
      if (dateRange) queryParams.set("dateRange", dateRange);
      queryParams.set("limit", limit);
      queryParams.set("offset", offset);

      const token = req.headers.get("Authorization");
      const response = await fetch(
        `${process.env.BOT_API_URL}/logs/${botId}?${queryParams.toString()}`,
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
          { error: "Error fetching logs" },
          { status: response.status },
        );
      }

      const data = await response.json();
      return NextResponse.json(data);
    } catch (error: any) {
      console.error("Error fetching logs:", error);
      return NextResponse.json(
        { error: "Error fetching logs" },
        { status: 500 },
      );
    }
  });
}
