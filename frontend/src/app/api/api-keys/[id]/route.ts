import { NextRequest, NextResponse } from "next/server";
import { withAdminAuth } from "@/lib/middleware/admin-auth";

export async function GET(
  req: NextRequest,
  { params }: { params: Promise<{ id: string }> },
) {
  return withAdminAuth(req, async (req: NextRequest) => {
    try {
      const token = req.headers.get("Authorization");
      const { id } = await params;
      const response = await fetch(
        `${process.env.BOT_API_URL}/api-keys/${id}`,
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
      // Convert Firestore timestamps to ISO strings if necessary
      return NextResponse.json(data);
    } catch (error: any) {
      console.error("Error fetching API key:", error);
      return NextResponse.json(
        {
          error: {
            message: "Error fetching API key",
            statusCode: 500,
            error: "Internal Server Error",
          },
        },
        { status: 500 },
      );
    }
  });
}

export async function DELETE(
  req: NextRequest,
  { params }: { params: Promise<{ id: string }> },
) {
  return withAdminAuth(req, async (req: NextRequest) => {
    try {
      const token = req.headers.get("Authorization");
      const { id } = await params;

      console.log("🗑️ Frontend DELETE handler called for API key ID:", id);

      const response = await fetch(
        `${process.env.BOT_API_URL}/api-keys/${id}`,
        {
          method: "DELETE",
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
      console.error("Error deleting API key:", error);
      return NextResponse.json(
        {
          error: {
            message: "Error deleting API key",
            statusCode: 500,
            error: "Internal Server Error",
          },
        },
        { status: 500 },
      );
    }
  });
}
