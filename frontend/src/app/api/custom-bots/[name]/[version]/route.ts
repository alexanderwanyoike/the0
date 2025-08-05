import { NextRequest, NextResponse } from "next/server";
import { withAdminAuth } from "@/lib/middleware/admin-auth";

export async function GET(
  req: NextRequest,
  { params }: { params: Promise<{ name: string; version: string }> },
) {
  return withAdminAuth(req, async (req: NextRequest) => {
    try {
      const { name, version } = await params;
      const token = req.headers.get("Authorization");

      // Get custom bot details from the bot API (globally accessible)
      const botResponse = await fetch(
        `${process.env.BOT_API_URL}/custom-bots/${encodeURIComponent(name)}/${encodeURIComponent(version)}`,
        {
          method: "GET",
          headers: {
            "Content-Type": "application/json",
            Authorization: token,
          } as HeadersInit,
        },
      );

      if (!botResponse.ok) {
        const errorData = await botResponse.json();
        return NextResponse.json(
          {
            success: false,
            error: errorData.message || "Failed to get custom bot details",
            message: errorData.message || "Failed to get custom bot details",
          },
          { status: botResponse.status },
        );
      }

      const botResult = await botResponse.json();

      if (!botResult.success) {
        return NextResponse.json(botResult);
      }

      // Check access control: user either owns the bot OR has purchased it
      let hasAccess = false;

      // Check 1: Does user own this custom bot?
      const userCustomBotsResponse = await fetch(
        `${process.env.BOT_API_URL}/custom-bots`,
        {
          method: "GET",
          headers: {
            "Content-Type": "application/json",
            Authorization: token,
          } as HeadersInit,
        },
      );

      if (userCustomBotsResponse.ok) {
        const userCustomBotsResult = await userCustomBotsResponse.json();
        if (userCustomBotsResult.success && userCustomBotsResult.data) {
          // Check if user owns a custom bot with this name
          const ownsBot = userCustomBotsResult.data.some(
            (customBot: any) => customBot.name === name,
          );
          if (ownsBot) {
            hasAccess = true;
          }
        }
      }

      // Check 2: Has user purchased/installed this bot?
      if (!hasAccess) {
        const hasUserBotResponse = await fetch(
          `${process.env.BOT_API_URL}/user-bots/has/${encodeURIComponent(name)}`,
          {
            method: "GET",
            headers: {
              "Content-Type": "application/json",
              Authorization: token,
            } as HeadersInit,
          },
        );

        if (hasUserBotResponse.ok) {
          const hasUserBotResult = await hasUserBotResponse.json();
          if (hasUserBotResult.success && hasUserBotResult.hasBot) {
            hasAccess = true;
          }
        }
      }

      const result = {
        success: true,
        data: {
          ...botResult.data,
          hasAccess: hasAccess,
        },
        message: botResult.message,
      };

      return NextResponse.json(result);
    } catch (error: any) {
      console.error("Error getting custom bot details:", error);
      return NextResponse.json(
        {
          success: false,
          error: "Failed to get custom bot details",
          message: error.message,
        },
        { status: 500 },
      );
    }
  });
}
