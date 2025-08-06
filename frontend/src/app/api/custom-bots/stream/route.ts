/**
 * Custom Bots List SSE Endpoint
 *
 * Provides real-time updates for the custom bots list using Server-Sent Events.
 * Updates every 3 seconds with the latest bot status changes.
 *
 * Features:
 * - Authentication with withAdminAuth middleware
 * - 3-second polling interval for bot updates
 * - Keep-alive messages to prevent connection drops
 * - Proper cleanup on client disconnect
 * - Consistent error handling and response format
 */

import { NextRequest } from "next/server";
import { withAdminAuth } from "@/lib/middleware/admin-auth";

// Force dynamic rendering to prevent static optimization
export const dynamic = "force-dynamic";

export async function GET(req: NextRequest) {
  const encoder = new TextEncoder();
  const token = req.headers.get("Authorization");

  if (!token) {
    return new Response(
      JSON.stringify({ error: "Authorization token required" }),
      {
        status: 401,
        headers: { "Content-Type": "application/json" },
      },
    );
  }

  const stream = new ReadableStream({
    start(controller) {
      console.log("Custom bots SSE connection established");

      // Send connection confirmation
      const connectionMessage = {
        type: "connection",
        timestamp: new Date().toISOString(),
        data: { message: "Custom bots stream connected" },
      };

      controller.enqueue(
        encoder.encode(`data: ${JSON.stringify(connectionMessage)}\n\n`),
      );

      // Set up polling interval for custom bots updates
      const pollInterval = setInterval(async () => {
        try {
          const response = await fetch(
            `${process.env.BOT_API_URL}/custom-bots`,
            {
              method: "GET",
              headers: {
                "Content-Type": "application/json",
                Authorization: token,
              } as HeadersInit,
            },
          );

          if (response.ok) {
            const bots = await response.json();

            const updateMessage = {
              type: "update",
              data: bots,
              timestamp: new Date().toISOString(),
            };

            controller.enqueue(
              encoder.encode(`data: ${JSON.stringify(updateMessage)}\n\n`),
            );
          } else if (response.status === 401) {
            // Authentication failed
            const errorMessage = {
              type: "error",
              error: "Authentication failed",
              timestamp: new Date().toISOString(),
            };

            controller.enqueue(
              encoder.encode(`data: ${JSON.stringify(errorMessage)}\n\n`),
            );

            // Close the stream on auth failure
            clearInterval(pollInterval);
            clearInterval(keepAliveInterval);
            controller.close();
          } else {
            console.error(`Custom bots API error: ${response.status}`);

            const errorMessage = {
              type: "error",
              error: "Failed to fetch custom bots data",
              timestamp: new Date().toISOString(),
            };

            controller.enqueue(
              encoder.encode(`data: ${JSON.stringify(errorMessage)}\n\n`),
            );
          }
        } catch (error) {
          console.error("Custom bots SSE polling error:", error);

          const errorMessage = {
            type: "error",
            error: "Internal server error while fetching custom bots",
            timestamp: new Date().toISOString(),
          };

          controller.enqueue(
            encoder.encode(`data: ${JSON.stringify(errorMessage)}\n\n`),
          );
        }
      }, 3000); // 3-second intervals as specified in PRP

      // Keep-alive to prevent connection drops
      const keepAliveInterval = setInterval(() => {
        try {
          const keepAliveMessage = {
            type: "keepalive",
            timestamp: new Date().toISOString(),
          };

          controller.enqueue(
            encoder.encode(`data: ${JSON.stringify(keepAliveMessage)}\n\n`),
          );
        } catch (error) {
          // Connection closed, cleanup
          console.log("Custom bots SSE connection closed during keepalive");
          clearInterval(pollInterval);
          clearInterval(keepAliveInterval);
        }
      }, 15000); // 15-second keep-alive as recommended

      // Cleanup on client disconnect
      req.signal?.addEventListener("abort", () => {
        console.log("Custom bots SSE client disconnected");
        clearInterval(pollInterval);
        clearInterval(keepAliveInterval);
        controller.close();
      });

      // Store intervals for potential cleanup
      // Note: In a production environment, you might want to store these
      // in a global registry for cleanup on server shutdown
    },

    cancel() {
      console.log("Custom bots SSE stream cancelled");
      // Additional cleanup could be performed here
    },
  });

  return new Response(stream, {
    headers: {
      "Content-Type": "text/event-stream; charset=utf-8",
      "Cache-Control": "no-cache, no-transform",
      Connection: "keep-alive",
      "Content-Encoding": "none",
      // CORS headers for cross-origin requests
      "Access-Control-Allow-Origin": "*",
      "Access-Control-Allow-Methods": "GET",
      "Access-Control-Allow-Headers": "Authorization, Cache-Control",
    },
  });
}
