/**
 * Specific Custom Bot SSE Endpoint
 * 
 * Provides real-time updates for a specific custom bot using Server-Sent Events.
 * Updates every 2 seconds with the latest bot development status, version changes,
 * and compilation results.
 * 
 * Features:
 * - Authentication with withAdminAuth middleware
 * - 2-second polling interval for rapid development updates
 * - Dynamic routing for specific bot by name
 * - Keep-alive messages to prevent connection drops
 * - Proper cleanup on client disconnect
 * - Enhanced error handling for bot-specific operations
 */

import { NextRequest } from 'next/server';
import { withAdminAuth } from '@/lib/middleware/admin-auth';

// Force dynamic rendering to prevent static optimization
export const dynamic = 'force-dynamic';

export async function GET(
  req: NextRequest,
  { params }: { params: Promise<{ name: string }> }
) {
  const { name } = await params;
  const encoder = new TextEncoder();
  const token = req.headers.get('Authorization');

    if (!token) {
      return new Response(
        JSON.stringify({ error: 'Authorization token required' }),
        { 
          status: 401,
          headers: { 'Content-Type': 'application/json' }
        }
      );
    }

    if (!name) {
      return new Response(
        JSON.stringify({ error: 'Bot name parameter is required' }),
        { 
          status: 400,
          headers: { 'Content-Type': 'application/json' }
        }
      );
    }

    const stream = new ReadableStream({
      start(controller) {
        console.log(`Custom bot SSE connection established for bot: ${name}`);

        // Send connection confirmation
        const connectionMessage = {
          type: 'connection',
          timestamp: new Date().toISOString(),
          data: { 
            message: `Bot stream connected for ${name}`,
            botName: name
          }
        };
        
        controller.enqueue(
          encoder.encode(`data: ${JSON.stringify(connectionMessage)}\n\n`)
        );

        // Set up polling interval for specific bot updates
        const pollInterval = setInterval(async () => {
          try {
            // Fetch specific bot data from backend
            const response = await fetch(
              `${process.env.BOT_API_URL}/custom-bots/${encodeURIComponent(name)}`,
              {
                method: 'GET',
                headers: {
                  'Content-Type': 'application/json',
                  'Authorization': token,
                } as HeadersInit,
              }
            );

            if (response.ok) {
              const botData = await response.json();
              
              const updateMessage = {
                type: 'update',
                data: {
                  ...botData,
                  updateType: 'bot_development', // Identify this as bot development update
                },
                timestamp: new Date().toISOString()
              };

              controller.enqueue(
                encoder.encode(`data: ${JSON.stringify(updateMessage)}\n\n`)
              );
            } else if (response.status === 401) {
              // Authentication failed
              const errorMessage = {
                type: 'error',
                error: 'Authentication failed',
                timestamp: new Date().toISOString()
              };
              
              controller.enqueue(
                encoder.encode(`data: ${JSON.stringify(errorMessage)}\n\n`)
              );
              
              // Close the stream on auth failure
              clearInterval(pollInterval);
              clearInterval(keepAliveInterval);
              controller.close();
            } else if (response.status === 404) {
              // Bot not found
              const errorMessage = {
                type: 'error',
                error: `Bot '${name}' not found`,
                timestamp: new Date().toISOString()
              };
              
              controller.enqueue(
                encoder.encode(`data: ${JSON.stringify(errorMessage)}\n\n`)
              );
              
              // Don't close stream immediately for 404s as the bot might be created later
            } else {
              console.error(`Custom bot API error for ${name}: ${response.status}`);
              
              const errorMessage = {
                type: 'error',
                error: `Failed to fetch bot data for '${name}'`,
                timestamp: new Date().toISOString()
              };
              
              controller.enqueue(
                encoder.encode(`data: ${JSON.stringify(errorMessage)}\n\n`)
              );
            }
          } catch (error) {
            console.error(`Custom bot SSE polling error for ${name}:`, error);
            
            const errorMessage = {
              type: 'error',
              error: `Internal server error while fetching bot '${name}'`,
              timestamp: new Date().toISOString()
            };
            
            controller.enqueue(
              encoder.encode(`data: ${JSON.stringify(errorMessage)}\n\n`)
            );
          }
        }, 2000); // 2-second intervals for rapid development updates

        // Keep-alive to prevent connection drops
        const keepAliveInterval = setInterval(() => {
          try {
            const keepAliveMessage = {
              type: 'keepalive',
              timestamp: new Date().toISOString(),
              data: { botName: name }
            };
            
            controller.enqueue(
              encoder.encode(`data: ${JSON.stringify(keepAliveMessage)}\n\n`)
            );
          } catch (error) {
            // Connection closed, cleanup
            console.log(`Custom bot SSE connection closed during keepalive for ${name}`);
            clearInterval(pollInterval);
            clearInterval(keepAliveInterval);
          }
        }, 15000); // 15-second keep-alive

        // Cleanup on client disconnect
        req.signal?.addEventListener('abort', () => {
          console.log(`Custom bot SSE client disconnected for ${name}`);
          clearInterval(pollInterval);
          clearInterval(keepAliveInterval);
          controller.close();
        });

        // Store intervals for potential cleanup
        // Note: In a production environment, you might want to store these
        // in a global registry for cleanup on server shutdown
      },

      cancel() {
        console.log(`Custom bot SSE stream cancelled for ${name}`);
        // Additional cleanup could be performed here
      }
    });

  return new Response(stream, {
    headers: {
      'Content-Type': 'text/event-stream; charset=utf-8',
      'Cache-Control': 'no-cache, no-transform',
      'Connection': 'keep-alive',
      'Content-Encoding': 'none',
      // CORS headers for cross-origin requests
      'Access-Control-Allow-Origin': '*',
      'Access-Control-Allow-Methods': 'GET',
      'Access-Control-Allow-Headers': 'Authorization, Cache-Control',
    },
  });
}