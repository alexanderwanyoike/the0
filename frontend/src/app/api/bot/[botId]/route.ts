import { NextRequest, NextResponse } from 'next/server';
import { withAdminAuth } from '@/lib/middleware/admin-auth';

export async function GET(
  req: NextRequest,
  { params }: { params: Promise<{ botId: string }> },
) {
  return withAdminAuth(req, async (req: NextRequest) => {
    try {
      const { botId } = await params;
      const token = req.headers.get('Authorization');
      const response = await fetch(`${process.env.BOT_API_URL}/bot/${botId}`, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
          Authorization: token,
        } as HeadersInit,
      });
      if (!response.ok) {
        return NextResponse.json(
          { error: 'Error fetching bots' },
          { status: response.status },
        );
      }
      const data = await response.json();
      return NextResponse.json(data);
    } catch (error: any) {
      console.error('Error fetching bots:', error);
      return NextResponse.json(
        { error: 'Error fetching bots' },
        { status: 500 },
      );
    }
  });
}

export async function DELETE(
  req: NextRequest,
  { params }: { params: Promise<{ botId: string }> },
) {
  return withAdminAuth(req, async (req: NextRequest) => {
    try {
      const { botId } = await params;
      const token = req.headers.get('Authorization');

      const response = await fetch(`${process.env.BOT_API_URL}/bot/${botId}`, {
        method: 'DELETE',
        headers: {
          'Content-Type': 'application/json',
          Authorization: token,
        } as HeadersInit,
      });
      
      if (!response.ok) {
        let errorMessage = 'Error deleting bot';
        try {
          const errorData = await response.json();
          errorMessage = errorData.message || errorData.error || errorMessage;
        } catch (e) {
          // If response is not JSON, use status text
          errorMessage = response.statusText || errorMessage;
        }
        return NextResponse.json(
          { error: errorMessage },
          { status: response.status },
        );
      }
      
      // Handle successful deletion - might return empty response
      let data = null;
      try {
        const text = await response.text();
        if (text) {
          data = JSON.parse(text);
        }
      } catch (e) {
        // If no JSON response, that's fine for deletion
        data = { success: true };
      }
      
      return NextResponse.json(data || { success: true });
    } catch (error: any) {
      console.error('Error deleting bot:', error);
      return NextResponse.json(
        { error: error.message || 'Error deleting bot' },
        { status: 500 },
      );
    }
  });
}

export async function PUT(
  req: NextRequest,
  { params }: { params: Promise<{ botId: string }> },
) {
  return withAdminAuth(req, async (req: NextRequest) => {
    try {
      const token = req.headers.get('Authorization');
      const { botId } = await params;
      const bot = await req.json();
      console.log('Updating bot with body:', bot);
      const response = await fetch(`${process.env.BOT_API_URL}/bot/${botId}`, {
        method: 'PUT',
        headers: {
          'Content-Type': 'application/json',
          Authorization: token,
        } as HeadersInit,
        body: JSON.stringify(bot),
      });
      if (!response.ok) {
        console.error('Error updating bot:', response.statusText);
        return NextResponse.json(
          { error: await response.json() },
          { status: response.status },
        );
      }
      const data = await response.json();
      return NextResponse.json(data);
    } catch (error: any) {
      console.error('Error updating bot:', error);
      return NextResponse.json(
        {
          error: {
            message: 'Error creating bot',
            statusCode: 500,
            error: 'Internal Server Error',
          },
        },
        { status: 500 },
      );
    }
  });
}
