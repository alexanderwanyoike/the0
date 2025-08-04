import { NextRequest, NextResponse } from 'next/server';
import { withAdminAuth } from '@/lib/middleware/admin-auth';

export async function GET(req: NextRequest) {
  return withAdminAuth(req, async (req: NextRequest) => {
    try {
      // Verify authentication
      const token = req.headers.get('Authorization');
      // Delegate to the bot API
      const response = await fetch(`${process.env.BOT_API_URL}/user-bots`, {
        method: 'GET',
        headers: {
          'Content-Type': 'application/json',
          Authorization: token,
        } as HeadersInit,
      });

      if (!response.ok) {
        const errorData = await response.json();
        return NextResponse.json(
          {
            success: false,
            error: errorData.message || 'Failed to get user bots',
            message: errorData.message || 'Failed to get user bots',
          },
          { status: response.status },
        );
      }

      const result = await response.json();
      return NextResponse.json(result);
    } catch (error: any) {
      console.error('Error getting user bots:', error);
      return NextResponse.json(
        {
          success: false,
          error: 'Failed to get user bots',
          message: error.message,
        },
        { status: 500 },
      );
    }
  });
}

export async function POST(req: NextRequest) {
  return withAdminAuth(req, async (req: NextRequest) => {
    try {
      // Verify authentication
      const authHeader = req.headers.get('authorization');
      if (!authHeader?.startsWith('Bearer ')) {
        return NextResponse.json({ error: 'Unauthorized' }, { status: 401 });
      }

      const body = await req.json();
      const { customBotName, customBotVersion, price = 0 } = body;

      if (!customBotName || !customBotVersion) {
        return NextResponse.json(
          { error: 'Custom bot name and version are required' },
          { status: 400 },
        );
      }

      // Only allow free bot installations through this route
      // Paid bots should go through the Stripe checkout flow
      if (price > 0) {
        return NextResponse.json(
          { error: 'Paid bots must be purchased through Stripe checkout' },
          { status: 400 },
        );
      }

      // Delegate to the bot API for free bot installation
      const response = await fetch(
        `${process.env.BOT_API_URL}/user-bots/install/${encodeURIComponent(customBotName)}`,
        {
          method: 'POST',
          headers: {
            'Content-Type': 'application/json',
            Authorization: authHeader,
          } as HeadersInit,
          body: JSON.stringify({
            version: customBotVersion,
            source: 'marketplace',
          }),
        },
      );

      if (!response.ok) {
        const errorData = await response.json();
        return NextResponse.json(
          {
            error: errorData.message || 'Installation failed',
            details: errorData,
          },
          { status: response.status },
        );
      }

      const result = await response.json();
      return NextResponse.json(result);
    } catch (error: any) {
      console.error('Error installing bot:', error);
      return NextResponse.json(
        {
          success: false,
          error: 'Failed to install bot',
          message: error.message,
        },
        { status: 500 },
      );
    }
  });
}
