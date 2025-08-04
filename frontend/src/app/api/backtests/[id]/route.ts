import { NextRequest, NextResponse } from 'next/server';
import { withAdminAuth } from '@/lib/middleware/admin-auth';

export async function GET(
  req: NextRequest,
  { params }: { params: Promise<{ id: string }> },
) {
  return withAdminAuth(req, async (req: NextRequest) => {
    try {
      const { id } = await params;
      const token = req.headers.get('Authorization');
      const response = await fetch(
        `${process.env.BOT_API_URL}/backtest/${id}`,
        {
          method: 'GET',
          headers: {
            'Content-Type': 'application/json',
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
      return NextResponse.json(
        { error: { message: 'Error fetching backtest', statusCode: 500 } },
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
      const { id } = await params;
      const token = req.headers.get('Authorization');
      const response = await fetch(
        `${process.env.BOT_API_URL}/backtest/${id}`,
        {
          method: 'DELETE',
          headers: {
            'Content-Type': 'application/json',
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

      return NextResponse.json(null, { status: 204 });
    } catch (error: any) {
      return NextResponse.json(
        { error: { message: 'Error deleting backtest', statusCode: 500 } },
        { status: 500 },
      );
    }
  });
}
