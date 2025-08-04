import { NextRequest, NextResponse } from 'next/server';
import { withAdminAuth } from '@/lib/middleware/admin-auth';

export async function GET(
  req: NextRequest,
  { params }: { params: Promise<{ name: string }> },
) {
  return withAdminAuth(req, async (req: NextRequest) => {
    const { name } = await params;
    const token = req.headers.get('Authorization');

    try {
      const response = await fetch(
        `${process.env.BOT_API_URL}/custom-bots/${encodeURIComponent(name)}`,
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
          { error: 'Failed to fetch bot versions' },
          { status: response.status },
        );
      }

      const result = await response.json();

      // Extract versions from CustomBotWithVersions
      if (result.success && result.data && result.data.versions) {
        const versions = result.data.versions.map((v: any) => v.version);
        return NextResponse.json({
          success: true,
          data: versions,
        });
      }

      return NextResponse.json({ success: false, error: 'No versions found' });
    } catch (error) {
      console.error('Error fetching bot versions:', error);
      return NextResponse.json(
        { error: 'Internal server error' },
        { status: 500 },
      );
    }
  });
}
