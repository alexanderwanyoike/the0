import { NextRequest, NextResponse } from 'next/server';
import { DocsSearchService } from '@/lib/docs/search-service';

export async function GET(request: NextRequest) {
  try {
    const { searchParams } = new URL(request.url);
    const query = searchParams.get('q');
    const type = searchParams.get('type') || 'search'; // 'search' or 'suggestions'

    if (!query || query.length < 2) {
      return NextResponse.json({
        success: true,
        data: [],
      });
    }

    const searchService = DocsSearchService.getInstance();

    // Ensure index is built
    await searchService.indexDocuments();

    let results;
    if (type === 'suggestions') {
      results = await searchService.getSuggestions(query);
    } else {
      results = await searchService.search(query);
    }

    return NextResponse.json({
      success: true,
      data: results,
    });
  } catch (error: any) {
    console.error('Error performing docs search:', error);
    return NextResponse.json(
      {
        success: false,
        error: 'Failed to search documentation',
      },
      { status: 500 },
    );
  }
}
