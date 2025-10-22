import { NextRequest, NextResponse } from "next/server";
import { DocsFileSystem } from "@/lib/docs/file-system";

/**
 * GET /api/docs/raw/[...slug]
 * Returns raw markdown content for LLM consumption
 */
export async function GET(
  request: NextRequest,
  { params }: { params: Promise<{ slug: string[] }> },
) {
  try {
    const { slug } = await params;
    const fileSystem = new DocsFileSystem();
    const doc = await fileSystem.getDocContent(slug);

    if (!doc) {
      return NextResponse.json(
        { error: "Document not found" },
        { status: 404 },
      );
    }

    // Return raw markdown as plain text
    return new NextResponse(doc.content, {
      status: 200,
      headers: {
        "Content-Type": "text/markdown; charset=utf-8",
      },
    });
  } catch (error) {
    console.error("Error fetching raw markdown:", error);
    return NextResponse.json(
      { error: "Failed to fetch document" },
      { status: 500 },
    );
  }
}
