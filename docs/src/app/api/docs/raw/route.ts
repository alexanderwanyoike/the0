import { NextResponse } from "next/server";
import { DocsFileSystem } from "@/lib/docs/file-system";

/**
 * GET /api/docs/raw
 * Returns a list of all available documentation files for LLM navigation
 */
export async function GET() {
  try {
    const fileSystem = new DocsFileSystem();
    const navigation = await fileSystem.getNavigationTree();

    // Flatten the navigation tree into a list of paths with metadata
    const flattenDocs = (items: any[], prefix: string = ""): any[] => {
      let result: any[] = [];

      for (const item of items) {
        const path = prefix ? `${prefix}/${item.slug}` : item.slug;

        if (item.type === "file") {
          result.push({
            path: path,
            url: `/api/docs/raw/${path}/`,
            title: item.title,
            description: item.description,
          });
        }

        if (item.children && item.children.length > 0) {
          result = result.concat(flattenDocs(item.children, path));
        }
      }

      return result;
    };

    const docs = flattenDocs(navigation);

    return NextResponse.json({
      success: true,
      count: docs.length,
      docs: docs,
    });
  } catch (error) {
    console.error("Error fetching docs index:", error);
    return NextResponse.json(
      { success: false, error: "Failed to fetch docs index" },
      { status: 500 },
    );
  }
}
