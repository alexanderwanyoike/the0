import { NextRequest, NextResponse } from "next/server";
import { DocsFileSystem } from "@/lib/docs/file-system";

export async function GET(request: NextRequest) {
  try {
    const fileSystem = new DocsFileSystem();
    const navigation = await fileSystem.getNavigationTree();

    return NextResponse.json({
      success: true,
      data: navigation,
    });
  } catch (error: any) {
    console.error("Error fetching docs navigation:", error);
    return NextResponse.json(
      {
        success: false,
        error: "Failed to load documentation navigation",
      },
      { status: 500 },
    );
  }
}
